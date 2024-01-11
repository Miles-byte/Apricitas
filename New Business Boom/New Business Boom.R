pacman::p_load(ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

BIZ_APPS <- fredr("BABATOTALSAUS", observation_start = as.Date("2016-01-01"))
HPA_BIZ_APPS <- fredr("BAHBATOTALSAUS", observation_start = as.Date("2016-01-01"))
PLW_BIZ_APPS <- fredr("BAWBATOTALSAUS", observation_start = as.Date("2016-01-01"))

BIZ_APPS_GRAPH <- ggplot() + #Graphing Business Applications Data
  geom_line(data=BIZ_APPS, aes(x=date,y= value/1000, color= "Business Applications"), size = 1.25) +
  geom_line(data=HPA_BIZ_APPS, aes(x=date,y= value/1000, color= "Business Applications with High Propensity of Hiring"), size = 1.25) +
  geom_line(data=PLW_BIZ_APPS, aes(x=date,y= value/1000, color= "Business Applications with Planned Wages"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,650), expand = c(0,0)) +
  ylab("Number of Business Applications, Monthly") +
  ggtitle("America's New Business Boom") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Americans Continue to Found New Businesses at Near-Record Rates") +
  theme_apricitas + theme(legend.position = c(.325,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Business Applications","Business Applications with High Propensity of Hiring","Business Applications with Planned Wages")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIZ_APPS_GRAPH, "Biz Apps.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BFS_Data <- getCensus(
  name = "timeseries/eits/bfs",
  vars = c("data_type_code","time_slot_id","seasonally_adj", "program_code","category_code","geo_level_code","time_slot_date", "time_slot_name","cell_value","error_data"),
  time = paste("from 2016 to", format(Sys.Date(), "%Y")),
  seasonally_adj = "yes",
  data_type_code = "BA_BA",
  region = "us"
)

#https://www.census.gov/econ/bfs/pdf/bfs_monthly_data_dictionary.pdf

BFS_Data <- BFS_Data %>%
  transmute(category_code,value = cell_value, time = as.Date(paste0(time, "-01"))) %>%
  pivot_wider(names_from = category_code) %>%
  setNames(c("date","Transportation & Warehousing","Total","NoNAICS","Retail Trade","Agriculture","Mining","Construction","Wholesale Trade","Information","Utilities","Arts & Entertainment","Food Services & Accomodation","Other Services","Manufacturing","Health Care & Social Assistance","Finance & Insurance","Administrative & Support","Education Services","Professional Services","Real Estate","Management of Companies")) %>%
  mutate(across(where(is.character), as.numeric))

BIZ_APPS_DETAIL_GRAPH <- ggplot() + #Graphing Business Applications Data
  geom_line(data=BFS_Data, aes(x=date,y= `Food Services & Accomodation`/1000, color= "Food Services & Accomodation"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= `Health Care & Social Assistance`/1000, color= "Healthcare & Social Assistance"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= `Administrative & Support`/1000, color= "Admin, Support, & Sanitation Services"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= `Transportation & Warehousing`/1000, color= "Transportation & Warehousing"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= (`Finance & Insurance`+`Real Estate`)/1000, color= "Finance, Insurance, & Real Estate"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= `Construction`/1000, color= "Construction"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= `Professional Services`/1000, color= "Professional, Scientific, & Technical Services"), size = 1.25) +
  geom_line(data=BFS_Data, aes(x=date,y= `Retail Trade`/1000, color= "Retail Trade"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,125), expand = c(0,0)) +
  ylab("Number of Business Applications, Monthly") +
  ggtitle("America's New Business Boom") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Business Applications are Up, With Retail, Professional Services, Construction, & Finance Leading") +
  theme_apricitas + theme(legend.position = c(.28,.75), legend.spacing.y = unit(0, 'cm'), legend.key.height = unit(0.45, "cm"),legend.text = (element_text(size = 14)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Business Applications by Industry" ,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("Retail Trade","Professional, Scientific, & Technical Services","Construction","Finance, Insurance, & Real Estate","Transportation & Warehousing","Admin, Support, & Sanitation Services","Healthcare & Social Assistance","Food Services & Accomodation")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*125), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIZ_APPS_DETAIL_GRAPH, "Biz Apps Detail.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



SELF_EMP_UNINC <- fredr("LNS12027714", observation_start = as.Date("2000-01-01"), aggregation_method = "avg",frequency = "a") %>%
  transmute(date, value, category = "Self-Employed, Unincorporated")
SELF_EMP_INC <- fredr("LNU02048984", observation_start = as.Date("2000-01-01"), aggregation_method = "avg",frequency = "a") %>%
  transmute(date, value, category = "Self-Employed, Incorporated")

SELF_EMP <- rbind(SELF_EMP_UNINC,SELF_EMP_INC)


SELF_EMP_GRAPH <- ggplot() + #Graphing Business Applications Data
  geom_line(data=SELF_EMP_UNINC, aes(x=date,y= value/1000, color= "Self Employed, Unincorporated"), size = 1.25) +
  geom_line(data=SELF_EMP_INC, aes(x=date,y= value/1000, color= "Self Employed, Incorporated"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), limits = c(0,11), expand = c(0,0)) +
  ylab("Number of Self-Employed, Millions, Annual") +
  ggtitle("America's Growing Self Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Self Employment Continues Growing, and Incorporated Self-Employment is at Record Highs") +
  theme_apricitas + theme(legend.position = c(.325,.7)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Self Employed, Unincorporated","Self Employed, Incorporated")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*11), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SELF_EMP_GRAPH, "self emp line.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SELF_EMP_Graph <- ggplot(data = SELF_EMP, aes(x = date, y = value/1000, fill = category)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Number of Self-Employed, Millions, Annual") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"), breaks = c(0,5,10,15,20), limits = c(0,20), expand = c(0,0)) +
  ggtitle("America's Self Employment Boom") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "Self Employment Continues Growing, and Incorporated Self-Employment is at Record Highs") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SELF_EMP_Graph, "self emp bar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PROP_INCOME <- fredr("A041RC1", observation_start = as.Date("2016-01-01"))
PCEPI <- fredr("PCEPI", observation_start = as.Date("2016-01-01"))

REAL_PROP_INCOME <- merge(PROP_INCOME,PCEPI, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

REAL_PROP_INCOME_Graph <- ggplot() + #indexed employment rate
  geom_line(data = REAL_PROP_INCOME, aes(x=date, y = value/1000, color = "Real Proprietor's Income\nWith Inventory Valuation &\nCapital Consumption Adjustments"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.1), breaks = c(1.3,1.4,1.5,1.6,1.7), limits = c(1.25,1.75), expand = c(0,0)) +
  ylab("Trillions of 2017 Dollars") +
  ggtitle("Real Self-Employed Income Has Stagnated") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Proprietor's Income Has Stagnated Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 1.25-(.3*.50), ymax = 1.25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PROP_INCOME_Graph, "real prop inc.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#https://www.census.gov/programs-surveys/bds/news-updates/updates.html

BUSINESS_BANKRUPTCY_FILINGS <- data.frame(date = as.Date(c("2016-03-01","2017-03-01","2018-03-01","2019-03-01","2020-03-01","2021-03-01","2022-03-01","2023-03-01")),
                                          value = c(24457,23109,22103,22910,22391,16140,13125,17051))

BUSINESS_BANKRUPTCY_FILINGS_Graph <- ggplot() + #Bankruptcy
  geom_line(data = BUSINESS_BANKRUPTCY_FILINGS, aes(x=date, y = value/1000, color = "US Business Bankruptcies"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ylab("Business Bankruptcies, Year Ending September") +
  ggtitle("Bankruptcies are Up but Below 2019 Levels") +
  labs(caption = "Graph created by @JosephPolitano using US Court data",subtitle = "Bankruptcies Declines Significantly During Early-COVID, but Have Partially Risen Since") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-03-01")-(.1861*(today()-365-as.Date("2016-03-01"))), xmax = as.Date("2016-03-01")-(0.049*(today()-365-as.Date("2016-03-01"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BUSINESS_BANKRUPTCY_FILINGS_Graph, "Biz Bankruptcy Filings.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SELF_EMPLOYMENT_TRANS <- bls_api("LNU02037801", startyear = 2014, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(year) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, "-01-01")), value)
  
SELF_EMPLOYMENT_TRANS_Graph <- ggplot() + #Bankruptcy
  geom_line(data = SELF_EMPLOYMENT_TRANS, aes(x=date, y = value, color = "Self Employed, Transportation & Warehousing, Unincorporated"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,250,500,750), limits = c(0,750), expand = c(0,0)) +
  ylab("Employment, Thousands, Annual Average") +
  ggtitle("The Number of Self-Employed Drivers is Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Self-Employment in Transportation & Warehousing is Booming, Driven by Ridesharing and Trucking") +
  theme_apricitas + theme(legend.position = c(.40,.95), plot.title = element_text(size = 24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-365-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-365-as.Date("2014-01-01"))), ymin = 0-(.3*750), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SELF_EMPLOYMENT_TRANS_Graph, "Self Employment Trans.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
  
ESTABLISHMENT_BIRTHS <- bls_api("BDS0000000000000000110007LQ5", startyear = 2016) %>% #headline cpi data
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y")))

ESTABLISHMENT_BIRTHS_Graph <- ggplot() + #Employment Gains Establishment Births
  geom_line(data = ESTABLISHMENT_BIRTHS, aes(x=date, y = value/1000, color = "Employment Gains From Establishment Births, Quarterly"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(0.6,0.7,0.8,0.9,1,1.1), limits = c(0.6,1.15), expand = c(0,0)) +
  ylab("Employment Growth, Millions, Quarterly") +
  ggtitle("New Establishments are Hiring Rapidly") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Establishment Births—New Workplaces at New or Existing Firms—are Driving More Job Growth") +
  theme_apricitas + theme(legend.position = c(.40,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-365-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-365-as.Date("2016-01-01"))), ymin = 0.6-(.3*.55), ymax = 0.6) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ESTABLISHMENT_BIRTHS_Graph, "Establishment Births Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EST_BIRTHS_TRANS <- bls_api("BDS0000000000200030110007LQ5", startyear = 2016, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(year) %>%
  filter(n() == 4) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, "-01-01")), value)

EST_BIRTHS_MANUF <- bls_api("BDS0000000000100030110007LQ5", startyear = 2016, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(year) %>%
  filter(n() == 4) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, "-01-01")), value)

ESTABLISHMENT_BIRTHS_TRANS_MANU_Graph <- ggplot() + #Employment Gains Establishment Births
  geom_line(data = EST_BIRTHS_TRANS, aes(x=date, y = value, color = "Transportation & Warehousing"), size = 1.25) +
  geom_line(data = EST_BIRTHS_MANUF, aes(x=date, y = value, color = "Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,50,100,150,200,250), limits = c(0,250), expand = c(0,0)) +
  ylab("Employment Growth, Thousands, Annual") +
  ggtitle("Jobs at New Transport & Manufacturing Establishments") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "New Transportation and Manufacturing Establishments Have Boomed During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.35,.75), plot.title = element_text(size = 21)) +
  scale_color_manual(name= "Employment Gains From Establishment Births, Annual",values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Transportation & Warehousing", "Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-365-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-365-as.Date("2016-01-01"))), ymin = 0-(.3*250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ESTABLISHMENT_BIRTHS_TRANS_MANU_Graph, "Establishment Births Trans Manu Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EST_BIRTHS_PROF <- bls_api("BDS0000000000200070110007LQ5", startyear = 2016, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(year) %>%
  filter(n() == 4) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, "-01-01")), value)

EST_BIRTHS_EDU <- bls_api("BDS0000000000200080110007LQ5", startyear = 2016, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(year) %>%
  filter(n() == 4) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, "-01-01")), value)

ESTABLISHMENT_BIRTHS_EDUC_PROF_Graph <- ggplot() + #Employment Gains Establishment Births
  geom_line(data = EST_BIRTHS_EDU, aes(x=date, y = value, color = "Healthcare & Private Education Services"), size = 1.25) +
  geom_line(data = EST_BIRTHS_PROF, aes(x=date, y = value, color = "Professional & Business Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,250,500,750), limits = c(0,850), expand = c(0,0)) +
  ylab("Employment Growth, Thousands, Annual") +
  ggtitle("Jobs at New Business & Healthcare Establishments") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "New Business and Healthcare Establishments Have Boomed During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.35,.875), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Employment Gains From Establishment Births, Annual",values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Professional & Business Services", "Healthcare & Private Education Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-365-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-365-as.Date("2016-01-01"))), ymin = 0-(.3*850), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ESTABLISHMENT_BIRTHS_EDUC_PROF_Graph, "Establishment Births Health Educ Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EST_OPEN_SEARCH <- bls_api("BDS0000000000300519110003LQ5", startyear = 2016, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y")))

EST_OPEN_INFRA <- bls_api("BDS0000000000300518110003LQ5", startyear = 2016, endyear = 2024, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y")))

ESTABLISHMENT_OPENINGS_TECH_Graph <- ggplot() + #Employment Gains Establishment Births
  geom_line(data = EST_OPEN_SEARCH, aes(x=date, y = value/1000, color = "Web Search Portals, Libraries, Archives, and Other Information Services"), size = 1.25) +
  geom_line(data = EST_OPEN_INFRA, aes(x=date, y = value/1000, color = "Computing Infrastructure Providers, Data Processing, Web Hosting, and Related Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,2,4,6,8), limits = c(0,9), expand = c(0,0)) +
  ylab("Employment Growth, Thousands, Annual") +
  ggtitle("Jobs at Opening Tech Establishments") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "New Business and Healthcare Establishments Have Boomed During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.525,.15)) +
  scale_color_manual(name= "Employment Gains From Establishment Openings, Quarterly",values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-450-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-450-as.Date("2016-01-01"))), ymin = 0-(.3*9), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ESTABLISHMENT_OPENINGS_TECH_Graph, "Establishment Openings Tech Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TELEWORK_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Business%20Boom/Remote_Work_Data_CSV.csv") %>%
  mutate(worker = factor(worker, levels = rev(c("Unincorporated Self-Employed", "Incorporated Self-Employed", "Private Sector", "Government")))) %>%
  mutate(category = factor(category, levels = rev(c("Fully Remote","Hybrid"))))

TELEWORK_DATA_graph <- ggplot(data = TELEWORK_DATA, aes(x = worker, y = value, fill = category)) + #plotting Deposits, Insured and Uninsured
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Class of Worker") +
  ylab("Percent of Workers") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.35), expand = c(0,0)) +
  scale_x_discrete(breaks = c("Unincorporated Self-Employed","Incorporated Self-Employed","Private Sector","Government"), labels = c("Self-Employed\nUnincorporated","Self-Employed\nIncorporated","Private Sector","Government"), expand = c(0,0)) +
  ggtitle("Telework Rate by Worker Type") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Self-Employed Workers Had the Highest Rates of Telework in 2023") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Share of Workers, 2023",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Fully Remote","Hybrid")) +
  coord_flip()

ggsave(dpi = "retina",plot = TELEWORK_DATA_graph, "Telework Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


https://www.federalreserve.gov/publications/files/scf23.pdf

#https://www.uscourts.gov/news/2023/10/26/bankruptcy-filings-rise-13-percent

https://qwiexplorer.ces.census.gov/#x=0&g=0

#CEX DATA IS AN AVERAGE
  
#LOOK AT TRUCKING FORMATIONS

#BEA SELF EMPLOYMENT PROPRIETORS INCOME
#SELF EMPLOYMENT TELEWORK DATA

#LOOK AT STEM BUSINESS FORMATION DATA

#MARCH-TO-MARCH DATA BUSINESS FORMATION

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

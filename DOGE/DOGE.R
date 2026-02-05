pacman::p_load(readr,janitor,bea.R,RcppRoll,sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FED_WFH_PCT <- bls_api("LNU0201BAC7", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  mutate(series_id = "Federal") %>%
  drop_na()

PRI_WFH_PCT <- bls_api("LNU0201BAE5", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  mutate(series_id = "Federal") %>%
  drop_na()


FED_WFH_PCT_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=FED_WFH_PCT, aes(x=date,y= value/100,color= "Federal Employees, % of All Hours Worked From Home"), size = 1.25) +
  geom_line(data=PRI_WFH_PCT, aes(x=date,y= value/100,color= "Private Sector Employees, % of All Hours Worked From Home"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.25), breaks = c(0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Percent of All Hours Worked") +
  ggtitle("DOGE Reduced Federal Work From Home") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Federal Work From Home Hours Were Cut By More than 50% Amidst DOGE") +
  theme_apricitas + theme(legend.position = c(.4,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-10-01")-(.1861*(today()-as.Date("2022-10-01"))), xmax = as.Date("2022-10-01")-(0.049*(today()-as.Date("2022-10-01"))), ymin = 0-(.3*0.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_WFH_PCT_Graph, "Federal WFH Percent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FED_EMP_IND_2025 <- bls_api("CES9091000001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Total Federal Government") %>%
  select(date,value,name,pct)

CIV_EMP_IND_2025 <- bls_api("CES9091999901", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Civilian Agencies") %>%
  select(date,value,name,pct)

DOD_EMP_IND_2025 <- bls_api("CES9091911001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Department of Defense") %>%
  select(date,value,name,pct)

POS_EMP_IND_2025 <- bls_api("CES9091912001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Post Office") %>%
  select(date,value,name,pct)

MED_EMP_IND_2025 <- bls_api("CES9091622001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Federal Hospitals (VA/IHS)") %>%
  select(date,value,name,pct)

FED_RBIND <- rbind(CIV_EMP_IND_2025,DOD_EMP_IND_2025,POS_EMP_IND_2025,MED_EMP_IND_2025) %>%
  group_by(date) %>%
  filter(n() == 4)

FED_EMP_IND_2025 <- FED_EMP_IND_2025 %>%
  filter(date <= max(FED_RBIND$date))

US_FED_EMP_IND_DECOMP_graph <- ggplot(data = filter(FED_RBIND, date >= as.Date("2023-01-01")), aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(FED_EMP_IND_2025, date >= as.Date("2023-01-01")), aes(x=date, y = value, color = "Total Federal Government"), size = 2) +
  xlab("Date") +
  ylab("Change in Jobs, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-300,-200,-100,0), limits = c(-300,50), expand = c(0,0)) +
  ggtitle("US Federal Employment is Falling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Federal Employment has Decreased by ", -FED_EMP_IND_2025$value[1], "k, or ", -round(FED_EMP_IND_2025$pct[1],4)*100,"% since January amidst DOGE cuts")) +
  theme_apricitas + theme(legend.position = c(.35,.25)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= "Change in Federal Employment Since Jan 2025",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Post Office","Federal Hospitals (VA/IHS)","Department of Defense","Civilian Agencies")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-01")-(.1861*(today()-as.Date("2025-01-01"))), xmax = as.Date("2025-01-01")-(0.049*(today()-as.Date("2025-01-01"))), ymin = -300-(.3*350), ymax = -300) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_FED_EMP_IND_DECOMP_graph, "US Fed Empy Ind Decomp.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FED_UNEMP <- bls_api("LNU03097260", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  select(date,value,name) %>%
  arrange(date) %>%
  mutate(roll = c(0,0,rollmean(value,3)))

FED_UNEMP <- bls_api("LNU03000000", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  select(date,value,name) %>%
  arrange(date) %>%
  mutate(roll = c(0,0,rollmean(value,3)))


US_FED_UNEMP_graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data = filter(FED_UNEMP, date >= as.Date("2022-01-01")), aes(x=date, y = roll, color = "Unemployed Former Federal Employees"), size = 1.25) +
  geom_line(data = filter(FED_UNEMP, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Unemployed Former Federal Employees"), size = 0.75,linetype = "dashed", alpha = 0.75) +
  xlab("Date") +
  ylab("Unemployment Level") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200), limits = c(0,250), expand = c(0,0)) +
  ggtitle("200K Former Federal Employees are Unemployed") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("The Number of Unemployed Former Federal Employees Has Doubled to More than 200k Amidst DOGE")) +
  theme_apricitas + theme(legend.position = c(.35,.85)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_color_manual(name= "Dashed = Monthly, Solid = 3M Moving Average\nNot Seasonally Adjusted",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = US_FED_UNEMP_graph, "US Fed Unemp Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


FED_HIRES <- bls_api("JTS910000000000000HIL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Federal Hires") %>%
  select(date,value,name)

FED_FIRES <- bls_api("JTS910000000000000LDL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Federal Fires") %>%
  select(date,value,name)

FED_RETR <- bls_api("JTS910000000000000OSL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Federal Retirements") %>%
  select(date,value,name)

FED_QUITS <- bls_api("JTS910000000000000QUL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Federal Quits") %>%
  select(date,value,name)

FED_LAB_FLO_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data = filter(FED_HIRES, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Hires"), size = 1.25) +
  geom_line(data = filter(FED_FIRES, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Layoffs & Discharges"), size = 1.25) +
  geom_line(data = filter(FED_RETR, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Retirements & Other Separations"), size = 1.25) +
  geom_line(data = filter(FED_QUITS, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Quits"), size = 1.25) +
  annotate(geom = "segment", x = as.Date("2025-01-01"), xend = as.Date("2025-01-01"), y = 0, yend = 55, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "DOGE\nBegins", x = as.Date("2024-08-15"), y = 23, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("text", label = "Defferred\nResignations", x = as.Date("2025-05-01"), y = 50, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Thousands, Monthly") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,10,20,30,40,50,60), limits = c(0,55), expand = c(0,0)) +
  ggtitle("Federal Employee Labor Flows") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Federal Employment Fell Due to Reduced Hires, Increased Quits, More Retirements, and Layoffs")) +
  theme_apricitas + theme(legend.position = c(.40,.65)) + theme(legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Hires","Quits","Retirements & Other Separations","Layoffs & Discharges")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*60), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = FED_LAB_FLO_Graph, "US Fed Labor Flo Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BARGAINING_UNIT <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/DOGE/Federal_workforce_by_bargaining_unit_status_data.csv") %>%
  mutate(Date = as.Date(paste0("01-", Date), format = "%d-%b-%y"))

FED_BARGAINING_UNIT_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data = filter(BARGAINING_UNIT, Date >= as.Date("2023-01-01")), aes(x=Date, y = `In.a.bargaining.unit`/1000000, color = "In a Bargaining Unit"), size = 1.25) +
  geom_line(data = filter(BARGAINING_UNIT, Date >= as.Date("2023-01-01")), aes(x=Date, y = `Eligible.but.not.in.bargaining.unit`/1000000, color = "Eligible but not in a Bargaining Unit"), size = 1.25) +
  geom_line(data = filter(BARGAINING_UNIT, Date >= as.Date("2023-01-01")), aes(x=Date, y = `Not.eligible`/1000000, color = "Not Eligible for Bargaining Unit"), size = 1.25) +
  xlab("Date") +
  ylab("Federal Employees") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, suffix = "M"), breaks = c(0,.25,.5,.75,1,1.25), limits = c(0,1.4), expand = c(0,0)) +
  ggtitle("Federal Employees by Union Status") +
  labs(caption = "Graph created by @JosephPolitano using OMB data",subtitle = paste0("The Number of Unionized Federal Employees Fell By 40% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.40,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("In a Bargaining Unit","Not Eligible for Bargaining Unit","Eligible but not in a Bargaining Unit")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = 0-(.3*1.4), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_BARGAINING_UNIT_Graph, "US Fed Bargaining Unit Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SCIENCE_SERVICES_EMP <- bls_api("CEU6054170001", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(yoy = value-lag(value,12)) %>%
  mutate(name = "Science Services Employment") %>%
  select(date,value,name,yoy)

SCIENCE_SERVICES_EMP_Graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = filter(SCIENCE_SERVICES_EMP, date >= as.Date("2019-01-01")), aes(x=date, y = yoy, color = "Scientific Research & Development Services\nYear-on-year Change in Employment"), size = 1.25) +
  xlab("Date") +
  ylab("Year-on-year Change, Employees") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-25,0,25,50,75,100), limits = c(-25,100), expand = c(0,0)) +
  ggtitle("Science Employment is Falling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Research & Development Employment is Down Amidst Larger Cuts to American Science Funding")) +
  theme_apricitas + theme(legend.position = c(.30,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -25-(.3*125), ymax = -25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SCIENCE_SERVICES_EMP_Graph, "Scientific Services Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_FED_OUTPUT_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T31006',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

BEA_FED_OUTPUT <- beaGet(BEA_FED_OUTPUT_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

BEA_FED_OUTPUT_graph <- ggplot() + #plotting defense and nondefense intermediate goods spending
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = filter(BEA_FED_OUTPUT, date >= as.Date("2023-01-01")), aes(x=date, y = t31006_w087rx_28_intermediate_goods_and_services_purchased_chained_dollars_level_6/t31006_w087rx_28_intermediate_goods_and_services_purchased_chained_dollars_level_6[8]-1, color = "Defense"), size = 1.25) +
  geom_line(data = filter(BEA_FED_OUTPUT, date >= as.Date("2023-01-01")), aes(x=date, y = t31006_w131rx_39_intermediate_goods_and_services_purchased_chained_dollars_level_6/t31006_w131rx_39_intermediate_goods_and_services_purchased_chained_dollars_level_6[8]-1, color = "Nondefense"), size = 1.25) +
  geom_line(data = filter(BEA_FED_OUTPUT, date >= as.Date("2023-01-01")), aes(x=date, y = t31006_w113rx_17_intermediate_goods_and_services_purchased_chained_dollars_level_6/t31006_w113rx_17_intermediate_goods_and_services_purchased_chained_dollars_level_6[8]-1, color = "Total"), size = 2.25) +
  annotate(geom = "segment", x = as.Date("2024-10-01"), xend = as.Date("2024-10-01"), y = -.2, yend = 0, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "DOGE\nBegins", x = as.Date("2024-07-01"), y = -.1, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Change in Real Spending, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.2,0.1), breaks = c(-.2,-.1,0,.1), expand = c(0,0)) +
  ggtitle("US Federal Contractor Spending is Down") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Federal Contractor Spending Is Down, With Cuts Especially Concentrated in Nondefense Spending") +
  theme_apricitas + theme(legend.position = c(.40,.85)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) +
  scale_color_manual(name= "Real Federal Purchases of Intermediate Goods & Services\n(Proxy for Contractor Spending), Change Since Q4 2024",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Defense","Nondefense","Total")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = -.2-(.3*0.3), ymax = -.2) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_FED_OUTPUT_graph, "BEA Fed Output Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_FED_FINANCE_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T30200',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

BEA_FED_FINANCE <- beaGet(BEA_FED_FINANCE_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date,AID = t30200_w017rc_32_to_the_rest_of_the_world_current_dollars_level_6)

BEA_GDP_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10105',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

BEA_GDP <- beaGet(BEA_GDP_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date,GDP = t10105_a191rc_1_gross_domestic_product_current_dollars_level_6)

BEA_GDP_FINANCE_MERGE <- merge(BEA_FED_FINANCE,BEA_GDP, by = "date")

BEA_FED_AID_GDP_graph <- ggplot() + #plotting Aid % of GDP
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = BEA_GDP_FINANCE_MERGE, aes(x=date, y = AID/GDP, color = "Federal Transfers to the Rest of the World\n(Mostly Humanitarian & Military Aid)\n% of GDP"), size = 1.25) +
  annotate(geom = "segment", x = as.Date("2024-10-01"), xend = as.Date("2024-10-01"), y = 0, yend = 0.005, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2022-01-01"), xend = as.Date("2022-01-01"), y = 0, yend = 0.005, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "DOGE\nBegins", x = as.Date("2024-09-01"), y = 0.0015, color = "white", size = 5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("text", label = "Ukraine\nWar", x = as.Date("2022-02-01"), y = 0.002, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("% of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,0.005), breaks = c(0,0.001,0.002,0.003,0.004,0.005), expand = c(0,0)) +
  ggtitle("US International Aid Spending is Down") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Aid Spending as a Share of GDP has Dropped Significantly Since DOGE") +
  theme_apricitas + theme(legend.position = c(.30,.85)) + 
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*0.005), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_FED_AID_GDP_graph, "BEA Fed AID GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


FED_EMP_LONG <- fredr(series_id = "CES9091000001")

PAYEMS_LONG <- fredr(series_id = "PAYEMS")

FED_EMP_PCT <- merge(FED_EMP_LONG,PAYEMS_LONG, by = "date") %>%
  transmute(date, value = value.x/value.y)
  
FED_GDP_1929 <- fredr(series_id = "FGCEA") %>%
  filter(date <= as.Date("1946-01-01"))

FED_GDP_1947 <- fredr(series_id = "FGCE") %>%
  drop_na()

GDP_1929 <- fredr(series_id = "GDPA") %>%
  filter(date <= as.Date("1946-01-01"))

GDP_1947 <- fredr(series_id = "GDP") %>%
  drop_na()

FED_GDP_RBIND <- rbind(FED_GDP_1929,FED_GDP_1947)

GDP_RBIND <- rbind(GDP_1929,GDP_1947)

BEA_GDP_FED_PCT <- merge(FED_GDP_RBIND,GDP_RBIND, by = "date") %>%
  transmute(date, value = value.x/value.y)


FED_GDP_EMPLOYEE_PCT_1929_graph <- ggplot() + #plotting defense and nondefense intermediate goods spending
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = FED_EMP_PCT, aes(x=date, y = value, color = "Federal Employees, % of All Employees"), size = 1.25) +
  geom_line(data = BEA_GDP_FED_PCT, aes(x=date, y = value, color = "Federal Direct Consumption & Investment, % of GDP"), size = 1.25) +
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5), breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ggtitle("The Federal Government in the Long Run") +
  labs(caption = "Graph created by @JosephPolitano using BEA & BLS data",subtitle = "Excluding Redistributive Programs, the Federal Government Has Been Shrinking for Decades") +
  theme_apricitas + theme(legend.position = c(.60,.85)) + 
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Federal Direct Consumption & Investment, % of GDP","Federal Employees, % of All Employees")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1929-01-01")-(.1861*(today()-as.Date("1929-01-01"))), xmax = as.Date("1929-01-01")-(0.049*(today()-as.Date("1929-01-01"))), ymin = 0-(.3*0.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_GDP_EMPLOYEE_PCT_1929_graph, "Fed GDP Employee Pct Graph 1929.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


FED_GDP_EMPLOYEE_PCT_1953_graph <- ggplot() + #plotting defense and nondefense intermediate goods spending
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = filter(FED_EMP_PCT, date >="1953-01-01"), aes(x=date, y = value, color = "Federal Employees, % of All Employees"), size = 1.25) +
  geom_line(data = filter(BEA_GDP_FED_PCT, date >="1953-01-01"), aes(x=date, y = value, color = "Federal Direct Consumption & Investment, % of GDP"), size = 1.25) +
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.2), breaks = c(0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ggtitle("The Federal Government in the Long Run") +
  labs(caption = "Graph created by @JosephPolitano using BEA & BLS data",subtitle = "Excluding Redistributive Programs, the Federal Government Has Been Shrinking for Decades") +
  theme_apricitas + theme(legend.position = c(.60,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Federal Direct Consumption & Investment, % of GDP","Federal Employees, % of All Employees")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1953-01-01")-(.1861*(today()-as.Date("1953-01-01"))), xmax = as.Date("1953-01-01")-(0.049*(today()-as.Date("1953-01-01"))), ymin = 0-(.3*0.2), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_GDP_EMPLOYEE_PCT_1953_graph, "Fed GDP Employee Pct Graph 1953.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


TREASURY_RECEIPTS_OUTLAYS <- read.csv("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/mts/mts_table_9?format=csv&page[size]=1000000&filter=record_date:gte:2023-01-01,record_date:lte:2025-12-31") %>%
  filter(line_code_nbr != 120) %>%
  mutate(date = as.Date(record_date)) %>%
  filter(classification_desc %in% c("Agriculture", "Commerce and Housing Credit", "Transportation",
                                    "Community and Regional Development", "Education, Training, Employment, and Social Services",
                                    "Health", "Medicare", "Income Security", "Social Security",
                                    "Veterans Benefits and Services", "Administration of Justice",
                                    "General Government", "Net Interest", "Undistributed Offsetting Receipts",
                                    "National Defense", "International Affairs",
                                    "General Science, Space, and Technology", "Energy",
                                    "Natural Resources and Environment")) %>%
  mutate(classification_desc = gsub(" and ", " & ", classification_desc)) %>%
  mutate(classification_desc = ifelse(classification_desc == "Health", "Medicaid & Health", classification_desc)) %>%
  mutate(classification_desc = ifelse(classification_desc == "Administration of Justice", "Justice", classification_desc)) %>%
  transmute(year = year(date), classification_id, classification_desc, spending = as.numeric(current_month_rcpt_outly_amt)) %>%
  group_by(year,classification_desc) %>%
  summarize(spending = sum(spending, na.rm = TRUE)) %>%
  mutate(classification_desc = factor(classification_desc, levels = c("Energy", "General Government", "Agriculture",
                                                                      "General Science, Space, & Technology", "International Affairs",
                                                                      "Natural Resources & Environment", "Justice",
                                                                      "Community & Regional Development", "Transportation",
                                                                      "Education, Training, Employment, & Social Services",
                                                                      "Veterans Benefits & Services", "Income Security",
                                                                      "National Defense", "Net Interest", "Medicaid & Health",
                                                                      "Medicare", "Social Security"))) %>%
  filter(classification_desc !="Education, Training, Employment, & Social Services") %>% 
  filter(spending >= 100000000000)

TREASURY_SPENDING_CHART <- ggplot(data = filter(TREASURY_RECEIPTS_OUTLAYS, year %in% c(2024, 2025), classification_desc != "Total"), aes(x = classification_desc, y = spending/1000000000000, fill = as.factor(year))) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("Spending, Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "T", scale = 1), expand = c(0,0)) +
  scale_fill_manual(name = NULL, values = c("#FFE98F","#00A99D"), breaks = c("2025","2024")) +
  ggtitle("Federal Spending by Selected Function, 2024-2025") +
  labs(caption = "Graph created by @JosephPolitano using Treasury data by Calendar Year", subtitle = "Gross Federal Spending Increased in 2025, Against the Ostensible Aims of DOGE") +
  theme_apricitas + theme(legend.position = c(.85,.75), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.3), "cm"), plot.title = element_text(size = 25)) +
  coord_flip()  +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TREASURY_SPENDING_CHART, "Treasury Spending Chart.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Manually downloading OPM data from here:
#https://data.opm.gov/explore-data/data/data-downloads

EMPLOYMENT_NOV_2025 <- read_delim("C:\\Users\\Joseph\\Downloads\\employment_202511_1_2026-01-20.csv", 
                                  delim = "|")
  
EMPLOYMENT_NOV_2024 <- read_delim("C:\\Users\\Joseph\\Downloads\\employment_202411_1_2026-01-20.csv", 
                                  delim = "|") 


GROUPED_EMPLOYMENT_NOV_2025 <- EMPLOYMENT_NOV_2025 %>%
  group_by(agency_subelement) %>%
  summarise(Emp_2025 = sum(count, na.rm = TRUE))

GROUPED_EMPLOYMENT_NOV_2024 <- EMPLOYMENT_NOV_2024 %>%
  group_by(agency_subelement) %>%
  summarise(Emp_2024 = sum(count, na.rm = TRUE))

FED_EMP_2024_2025 <- merge(GROUPED_EMPLOYMENT_NOV_2024,GROUPED_EMPLOYMENT_NOV_2025,by = "agency_subelement") %>%
  mutate(RAW_CHG = Emp_2025-Emp_2024, PCT_CHG = (Emp_2025-Emp_2024)/Emp_2024) %>% 
  filter(agency_subelement != "OFFICE OF THE SECRETARY OF THE INTERIOR") %>%
  slice_max(order_by = abs(RAW_CHG), n = 10) %>%
  mutate(agency_subelement = c("Veterans Health Administration","Internal Revenue Service","Social Security Administration","Immigration & Customs Enforcement","Air Force Materiel Command","Forest Service","USAID","Food & Drug Administration","Veterans Benefits Administration","National Institutes of Health")) %>%
  mutate(agency_subelement = factor(agency_subelement, levels = rev(c("Veterans Health Administration","Internal Revenue Service","Social Security Administration","Air Force Materiel Command","Forest Service","USAID","Food & Drug Administration","Veterans Benefits Administration","National Institutes of Health","Immigration & Customs Enforcement")))) %>%
  mutate(PCT_CHG_LABL = paste0(round(PCT_CHG,2)*100, "%")) %>%
  mutate(PCT_CHG = case_when(
    PCT_CHG < -0.3 ~ -0.3,
    TRUE ~ PCT_CHG
  ))
  

FED_EMP_2024_2025_GRAPH <- ggplot(data = FED_EMP_2024_2025, aes(x = agency_subelement, y = RAW_CHG/1000, fill = PCT_CHG)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_text(aes(label = PCT_CHG_LABL, perl = TRUE), 
            position = position_stack(vjust = 0), 
            angle = 0, 
            hjust = 0, 
            size = 6, 
            color = "black", 
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("Federal Agencies With the Largest\nEmployment Change, Nov 2024-Nov 2025") +
  ylab("Numerical Change in Employees") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(-25,10), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  scale_fill_gradientn(name= "Percent Change\nin Employees",colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label =c("-30%+","-15%","0%","15%","30%+"),breaks = c(-.30,-.15,0,.15,.30), limits = c(-.3,.3), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using OMB Data. Note: Office of the Secretary of the Interior Ignored Because of Departmental Reorganization", subtitle = "DOGE Has Made Significant Cuts to the Federal Workforce, with ICE the Only Major Agency Adding Jobs") +
  theme_apricitas + theme(legend.position = c(.3,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 15, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip() +
  theme(plot.title.position = "plot") +
  theme(legend.title = element_text(margin = margin(b = 15)))

ggsave(dpi = "retina",plot = FED_EMP_2024_2025_GRAPH, "Fed Emp 2024 2025 Chart.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

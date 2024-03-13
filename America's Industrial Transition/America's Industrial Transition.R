pacman::p_load(prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

REAL_PCE_DURABLE <- fredr(series_id = "PCEDGC96",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
REAL_PCE_NONDURABLE <- fredr(series_id = "PCENDC96",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
#REAL_PCE_SERVICES <- fredr(series_id = "PCESC96",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

REAL_PCE_GOODS_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_DURABLE, aes(x=date, y = value/value[13]*100, color = "Durable Goods"), size = 1.25) + 
  geom_line(data = REAL_PCE_NONDURABLE, aes(x=date, y = value/value[13]*100, color = "Nondurable Goods"), size = 1.25) + 
  #geom_line(data = REAL_PCE_SERVICES, aes(x=date, y = value/value[13]*100, color = "Services"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(75,140), breaks = c(80,90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("The Goods Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Consumption of Goods in the US has Stalled Over the Last Two Years") +
  theme_apricitas + theme(legend.position = c(.245,.80)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Durable Goods","Nondurable Goods","Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 75-(.3*65), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PCE_GOODS_GRAPH, "Real PCE Goods.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ECOM_PCT <- fredr(series_id = "ECOMPCTSA",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

ECOM_PCT_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=ECOM_PCT, aes(x=date,y= value/100,color= "E-Commerce Retail Sales as a Percent of Total Sales"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Retail Sales") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10,0.15,.2), limits = c(0,.20), expand = c(0,0)) +
  ggtitle("Window Shopping") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "The Pandemic Supercharged E-Commerce's Share of Spending, but Then Growth Stagnated") +
  theme_apricitas + theme(legend.position = c(.4,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECOM_PCT_Graph, "ECOM PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

HOURS_PRODUCTION <- fredr(series_id = "CES3000000034",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
EMPLOYEES_PRODUCTION <- fredr(series_id = "CES3000000006",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

HOURS_EMPLOYEES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = HOURS_PRODUCTION, aes(x=date, y = value/value[25]*100, color = "Index of All Production and Nonsupervisory Employees: Manufacturing"), size = 1.25) + 
  geom_line(data = EMPLOYEES_PRODUCTION, aes(x=date, y = value/value[25]*100, color = "Index of Aggregate Weekly Hours of Production and Nonsupervisory Employees: Manufacturing"), size = 1.25) + 
  #geom_line(data = REAL_PCE_SERVICES, aes(x=date, y = value/value[13]*100, color = "Services"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,107.5), breaks = c(80,85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Less With More") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Aggregate Manufacturing Hours Have Recovered Less than Employment, and Now Show Weakness") +
  theme_apricitas + theme(legend.position = c(.5,.95), legend.text = element_text(size = 12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Index of All Production and Nonsupervisory Employees: Manufacturing","Index of Aggregate Weekly Hours of Production and Nonsupervisory Employees: Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 80-(.3*25), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HOURS_EMPLOYEES_GRAPH, "Employment and Work.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

AVGWE_DURABLE <- fredr(series_id = "CES3100000007",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)
AVGWE_NONDURABLE <- fredr(series_id = "CES3200000007",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)

AVGWE_MANUFACTURING_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = AVGWE_DURABLE, aes(x=date, y = value, color = "Durable Goods Manufacturing"), size = 1.25) + 
  geom_line(data = AVGWE_NONDURABLE, aes(x=date, y = value, color = "Nondurable Goods Manufacturing"), size = 1.25) + 
  #geom_line(data = REAL_PCE_SERVICES, aes(x=date, y = value/value[13]*100, color = "Services"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(38,43), breaks = c(38,39,40,41,42,43), expand = c(0,0)) +
  ylab("Average Weekly Hours") +
  ggtitle("Undertime") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Hours Worked in Manufacturing Have Declined Recently—A Possible Bearish Leading Indicator") +
  theme_apricitas + theme(legend.position = c(.4,.15)) +
  scale_color_manual(name= "Average Weekly Hours of Production and Nonsupervisory Employees",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Nondurable Goods Manufacturing","Durable Goods Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 38-(.3*5), ymax = 38) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AVGWE_MANUFACTURING_GRAPH, "AVGWE Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

TX_MANUFACT_DIFF <- fredr(series_id = "BACTSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
PHI_MANUFACT_DIFF <- fredr(series_id = "GACDFSA066MSFRBPHI",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
NY_MANUFACT_DIFF <- fredr(series_id = "GACDISA066MSFRBNY",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
RCH_MANUFACT_DIFF <- read.xlsx("https://www.richmondfed.org/-/media/RichmondFedOrg/region_communities/regional_data_analysis/regional_economy/surveys_of_business_conditions/manufacturing/data/mfg_historicaldata.xlsx") %>%
  mutate(date = seq.Date(from = as.Date("1993-11-01"), by = "month", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  transmute(date, value = sa_mfg_composite)
#Note: Date has to be manually updated for KS Fed release schedule
KC_MANUFACT_DIFF <- read.xlsx("https://www.kansascityfed.org/Manufacturing/documents/9570/2023May25-Historical-Manufacturing-Survey.xlsx") %>%
  transpose() %>%
  .[-1, ] %>%
  transmute(date = V2, value = as.numeric(V5)) %>%
  mutate(date = seq.Date(from = as.Date("2001-07-01"), by = "month", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-01-01"))
  
MANUFACT_DIFF_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = KC_MANUFACT_DIFF, aes(x = date, y = value, color = "Kansas City Fed"), size = 1.25) +
  geom_line(data = RCH_MANUFACT_DIFF, aes(x = date, y = value, color = "Richmond Fed"), size = 1.25) +
  geom_line(data = PHI_MANUFACT_DIFF, aes(x = date, y = value, color = "Philadelphia Fed"), size = 1.25) +
  geom_line(data = NY_MANUFACT_DIFF, aes(x = date, y = value, color = "New York Fed"), size = 1.25) +
  geom_line(data = TX_MANUFACT_DIFF, aes(x = date, y = value, color = "Dallas Fed"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-80,60), breaks = c(-40,-20,0,20,40,60), expand = c(0,0)) +
  ylab("Diffusion Index, Positive Number Indicates Growth") +
  ggtitle("America's Manufacturing Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Manufacturers Across the Country Are Contracting Amidst A Nationwide Slowdown") +
  theme_apricitas + theme(legend.position = c(.22,.25)) +
  scale_color_manual(name= "General Business Activity Index",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED"), breaks = c("Dallas Fed","New York Fed","Philadelphia Fed","Richmond Fed","Kansas City Fed")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -80-(.3*140), ymax = -80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANUFACT_DIFF_Graph, "Regional Fed Diffusion Indices.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Making USMAP
sf_use_s2(FALSE)

div_dat <- states(cb = FALSE, resolution = '20m') %>%
  st_drop_geometry() %>%
  select(NAME, DIVISION) %>%
  mutate(ID = tolower(NAME))

# get state data, convert to sf, join with division data
states <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  left_join(div_dat)

states <- st_make_valid(states)

# create division polygons
div <- states %>%
  group_by(DIVISION) %>% 
  summarize()

CENSUS_SUBNATIONAL_MFG_SPENDING <- read.xlsx("https://www.census.gov/construction/c30//xlsx/privmfgtime.xlsx") %>%
  .[-1,] %>%
  row_to_names(1) %>%
  select(`New England`,
         `Mid Atlantic`,
         `East North\nCentral`,
         `West North\nCentral`,
         `South Atlantic`,
         `East South\nCentral`,
         `West South\n Central`,
         `Mountain`,
         `Pacific`) %>%
  `colnames<-`(c("1","2","3","4","5","6","7","8","9")) %>%
  mutate_all(as.numeric) %>%
  slice(1:12) %>%
  summarize_all(sum) %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("YRSPENDING")) %>%
  mutate(DIVISION = c("1","2","3","4","5","6","7","8","9"))

states <- merge(states,CENSUS_SUBNATIONAL_MFG_SPENDING, by = "DIVISION")

# plot it
REGIONAL_MFG_SPENDING_GRAPH <- ggplot() + 
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  geom_sf(data = states, 
          aes(fill = YRSPENDING/1000), 
          color = 'grey25') +
  geom_sf(data = div, 
          color = 'black', 
          fill = NA,
          lwd = 1.25) +
  scale_fill_viridis_c(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50,60,70), expand = c(0,0)) +
  # scale_fill_gradient(low = "#00A99D",
  #                      high = "#FFE98F",
  #                      space = "Lab",
  #                      na.value = "grey50",
  #                      guide = "colourbar",
  #                      aesthetics = "fill",
  #                     breaks = c(0,10,20,30,40,50), 
  #                     labels = c("$0B","$10B","$20B","$30B","$40B","$50B"),
  #                     limits = c(0,50)) +
  coord_sf(crs = 5070) +
  geom_text(
    data = filter(states, DIVISION == 1), 
    aes(x = 1950000, y = 2600000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    color = "white",
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 2), 
    aes(x = 1600000, y = 2250000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 3), 
    aes(x = 800000, y = 1950000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 4), 
    aes(x = 00000, y = 2000000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 5), 
    aes(x = 1300000, y = 1300000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 6), 
    aes(x = 725000, y = 1200000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 7), 
    aes(x = -250000, y = 950000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 8), 
    aes(x = -1350000, y = 1800000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 9), 
    aes(x = -2000000, y = 2600000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  ggtitle("  Manufacturing Construction in the Last 12M by Region") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using US Census data") +
  labs(fill = NULL) +
  theme_apricitas + theme(plot.title = element_text(size = 24), legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = REGIONAL_MFG_SPENDING_GRAPH, "Regional Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

REAL_IMPORTS <- read.xlsx("https://www.census.gov/foreign-trade/statistics/historical/realimp.xlsx") %>%
  drop_na() %>%
  filter(!str_detect(Seasonally.Adjusted,"-")) %>%
  `colnames<-`(c("total","date","food","industrial","capital","automotive","consumer","other","residual")) %>%
  mutate(date = seq.Date(from = as.Date("1994-01-01"), by = "month", length.out = nrow(.))) %>%
  mutate_if(is.character,as.numeric)
  
REAL_IMPORTS_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = subset(REAL_IMPORTS, date >= as.Date("2018-01-01")), aes(x=date, y = industrial/1000, color = "Industrial Supplies Incl. Petroleum"), size = 1.25) + 
  geom_line(data = subset(REAL_IMPORTS, date >= as.Date("2018-01-01")), aes(x=date, y = capital/1000, color = "Capital Goods"), size = 1.25) + 
  geom_line(data = subset(REAL_IMPORTS, date >= as.Date("2018-01-01")), aes(x=date, y = consumer/1000, color = "Consumer Goods ex-Automotive"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(45,82), breaks = c(50,60,70,80), expand = c(0,0)) +
  ylab("2012 Dollars") +
  ggtitle("The Goods Bust") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Imports of Goods, Especially Consumer Goods, has Fallen") +
  theme_apricitas + theme(legend.position = c(.23,.85)) +
  scale_color_manual(name= "Real Imports, 2012 Dollars",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Capital Goods","Consumer Goods ex-Automotive","Industrial Supplies Incl. Petroleum")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 45-(.3*38), ymax = 45) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_IMPORTS_GRAPH, "Real Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

IPMANPCT <- fredr(series_id = "IPMAN", observation_start = as.Date("2018-01-01"), units = "pc1")

IPMANPCT_GRAPH <- ggplot() + #plotting net tightening data
  geom_line(data=IPMANPCT, aes(x=date,y= value/100,color= "US Industrial Production: Manufacturing, Annual Growth"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent Growth, Annual") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.20,-.10,0,0.10,0.20), limits = c(-.20,.23), expand = c(0,0)) +
  ggtitle("The Goods Bust") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "US Manufacturing Output Has Shrunk Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.4,100)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.20-(.3*.41), ymax = -0.20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IPMANPCT_GRAPH, "IPMANPCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

WAREHOUSE <- fredr(series_id = "CES4349300001",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
TRUCK <- fredr(series_id = "CES4348400001", observation_start = as.Date("2018-01-01"))

WAREHOUSE_Graph <- ggplot() + #plotting total quits
  geom_line(data=WAREHOUSE, aes(x=date,y= value/1000,color= "All Employees, Warehousing and Storage"), size = 1.25)+ 
  geom_line(data=TRUCK, aes(x=date,y= value/1000,color= "All Employees, Truck Transportation"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(0.5,1,1.5,2), limits = c(1,2), expand = c(0,0)) +
  ggtitle("Total Fulfillment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Warehousing and Trucking Employment Growth Has Stalled This Year") +
  theme_apricitas + theme(legend.position = c(.40,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 1-(.3*1), ymax = 1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WAREHOUSE_Graph, "Warehouse Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

SHORT_SUPPLY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Structural%20Inflation/Structural_Inflation.csv") %>%
  select(-total) %>%
  drop_na() %>%
  mutate(date = as.Date(date))

colnames(SHORT_SUPPLY) <- c("date","1 Month","2-3 Months","4-6 Months","7-12 Months","13-24 Months","25+ Months")

SHORT_SUPPLY <- pivot_longer(SHORT_SUPPLY, cols = `1 Month`:`25+ Months`) %>%
  mutate(name =  factor(name, levels = c("1 Month","2-3 Months","4-6 Months","7-12 Months","13-24 Months","25+ Months")))

ISM_SHORT_SUPPLY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = SHORT_SUPPLY, aes(x = date, y = value, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Number") +
  ggtitle("Commodities in Short Supply") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Semiconductors, Electrical Components, and Electronic Components Have Persistent Supply Issues") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Number of Consecutive Months in Short Supply",values = c("#ea3c2e","#d12215","#a21b10","#74130c","#460b07","#170402")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ISM_SHORT_SUPPLY_GRAPH, "ISM Short Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MFG_SPENDING_CATEGORIES <- read.xlsx("https://www.census.gov/construction/c30/xlsx/privsatime.xlsx") %>%
  drop_na() %>%
  row_to_names(1) %>%
  select(67:74) %>%
  `colnames<-`(c("Total","Food/Beverage/Tobacco","Chemical","Plastic/Rubber","Nonmetallic Mineral","Fabricated Metal","Computer/Electronic/Electrical","Transportation Equipment")) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(Other = Total-`Food/Beverage/Tobacco`-Chemical-`Plastic/Rubber`-`Nonmetallic Mineral`-`Fabricated Metal`-`Computer/Electronic/Electrical`-`Transportation Equipment`) %>%
  select(-Total) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = seq.Date(from = as.Date("1993-01-01"), by = "month", length.out = nrow(.))) %>%
  mutate_if(is.character,as.numeric) %>%
  pivot_longer(cols = `Food/Beverage/Tobacco`:`Other`) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(name = factor(name, levels = c("Computer/Electronic/Electrical","Chemical","Food/Beverage/Tobacco","Transportation Equipment","Nonmetallic Mineral","Plastic/Rubber","Fabricated Metal","Other")))

MFG_SPENDING_CATEGORIES_GRAPH <- ggplot() + #plotting components of manufacturing construction
  geom_bar(data = MFG_SPENDING_CATEGORIES, aes(x = date, y = value/1000, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,250), breaks = c(0,50,100,150,200,250), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Manufacturing Construction Spending") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Computer/Electronic Manufacturing Makes Up More Than 50% of Manufacturing Construction") +
  theme_apricitas + theme(legend.position = c(0.25,0.75), legend.key.size = unit(0.5,"cm")) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*250), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MFG_SPENDING_CATEGORIES_GRAPH, "MFG Spending Categories Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
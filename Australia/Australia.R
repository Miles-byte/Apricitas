pacman::p_load(readrba,readabs,bea.R,readxl,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

US_GDP <- fredr(series_id = "GDPC1", observation_start = as.Date("1992-01-01")) %>%
  drop_na()
AUS_GDP <- fredr(series_id = "AUSGDPRQDSMEI", observation_start = as.Date("1992-01-01")) %>%
  drop_na()

#GDP_BULK <- read_abs("5206.0", tables = 1)
#How to Work with Bulk Tables 
#Table Directory Here: https://www.abs.gov.au/websitedbs/D3310114.nsf/home/Time+Series+Directory+-+URL+formula+instructions+for+access+to+Time+Series+Metadata

AUS_GDP <- read_abs(series_id = "A2304402X") %>%
  subset(date >= as.Date("1992-01-01"))

US_AUS_GDP_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=US_GDP, aes(x=date,y= value/value[1]*100, color= "United States"), size = 1.25) +
  geom_line(data=AUS_GDP, aes(x=date-60,y= value/value[1]*100, color= "Australia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(95,270),breaks = c(100,150,200,250), expand = c(0,0)) +
  ylab("Index: Q1 1992 = 100") +
  ggtitle("Australia's Miracle") +
  labs(caption = "Graph created by @JosephPolitano using BEA and ABS data",subtitle = "Australia Managed to Dodge a Recession from 1991 all the Way to 2020") +
  theme_apricitas + theme(legend.position = c(.35,.65)) +
  scale_color_manual(name= "Real GDP" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*(today()-as.Date("1992-01-01"))), xmax = as.Date("1992-01-01")-(0.049*(today()-as.Date("1992-01-01"))), ymin = 100-(.3*170), ymax = 100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_AUS_GDP_Graph, "US AUS GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CPI_MON <- read_abs(series_id = "A128478318V") %>%
  subset(date >= as.Date("2018-01-01"))
CPI_QTR <- read_abs(series_id = "A2325847F") %>%
  subset(date >= as.Date("2018-01-01"))

AUS_CPI <- ggplot() + #CPI
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.02, ymax = 0.03, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "RBA 2-3% Inflation Target", x = as.Date("2019-01-01"), y = 0.033, color = "#EE6055", alpha = 0.6, size = 4) +
  geom_line(data=CPI_MON, aes(x=date,y= value/100, color= "Monthly CPI Indicator"), size = 1.25) +
  geom_line(data=CPI_QTR, aes(x=date-60,y= value/100, color= "Quarterly Official CPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.01,.09), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Australia's Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "Australian Inflation Remains Well Above Target") +
  theme_apricitas + theme(legend.position = c(.45,.7)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Quarterly Official CPI","Monthly CPI Indicator")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.01-(.3*.1), ymax = -0.01) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AUS_CPI, "AUS CPI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

COE_AUS <- read_abs(series_id = "A2303359K") %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(trend = c(NA,NA,NA,NA,NA,NA,100*1.013475^(0:(nrow(.)-7)))) %>%
  mutate(value = value/value[7]*100)

NGDP_AUS <- read_abs(series_id = "A2304418T") %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100) 

COE_NGDP_Trend_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=COE_AUS, aes(x=date-60,y= trend, color= "Hypothetical 5.5% Growth Rate Trend"), size = 0.75, linetype = "dashed") +
  geom_line(data=NGDP_AUS, aes(x=date-60,y= value, color= "Nominal Gross Domestic Product"), size = 1.25) +
  geom_line(data=COE_AUS, aes(x=date-60,y= value, color= "Nominal Gross Compensation of Employees"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,125),breaks = c(90,100,110,120), expand = c(0,0)) +
  ylab("Index: Q3 2019 = 100") +
  ggtitle("Pulling Above Trend") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "Australian NGDP and Nominal Gross Labor Income Have Pulled Above Trend") +
  theme_apricitas + theme(legend.position = c(.35,.65)) +
  scale_color_manual(name= "Australia, Index, Q3 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Nominal Gross Compensation of Employees","Nominal Gross Domestic Product","Hypothetical 5.5% Growth Rate Trend"), guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,0.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01")))+30, xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01")))+30, ymin = 90-(.3*35), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COE_NGDP_Trend_Graph, "NGDP .png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


testrba <- rba_forecasts()

WPI <- read_abs(series_id = "A83895396W") %>%
  subset(date >= as.Date("2000-01-01"))
WPI_RBA_FORECAST <- rba_forecasts() %>%
  subset(series == "wpi_change") %>%
  subset(forecast_date == max(forecast_date))
  
AUS_WPI_Graph <- ggplot() + #WPI
  geom_line(data=WPI, aes(x=date-60,y= value/100, color= "Wage Price Index"), size = 1.25) +
  geom_line(data=WPI_RBA_FORECAST, aes(x=date-60,y= value/100, color= "WPI: RBA Forecast"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Ending Australia's Wage Stagnation") +
  labs(caption = "Graph created by @JosephPolitano using ABS and RBA data",subtitle = "The RBA Forecasts Wage Growth Reaching Pre-Great Recession Highs Before Tempering") +
  theme_apricitas + theme(legend.position = c(.35,.4)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Wage Price Index","WPI: RBA Forecast")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0.0-(.3*.05), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AUS_WPI_Graph, "AUS WPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EPOP_US <- fredr("LNS12300060", observation_start = as.Date("1990-01-01"))
EPOP_AUS <- fredr("LREM25TTAUM156S", observation_start = as.Date("1990-01-01"))

EPOP_US_AUS_GRAPH <- ggplot() + #EPOP Growth
  geom_line(data=EPOP_US, aes(x=date,y= value/100, color= "United States"), size = 1.25) +
  geom_line(data=EPOP_AUS, aes(x=date,y= value/100, color= "Australia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.675,.875), breaks = c(.70,.74,.78,.82,.86), expand = c(0,0)) +
  ylab("Employment Rate") +
  ggtitle("Fuller Employment") +
  labs(caption = "Graph created by @JosephPolitano using ABS and BLS data",subtitle = "Australia's Employment Rates Have Exceeded Pre-Pandemic Levels—And Beaten America's") +
  theme_apricitas + theme(legend.position = c(.4825,.215)) +
  scale_color_manual(name= "25-54 Employment-Population Ratios" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .675-(.3*.20), ymax = .675) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_US_AUS_GRAPH, "AUS EPOP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Graphing Migration
Arrivals <- read_abs(series_id = "A85232561W")
Departures <- read_abs(series_id = "A85232570X")

Net <- merge(Arrivals, Departures, by = "date") %>%
  mutate(value = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(value.x-value.y,12))) %>%
  subset(date >= as.Date("2018-01-01"))

AUS_NET_MIGRATION_GRAPH <- ggplot() + #Net Migration
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=Net, aes(x=date,y= value/1000, color= "Australia, Net Arrivals, 12M Moving Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(-300,750), breaks = c(-250,0,250,500,750), expand = c(0,0)) +
  ylab("Arrivals Net Departures, 12MMT") +
  ggtitle("Opening Up Again") +
  labs(caption = "Graph created by @JosephPolitano using ABS and BLS data",subtitle = "Net Immigration to Australia is Approaching Pre-Pandemic Levels Again") +
  theme_apricitas + theme(legend.position = c(.725,.975)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -300-(.3*1050), ymax = -300) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AUS_NET_MIGRATION_GRAPH, "Net Arrivals.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ICP_AUS <- read_rba(series_id = "GRCPAIAD") %>%
  subset(date >= as.Date("2015-12-01"))

ICP_AUS_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=ICP_AUS, aes(x=date,y= value, color= "Index of Commodity Prices, Reserve Bank of Australia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(0,200),breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Australian Dollars") +
  ggtitle("Australia's Commodity Boom") +
  labs(caption = "Graph created by @JosephPolitano using RBA data",subtitle = "Australia Export Commodity Prices Rose Nearly 75% From Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*200), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ICP_AUS_Graph, "ICP AUS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#CHANGE TO CALENDAR YEAR TOTALS WHEN RELEASED IN JUN
EDUCATION_EXPORTS <- download_abs_data_cube("international-trade-supplementary-information-financial-year","536805500309") %>%
  read_excel(
  sheet = "Table 9.1",
  skip = 5
) %>%
  drop_na() %>%
  transpose() %>%
  select(V23) %>%
  transmute(value = as.numeric(V23)) %>%
  drop_na() %>%
  mutate(date = seq.Date(from = as.Date("2001-06-01"), to = as.Date("2022-06-01"), by = "year"))

TRAVEL_EXPORTS <- read_abs(series_id = "A2718643L") %>% 
  subset(date >= as.Date("2001-06-01"))

Services_Exports_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=TRAVEL_EXPORTS, aes(x=date,y= value/1000*12, color= "Travel Services Incl. Education"), size = 1.25) +
  geom_line(data=EDUCATION_EXPORTS, aes(x=date,y= value/1000, color= "Education Services (Annual Average)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,75),breaks = c(0,25,50,75), expand = c(0,0)) +
  ylab("Australian Dollars, Annual Rate") +
  ggtitle("Australia's Other Commodity Boom") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "Australian Exports of Travel and Education Suffered Amidst Travel Bans, But Are Rebounding") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name= "Australian Services Exports" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Travel Services Incl. Education","Education Services (Annual Average)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-06-01")-(.1861*(today()-as.Date("2001-06-01"))), xmax = as.Date("2001-06-01")-(0.049*(today()-as.Date("2001-06-01"))), ymin = 0-(.3*75), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Services_Exports_Graph, "Services AUS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


METAL_EXP <- read_abs(series_id = "A2718588J") %>%
  subset(date >= as.Date("2016-01-01"))

COAL_EXP <- read_abs(series_id = "A2718589K") %>%
  subset(date >= as.Date("2016-01-01"))

MINERALS_EXP <- read_abs(series_id = "A2718590V") %>%
  subset(date >= as.Date("2016-01-01"))

Goods_Exports_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=METAL_EXP, aes(x=date,y= value/1000*12, color= "Metal Ores and Minerals"), size = 1.25) +
  geom_line(data=COAL_EXP, aes(x=date,y= value/1000*12, color= "Coal, Coke, and Briquettes"), size = 1.25) +
  geom_line(data=MINERALS_EXP, aes(x=date,y= value/1000*12, color= "Other Mineral Fuels (Including LNG)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,300),breaks = c(0,100,200,300), expand = c(0,0)) +
  ylab("Australian Dollars, Annual Rate") +
  ggtitle("Australia's Commodity Boom") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "The Value of Australian Commodity Exports Has Surged Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name= "Australian Goods Exports" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Metal Ores and Minerals","Coal, Coke, and Briquettes","Other Mineral Fuels (Including LNG)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Goods_Exports_Graph, "Goods Exports AUS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CHINA_IMPORTS <- read_abs(series_id = "A1829346X") %>%
  subset(date >= as.Date("2016-01-01"))

CHINA_EXPORTS <- read_abs(series_id = "A1829030C") %>%
  subset(date >= as.Date("2016-01-01"))

China_Exports_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=CHINA_IMPORTS, aes(x=date,y= value/1000*12, color= "Imports"), size = 1.25) +
  geom_line(data=CHINA_EXPORTS, aes(x=date,y= value/1000*12, color= "Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,300),breaks = c(0,100,200,300), expand = c(0,0)) +
  ylab("Australian Dollars, Annual Rate") +
  ggtitle("Frenemies") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "Even Amidst Ongoing Trade Spats, Australian Exports to China Have Grown") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name= "Australian Goods Trade with China" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Imports","Exports")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = China_Exports_Graph, "China Exports AUS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


LITHIUM_EXPORTS <- read_abs(series_id = "A1827825W") %>%
  subset(date >= as.Date("2016-01-01"))

Lithium_Exports_Graph <- ggplot() + #plotting Australian Lithium Exports
  geom_line(data=LITHIUM_EXPORTS, aes(x=date,y= value/1000*12, color= "Australia, Exports of Lithium & Crude Minerals/Fertilizers n.e.s (SITC 27)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,20),breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Australian Dollars, Annual Rate") +
  ggtitle("Powering the Energy Transition") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "The Value of Australian Lithium Exports Have Skyrocketed Amidst Rising Demand") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Lithium_Exports_Graph, "Lithium Exports AUS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

HOUSEHOLD_SAVING <- read_abs(series_id = "A2323382F") %>%
  subset(date >= as.Date("2018-01-01"))

HOUSEHOLD_SAVING_Graph <- ggplot() + #HH Saving
  geom_line(data=HOUSEHOLD_SAVING, aes(x=date-60,y= value/100, color= "Household Saving Ratio, Australia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.25), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Slowly Spending Savings") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "Australians Have Still Not Spent Down Significant 'Excess Savings'") +
  theme_apricitas + theme(legend.position = c(.45,.15)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = HOUSEHOLD_SAVING_Graph, "Household Saving.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


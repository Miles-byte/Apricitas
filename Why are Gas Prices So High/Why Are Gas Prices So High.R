pacman::p_load(httr,quantmod,tidyquant,datetime,yearmon,lubridate,readr,stringi,jsonlite,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)
devtools::install_github("jameelalsalam/eia2")
library("eia2")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

GASREGW <- fredr(series_id = "GASREGW") #downloading regular all formulations gas price
RETAIL_GAS <- fredr(series_id = "RSGASS") #downloading advanced retail sales of gas
RETAIL_ALL <- fredr(series_id = "RSXFS") #downloading all retail sales

RETAIL_GAS_ALL <- merge(RETAIL_GAS, RETAIL_ALL, by = "date")
RETAIL_GAS_ALL <- subset(RETAIL_GAS_ALL,select = c(date, value.x, value.y))
colnames(RETAIL_GAS_ALL) <- c("date", "gas", "all")


VMT <- fredr(series_id = "VMTD11", observation_start = as.Date("2018-01-01"))

#downloading expenditure data for consumer units 
GAS_EXPEND_BOTTOM1 <- bls_api("CXUGASOILLB0102M", startyear = 1984, endyear = 2003, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
GAS_EXPEND_BOTTOM2 <- bls_api("CXUGASOILLB0102M", startyear = 2004, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  subset(select = -latest)
GAS_EXPEND_BOTTOM <- rbind(GAS_EXPEND_BOTTOM2,GAS_EXPEND_BOTTOM1)


TOTAL_EXPEND_BOTTOM1 <- bls_api("CXUTOTALEXPLB0102M", startyear = 1984, endyear = 2003, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
TOTAL_EXPEND_BOTTOM2 <- bls_api("CXUTOTALEXPLB0102M", startyear = 2004, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  subset(select = -latest)
TOTAL_EXPEND_BOTTOM <- rbind(TOTAL_EXPEND_BOTTOM2,TOTAL_EXPEND_BOTTOM1)

#merging total and gas expenditures
GAS_TOTAL_MERGE_BOTTOM <- merge(GAS_EXPEND_BOTTOM, TOTAL_EXPEND_BOTTOM, by = "year") 
GAS_TOTAL_MERGE_BOTTOM <- subset(GAS_TOTAL_MERGE_BOTTOM,select = c(year, value.x, value.y))
colnames(GAS_TOTAL_MERGE_BOTTOM) <- c("year", "gas", "total")
GAS_TOTAL_MERGE_BOTTOM$year <- ymd(GAS_TOTAL_MERGE_BOTTOM$year, truncated = 2L)

GAS_EXPEND_MEDIAN1 <- bls_api("CXUGASOILLB0104M", startyear = 1984, endyear = 2003, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
GAS_EXPEND_MEDIAN2 <- bls_api("CXUGASOILLB0104M", startyear = 2004, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  subset(select = -latest)
GAS_EXPEND_MEDIAN <- rbind(GAS_EXPEND_MEDIAN2,GAS_EXPEND_MEDIAN1)

TOTAL_EXPEND_MEDIAN1 <- bls_api("CXUTOTALEXPLB0104M", startyear = 1984, endyear = 2003, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
TOTAL_EXPEND_MEDIAN2 <- bls_api("CXUTOTALEXPLB0104M", startyear = 2004, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))  %>%
  subset(select = -latest)
TOTAL_EXPEND_MEDIAN <- rbind(TOTAL_EXPEND_MEDIAN2,TOTAL_EXPEND_MEDIAN1)

#merging total and gas expenditures
GAS_TOTAL_MERGE_MEDIAN <- merge(GAS_EXPEND_MEDIAN, TOTAL_EXPEND_MEDIAN, by = "year") 
GAS_TOTAL_MERGE_MEDIAN <- subset(GAS_TOTAL_MERGE_MEDIAN,select = c(year, value.x, value.y))
colnames(GAS_TOTAL_MERGE_MEDIAN) <- c("year", "gas", "total")
GAS_TOTAL_MERGE_MEDIAN$year <- ymd(GAS_TOTAL_MERGE_MEDIAN$year, truncated = 2L)

install.packages("tidyquant")
library(tidyquant)
XOP <- tq_get("XOP", from = "2006-07-01") #downloading XOP returns
SPY <- tq_get("SPY", from = "2006-07-01") #downloading SPY returns

WTI <- tq_get("CL=F", from = "2006-07-01")
Baker_Hughes <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Why%20are%20Gas%20Prices%20So%20High/Baker_Hughes.csv")

Baker_Hughes$Date <- as.Date(Baker_Hughes$Date)
Baker_Hughes$RIG_COUNT <- gsub(",","",Baker_Hughes$RIG_COUNT)
Baker_Hughes$RIG_COUNT <- as.numeric(Baker_Hughes$RIG_COUNT)

Baker_Hughes <- subset(Baker_Hughes, Date > "2006-07-05")

Petrol_Profits <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Why%20are%20Gas%20Prices%20So%20High/Petrol_Profits.csv")
Petrol_Profits$Date <- as.Date(Petrol_Profits$Date)

Capital_Discipline <- data.frame(answer = c("Capital Discipline","Other","ESG Issues","Lack of Access to Financing","Government Regulations"), percent = c(.59,.15,.11,.08,.06))
Capital_Discipline$answer <- factor(Capital_Discipline$answer, levels = c("Government Regulations","Lack of Access to Financing","ESG Issues","Other","Capital Discipline"), ordered = TRUE)

#modeled crude production and real production data
Crude_ProductionWeekly <- eia1_series("PET.WCRFPUS2.W") %>%
  transmute(date = as.Date(period),value) %>%
  filter(date >= as.Date("2019-01-01"))

Crude_ProductionMonthly <- eia1_series("PET.MCRFPUS2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")),value) %>%
  filter(date >= as.Date("2019-01-01"))

#crude, gas, and diesel prices
WTIEIA <- eia1_series("PET.RWTC.D") %>%
  transmute(date = as.Date(period),value, product = "Crude") %>%
  filter(date >= as.Date("2019-01-01") & value > 0.01)

GASEIA <- eia1_series("PET.EER_EPMRU_PF4_RGC_DPG.D") %>%
  transmute(date = as.Date(period),value, product = "Gasoline") %>%
  filter(date >= as.Date("2019-01-01") & value > 0.01)

DIESELEIA <- eia1_series("PET.EER_EPD2DXL0_PF4_RGC_DPG.D") %>%
  transmute(date = as.Date(period),value, product = "Diesel") %>%
  filter(date >= as.Date("2019-01-01") & value > 0.01)

KEROSENEEIA <- eia1_series("PET.EER_EPJK_PF4_RGC_DPG.D") %>%
  transmute(date = as.Date(period),value, product = "Kerosene_Jet") %>%
  filter(date >= as.Date("2019-01-01") & value > 0.01)


REFINERY_CAPACITY <- eia1_series("PET.MOCLEUS2.M")  %>%
  transmute(date = as.Date(paste0(period,"-01")),value) %>%
  filter(date >= as.Date("2019-01-01"))

REFINERY_OPERATING_CAPACITY <- eia1_series("PET.MOCGGUS2.M")  %>%
  transmute(date = as.Date(paste0(period,"-01")),value) %>%
  filter(date >= as.Date("2019-01-01"))

#SPR Levels
SPR_LEVEL <- eia_series("PET.WCSSTUS1.W", start = "2019")
SPR_LEVEL <- as.data.frame(SPR_LEVEL$data)

#STEO Crude Production, SPR Drawdowns and Forecasts
STEO_Crude_Production <- eia_series("STEO.COPRPUS.M", start = "2019", end = "2026")
STEO_Crude_Production <- as.data.frame(STEO_Crude_Production$data)
STEO_SPR_Withdrawls <- eia_series("STEO.COSQ_DRAW.M", start = "2019", end = "2026")
STEO_SPR_Withdrawls <- as.data.frame(STEO_SPR_Withdrawls$data)

STEO_SPR_Prod <- merge(STEO_Crude_Production,STEO_SPR_Withdrawls, by = "date")

STEO_WTI <- eia_series("STEO.WTIPUUS.M", start = "2019", end = "2026") 
STEO_WTI <- as.data.frame(STEO_WTI$data)

Crude_Curve_10_25_2022 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Why%20are%20Gas%20Prices%20So%20High/crude-oil-wti-prices-intraday-10-25-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

CPI_ELECTRICITY <- fredr(series_id = "CUSR0000SEHF01",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)
CPI_UTILITY_GAS <- fredr(series_id = "CUSR0000SEHF02",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)
CPI_GASOLINE <- fredr(series_id = "CUSR0000SETB01",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)

VMT_Graph <- ggplot() + #plotting VMT
  geom_line(data=VMT, aes(x=date,y= value/1000, color= "Vehicle Miles Travelled"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "T"), limits = c(160,300), expand = c(0,0)) +
  ylab("VMT, Seasonally Adjusted Annual Rate") +
  ggtitle("Life is a Highway") +
  labs(caption = "Graph created by @JosephPolitano using BTS data",subtitle = "Despite the Pandemic, Americans are Driving as Much as Ever") +
  theme_apricitas + theme(legend.position = c(.7,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1300), xmax = as.Date("2018-01-01")-(0.049*1300), ymin = 160-(.3*140), ymax = 160) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

XOP_Graph <- ggplot() + #plotting returns to XOP
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=XOP, aes(x=date,y= (adjusted/133.1416)-1, color= "XOP"), size = 1.25) +
  geom_line(data=SPY, aes(x=date,y= (adjusted/93.76001)-1, color= "SPY"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-1,5), expand = c(0,0)) +
  ylab("Total Return since June 2006, %") +
  ggtitle("Understanding 'Capital Discipline'") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "Exploration and Production Companies Were Wrecked by the 2014 and 2020 Oil Crashes") +
  theme_apricitas + theme(legend.position = c(.7,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-07-03")-(.1861*5948), xmax = as.Date("2006-07-03")-(0.049*5948), ymin = -1-(.3*6), ymax = -1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Crude_Production_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=Crude_ProductionMonthly, aes(x=date,y= value/1000, color= "US Crude Oil Production (Monthly Official Data)"), size = 1.25) +
  geom_line(data=Crude_ProductionWeekly, aes(x=date,y= value/1000, color= "US Crude Oil Production (Weekly Modeled Estimates)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(9,14),breaks = c(9,10,11,12,13,14), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("America's Oil Recovery") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Oil Production Is Closing in on Record High Levels") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 9-(.3*5), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Refinery_Capacity_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=REFINERY_CAPACITY, aes(x=date,y= value/1000, color= "U.S. Crude Oil Distillation Capacity"), size = 1.25) +
  geom_line(data=REFINERY_OPERATING_CAPACITY, aes(x=date,y= value/1000, color= "U.S. Crude Oil Distillation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(15,19),breaks = c(15,16,17,18,19), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("Unrefined Results") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Refineries are Running at Nearly Full Capacity") +
  theme_apricitas + theme(legend.position = c(.25,.35)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1400), xmax = as.Date("2019-01-01")-(0.049*1400), ymin = 15-(.3*4), ymax = 15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Petroleum_Profits_Graph <- ggplot() + #plotting US Crude Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=Petrol_Profits, aes(x=Date,y= PETROL_PROFITS, color= "Corporate Profits: Petroleum and Coal Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(-80,120),breaks = c(-50,0,50,100), expand = c(0,0)) +
  ylab("Billions of Dollars, Seasonally Adjusted at Annual Rates") +
  ggtitle("Understanding 'Capital Discipline'") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The US Oil Industry Has Followed a Boom-and-Bust Rollercoaster") +
  theme_apricitas + theme(legend.position = c(.65,.95)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-04-01")-(.1861*5948), xmax = as.Date("2001-04-01")-(0.049*5948), ymin = 3-(.3*11), ymax = 3) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

WTI_Baker_Graph <- ggplot() + #plotting WTI and Baker Hughes
  geom_line(data=WTI, aes(x=date,y= adjusted, color= "Crude Oil Price (WTI) (lhs)"), size = 1.25) +
  geom_line(data=Baker_Hughes, aes(x=Date,y= RIG_COUNT*.069, color= "Baker Hughes Oil Rig Count (rhs)", group = 1), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,150), expand = c(0,0), sec.axis = sec_axis(~.*1/.069, name="Oil Rig Count")) +
  ylab("Dollars") +
  ggtitle("Understanding 'Capital Discipline'") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance and Baker Hughes data",subtitle = "The Post-2012 Relationship Between Oil Prices and Investment is No Longer Holding") +
  theme_apricitas + theme(legend.position = c(.7,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-07-01")-(.1861*5948), xmax = as.Date("2006-07-01")-(0.049*5948), ymin = 0-(.3*150), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

GASREGW_Graph <- ggplot() + #plotting Gas Prices
  geom_line(data=drop_na(GASREGW), aes(x=date,y= value, color= "US Regular All Formulations Gas Price"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,5), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("Pain at the Pump") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Nominal Gas Prices are at Record Highs") +
  theme_apricitas + theme(legend.position = c(.7,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-08-20")-(.1861*11000), xmax = as.Date("1990-08-20")-(0.049*11561), ymin = 0-(.3*5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SPREADS_Graph <- ggplot() + #plotting Gas Prices
  geom_line(data=drop_na(WTIEIA), aes(x=date,y= value, color= "Crude Oil (WTI)"), size = 1.25) +
  geom_line(data=drop_na(DIESELEIA), aes(x=date,y= value*42, color= "Diesel"), size = 1.25) +
  geom_line(data=drop_na(KEROSENEEIA), aes(x=date,y= value*42, color= "Kerosene Type Jet Fuel"), size = 1.25) +
  geom_line(data=drop_na(GASEIA), aes(x=date,y= value*42, color= "Gas (Regular)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,225), expand = c(0,0)) +
  ylab("Dollars Per Barrel") +
  ggtitle("Dawn of the Spread") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Refinery Spreads are Easing But Still High-Especially for Diesel and Jet Fuel") +
  theme_apricitas + theme(legend.position = c(.3,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI)","Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*225), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Spreads_Rbind <- rbind(WTIEIA, GASEIA, DIESELEIA, KEROSENEEIA) %>% pivot_wider(names_from = product, values_from = value)

SPREADS_DISGraph <- ggplot() + #plotting Refinery Spreads
  geom_line(data=drop_na(Spreads_Rbind), aes(x=date,y= (Diesel*42)-Crude, color= "Diesel"), size = 1.25) +
  geom_line(data=drop_na(Spreads_Rbind), aes(x=date,y= (Kerosene_Jet*42)-Crude, color= "Kerosene Type Jet Fuel"), size = 1.25) +
  geom_line(data=drop_na(Spreads_Rbind), aes(x=date,y= (Gasoline*42)-Crude, color= "Gas (Regular)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(-10,125), expand = c(0,0)) +
  ylab("Dollars Per Barrel") +
  ggtitle("Dawn of the Spread") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Spreads for Refined Products Remain High") +
  theme_apricitas + theme(legend.position = c(.4,.80)) +
  scale_color_manual(name= "Refinery Spreads" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -10-(.3*135), ymax = -10) +
  coord_cartesian(clip = "off")

WTI_Graph <- ggplot() + #plotting Crude Futures
  geom_line(data=drop_na(WTI), aes(x=date,y= close, color= "Crude Oil (WTI)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,125), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2019-01-01"),as.Date(today()))) +
  ylab("Dollars Per Barrel") +
  ggtitle("Price Pullback") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "Oil Prices Have Given Up Most of Their Post-War Gains, But Remain High") +
  theme_apricitas + theme(legend.position = c(.6,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI)","Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*125), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

WTI_STEO_Graph <- ggplot() + #plotting Crude Futures
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2023-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-05-30"), y = 20, color = "#EE6055", size = 5, alpha = 0.6) +
  annotate("text", label = "SPR Refill Target", x = as.Date("2023-05-15"), y = 62, color = "white", size = 4.85) +
  annotate("segment", x = floor_date(as.Date(today() -33), "month"), xend = as.Date("2023-12-01"), y = 72, yend = 72, linetype = "dashed", color = "white", size = 1.25) +
  annotate("segment", x = floor_date(as.Date(today() -33), "month"), xend = as.Date("2023-12-01"), y = 67, yend = 67, linetype = "dashed", color = "white", size = 1.25) +
  geom_line(data=drop_na(STEO_WTI), aes(x=date,y= value, color= "Crude Oil (WTI)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,125), expand = c(0,0)) +
  ylab("Dollars Per Barrel") +
  ggtitle("Price Pullback") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Oil Prices Have Given Up Most of Their Post-Invasion Gains, But are Expected to Remain High") +
  theme_apricitas + theme(legend.position = c(.4,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI)","Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+365-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+365-as.Date("2019-01-01"))), ymin = 0-(.3*125), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Capital_Discipline_Graph <- ggplot(Capital_Discipline, aes(x = answer, y = percent))+
  geom_bar(aes(fill = answer), position = "dodge", stat = "identity", width = 0.7, color = NA) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.6), breaks = c(0,.1,.2,.3,.4,.5,.6), expand = c(0,0)) + #adding % format
  theme_apricitas +  theme(legend.position = "none") +
  ylab("Share of Responses from Oil and Gas Executives") +
  xlab(NULL) +
  ggtitle("Understanding 'Capital Discipline'") +
  labs(caption = "Graph created by @JosephPolitano Using Dallas Fed data",subtitle = "Why are Publicly Traded Oil Companies Restraining Growth?") +
  scale_fill_manual(values = c("#FFE98F","#FFE98F","#FFE98F","#FFE98F","#FFE98F","#FFE98F","#FFE98F")) +
  coord_flip()

GAS_EXPENDITURE_Graph <- ggplot() + #plotting sales at gas stations as a share of total sales
  geom_line(data=RETAIL_GAS_ALL, aes(x=date,y= gas/all, color= "Advanced Retail Sales at Gas Stations as a % of All Retail Trade"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.15),breaks = c(0,0.05,0.1,0.15), expand = c(0,0)) +
  ylab("Percent of Total Retail Sales, %") +
  ggtitle("Pain at the Pump") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "The Share of Retail Spending Going to Gas is at the Highest Level in 5 Years") +
  theme_apricitas + theme(legend.position = c(.5,.40)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*11000), xmax = as.Date("1992-01-01")-(0.049*11000), ymin = 0-(.3*.15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SPR_LEVEL_Graph <- ggplot() + #plotting US SPR Crude Oil Stocks
  geom_line(data=SPR_LEVEL, aes(x=date,y= value/1000, color= "Stocks of Crude Oil in the Strategic Petroleum Reserve"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(0,675),breaks = c(0,150,300,450,600), expand = c(0,0)) +
  ylab("Mbbl") +
  ggtitle("Breaking the Emergency Glass") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "The Drawdown in the Strategic Petroleum Reserve is Historicly Large") +
  theme_apricitas + theme(legend.position = c(.45,.78)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*675), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

STEO_Prod_SPR_Graph <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2024-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-04-30"), y = 10.25, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=STEO_SPR_Prod, aes(x=date,y= value.x, color= "US Crude Oil Production"), size = 1.25) +
  geom_line(data=STEO_SPR_Prod, aes(x=date,y= value.x+value.y, color= "US Crude Oil Production With Net SPR Withdrawls"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(9,14),breaks = c(9,10,11,12,13,14), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("Sand Trap") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Marginal US Crude Oil Production Will Be Handed From the SPR to the Private Sector") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01")+365)), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01")+365)), ymin = 9-(.3*5), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

WTI_Curve_10_25_Graph <- ggplot() + #plotting Crude Futures
  geom_line(data=drop_na(Crude_Curve_10_25_2022), aes(x=Contract,y= Last, color= "Crude Oil (WTI) Curve, 10/25"), size = 1.25) +
  annotate("text", label = "SPR Refill Target", x = as.Date("2028-03-25"), y = 70, color = "white", size = 5) +
  annotate("segment", x = as.Date("2022-12-01"), xend = as.Date("2030-01-01"), y = 72, yend = 72, linetype = "dashed", color = "white", size = 1.25) +
  annotate("segment", x = as.Date("2022-12-01"), xend = as.Date("2030-01-01"), y = 67, yend = 67, linetype = "dashed", color = "white", size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(50,90), expand = c(0,0)) +
  scale_x_date(limits = as.Date(c("2022-12-01", "2030-01-01"))) +
  ylab("Dollars Per Barrel") +
  ggtitle("Back to Backwardation") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Oil Prices are Still in Backwardation, With Futures Prices Lower Through 2030") +
  theme_apricitas + theme(legend.position = c(.6,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI) Curve, 10/25")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(3000)), xmax = as.Date("2023-01-01")-(0.049*(3000)), ymin = 50-(.3*40), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_ENERGY_Graph <- ggplot() + #plotting CPI for Different Energy Goods
  geom_line(data=CPI_ELECTRICITY, aes(x=date,y= (value/2.14475) ,color= "Electricity"), size = 1.25) +
  geom_line(data=CPI_UTILITY_GAS, aes(x=date,y= (value/1.70689) ,color= "Utility (Piped) Gas Service"), size = 1.25) +
  geom_line(data=CPI_GASOLINE, aes(x=date,y= (value/2.36591) ,color= "Gasoline"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,180), breaks = c(0,50,100,150), expand = c(0,0)) +
  ylab("CPI: January 2020 = 100") +
  ggtitle("The Energy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for a Variety of Energy Commodities are High") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "Consumer Price Index:",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*160), ymax = 0) +
  coord_cartesian(clip = "off")

OPEC_Crude_Production_STEO <- eia_series("STEO.PAPR_OPEC.M", start = "2019", end = "2026")
OPEC_Crude_Production_STEO <- as.data.frame(OPEC_Crude_Production_STEO$data)

OPEC_Crude_Production_STEO_Graph <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2023-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-04-30"), y = 27.5, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=OPEC_Crude_Production_STEO, aes(x=date,y= value, color= "OPEC Crude Oil Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(25,40),breaks = c(25,30,35,40), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("OPEC Production") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "OPEC Crude Production is Expected to Remain Below the Early 2019 Peak Through 2024") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01")+365)), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01")+365)), ymin = 25-(.3*15), ymax = 25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Large_Solar_Capacity_STEO <- eia_series("STEO.SOEPCAPX_US.M", start = "2019", end = "2026")
Large_Solar_Capacity_STEO <- as.data.frame(Large_Solar_Capacity_STEO$data)

Small_Solar_Capacity_STEO <- eia_series("STEO.SODTC_US.M", start = "2019", end = "2026")
Small_Solar_Capacity_STEO <- as.data.frame(Small_Solar_Capacity_STEO$data)

Solar_Merge_STEO <- merge(Small_Solar_Capacity_STEO,Large_Solar_Capacity_STEO,by = "date")

Wind_Capacity_STEO <- eia_series("STEO.WNEPCAPX_US.M", start = "2019", end = "2026")
Wind_Capacity_STEO <- as.data.frame(Wind_Capacity_STEO$data)

US_Solar_Wind_Graph <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2024-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-04-30"), y = 35, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=Wind_Capacity_STEO, aes(x=date,y= value/1000, color= "US Wind Power Capacity"), size = 1.25) +
  geom_line(data=Solar_Merge_STEO, aes(x=date,y= (value.x + value.y)/1000, color= "US Large and Small Scale Solar Power Capacity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " GW", accuracy = 1), limits = c(0,220),breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Capacity, GW") +
  ggtitle("The Long Term Transition") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "US Renewable Power Capacity is Growing Rapidly") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01")+365)), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01")+365)), ymin = 0-(.3*220), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


Nat_Gas_STEO <- eia_series("STEO.NGMPPUS.M", start = "2019", end = "2026")
Nat_Gas_STEO <- as.data.frame(Nat_Gas_STEO$data)
  

Crude_Russia <- eia_series("INTL.57-1-RUS-TBPD.M", start = "2019")
Crude_Russia <- as.data.frame(Crude_Russia$data)
Crude_Russia <- select(Crude_Russia,date, value) %>%
  mutate(country = "Russia")

Crude_US <- eia_series("INTL.57-1-USA-TBPD.M", start = "2019")
Crude_US <- as.data.frame(Crude_US$data)
Crude_US <- select(Crude_US,date, value) %>%
  mutate(country = "United States")

Crude_OPEC <- eia_series("INTL.57-1-OPEC-TBPD.M", start = "2019")
Crude_OPEC <- as.data.frame(Crude_OPEC$data)
Crude_OPEC <- select(Crude_OPEC,date, value) %>%
  mutate(country = "OPEC")

Crude_NoPEC <- eia_series("INTL.57-1-OPNO-TBPD.M", start = "2019")
Crude_NoPEC <- as.data.frame(Crude_NoPEC$data)

#crude that is not from OPEC, Russia, or the US
Crude_NoPEC_ex_RU <- merge(Crude_NoPEC,Crude_Russia, by = "date") %>%
  mutate(value = value.x-value.y) %>%
  select(.,date, value)

Crude_NoPEC_ex_RU_US <- merge(Crude_NoPEC_ex_RU,Crude_US, by = "date") %>%
  mutate(value = value.x-value.y) %>%
  select(.,date, value) %>%
  mutate(country = "Rest of World")

World_Oil_Merge <- rbind(Crude_OPEC,Crude_US,Crude_Russia,Crude_NoPEC_ex_RU_US)

Crude_Russia_Change <- Crude_Russia %>% 
  mutate(value = value - 10853.362)
Crude_US_Change <- Crude_US %>%
  mutate(value = value - 12859.767)
Crude_OPEC_Change <- Crude_OPEC %>%
  mutate(value = value - 31051.75)
Crude_NoPEC_ex_RU_US_Change <- Crude_NoPEC_ex_RU_US %>% 
  mutate(value = value - 28644.50)

World_Oil_Merge_Change <- rbind(Crude_OPEC_Change,Crude_US_Change,Crude_Russia_Change,Crude_NoPEC_ex_RU_US_Change)

World_Oil_Merge_Change_Graph <- ggplot(data = subset(World_Oil_Merge_Change, date > as.Date("2019-10-01")), aes(x = date, y = value/1000, fill = country)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Mbbl Per Day, Change Since Nov 2019") +
  scale_y_continuous(labels = scales::number_format(suffix = "Mbbl", accuracy = 1), breaks = c(-10,-5,0,5), limits = c(-13.5,2.5), expand = c(0,0)) +
  ggtitle("The Crude Crunch") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "'Missing' Barrels (Compared to Pre-Pandemic) Come From a Variety of Sources") +
  theme_apricitas + theme(legend.position = c(.85,.29)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-11-01")-(.1861*(today()-as.Date("2019-11-01"))), xmax = as.Date("2019-11-01")-(0.049*(today()-as.Date("2019-11-01"))), ymin = -13.5-(.3*16), ymax = -13.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

World_Oil_Merge_Graph <- ggplot(data = World_Oil_Merge, aes(x = date, y = value/1000, fill = country)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Mbbl Per Day") +
  scale_y_continuous(labels = scales::number_format(suffix = "Mbbl", accuracy = 1), breaks = c(0,25,50,75,100), limits = c(0,110), expand = c(0,0)) +
  ggtitle("The Crude Crunch") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "'Missing' Barrels (Compared to Pre-Pandemic) Come From a Variety of Sources") +
  theme_apricitas + theme(legend.position = c(.52,.87)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-120-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-120-as.Date("2019-01-01"))), ymin = 0-(.3*110), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Nat_Gas_STEO <- as.data.frame(Nat_Gas_STEO$data)

Nat_Gas_STEO_Graph <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2023-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-04-30"), y = 27.5, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=Nat_Gas_STEO, aes(x=date,y= value, color= "US Natural Gas Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " Bcf", accuracy = 1), limits = c(93,110),breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Bcf Per Day") +
  ggtitle("Pumping Up") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Fueled by Global Shortages, US Natural Gas Production is Crossing New Highs") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01")+365)), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01")+365)), ymin = 93-(.3*17), ymax = 93) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = VMT_Graph, "VMT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = XOP_Graph, "XOP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Crude_Production_Graph, "Crude Prodution.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Petroleum_Profits_Graph, "Petroleum Profits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = WTI_Baker_Graph, "WTI Baker.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GASREGW_Graph, "Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Capital_Discipline_Graph, "Capital Discipline.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GAS_EXPENDITURE_Graph, "Gas Expenditure.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SPREADS_Graph, "Spreads.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SPREADS_DISGraph, "Spreads Disagg.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Refinery_Capacity_Graph, "Refinery Capacity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = WTI_Graph, "WTI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SPR_LEVEL_Graph, "SPR Level Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STEO_Prod_SPR_Graph, "STEO Prod Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = WTI_STEO_Graph, "WTI STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = WTI_Curve_10_25_Graph, "WTI Curve 10 25.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_ENERGY_Graph, "CPI Energy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = OPEC_Crude_Production_STEO_Graph, "OPEC Crude Oil Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Nat_Gas_STEO_Graph, "Nat Gas STEO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = US_Solar_Wind_Graph, "US Solar and Wind.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = World_Oil_Merge_Change_Graph, "World Oil Merge Change.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = World_Oil_Merge_Graph, "World Oil Merge.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

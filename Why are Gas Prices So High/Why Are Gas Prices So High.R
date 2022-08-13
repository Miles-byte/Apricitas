pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)
pacman::p_load(eia)

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
Crude_ProductionWeekly <- eia_series("PET.WCRFPUS2.W", start = "2019")
Crude_ProductionWeekly <- as.data.frame(Crude_ProductionWeekly$data)
Crude_ProductionMonthly <- eia_series("PET.MCRFPUS2.M", start = "2019")
Crude_ProductionMonthly <- as.data.frame(Crude_ProductionMonthly$data)

#crude, gas, and diesel prices
WTIEIA <- eia_series("PET.RWTC.D", start = 2019, end = today())
WTIEIA <- as.data.frame(WTIEIA$data) %>% mutate(product = "Crude")
GASEIA <- eia_series("PET.EER_EPMRU_PF4_RGC_DPG.D", start = "2019", end = today())
GASEIA <- as.data.frame(GASEIA$data) %>% mutate(product = "Gasoline")
DIESELEIA <- eia_series("PET.EER_EPD2DXL0_PF4_RGC_DPG.D", start = "2019", end = today())
DIESELEIA <- as.data.frame(DIESELEIA$data) %>% mutate(product = "Diesel")
DIESELEIA <- subset(DIESELEIA, date != as.Date("2022-02-21")) #random date has a 0 here, likely due to some error in EIA
KEROSENEEIA <- eia_series("PET.EER_EPJK_PF4_RGC_DPG.D", start = "2019", end = today())
KEROSENEEIA <- as.data.frame(KEROSENEEIA$data)  %>% mutate(product = "Kerosene_Jet")

REFINERY_CAPACITY <- eia_series("PET.MOCLEUS2.M", start = "2019", end = today())
REFINERY_CAPACITY <- as.data.frame(REFINERY_CAPACITY$data)

REFINERY_OPERATING_CAPACITY <- eia_series("PET.MOCGGUS2.M", start = "2019", end = today())
REFINERY_OPERATING_CAPACITY <- as.data.frame(REFINERY_OPERATING_CAPACITY$data)


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
  geom_line(data=Crude_ProductionMonthly, aes(x=date,y= value/1000, color= "US Crude Oil Production (Monthly Official)"), size = 1.25) +
  geom_line(data=Crude_ProductionWeekly, aes(x=date,y= value/1000, color= "US Crude Oil Production (Weekly Estimates)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(9,14),breaks = c(9,10,11,12,13,14), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("Sand Trap") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Oil Production has not Recovered to Pre-Pandemic Levels, Despite the Jump in Prices") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1400), xmax = as.Date("2019-01-01")-(0.049*1400), ymin = 9-(.3*5), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Refinery_Capacity_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=REFINERY_CAPACITY, aes(x=date,y= value/1000, color= "U.S. Crude Oil Distillation Capacity"), size = 1.25) +
  geom_line(data=REFINERY_OPERATING_CAPACITY, aes(x=date,y= value/1000, color= "U.S. Crude Oil Distillation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(15,19),breaks = c(15,16,17,18,19), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("Unrefined Results") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Refinery Capacity Has Shrunk During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.35,.42)) +
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
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Refinery Spreads are High as the World Runs into a Refining Capacity Shortage") +
  theme_apricitas + theme(legend.position = c(.6,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI)","Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1400), xmax = as.Date("2019-01-01")-(0.049*1400), ymin = 0-(.3*225), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Refinery Spreads are High as the World Runs into a Refining Capacity Shortage") +
  theme_apricitas + theme(legend.position = c(.7,.80)) +
  scale_color_manual(name= "Refinery Spreads" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
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


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

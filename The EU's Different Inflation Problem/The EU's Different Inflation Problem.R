pacman::p_load(ecb, eurostat,censusapi,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EU_CPI <- fredr(series_id = "EA19CPALTT01GYM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU inflation
US_CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #US inflation

EU_CORE_CPI <- fredr(series_id = "EA19CPGRLE01GYM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU core inflation
US_CORE_CPI <- fredr(series_id = "CPILFESL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #US core inflation

EU_EMP <- fredr(series_id = "LREM25TTEZQ156S",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU prime age epop
US_EMP <- fredr(series_id = "LREM25TTUSQ156S",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

EU_Nat_Gas <- fredr(series_id = "PNGASEUUSDM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
US_Nat_Gas <- fredr(series_id = "PNGASUSUSDM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

ICE_HY_SPREAD_EURO <- fredr(series_id = "BAMLHE00EHYIOAS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

EURO_NGDP <- fredr(series_id = "EUNNGDP",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US NGDP
US_NGDP <- fredr(series_id = "GDP",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU NGDP

GDPTrend <- data.frame(date = c(seq(as.Date("2019-10-01"), as.Date("2022-10-01"), "months")), trend = 21694.46*1.003274^(0:36))

GRETENYR <- fredr(series_id = "IRLTLT01GRM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
ITATENYR <- fredr(series_id = "IRLTLT01ITM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
PORTENYR <- fredr(series_id = "IRLTLT01PTM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
GERTENYR <- fredr(series_id = "IRLTLT01DEM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

AHEUS <- fredr(series_id = "CES0500000003",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
AHEEU <- fredr(series_id = "LCEAPR01EZQ661S",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
AHEND <- fredr(series_id = "LCWRPR01NLM661S",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
AHEAT <- fredr(series_id = "LCWRIN01ATM661N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

ITA10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/ITALY10YR.csv")
GER10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/GERMANY10YR.csv")
GRE10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/GREECE10YR.csv")

ITA10YR$?..Date <- as.Date(ITA10YR$?..Date, "%m/%d/%Y")
GER10YR$?..Date <- as.Date(GER10YR$?..Date, "%m/%d/%Y")
GRE10YR$?..Date <- as.Date(GRE10YR$?..Date, "%m/%d/%Y")

colnames(ITA10YR) <- c("date","value")
colnames(GER10YR) <- c("date","value")
colnames(GRE10YR) <- c("date","value")

ITASPREADS <- merge(ITA10YR,GER10YR, by = "date")
GRESPREADS <- merge(GRE10YR,GER10YR, by = "date")

SpreadsRbind <- pivot_wider(rbind(GRETENYR,ITATENYR,PORTENYR,GERTENYR), names_from = series_id)

EU_Spreads_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  geom_line(data=SpreadsRbind, aes(x=date,y= (IRLTLT01GRM156N-IRLTLT01DEM156N)/100, color= "Greece"), size = 1.25) +
  geom_line(data=SpreadsRbind, aes(x=date,y= (IRLTLT01ITM156N-IRLTLT01DEM156N)/100, color= "Italy"), size = 1.25) +
  geom_line(data=SpreadsRbind, aes(x=date,y= (IRLTLT01PTM156N-IRLTLT01DEM156N)/100, color= "Portugal"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Spreads with German Bonds, %") +
  ggtitle("European Financial Tightening") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Financial Tightening is Increasing Eurozone Bond Spreads Again") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "Spreads Between 10 Year German Government Bonds",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

US_EU_CPI_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=EU_CPI, aes(x=date,y= (value)/100, color= "Eurozone CPI"), size = 1.25) +
  geom_line(data=US_CPI, aes(x=date,y= (value)/100, color= "US CPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.01,0.09), breaks = c(0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Change From Year Ago, %") +
  ggtitle("The Global Inflation Problem?") +
  labs(caption = "Graph created by @JosephPolitano using OECD and BLS data",subtitle = "Headline Inflation is High in the Eurozone and US") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -0.01-(.3*0.1), ymax = -0.01) +
  coord_cartesian(clip = "off")

US_EU_CORE_CPI_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=EU_CORE_CPI, aes(x=date,y= (value)/100, color= "Eurozone Core CPI"), size = 1.25) +
  geom_line(data=US_CORE_CPI, aes(x=date,y= (value)/100, color= "US Core CPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.01,0.09), breaks = c(0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Change From Year Ago, %") +
  ggtitle("The Global Inflation Problem?") +
  labs(caption = "Graph created by @JosephPolitano using OECD and BLS data",subtitle = "Core Inflation Remains Much Weaker in the Eurozone") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -0.01-(.3*0.1), ymax = -0.01) +
  coord_cartesian(clip = "off")

US_EU_EPOP_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  geom_line(data=EU_EMP, aes(x=date,y= (value)/100, color= "Eurozone Prime Age (25-54) Employment Rate"), size = 1.25) +
  geom_line(data=US_EMP, aes(x=date,y= (value)/100, color= "US Prime Age (25-54) Employment Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.71,.84), breaks = c(0.72,0.74,0.76,0.78,0.80,0.82,0.84), expand = c(0,0)) +
  ylab("EmploymenT Rate, %") +
  ggtitle("Job Retention Schemes") +
  labs(caption = "Graph created by @JosephPolitano using OECD and BLS data",subtitle = "Job Retention Schemes Kept Employment From Falling in the EU") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0.71-(.3*0.12), ymax = 0.71) +
  coord_cartesian(clip = "off")

EU_Spreads_Graph_2 <- ggplot() + #plotting bond spreads in the EU
  geom_line(data=GRESPREADS, aes(x=date,y= (value.x-value.y)/100, color= "Greece"), size = 1.25) +
  geom_line(data=ITASPREADS, aes(x=date,y= (value.x-value.y)/100, color= "Italy"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Spreads with German Bonds, %") +
  ggtitle("When All You Have is A Hammer...") +
  labs(caption = "Graph created by @JosephPolitano using Investing.com data",subtitle = "Financial Tightening is Increasing Eurozone Bond Spreads Again") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "Spreads Between 10 Year German Government Bonds",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

US_EU_NGDP_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = EURO_NGDP, aes(x=date, y = value/30252.62, color = "Eurozone NGDP"), size = 1.25) + 
  geom_line(data = US_NGDP, aes(x=date, y = value/216.9446 , color = "US NGDP"), size = 1.25) + 
  geom_line(data = GDPTrend, aes(x=date, y = trend/216.9446, color = "4% NGDP Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,120), breaks = c(85,90,95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("The EU's Different Inflation Problem") +
  labs(caption = "Graph created by @JosephPolitano using BEA and EuroStat data",subtitle = "Aggregate Spending is Above Trend in the US-But Not in the EU") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("US NGDP","Eurozone NGDP","4% NGDP Growth Trend"),guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 85-(.3*35), ymax = 85) +
  coord_cartesian(clip = "off")

US_EU_Wage_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = AHEUS, aes(x=date, y = value/.2843, color = "Average Hourly Earnings: Private Sector: US"), size = 1.25) + 
  geom_line(data = AHEND, aes(x=date, y = value/1.095978, color = "Average Wage Rate: Private Sector: Netherlands"), size = 1.25) + 
  geom_line(data = AHEAT, aes(x=date, y = value/1.118094, color = "Average Wage Rate: Industry: Austria"), size = 1.25) + 
  geom_line(data = AHEEU, aes(x=date, y = value/1.109704 , color = "Average Hourly Earnings: Private Sector: Eurozone"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(95,115), breaks = c(95,100,105,110,115), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("A Tale of Two Labor Markets") +
  labs(caption = "Graph created by @JosephPolitano using BLS and OECD data",subtitle = "Nominal Wages Are Rising Rapidly in the US-but not in the EU") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Average Hourly Earnings: Private Sector: US","Average Hourly Earnings: Private Sector: Eurozone","Average Wage Rate: Private Sector: Netherlands","Average Wage Rate: Industry: Austria")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 95-(.3*20), ymax = 95) +
  coord_cartesian(clip = "off")

TTF_FUTURES <- tq_get("TTFQ22.NYM", from = "2019-01-01") #Dutch TTF Futures
HENRY_HUB <- tq_get("NGQ22.NYM", from = "2019-01-01") #US Nat Gas Futures
#USDEUEX <- fredr(series_id = "DEXUSEU", observation_start = as.Date("2019-01-01")) #US Euro Exchange Rate
USDEUEX <- tq_get("USDEUR=X", from = "2019-01-01") #US Euro Exchange Rate

TTF_USD <- merge(TTF_FUTURES, USDEUEX, by = "date")
TTF_USD <- select(TTF_USD, c("date","close.x","close.y"))
colnames(TTF_USD) <- c("date","TTF","EUUSD")
TTF_USD <- transmute(TTF_USD, date, TTFUSD = (TTF/EUUSD)/3.639)
TTF_USD <- drop_na(TTF_USD)

US_EU_NAT_GAS_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  geom_line(data=US_Nat_Gas, aes(x=date,y= value, color= "US Natural Gas"), size = 1.25) +
  geom_line(data=EU_Nat_Gas, aes(x=date,y= value, color= "EU Natural Gas"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,35), breaks = c(0,10,20,30), expand = c(0,0)) +
  ylab("US Dollars per MMBtu") +
  ggtitle("The EU's Different Inflation Problem") +
  labs(caption = "Graph created by @JosephPolitano using IMF data",subtitle = "Energy Prices Are Spiking in the EU, Pulling Up Inflation") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*35), ymax = 0) +
  coord_cartesian(clip = "off")

US_EU_NAT_GAS_FUTURES_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  geom_line(data=HENRY_HUB, aes(x=date,y= close, color= "US Natural Gas (Henry Hub)"), size = 1.25) +
  geom_line(data=TTF_USD, aes(x=date,y= TTFUSD, color= "EU Natural Gas (TTF)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,60), breaks = c(0,10,20,30,40,50,60), expand = c(0,0)) +
  ylab("US Dollars per MMBtu") +
  ggtitle("The EU's Different Inflation Problem") +
  labs(caption = "Graph created by @JosephPolitano using IMF data",subtitle = "Energy Prices Are Spiking in the EU, Pulling Up Inflation") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "August 2022 Futures",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1300), xmax = as.Date("2019-01-01")-(0.049*1300), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

#using getcensus to import data on us exports of nat gas by weight
US_NAT_GAS_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "VES_WGT_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "2711110000", #nat gas commodity code
  CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
)

US_NAT_GAS_EXPORTS$time <- as.Date(as.yearmon(US_NAT_GAS_EXPORTS$time))
US_NAT_GAS_EXPORTS$VES_WGT_MO <- as.numeric(US_NAT_GAS_EXPORTS$VES_WGT_MO)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("-","All US LNG Exports",US_NAT_GAS_EXPORTS$CTY_CODE)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("4XXX","US LNG Exports to Europe",US_NAT_GAS_EXPORTS$CTY_CODE)

US_NAT_GAS_EXPORTS_Graph <- ggplot() + #plotting components of excess savings
  geom_area(data = US_NAT_GAS_EXPORTS, aes(x = time, y = VES_WGT_MO/1000000000, fill = CTY_CODE), color = NA, size = 0, position = "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "B", accuracy = 1),limits = c(0,8), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  ylab("Billions of kg") +
  ggtitle("Arsenal of Democracy") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Natural Gas Exports Are Helping Ease A European Energy Shortage") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*2250), xmax = as.Date("2016-01-01")-(0.049*2250), ymin = 0-(.3*8), ymax = 0) +
  coord_cartesian(clip = "off")

#EU Stacked Nat Gas Imports

EU_RU_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","RU","UA","BY","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(BY:RU))) %>%
  select(time,values) %>%
  mutate(partner = "Russia, Ukraine, and Belarus")

EU_US_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","US","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  select(partner, time, values) %>%
  mutate(partner = "United States")

EU_NO_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","NO","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values)%>%
  mutate(partner = "Norway")

EU_QA_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","QA","NG","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(QA:NG))) %>%
  select(time,values) %>%
  mutate(partner = "Qatar and Nigeria")

EU_AL_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","DZ","MA","TN","LY","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(DZ:TN))) %>%
  select(time,values) %>%
  mutate(partner = "Algeria, Tunisia, Morocco, and Libya")

EU_OTHER_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                          filters=c("EU27_2020","MIO_M3","G3000"),
                                          date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  select(-TOTAL,-EUR_OTH,-BE,-BG,-CZ,-DK,-DE,-EE,-IE,-EL,-ES,-FR,-HR,-IT,-CY,-LV,-LT,-LU,-HU,-MT,-NL,-AT,-PL,-PT,-RO,-SI,-SK,-FI,-SE,-NO,-DZ,-US,-QA,-RU,-UA,-BY,-CH,-MA,-TN,-LY,-NG) %>%
  rowwise() %>%
  mutate(values = sum(c_across(AD:ZA))) %>%
  select(time,values) %>%
  mutate(partner = "Other (Including Re-Exports from UK/Turkey/etc)")

EU_STACKED_GAS_IMPORTS <- rbind(EU_OTHER_GAS_IMPORTS,EU_AL_GAS_IMPORTS,EU_QA_GAS_IMPORTS,EU_NO_GAS_IMPORTS,EU_US_GAS_IMPORTS,EU_RU_GAS_IMPORTS) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  pivot_longer(cols = c(`Other (Including Re-Exports from UK/Turkey/etc)`:`Russia, Ukraine, and Belarus`)) %>%
  mutate(name = factor(name,levels = c("Other (Including Re-Exports from UK/Turkey/etc)","United States","Qatar and Nigeria","Algeria, Tunisia, Morocco, and Libya","Norway","Russia, Ukraine, and Belarus")))

EU_STACKED_GAS_IMPORTS_graph <- ggplot(data = EU_STACKED_GAS_IMPORTS, aes(x = time, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Cubic Meters") +
  ggtitle("EU-27 Natural Gas Imports") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40), limits = c(0,47.5), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "Imports Through Russia are Down Significantly, But the EU is Making Up the Difference") +
  theme_apricitas + theme(legend.position = c(.325,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Russia, Ukraine, and Belarus","Norway","Algeria, Tunisia, Morocco, and Libya","Qatar and Nigeria","United States","Other (Including Re-Exports from UK/Turkey/etc)")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*47.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_STACKED_GAS_IMPORTS_graph, "EU Stacked Gas Imports.png", type = "cairo-png") #cairo gets rid of anti aliasing

#
EA_MANU_SURVEY <- get_eurostat_data("EI_BSIN_Q_R2",
                                    filters=c("EA19","SA","BS-FLP1-PC","BS-FLP2-PC","BS-FLP3-PC","BS-FLP4-PC","BS-FLP5-PC","BS-FLP6-PC"),
                                    date_filter=">2004-01-01") %>%
  mutate(time = as.Date(as.yearqtr(time,"%Y-Q%q"))) %>%
  subset(geo == "EA19") %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EA_MANU_SURVEY_DEMAND_Materials_graph <- ggplot() + #plotting BIE
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= (`BS-FLP1-PC`+`BS-FLP2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP4-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP6-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP5-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.60), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Manufacturers Say Materials Constraints are Binding, but Easing") +
  theme_apricitas + theme(legend.position = c(.45,.45)) +
  scale_color_manual(name= "Factors Limiting Production in EA-19 Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials and Equipment","Other (Including COVID)","Financial Constraints")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.60), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_MANU_SURVEY_DEMAND_Materials_graph, "EA_MANU_SURVEY_DEMAND_MATERIALS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#
EA_SERV_SURVEY <- get_eurostat_data("EI_BSSE_Q_R2",
                                    filters=c("EA19","SA","BS-FLB1-PC","BS-FLB2-PC","BS-FLB3-PC","BS-FLB4-PC","BS-FLB5-PC","BS-FLB6-PC"),
                                    date_filter=">2004-01-01") %>%
  mutate(time = as.Date(as.yearqtr(time,"%Y-Q%q"))) %>%
  subset(geo == "EA19") %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EU_SERV_SURVEY_graph <- ggplot() + #plotting BIE
  #geom_line(data=EA_SERV_SURVEY, aes(x=time,y= (`BS-FLB1-PC`+`BS-FLB2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB4-PC`/100,color= "Shortage of Materials, Equipment, or Space"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB5-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB6-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.6), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Service Sector Firms Say Labor Constraints are Tight as COVID Constraints Ease") +
  theme_apricitas + theme(legend.position = c(.50,.67)) +
  scale_color_manual(name= "Factors Limiting Production in EA-19 Service Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials, Equipment, or Space","Other (Including COVID)","Financial Constraints")) + #, breaks = c("None or Insufficient Demand","Shortage of Materials, Equipment, or Space","Shortage of Labor","Financial Constraints","Other (Including COVID)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.6), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_SERV_SURVEY_graph, "EA_Serv Survey.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Fin Constraints
EA_FIN_CONSTRAINTS_graph <- ggplot() + #plotting BIE
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB5-PC`/100,color= "Service Sector"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP6-PC`/100,color= "Manufacturing Sector"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = c(0,.05,0.1,0.15,0.2), limits = c(0,.225), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Tightening Again") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Firms' Production is Being Hit By Rising Rates and Tightening Monetary Policy") +
  theme_apricitas + theme(legend.position = c(.50,.89)) +
  scale_color_manual(name= "Share of EA-19 Firms Citing Financial Constraints as an Impediment to Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Service Sector","Manufacturing Sector")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.23), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_FIN_CONSTRAINTS_graph, "EA Fin Constraints Survey.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Lack of Demand
EA_NO_DEMAND_graph <- ggplot() + #plotting BIE
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB2-PC`/100,color= "Service Sector"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP2-PC`/100,color= "Manufacturing Sector"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = c(0.2,0.4,0.6), limits = c(0,.65), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Demand Chains") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Firms Experienced Extremely High Demand in 2021—Especially in Manufacturing") +
  theme_apricitas + theme(legend.position = c(.50,.15)) +
  scale_color_manual(name= "Share of EA-19 Firms Citing Insufficient Demand as an Impediment to Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Service Sector","Manufacturing Sector")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.65), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_NO_DEMAND_graph, "EA Demand Constraints.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#INDPRO MOTOR VEHICLE
EA_IND_PRO_VEHICLE <- get_eurostat_data("STS_INPR_M",
                                    filters=c("EA19","SCA","C291","C2931","C2932","I15"),
                                    date_filter=">2019-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(nace_r2, time, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values)

EA_IND_PRO_VEHICLE_graph <- ggplot() + #plotting BIE
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C2932/C2932[1]*100,color= "Other Parts & Accessories for Motor Vehicles"), size = 1.25) +
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C2931/C2931[1]*100,color= "Electrical & Electronic Parts for Motor Vehicles"), size = 1.25) +
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C291/C291[1]*100,color= "Motor Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),breaks = c(0,20, 40,60,80,100), limits = c(0,110), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("The Eurozone Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "European Auto Output Has Struggled Amidst the Chip Shortage and Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.69,.15)) +
  scale_color_manual(name= "EA-19 Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Motor Vehicles","Electrical & Electronic Parts for Motor Vehicles","Other Parts & Accessories for Motor Vehicles")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(.1861*(today()-as.Date("2019-01-01")))), ymin = 0-(.3*110), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_IND_PRO_VEHICLE_graph, "EA INDPRO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Durables, Vehicles, etc inflation
HICP_DURABLES_CAR <- get_eurostat_data("PRC_HICP_MANR",
                                        filters=c("EA19","IGD_NNRG_D","CP07111","CP07112"),
                                        date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(coicop, time, values) %>%
  pivot_wider(names_from = coicop, values_from = values)

HICP_DURABLES_CAR_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data=HICP_DURABLES_CAR, aes(x=time,y= CP07111/100,color= "New Motor Cars"), size = 1.25) +
  geom_line(data=HICP_DURABLES_CAR, aes(x=time,y= CP07112/100,color= "Second-hand Motor Cars"), size = 1.25) +
  geom_line(data=HICP_DURABLES_CAR, aes(x=time,y= IGD_NNRG_D/100,color= "Durable Goods"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.13), breaks = c(-.02,0,.02,.04,.06,.08,.1,.12), expand = c(0,0)) +
  ylab("Annual Price Increase, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Durable Goods Inflation in the Eurozone is Extremely High") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "EA-19 HICP Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Durable Goods","New Motor Cars","Second-hand Motor Cars")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(.1861*(today()-as.Date("2018-01-01")))), ymin = -.025-(.3*.155), ymax = -.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HICP_DURABLES_CAR_graph, "HICP Durables.png", type = "cairo-png") #cairo gets rid of anti aliasing

#HICP rent

HICP_RENT_SERVICES <- get_eurostat_data("PRC_HICP_MANR",
                                       filters=c("EA19","CP041","SERV"),
                                       date_filter=">2000-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(coicop, time, values) %>%
  pivot_wider(names_from = coicop, values_from = values)

HICP_RENT_SERVICES_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data=HICP_RENT_SERVICES, aes(x=time,y= SERV/100,color= "Services"), size = 1.25) +
  geom_line(data=HICP_RENT_SERVICES, aes(x=time,y= CP041/100,color= "Actual Rentals for Housing"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab("Annual Price Increase, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Services Inflation in the Eurozone has Been Comparatively Muted but is Now Rising Rapidly") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "EA-19 HICP Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Services","Actual Rentals for Housing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(.1861*(today()-as.Date("2000-01-01")))), ymin = 0-(.3*.05), ymax = .0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HICP_RENT_SERVICES_graph, "HICP Rent Services.png", type = "cairo-png") #cairo gets rid of anti aliasing
#Retail Sales and Services Turnover
SERVICES_TURNOVER <- get_eurostat_data("STS_SETU_Q",
                                        filters=c("EA19","PCH_SM","CA","H-N_STS"),
                                        date_filter=">2000-01-01") %>%
  mutate(time = as.Date(as.yearqtr(time,"%Y-Q%q"))) %>%
  select(nace_r2, time, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values)

RETAIL_TURNOVER <- get_eurostat_data("STS_TRTU_Q",
                                     filters=c("TOVT","EA19","PCH_SM","CA","G47_X_G473"),
                                     date_filter=">2000-01-01") %>%
  mutate(time = as.Date(as.yearqtr(time,"%Y-Q%q"))) %>%
  select(nace_r2, time, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values)

RETAIL_SERVICES_TURNOVER_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data=SERVICES_TURNOVER, aes(x=time,y= `H-N_STS`/100,color= "Service Sales"), size = 1.25) +
  geom_line(data=RETAIL_TURNOVER, aes(x=time,y= `G47_X_G473`/100,color= "Retail Sales Excluding Motor Vehicles and Fuel"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.25,.25), breaks = c(-.2,-.1,0,0.1,0.2), expand = c(0,0)) +
  ylab("Annual Nominal Increase, Percent") +
  ggtitle("The Spending Surge") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Nominal Spending Growth in the Eurozone is Extremely High") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "EA-19 Turnover Growth, Year-on-Year",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Service Sales","Retail Sales Excluding Motor Vehicles and Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(.1861*(today()-as.Date("2000-01-01")))), ymin = -.25-(.3*.5), ymax = -.25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_SERVICES_TURNOVER_graph, "Retail Services Turnover.png", type = "cairo-png") #cairo gets rid of anti aliasing
#Employment Expectations
EMP_EXP_IND <- get_eurostat_data("EI_BSEE_M_R2",
                                   filters=c("INX","EA19"),
                                   date_filter=">2006-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(indic, time, values)

EMP_EXP_BAL <- get_eurostat_data("EI_BSEE_M_R2",
                                 filters=c("BAL","EA19"),
                                 date_filter=">2006-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EMP_EXP_IND_graph <- ggplot() + #plotting BIE
  geom_line(data=EMP_EXP_IND, aes(x=time,y= values,color= "EA-19 Firms' 3-Month Ahead Employment Growth Expectations"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),breaks = c(50,60,70,80,90,100,110,120), limits = c(50,120), expand = c(0,0)) +
  ylab("Index, 100 = Normal") +
  ggtitle("Eurozone Employment Expectations") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "European Firms' Employment Growth Expectations Have Fallen Back to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.40,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(.1861*(today()-as.Date("2006-01-01")))), ymin = 50-(.3*70), ymax = 50) +
  coord_cartesian(clip = "off")

EMP_EXP_BAL_graph <- ggplot() + #plotting BIE
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=EMP_EXP_BAL, aes(x=time,y= `BS-CEME-BAL`,color= "Construction"), size = 1.25) +
  geom_line(data=EMP_EXP_BAL, aes(x=time,y= `BS-REM-BAL`,color= "Retail Trade"), size = 1.25) +
  geom_line(data=EMP_EXP_BAL, aes(x=time,y= `BS-IEME-BAL`,color= "Industry"), size = 1.25) +
  geom_line(data=EMP_EXP_BAL, aes(x=time,y= `BS-SEEM-BAL`,color= "Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),breaks = c(-40,-30,-20,-10,0,10,20), limits = c(-45,20), expand = c(0,0)) +
  ylab("Balance, Increasing Minus Decreasing") +
  ggtitle("Eurozone Employment Expectations") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "European Companies' Employment Growth Expectations Have Decreased Marginally") +
  theme_apricitas + theme(legend.position = c(.54,.185)) +
  scale_color_manual(name= "EA-19 Firms' 3M Employment Expectations",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Services","Industry","Construction","Retail Trade")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(.1861*(today()-as.Date("2006-01-01")))), ymin = -45-(.3*65), ymax = -45) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_BAL_graph, "Employment Expectations Balance.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EMP_EXP_IND_graph, "Employment Expectations Index.png", type = "cairo-png") #cairo gets rid of anti aliasing

#wage growth
NEGOTIATED <- get_data("STS.Q.I8.N.INWR.000000.3.ANR", filter = list(startPeriod = "2000-Q1")) %>%
  mutate(obstime = as.Date(as.yearqtr(obstime,"%Y-Q%q")))

LCI <- get_eurostat_data("LC_LCI_R2_Q",
                         filters=c("SCA","EA19","B-S","D1_D4_MD5","PCH_SM"),
                         date_filter=">2006-01-01") %>%
  mutate(time = as.Date(as.yearqtr(time,"%Y-Q%q")))

NEGOTIATED_LCI_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data=LCI, aes(x=time,y= values/100, color= "EA-19 Labor Cost Index, Compensation, All Industries"), size = 1.25) +
  geom_line(data=NEGOTIATED, aes(x=obstime,y= obsvalue/100,color= "EA-19 Negotiated Wage Growth"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.002,0.04), breaks = c(0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("Annual Nominal Increase, Percent") +
  ggtitle("Eurozone Nominal Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat and ECB data",subtitle = "Right Now, Eurozone Wage Growth Remains Just Slightly Above Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.35,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EA-19 Labor Cost Index, Compensation, All Industries","EA-19 Negotiated Wage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(.1861*(today()-as.Date("2000-01-01")))), ymin = -.002-(.3*.042), ymax = -.002) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NEGOTIATED_LCI_graph, "Negotiated LCI.png", type = "cairo-png") #cairo gets rid of anti aliasing


#HICP Contribution
PRC_HICP_CTRB <- get_eurostat_data("PRC_HICP_CTRB",
  filters=c("FOOD","IGD_NNRG","NRG","SERV"),
  date_filter=">2001-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(coicop, time, values) %>%
  mutate(coicop = gsub("FOOD","Food, Alcohol, and Tobacco",coicop)) %>%
  mutate(coicop = gsub("IGD_NNRG", "Non-Energy Industrial Goods",coicop)) %>%
  mutate(coicop = gsub("NRG", "Energy",coicop)) %>%
  mutate(coicop = gsub("SERV", "Services",coicop))
  
HICP_CONTRIBUTION <- ggplot() + #plotting components of annual inflation
  geom_bar(data = PRC_HICP_CTRB, aes(x = time, y = values/100, fill = coicop), color = NA, width = 31, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.11), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Eurozone Inflation is More About Supply Shocks to Food, Energy, and Goods") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_fill_manual(name= "Contributions to Eurozone HICP Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services","Non-Energy Industrial Goods","Energy","Food, Alcohol, and Tobacco"), labels = c("Services","Non-Energy Industrial Goods","Energy","Food, Alcohol, and Tobacco")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = -0.025-(.3*.135), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HICP_CONTRIBUTION, "HICP Contribution.png", type = "cairo-png") #cairo gets rid of anti aliasing



ggsave(dpi = "retina",plot = US_EU_NAT_GAS_Graph, "US EU NAT GAS PRICES.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_NAT_GAS_EXPORTS_Graph, "US NAT GAS EXPORTS.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_NGDP_Graph, "US EU NGDP.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EU_Spreads_Graph, "EU Spreads.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EU_Spreads_Graph_2, "EU Spreads 2.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_Wage_Graph, "US EU Wage.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_NAT_GAS_FUTURES_Graph, "US EU Nat Gas Futures.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_CORE_CPI_Graph, "US EU Core CPI.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_CPI_Graph, "US EU CPI.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_EPOP_Graph, "US EU EPOP.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
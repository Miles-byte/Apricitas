pacman::p_load(ggpubr,ggrepel,tigris,purrr,forecast,imputeTS,tsibble,sf,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

install_github("keberwein/blscrapeR")
library(blscrapeR)

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

RGDP <- fredr(series_id = "GDPC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP
RGDI <- fredr(series_id = "A261RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDI
RGDO <- fredr(series_id = "LB0000091Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDI

PAYEMS <- fredr(series_id = "PAYEMS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Nonfarm Payrolls
ELEV <- fredr(series_id = "CE16OV",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Employment Levels
CPSADJ <- bls_api("LNS16000000", startyear = 2019) %>% #headline cpiadj
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

RFSALEDOMPRIV <- fredr(series_id = "LB0000031Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

RFSALEDOMPRIV_PCT <- fredr(series_id = "LB0000031Q020SBEA",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL, units = "cca")

REAL_PERSONAL_INCOME_LESS_TRANSFERS <- fredr(series_id = "W875RX1",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

RealPrivateInventories <- fredr(series_id = "A371RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

RGDPQuarterly <- fredr(series_id = "A191RL1Q225SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
PCEContribQuarterly <- fredr(series_id = "DPCERY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
IVSTContribQuarterly <- fredr(series_id = "A006RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
NEXContribQuarterly <- fredr(series_id = "A019RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
GOVContribQuarterly <- fredr(series_id = "A822RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 

ContribQuarterlyBind <- rbind(PCEContribQuarterly,IVSTContribQuarterly,NEXContribQuarterly,GOVContribQuarterly)

ContribQuarterlyBind$series_id <- gsub("DPCERY2Q224SBEA", "Consumption", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A006RY2Q224SBEA", "Investment", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A019RY2Q224SBEA", "Net Exports", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A822RY2Q224SBEA", "Government", ContribQuarterlyBind$series_id)

LaborProductivity <- fredr(series_id = "OPHNFB",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pca") #labor productivity

TradeDeficit <- fredr(series_id = "BOPGTB",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #trade deficit

Port_Throughput <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/021022/PortThroughput.csv")
Port_Throughput$Date <- as.Date(Port_Throughput$Date, "%m/%d/%Y")

REAL_GAS <- fredr(series_id = "DGOERX1Q020SBEA",observation_start = as.Date("2002-01-01"),realtime_start = NULL, realtime_end = NULL)

#Industrial Production
INDPRO <- fredr(series_id = "INDPRO",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
IPMAN <- fredr(series_id = "IPMAN",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

#Fixed investment
FIXED_RESIDENTIAL <- fredr(series_id = "PRFIC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
FIXED_INDUSTRIAL <- fredr(series_id = "A680RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)


NGDP <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
NGDI <- fredr(series_id = "GDI",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
#ngdp and statistical discrepancy merge
NGDP1947 <- fredr(series_id = "GDP",observation_start = as.Date("1947-01-01"),realtime_start = NULL, realtime_end = NULL)
STATDISC <- fredr(series_id = "A030RC1Q027SBEA",observation_start = as.Date("1947-01-01"),realtime_start = NULL, realtime_end = NULL)

STATDISC_MERGE <- merge(NGDP1947,STATDISC, by = "date")

#disposable personal income and spending
DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(DSPI$date, n=1), "months")), trend = 16622.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(DSPI$date, n=1), by = 'month')) - 1))) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(POUT$date, n=1), "months")), trend = 15328.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(POUT$date, n=1), by = 'month')) - 1)))

#Corporate Profits With IvA
CORP_PROFITS_IVA <- fredr(series_id = "A445RC1Q027SBEA",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
CORP_PROFITS <- fredr(series_id = "A446RC1Q027SBEA",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays

#GDI Wages vs PCE
GDI_Employees <- fredr(series_id = "A4102C1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
PCE <- fredr(series_id = "PCEC",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)

#CIPI
CIPI <- fredr(series_id = "CBI",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
INVENTORIES_NOMINAL_CHANGE <- fredr(series_id = "A371RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "chg")

#Gross Output and Industrial Production
GROSS_OUTPUT_MANUFACTURING <- fredr(series_id = "GOQIMA",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)
INDUSTRIAL_PRODUCTION_MANUFACTURING <- fredr(series_id = "IPMAN",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL, aggregation_method = "sum", frequency = "q")

#fixed investment
FIXED_SOFTWARE <- fredr(series_id = "B985RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
FIXED_RD <- fredr(series_id = "Y006RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
FIXED_COMPUTER <- fredr(series_id = "Y034RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)

NGDP <- fredr(series_id = "GDP",observation_start = as.Date("2018-01-01")) #downloading NGDP 
NGDPTrend <- data.frame(date = c(as.Date("2019-10-01"),as.Date("2022-10-01")),trend = c(21694.46,23267.40)) #creating 4% NGDP growth trend
NGDPTrend <- data.frame(date = c(seq(as.Date("2019-10-01"), tail(NGDP$date, n=1), "3 months")), trend = 21694.46*(1.003274^3)^(0:(length(seq(from = as.Date("2019-10-01"), to = tail(NGDP$date, n=1), by = '3 months')) - 1)))


NGDP_Graph <- ggplot() +
  geom_line(data = NGDP, aes(x=date, y = value/1000, color = "Nominal Gross Domestic Product"), size = 1.25) + 
  geom_line(data = NGDPTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Annual NGDP Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"),limits = c(19,26), breaks = c(19,20,21,22,23,24,25,26), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("Off Trend") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Gross Domestic Product is Significantly Above its Pre-Pandemic Trend") +
  theme_apricitas + theme(legend.position = c(.30,.7)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 19-(.3*7), ymax = 19) +
  coord_cartesian(clip = "off")

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), linewidth = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), linewidth = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Personal Income Remains on Trend, But Spending is Above Trend as Excess Savings Decrease") +
  theme_apricitas + theme(legend.position = c(.31,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), linewidth = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

STAT_DISC_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = STATDISC_MERGE, aes(x=date, y = (value.y/value.x), color = "GDP v GDI Statistical Discrepancy, % of GDP"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.040,0.040), breaks = c(-0.04,-.03,-0.02,-0.01,0,0.01,0.020,0.03,0.04), expand = c(0,0)) +
  ylab("Continuously Compounded Annual Change") +
  ggtitle("Anomaly Detected") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The GDP/GDI Discrepancy is Historically Large (With the Caveat that Recent Data is Unrevised)") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*(today()-as.Date("1947-01-01"))), xmax = as.Date("1947-01-01")-(0.049*(today()-as.Date("1947-01-01"))), ymin = -0.040-(.3*0.080), ymax = -0.040) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Corp Profits
Profits_Graph <- ggplot() +
  geom_line(data = CORP_PROFITS, aes(x=date, y = value/1000, color = "Pretax Corporate Profits of Domestic Business"), size = 1.25) + 
  geom_line(data = CORP_PROFITS_IVA, aes(x=date, y = value/1000, color = "Pretax Corporate Profits of Domestic Business with IVA and CCADJ"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(0,3), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Trillions of US Dollars") +
  ggtitle("The Trillion Dollar Mystery") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Overestimation of Corporate Profits Could be Contributing to the GDP-GDI Gap") +
  theme_apricitas + theme(legend.position = c(.45,.4)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CIPI_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = CIPI, aes(x=date, y = value, color = "Drawdown or Accumulation of Inventories"), size = 1.25) + 
  geom_line(data = INVENTORIES_NOMINAL_CHANGE, aes(x=date, y = value, color = "Change In Value for Existing Inventories Plus Inventory Drawdown/Accumulation"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-300,350), breaks = c(-300,-150,0,150,300), expand = c(0,0)) +
  ylab("Brillions of US Dollars") +
  ggtitle("The Trillion Dollar Mystery") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Underestimation of Inventory Growth Could be Contributing to the GDP-GDI Gap") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Drawdown or Accumulation of Inventories","Change In Value for Existing Inventories Plus Inventory Drawdown/Accumulation")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -300-(.3*650), ymax = -300) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

MANUFACTURING_Graph <- ggplot() +
  geom_line(data = GROSS_OUTPUT_MANUFACTURING, aes(x=date, y = value/1.03, color = "BEA: Real Gross Output, Manufacturing"), size = 1.25) + 
  geom_line(data = INDUSTRIAL_PRODUCTION_MANUFACTURING, aes(x=date, y = value/2.95, color = "FRB: Industrial Production, Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,110), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, Q1 2005 = 100") +
  ggtitle("The Trillion Dollar Mystery") +
  labs(caption = "Graph created by @JosephPolitano using BEA/FRB data",subtitle = "Misestimation of Manufacturing Data Could be Reducing GDP Data") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 80-(.3*30), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_INVESTMENT <- ggplot() +
  geom_line(data = FIXED_COMPUTER, aes(x=date, y = value, color = "Fixed Investment: Information Processing Equipment"), size = 1.25) + 
  geom_line(data = FIXED_SOFTWARE, aes(x=date, y = value, color = "Fixed Investment: Intellectual Property: Software"), size = 1.25) + 
  geom_line(data = FIXED_RD, aes(x=date, y = value, color = "Fixed Investment: Intellectual Property: R&D"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(300,800), breaks = c(300,400,500,600,700), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("The Trillion Dollar Mystery") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Spending on Tech Investment has Been Strong, But Not Spectacular") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 300-(.3*500), ymax = 300) +
  coord_cartesian(clip = "off")

#Graphing GDP
NGDP_Graph <- ggplot() +
  geom_line(data = NGDP, aes(x=date, y = value/1000, color = "Nominal GDP"), size = 1.25) + 
  geom_line(data = NGDI, aes(x=date, y = value/1000, color = "Nominal GDI"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(19,26), breaks = c(19,20,21,22,23,24,25,26), expand = c(0,0)) +
  ylab("Trillions of US Dollars") +
  ggtitle("The Trillion Dollar Mystery") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "'Equivalent' Official Measures of Aggregate Output Are Diverging") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Nominal GDP","Nominal GDI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 19-(.3*7), ymax = 19) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

GDI_PCE <- ggplot() +
  geom_line(data = GDI_Employees, aes(x=date, y = value/value[12]*100, color = "GDI: Compensation of Employees, Paid: Wages and Salaries"), size = 1.25) + 
  geom_line(data = PCE, aes(x=date, y = value/value[12]*100, color = "GDP: Personal Consumption Expenditures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,130), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  ylab("Index Q4 2019 = 100") +
  ggtitle("Hand to Mouth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending and Wage Growth Have Moved In Close Connection With Each Other") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 85-(.3*45), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

RGDP_Graph <- ggplot() +
  geom_line(data = RGDP, aes(x=date, y = value/1000, color = "Real GDP"), size = 1.25) + 
  geom_line(data = RGDI, aes(x=date, y = value/1000, color = "Real GDI"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(18.75,23), breaks = c(17,18,19,20,21,22,23), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "'Equivalent' Official Measures of Aggregate Output Are Diverging") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Real GDP","Real GDI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 18.75-(.3*4.25), ymax = 18.75) +
  coord_cartesian(clip = "off")

RGDO_Graph <- ggplot() +
  geom_line(data = RGDO, aes(x=date, y = value/1000, color = "Real GDO (Average of GDP and GDI)"), size = 1.25) + 
  geom_line(data = RGDI, aes(x=date, y = value/1000, color = "Real GDI"), size = 1.25) + 
  geom_line(data = RGDP, aes(x=date, y = value/1000, color = "Real GDP"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(18.75,23), breaks = c(17,18,19,20,21,22,23), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Measures of Output Show Extremely Weak Growth So Far This Year") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055"), breaks = c("Real GDP","Real GDI","Real GDO (Average of GDP and GDI)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 18.75-(.3*4.25), ymax = 18.75) +
  coord_cartesian(clip = "off")

REAL_GAS_Graph <- ggplot() +
  geom_line(data = REAL_GAS %>% drop_na(), aes(x=date, y = value, color = "Real Personal Consumption Expenditures: Gasoline And Other Energy Goods"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(240,335), breaks = c(250,275,300,325), expand = c(0,0)) +
  ylab("Billions of 2012 US Dollars") +
  ggtitle("Demand Destruction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Gasoline Consumption Has Fallen Below Pre-COVID Lows") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 240-(.3*95), ymax = 240) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Employment_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = PAYEMS, aes(x=date, y = value/1521.28, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = ELEV, aes(x=date, y = value/1586.53, color = "Employment Level (Household Survey)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(82,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 82-(.3*23), ymax = 82) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Employment_Graph <- ggplot() + #CPS with NFP adjusted concepts
  geom_line(data = PAYEMS, aes(x=date, y = value/1000, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = CPSADJ, aes(x=date, y = value/1000, color = "Employment Ajusted to Nonfarm Payrolls Concepts (Household Survey)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(120,160), breaks = c(120,130,140,150,160), expand = c(0,0)) +
  ylab("Payrolls/Employees, Millions") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 120-(.3*40), ymax = 120) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


#Real Final Sales to Domestic Final Purchasers
RFSALEDOMPRIV_Graph <- ggplot() + 
  geom_line(data = RFSALEDOMPRIV, aes(x=date, y = value/1000, color = "Real Final Sales to Private Domestic Purchasers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(16,20), breaks = c(15,16,17,18,19,20), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Growth In Real Final Private Domestic Consumption and Investment Has Weakened Considerably") +
  theme_apricitas + theme(legend.position = c(.40,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 16-(.3*4), ymax = 16) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Final Sales to Domestic Final Purchasers Percent Growth
RFSALEDOMPRIV_PCT_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data = RFSALEDOMPRIV_PCT, aes(x=date, y = value/100, color = "Real Final Sales to Private Domestic Purchasers, Quarterly Growth"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.40,0.40), breaks = c(-0.4,-.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  ylab("Continuously Compounded Annual Change") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Final Private Domestic Consumption and Investment Growth has Picked Up Recently") +
  theme_apricitas + theme(legend.position = c(.425,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.40-(.3*0.80), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Personal Income Less Transfers
REAL_PERSONAL_INCOME_LESS_TRANSFERS_Graph <- ggplot() + 
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data = REAL_PERSONAL_INCOME_LESS_TRANSFERS, aes(x=date, y = value/1000, color = "Real Personal Income Excluding Current Transfer Receipts"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(9,16), breaks = c(9,10,11,12,13,14,15), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Personal Income is Stalling-But Not Yet Shrinking") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 9-(.3*7), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Private Inventories
RealPrivateInventories_Graph <- ggplot() + 
  geom_line(data = RealPrivateInventories, aes(x=date, y = value/1000, color = "Real Private Inventories"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = .05),limits = c(2.7,3), breaks = c(2.7,2.75,2.8,2.85,2.9,2.95,3), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Taking Inventory") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Inventory Growth Was High-But Lower Than Last Quarter") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 2.7-(.3*.3), ymax = 2.7) +
  coord_cartesian(clip = "off")

GDPQuarterlyContrib_Graph <- ggplot(ContribQuarterlyBind, aes(fill=series_id, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.09), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Contributions to US GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Besides the Negative Contribution From Inventories, All Categories Contributed to Positive GDP in Q1") +
  theme_apricitas + theme(legend.position = c(.67,.85)) +
  scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.04-(.3*.13), ymax = -0.04) +
  coord_cartesian(clip = "off")

LaborProductivity_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = LaborProductivity, aes(x=date, y = value/100, color = "Nonfarm Business Sector: Labor Productivity for All Employed Persons"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.09,0.11), breaks = c(-0.05,0,0.05,.1), expand = c(0,0)) +
  ylab("Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Do You Buy It?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Drop in Output and Increase in Employment Caused Productivity to `Fall` Dramatically") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7000), xmax = as.Date("2000-01-01")-(0.049*7000), ymin = -0.09-(.3*.20), ymax = -0.09) +
  coord_cartesian(clip = "off")

TradeDeficit_Graph <- ggplot() + 
  geom_line(data = TradeDeficit, aes(x=date, y = -value/1000, color = "US Goods Trade Deficit, Balance of Payments Basis"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,140), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Trading Up") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "The US Trade Deficit Hit A Record High in March, but Then Pulled Back") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*140), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Port_Throughput_Graph <- ggplot() + 
  geom_line(data = Port_Throughput, aes(x = Date, y = LA/1000, color = "Los Angeles"), size = 1.25) +
  geom_line(data = Port_Throughput, aes(x = Date, y = LB/1000, color = "Long Beach"), size = 1.25) +
  geom_line(data = Port_Throughput, aes(x = Date, y = NYNJ/1000, color = "New York/New Jersey"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"),limits = c(0,610), breaks = c(0,200,400,600), expand = c(0,0)) +
  ylab("Loaded Imports, TEUs") +
  ggtitle("A Crisis of Abundance") +
  labs(caption = "Graph created by @JosephPolitano using LA,LB,and NY/NJ Port data",subtitle = "Import Throughput Has Jumped up at Major Ports During March") +
  theme_apricitas + theme(legend.position = c(.45,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 0-(.3*610), ymax = 0) +
  coord_cartesian(clip = "off")

INDUSTRIAL_PRODUCTION_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = INDPRO, aes(x=date, y = value, color = "Industrial Production"), size = 1.25) + 
  geom_line(data = IPMAN, aes(x=date, y = value, color = "Industrial Production: Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2017 = 100") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Industrial Production Data Has Slown Down, But Remains High") +
  theme_apricitas + theme(legend.position = c(.70,.40)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 80-(.3*25), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_INVESTMENT_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESIDENTIAL, aes(x=date, y = value/value[1]*100, color = "Real Fixed Investment: Residential"), size = 1.25) + 
  geom_line(data = FIXED_INDUSTRIAL, aes(x=date, y = value/value[1]*100, color = "Real Fixed Investment: Industrial Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(85,90,95,100,105,110,115,120,125), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("Unfixed Problems") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Fixed Investment is Declining in Key Sectors") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 80-(.3*45), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_RESIDENTIAL_2015 <- fredr(series_id = "PRFIC1",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)

FIXED_INVESTMENT_Residential_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESIDENTIAL_2015, aes(x=date, y = value/value[1]*100, color = "Real Fixed Investment: Residential"), size = 1.25) + 
  #geom_line(data = FIXED_INDUSTRIAL, aes(x=date, y = value/2.42, color = "Real Fixed Investment: Industrial Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,145), breaks = c(85,90,95,100,105,110,115,120,125,130,135,140,145), expand = c(0,0)) +
  ylab("Index, Q1 2019 = 100") +
  ggtitle("Unfixed Problems") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Fixed Investment is Declining in Key Sectors") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 80-(.3*65), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_Residential_Graph, "Fixed Residential.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


USNGDP <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
USNGDI <- fredr(series_id = "GDI",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")

NGDP_graph <- ggplot() + #plotting ukr GDP data
  geom_line(data=USNGDP, aes(x=date,y= value/100,color= "NGDP Growth"), size = 1.25)+ 
  #geom_line(data=USNGDI, aes(x=date,y= value/100,color= "NGDI Growth"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year-on-Year Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.15,-.1,-0.05,0,0.05,0.1,0.15,0.2), limits = c(-.1,.20), expand = c(0,0)) +
  ggtitle("US Nominal Growth") +
  labs(caption = "Graph created by @JosephPolitano using FRED data", subtitle = "US NGDP Growth is Still Well Above Pre Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.52,.75)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -.10-(.3*.30), ymax = -.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_graph, "NGDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#Underwater Graph

PAYEMS <- fredr(series_id = "PAYEMS",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(drawdown = (value/cummax(value))-1)
ELEV <- fredr(series_id = "CE16OV",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(drawdown = (value/cummax(value))-1)
REAL_PI <- fredr(series_id = "W875RX1",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(drawdown = (value/cummax(value))-1)
REAL_PCE <- fredr(series_id = "PCEC96",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(drawdown = (value/cummax(value))-1)
INDPRO <- fredr(series_id = "INDPRO",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(drawdown = (value/cummax(value))-1)
REAL_SALES <- fredr(series_id = "CMRMTSPL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(drawdown = (value/cummax(value))-1)

UNDERWATER_RECESSION_GRAPH <- ggplot() + #plotting NBER drawdown data
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=REAL_SALES, aes(x=date,y= drawdown,color= "Real Wholesale-Retail Sales"), size = 1.25)+ 
  geom_line(data=REAL_PCE, aes(x=date,y= drawdown,color= "Real Personal Consumption Expenditures"), size = 1.25)+ 
  geom_line(data=INDPRO, aes(x=date,y= drawdown,color= "Industrial Production"), size = 1.25)+ 
  geom_line(data=ELEV, aes(x=date,y= drawdown,color= "Employment Level"), size = 1.25)+ 
  geom_line(data=REAL_PI, aes(x=date,y= drawdown,color= "Real Personal Income Ex-Transfers"), size = 1.25)+ 
  geom_line(data=PAYEMS, aes(x=date,y= drawdown ,color= "Nonfarm Payrolls"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Drawdown From Prior Peak, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.20,0), expand = c(0,0)) +
  ggtitle("Recessions and Output Drawdowns") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, FRB, and Census data", subtitle = "Of All the Recession Indicators the NBER Focuses on, Only 2 Remains Below-Peak Right Now") +
  theme_apricitas + theme(legend.position = c(.285,.25)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Drawdowns from Prior Peak" ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Nonfarm Payrolls","Real Personal Income Ex-Transfers","Employment Level","Industrial Production","Real Personal Consumption Expenditures","Real Wholesale-Retail Sales")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = -.20-(.3*.20), ymax = -.20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNDERWATER_RECESSION_GRAPH, "Underwater Recession.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_PCE_YOY <- fredr(series_id = "PCECC96",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")

NOM_FI <- fredr(series_id = "FPI",observation_start = as.Date("2004-01-01"),realtime_start = NULL, realtime_end = NULL)
INF_FI <- fredr(series_id = "A007RD3Q086SBEA",observation_start = as.Date("2004-01-01"),realtime_start = NULL, realtime_end = NULL)
REAL_FI_YOY <- merge(NOM_FI,INF_FI, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  mutate(value = (value-lag(value, 4))/lag(value, 4)) %>%
  drop_na()
  

REAL_INVESTMENT_CONSUMPTION_GRAPH <- ggplot() +
  geom_line(data=REAL_FI_YOY, aes(x=date,y= value,color= "Real Private Fixed Investment"), size = 1.25)+ 
  geom_line(data=REAL_PCE_YOY, aes(x=date,y= value/100,color= "Real Personal Consumption Expenditures"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year on Year Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.20,.175), expand = c(0,0)) +
  ggtitle("The Investment Rebound") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Aggregate Real Private Investment Is Growing Again, After a Dip through Late 2022 and Early 2023") +
  theme_apricitas + theme(legend.position = c(.65,.2)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Year on Year Growth" ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = -.20-(.3*.375), ymax = -.20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_INVESTMENT_CONSUMPTION_GRAPH, "Real Investment Consumption.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_PRIVATE_FINAL <- fredr(series_id = "LB0000031Q020SBEA",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")

REAL_PRIVATE_FINAL_GRAPH <- ggplot() +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=REAL_PRIVATE_FINAL, aes(x=date,y= value/100,color= "Real Final Sales to Private Domestic Purchasers"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year on Year Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.10,.175), expand = c(0,0)) +
  ggtitle("America's 2022 Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Growth in Real Final Sales to Private Domestic Purchasers Has Declined Significantly") +
  theme_apricitas + theme(legend.position = c(.45,.7)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = -.10-(.3*.275), ymax = -.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PRIVATE_FINAL_GRAPH, "Real Private Final.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#real fixed investment in structures

REAL_FI_RESI_STRUC <- fredr(series_id = "A756RX1Q020SBEA",observation_start = as.Date("2016-01-01"),realtime_start = NULL, realtime_end = NULL)
REAL_FI_NONRESI_STRUC <- fredr(series_id = "A771RX1Q020SBEA",observation_start = as.Date("2016-01-01"),realtime_start = NULL, realtime_end = NULL)


FIXED_INVESTMENT_STRUCTURES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_FI_NONRESI_STRUC, aes(x=date, y = value/value[1]*100, color = "Real Fixed Investment: Nonresidential Structures"), size = 1.25) + 
  geom_line(data = REAL_FI_RESI_STRUC, aes(x=date, y = value/value[1]*100, color = "Real Fixed Investment: Residential Structures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,130), breaks = c(85,90,95,100,105,110,115,120,125,130), expand = c(0,0)) +
  ylab("Index, Q1 2016 = 100") +
  ggtitle("Structural Shortfall") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Residential and Nonresidential Structures is Below Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.40,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Real Fixed Investment: Residential Structures","Real Fixed Investment: Nonresidential Structures")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 85-(.3*45), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_STRUCTURES_GRAPH, "Real Fixed Investment In structures.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#REAL PCE Components
REAL_PCE_DURABLE <- fredr(series_id = "PCEDGC96",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
REAL_PCE_NONDURABLE <- fredr(series_id = "PCENDC96",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
REAL_PCE_SERVICES <- fredr(series_id = "PCESC96",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

REAL_PCE_MAIN_COMPONENTS_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_DURABLE, aes(x=date, y = value/value[13]*100, color = "Durable Goods"), size = 1.25) + 
  geom_line(data = REAL_PCE_NONDURABLE, aes(x=date, y = value/value[13]*100, color = "Nondurable Goods"), size = 1.25) + 
  geom_line(data = REAL_PCE_SERVICES, aes(x=date, y = value/value[13]*100, color = "Services"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(75,140), breaks = c(80,90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Consumption of Goods Stalled in 2022, Allowing Services to Catch Up a Bit") +
  theme_apricitas + theme(legend.position = c(.275,.80)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Durable Goods","Nondurable Goods","Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 75-(.3*65), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PCE_MAIN_COMPONENTS_GRAPH, "Real PCE Main Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


PCE_PC_YEAR <- fredr(series_id = "PCE", observation_start = as.Date("1995-01-01"), units = "pc1")

PCE_YEAR_Graph <- ggplot() + #plotting Wage Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=PCE_PC_YEAR, aes(x=date,y= value/100,color= "Nominal Personal Consumption Expenditures, Annual Growth"), size = 1.25) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", label = "5% Upper End of Normal Growth",y = 0.065, x = as.Date("2012-09-30"), color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.175,0.325), breaks = c(-.15,-.10,-0.05,0,0.05,0.10,0.15,0.20,.25,.30), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("US Spending Growth Has Normalized") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "Spending Growth Has Decelerated To The High End of Normal Levels") +
  theme_apricitas + theme(legend.position = c(.42,.72)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = -.175-(.3*0.50), ymax = -.175) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_YEAR_Graph, "PCE Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#Live Data Using BEA API

NGDP_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10105',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

NGDP_Growth <- beaGet(NGDP_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  mutate_if(is.numeric, funs((.-lag(.,4))/lag(.,4))) %>%
  drop_na()

RGDP_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10106',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

RGDP_Growth <- beaGet(RGDP_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  mutate_if(is.numeric, funs((.-lag(.,4))/lag(.,4))) %>%
  drop_na()

GDP_Growths_graph <- ggplot() + #plotting  GDP data
  geom_line(data=RGDP_Growth, aes(x=date,y= t10106_a191rx_1_gross_domestic_product_chained_dollars_level_6,color= "Real GDP"), size = 1.25)+ 
  geom_line(data=NGDP_Growth, aes(x=date,y= t10105_a191rc_1_gross_domestic_product_current_dollars_level_6,color= "Nominal GDP"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year-on-Year Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.15,-.1,-0.05,0,0.05,0.1,0.15,0.2), limits = c(-.1,.20), expand = c(0,0)) +
  ggtitle("US GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "US NGDP Growth is Decelerating, Though Above Pre Pandemic Levels—As Real Growth Recovers") +
  theme_apricitas + theme(legend.position = c(.4,.75)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.10-(.3*.30), ymax = -.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_Growths_graph, "GDP Growths.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

NGDP_Growth_QTR <- beaGet(NGDP_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  mutate_if(is.numeric, funs((1+(.-lag(.))/lag(.))^4-1)) %>%
  drop_na()

NGDP_Growth_QTR_Graph <- ggplot(subset(NGDP_Growth_QTR, date >= as.Date("2018-01-01")), aes(fill="Quarterly NGDP Growth, Annualized", x=date, y=t10105_a191rc_1_gross_domestic_product_current_dollars_level_6)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("hline", y = 0.04, yintercept = 0.04, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", label = "Pre-COVID Norm",y = 0.044, x = as.Date("2020-02-20"), color = "white", size = 3.5) +
  annotate("text", label = "*Note: Q1-Q3 2020 Excluded Because of Volatility",y = 0.08, x = as.Date("2019-02-01"), color = "white", size = 3.5) +
  #geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  #guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.15), breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("US Quarterly NGDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Spending Growth Has Declined Significantly From 2021/2022 Levels") +
  theme_apricitas + theme(legend.position = c(.3,.85)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_Growth_QTR_Graph, "NGDP Growth Quarter.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


RGDP_Contributions_Specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10102',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

RGDP_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A822RY 22 Government consumption expenditures and gross investment Quantity Contributions Level 0`,`T10102 A019RY 15 Net exports of goods and services Quantity Contributions Level 0`,`T10102 A006RY 7 Gross private domestic investment Quantity Contributions Level 0`,`T10102 DPCERY 2 Personal consumption expenditures Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Government", "Net Exports", "Investment", "Consumption","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(Government,`Net Exports`,Investment,Consumption)) %>%
  subset(date >= as.Date("2020-10-01"))

RGDPQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A191RL 1 Gross domestic product Fisher Quantity Index Percent change, annual rate 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2020-10-01"))

GDPMonthlyContribBEA_Graph <- ggplot(RGDP_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.09), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Contributions to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = c(.67,.88)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.04-(.3*.13), ymax = -0.04) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDPMonthlyContribBEA_Graph, "GDP Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_Investment_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A008RY 9 Nonresidential Quantity Contributions Level 0`,`T10102 A011RY 13 Residential Quantity Contributions Level 0`,`T10102 A014RY 14 Change in private inventories Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Nonresidential Fixed Investment", "Residential Fixed Investment", "Change in Private Inventories","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Nonresidential Fixed Investment`,`Residential Fixed Investment`,`Change in Private Inventories`)) %>%
  subset(date >= as.Date("2020-10-01"))

RGDPInvestmentQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A006RY 7 Gross private domestic investment Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2020-10-01"))

InvestmentQuarterlyContribBEA_Graph <- ggplot(RGDP_Investment_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPInvestmentQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.03,0.05), breaks = c(-.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Investment's Contribution to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = c(.72,.8), plot.title = element_text(size = 24)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Nonresidential Fixed Investment", "Residential Fixed Investment", "Change in Private Inventories")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.03-(.3*.08), ymax = -0.03) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = InvestmentQuarterlyContribBEA_Graph, "Investment Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_Consumption_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 DDURRY 4 Durable goods Quantity Contributions Level 0`,`T10102 DNDGRY 5 Nondurable goods Quantity Contributions Level 0`,`T10102 DSERRY 6 Services Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Durable Goods", "Nondurable Goods", "Services","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Durable Goods`,`Nondurable Goods`,`Services`)) %>%
  subset(date >= as.Date("2020-10-01"))

RGDPConsumptionQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 DPCERY 2 Personal consumption expenditures Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2020-10-01"))

ConsumptionQuarterlyContribBEA_Graph <- ggplot(RGDP_Consumption_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPConsumptionQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.03,0.09), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Consumption's Contribution to US RGDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = c(.67,.8), plot.title = element_text(size = 24)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Durable Goods", "Nondurable Goods", "Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.03-(.3*.12), ymax = -0.03) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ConsumptionQuarterlyContribBEA_Graph, "Consumption Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_Govt_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A824RY 24 National defense Quantity Contributions Level 0`,`T10102 A825RY 25 Nondefense Quantity Contributions Level 0`,`T10102 A829RY 26 State and local Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Federal Defense", "Federal Nondefense", "State & Local","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Federal Defense`,`Federal Nondefense`,`State & Local`)) %>%
  subset(date >= as.Date("2020-10-01"))

RGDPGovtnQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A822RY 22 Government consumption expenditures and gross investment Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2020-10-01"))

GovtQuarterlyContribBEA_Graph <- ggplot(RGDP_Govt_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPGovtnQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,0.016), breaks = c(-0.01,0,0.01), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Government's Contribution to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = c(.52,.8), plot.title = element_text(size = 23)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Federal Defense", "Federal Nondefense", "State & Local")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.01-(.3*.025), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GovtQuarterlyContribBEA_Graph, "Govt Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



PRIVATE_SALES_Specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10401',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2006, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PRIVATE_SALES <- beaGet(PRIVATE_SALES_Specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2006-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

REAL_FINAL_QUARTERLY <- ggplot() + #plotting  GDP data
  geom_line(data=PRIVATE_SALES, aes(x=date,y= t10401_pb000003_8_final_sales_to_private_domestic_purchasers_fisher_quantity_index_percent_change_annual_rate_0/100,color= "Real Final Sales to Private Domestic Purchasers, Quarterly"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Quarterly Growth, Annnualized") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.32,.41), expand = c(0,0)) +
  ggtitle("US 'Core' GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Growth in Real Final Sales to Private Domestic Purchasers Has Bounced Back") +
  theme_apricitas + theme(legend.position = c(.42,.75)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = -.31-(.3*.73), ymax = -.31) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_FINAL_QUARTERLY, "Real Final Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_FINAL_QUARTERLY_BAR <- ggplot(PRIVATE_SALES %>% filter(date >= as.Date("2020-10-01")), aes(fill="Real Final Sales to Private Domestic Purchasers, Quarterly Annualized Growth", x=date, y=t10401_pb000003_8_final_sales_to_private_domestic_purchasers_fisher_quantity_index_percent_change_annual_rate_0/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  ylab("Quarterly Growth, Annnualized") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.01,.13), expand = c(0,0)) +
  ggtitle("US 'Core' GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Growth in Real Final Sales to Private Domestic Purchasers Has Bounced Back") +
  theme_apricitas + theme(legend.position = c(.5,.975)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_fill_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-10-01")-(.1861*(today()-as.Date("2020-10-01"))), xmax = as.Date("2020-10-01")-(0.049*(today()-as.Date("2020-10-01"))), ymin = -.01-(.3*.14), ymax = -.01) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_FINAL_QUARTERLY_BAR, "Real Final Private Bar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


REAL_PCE_BREAKDOWN_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20403',
  'Frequency' = 'M',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_PCE_BREAKDOWN <- beaGet(REAL_PCE_BREAKDOWN_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "months"))) %>%
  clean_names() %>%
  drop_na()

REAL_PCE_BREAKDOWN_DURABLES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dnmvra_5_new_motor_vehicles_55_fisher_quantity_index_level_0/u20403_dnmvra_5_new_motor_vehicles_55_fisher_quantity_index_level_0[1]*100, color = "New Motor Vehicles"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dfdhra_23_furnishings_and_durable_household_equipment_fisher_quantity_index_level_0/u20403_dfdhra_23_furnishings_and_durable_household_equipment_fisher_quantity_index_level_0[1]*100, color = "Furnishings and Durable Household Equipment"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dwhlra_53_sports_and_recreational_vehicles_79_fisher_quantity_index_level_0/u20403_dwhlra_53_sports_and_recreational_vehicles_79_fisher_quantity_index_level_0[1]*100, color = "Sports and Recreational Vehicles"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(69,150), breaks = c(70,80,90,100,110,120,130,140,150), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Outside of Motor Vehicles, Consumption of Large Durable Goods Still Remains Elevated") +
  theme_apricitas + theme(legend.position = c(.74,.23)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 69-(.3*81), ymax = 69) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

REAL_PCE_BREAKDOWN_TECH_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dvaara_40_video_and_audio_equipment_fisher_quantity_index_level_0/u20403_dvaara_40_video_and_audio_equipment_fisher_quantity_index_level_0[1]*100, color = "Video and Audio Equipment"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dipera_48_information_processing_equipment_fisher_quantity_index_level_0/u20403_dipera_48_information_processing_equipment_fisher_quantity_index_level_0[1]*100, color = "Information Processing Equipment (Including Computers and Software)"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dtcera_71_telephone_and_related_communication_equipment_fisher_quantity_index_level_0/u20403_dtcera_71_telephone_and_related_communication_equipment_fisher_quantity_index_level_0[1]*100, color = "Telephone and Related Communication Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(75,300), breaks = c(100,150,200,250,300), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Tech-Related Durables Surged During The Pandemic, But Has Slowed Recently") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*225), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

REAL_PCE_BREAKDOWN_NONDURABLES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dclora_104_clothing_and_footwear_fisher_quantity_index_level_0/u20403_dclora_104_clothing_and_footwear_fisher_quantity_index_level_0[1]*100, color = "Clothing and Footwear"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dreira_126_recreational_items_parts_of_80_92_and_93_fisher_quantity_index_level_0/u20403_dreira_126_recreational_items_parts_of_80_92_and_93_fisher_quantity_index_level_0[1]*100, color = "Recreational Items"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dopcra_137_personal_care_products_part_of_118_fisher_quantity_index_level_0/u20403_dopcra_137_personal_care_products_part_of_118_fisher_quantity_index_level_0[1]*100, color = "Personal Care Products"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dfxara_73_food_and_beverages_purchased_for_off_premises_consumption_fisher_quantity_index_level_0/u20403_dfxara_73_food_and_beverages_purchased_for_off_premises_consumption_fisher_quantity_index_level_0[1]*100, color = "Food and Beverages\nfor Off-Premises Consumption"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(50,165), breaks = c(60,70,80,90,100,110,120,130,140,150,160,170), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Outside of Food, Consumption of Many Nondurable Goods Still Remains Elevated") +
  theme_apricitas + theme(legend.position = c(.75,.25)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Food and Beverages\nfor Off-Premises Consumption","Recreational Items","Clothing and Footwear","Personal Care Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 50-(.3*115), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

REAL_PCE_BREAKDOWN_SERVICES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dhlcra_172_health_care_fisher_quantity_index_level_0/u20403_dhlcra_172_health_care_fisher_quantity_index_level_0[1]*100, color = "Health Care Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dtrsra_190_transportation_services_fisher_quantity_index_level_0/u20403_dtrsra_190_transportation_services_fisher_quantity_index_level_0[1]*100, color = "Transportation Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_drcara_209_recreation_services_fisher_quantity_index_level_0/u20403_drcara_209_recreation_services_fisher_quantity_index_level_0[1]*100, color = "Recreation Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dfsera_235_food_services_fisher_quantity_index_level_0/u20403_dfsera_235_food_services_fisher_quantity_index_level_0[1]*100, color = "Food Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dperra_307_personal_care_and_clothing_services_14_and_parts_of_17_and_118_fisher_quantity_index_level_0/u20403_dperra_307_personal_care_and_clothing_services_14_and_parts_of_17_and_118_fisher_quantity_index_level_0[1]*100, color = "Personal Care and Clothing Services"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(0,125), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Services Consumption Is Climbing Back But Still Below Pre-Pandemic Levels in Many Areas") +
  theme_apricitas + theme(legend.position = c(.75,.25)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*125), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PCE_BREAKDOWN_DURABLES_GRAPH, "Real PCE Durables.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_PCE_BREAKDOWN_TECH_GRAPH, "Real PCE Tech.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_PCE_BREAKDOWN_NONDURABLES_GRAPH, "Real PCE Nondurables.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_PCE_BREAKDOWN_SERVICES_GRAPH, "Real PCE Services.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_RESI_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50406',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_RESI_INVEST_BULK <- beaGet(FIXED_RESI_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2007-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVESTMENT_BULK_MF_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST_BULK, aes(x=date, y = u50406_c292rx_40_multifamily_structures_chained_dollars_level_6/1000, color = "Real Fixed Investment, Multi-Family Structures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,110), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Real Multifamily Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Multifamily Housing is at a Multi-Decade High") +
  theme_apricitas + theme(legend.position = c(.60,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*110), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_INVESTMENT_BULK_MANU_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST_BULK, aes(x=date, y = u50406_c307rx_14_manufacturing_chained_dollars_level_6/1000, color = "Real Fixed Investment, Manufacturing Structures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,150), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Real Manufacturing Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Manufacturing is at Multi-Decade High") +
  theme_apricitas + theme(legend.position = c(.60,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*150), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_BULK_MF_Graph, "Fixed Multifamily Investment Bulk Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT_BULK_MANU_Graph, "Fixed Manufacturing Investment Bulk Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_EQUIP_INVEST_SPECS_REAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50506',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST_REAL <- beaGet(FIXED_EQUIP_INVEST_SPECS_REAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

US_CHIP_FIXED_EQUIP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = u50506_c272rx_18_special_industry_machinery_n_e_c_chained_dollars_level_6/1000, color = "Real Private Fixed Investment,\nSpecial Industry Machinery N.E.C.\n(Now Mostly Semiconductor Manufacturing Equipment)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,80), expand = c(0,0)) +
  ylab("Billions of 2012 Dollars") +
  ggtitle("US Chip Equipment Spending Has Surged") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Business Investment in Special Industry Equipment Has Nearly Doubled Since 2018") +
  theme_apricitas + theme(legend.position = c(.51,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*80), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CHIP_FIXED_EQUIP_INVEST_GRAPH, "US Chip Equipment Fixed Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Movie and Live Spectator Sports PCE
PCE_UNDERLYING_DETAIL_NOMINAL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20405',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_UNDERLYING_DETAIL_NOMINAL <- beaGet(PCE_UNDERLYING_DETAIL_NOMINAL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

PCE_MOVIES_LIVE_Graph <- ggplot() + #indexed employment rate
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", label = "Pre-COVID Level (Q4 2019)",y = 96, x = as.Date("2021-01-20"), color = "white", size = 4) +
  geom_line(data = PCE_UNDERLYING_DETAIL_NOMINAL, aes(x=date, y = u20405_dligrc_215_live_entertainment_excluding_sports_current_dollars_level_6/u20405_dligrc_215_live_entertainment_excluding_sports_current_dollars_level_6[4]*100, color = "Admissions to Live Entertainment Excluding Sports"), size = 1.25) + 
  geom_line(data = PCE_UNDERLYING_DETAIL_NOMINAL, aes(x=date, y = u20405_dmovrc_214_motion_picture_theaters_current_dollars_level_6/u20405_dmovrc_214_motion_picture_theaters_current_dollars_level_6[4]*100, color = "Admissions to Motion Picture Theatres"), size = 1.25) + 
  geom_line(data = PCE_UNDERLYING_DETAIL_NOMINAL, aes(x=date, y = u20405_dsperc_216_spectator_sports_current_dollars_level_6/u20405_dsperc_216_spectator_sports_current_dollars_level_6[4]*100, color = "Admissions to Spectator Sports"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(0,150), expand = c(0,0)) +
  ylab("Nominal PCE Index, Q4 2019 = 100") +
  ggtitle("The Spiderverse and Swift-Economy") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Spending on Live Entertainment and Movies has Fully Recovered to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= "Nominal Personal Consumption Expenditures, Index, Q4 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*135), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_MOVIES_LIVE_Graph, "PCE Movies Live Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_RESI_INVEST <- FIXED_RESI_INVEST_BULK %>%
  subset(date >= as.Date("2018-01-01"))

FIXED_INVESTMENT_RESIDENTIAL_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a944rx_39_single_family_structures_chained_dollars_level_6/u50406_a944rx_39_single_family_structures_chained_dollars_level_6[1]*100, color = "Single-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_c292rx_40_multifamily_structures_chained_dollars_level_6/u50406_c292rx_40_multifamily_structures_chained_dollars_level_6[1]*100, color = "Multi-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a946rx_44_improvements_chained_dollars_level_6/u50406_a946rx_44_improvements_chained_dollars_level_6[1]*100, color = "Residential Improvements"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,150), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("US Real Residential Fixed Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Single-Family Homes and Home Improvements are Declining") +
  theme_apricitas + theme(legend.position = c(.20,.70)) +
  scale_color_manual(name= "Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Single-Family Structures","Multi-Family Structures","Residential Improvements")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*65), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


FIXED_INVESTMENT_RESIDENTIAL_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a944rx_39_single_family_structures_chained_dollars_level_6/u50406_a944rx_39_single_family_structures_chained_dollars_level_6[1]*100, color = "Single-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_c292rx_40_multifamily_structures_chained_dollars_level_6/u50406_c292rx_40_multifamily_structures_chained_dollars_level_6[1]*100, color = "Multi-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a946rx_44_improvements_chained_dollars_level_6/u50406_a946rx_44_improvements_chained_dollars_level_6[1]*100, color = "Residential Improvements"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,150), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("US Real Residential Fixed Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Single-Family Homes and Home Improvements are Declining") +
  theme_apricitas + theme(legend.position = c(.20,.60)) +
  scale_color_manual(name= "Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Single-Family Structures","Multi-Family Structures","Residential Improvements")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*65), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


FIXED_INVESTMENT_NONRESIDENTIAL_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w031rx_4_office_chained_dollars_level_6/u50406_w031rx_4_office_chained_dollars_level_6[1]*100, color = "Offices"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w032rx_5_health_care_chained_dollars_level_6/u50406_w032rx_5_health_care_chained_dollars_level_6[1]*100, color = "Healthcare"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w038rx_12_warehouses_chained_dollars_level_6/u50406_w038rx_12_warehouses_chained_dollars_level_6[1]*100, color = "Warehouses"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_c307rx_14_manufacturing_chained_dollars_level_6/u50406_c307rx_14_manufacturing_chained_dollars_level_6[1]*100, color = "Manufacturing"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w036rx_10_multimerchandise_shopping_chained_dollars_level_6/u50406_w036rx_10_multimerchandise_shopping_chained_dollars_level_6[1]*100, color = "Multimerchandise Shopping"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(0,200), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Structural Shortfall") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Healthcare, Offices, and Shopping Malls are Well Below Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.21,.8)) +
  scale_color_manual(name= "Real Investment: Structures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*200), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_RESIDENTIAL_Graph, "Fixed Residential Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT_NONRESIDENTIAL_Graph, "Fixed Nonresidential Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_IP_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T50306',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_IP_INVEST <- beaGet(FIXED_IP_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_IP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_IP_INVEST, aes(x=date, y = t50306_b985rx_17_software_chained_dollars_level_6/t50306_b985rx_17_software_chained_dollars_level_6[1]*100, color = "Software"), size = 1.25) + 
  geom_line(data = FIXED_IP_INVEST, aes(x=date, y = t50306_y006rx_18_research_and_development_chained_dollars_level_6/t50306_y006rx_18_research_and_development_chained_dollars_level_6[1]*100, color = "Research and Development"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(90,180), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("The Tech Push") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Software and R&D Surged During the Pandemic—Though R&D is Cooling Now") +
  theme_apricitas + theme(legend.position = c(.21,.72)) +
  scale_color_manual(name= "Real Private Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*90), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_IP_INVEST_GRAPH, "Fixed IP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_EQUIP_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50506',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST <- beaGet(FIXED_EQUIP_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_EQUIP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_EQUIP_INVEST, aes(x=date, y = u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6/u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6[1]*100, color = "Computer Equipment"), size = 1.25) + 
  geom_line(data = FIXED_EQUIP_INVEST, aes(x=date, y = u50506_a680rx_12_industrial_equipment_chained_dollars_level_6/u50506_a680rx_12_industrial_equipment_chained_dollars_level_6[1]*100, color = "Industrial Equipment"), size = 1.25) + 
  geom_line(data = FIXED_EQUIP_INVEST, aes(x=date, y = u50506_a682rx_29_other_equipment_chained_dollars_level_6/u50506_a682rx_29_other_equipment_chained_dollars_level_6[1]*100, color = "Other Equipment"), size = 1.25) + 
  geom_line(data = FIXED_EQUIP_INVEST, aes(x=date, y = u50506_a681rx_21_transportation_equipment_chained_dollars_level_6/u50506_a681rx_21_transportation_equipment_chained_dollars_level_6[1]*100, color = "Transportation Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(40,140), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Re-Equipping") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Computer and Industrial Equipment Were Boosted By The Pandemic") +
  theme_apricitas + theme(legend.position = c(.245,.22)) +
  scale_color_manual(name= "Real Private Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 40-(.3*100), ymax = 40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_EQUIP_INVEST_GRAPH, "Fixed EQUIP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


NGDP_1947_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10105',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1947, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

NGDP_1947 <- beaGet(NGDP_1947_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1947-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na() %>%
  transmute(date, NGDP = t10105_a191rc_1_gross_domestic_product_current_dollars_level_6)

INTEREST_PAYMENTS_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T30200',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1947, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

INTEREST_PAYMENTS <- beaGet(INTEREST_PAYMENTS_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1947-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, Interest_AGG = t30200_a091rc_33_interest_payments_current_dollars_level_6, Social_Insurance_Funds_Savings = t30200_b243rc_38_social_insurance_funds_current_dollars_level_6, Social_Insurance_Funds_Contributions = t30200_w780rc_10_contributions_for_government_social_insurance_current_dollars_level_6, Interest_Receipts = t30200_b094rc_14_interest_receipts_current_dollars_level_6)

PROFITS_49_87_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T61600B',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1947, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PROFITS_88_00_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T61600C',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1988, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PROFITS_01_TODAY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T61600D',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2001, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PROFITS_49_87 <- beaGet(PROFITS_49_87_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1948-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, Fed_Profits = t61600b_j397rbc_11_federal_reserve_banks_current_dollars_level_6)

PROFITS_88_00 <- beaGet(PROFITS_88_00_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1988-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, Fed_Profits = t61600c_j397rc_11_federal_reserve_banks_current_dollars_level_6)

PROFITS_01_TODAY <- beaGet(PROFITS_01_TODAY_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2001-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, Fed_Profits = t61600d_b397rc_11_federal_reserve_banks_current_dollars_level_6)

FED_PROFITS_RBIND <- rbind(PROFITS_49_87,PROFITS_88_00,PROFITS_01_TODAY)


SOCIAL_EXPENSES_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U31200',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1959, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

SOCIAL_EXPENSES <- beaGet(SOCIAL_EXPENSES_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1959-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, Social_Insurance_Funds_Benefits = u31200_a1588c_4_benefits_from_social_insurance_funds_current_dollars_level_6 + u31200_w016rc_43_to_the_rest_of_the_world_current_dollars_level_6)

SOCIAL_INSURANCE_FUNDS_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T31400',
  'Frequency' = 'A',
  'Year' = paste(seq(from = 1947, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

SOCIAL_INSURANCE_FUNDS <- beaGet(SOCIAL_INSURANCE_FUNDS_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1947-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  transmute(date, Social_Insurance_Interest_Received = t31400_l31307_8_interest_received_current_dollars_level_6, Social_Insurance_Funds_Admin_Expenses = t31400_l31309_11_administrative_expenses_current_dollars_level_6) 

SOCIAL_INSURANCE_FUNDS$year <- year(SOCIAL_INSURANCE_FUNDS$date)

# Create a new data frame with all quarters for each year
all_quarters <- expand.grid(year = unique(SOCIAL_INSURANCE_FUNDS$year), quarter = 1:4) %>%
  unite("date", year, quarter, sep = " Q") %>%
  mutate(date = as.Date(as.yearqtr(date)))

# Join the new data frame with the original data frame
merged_data <- left_join(all_quarters, SOCIAL_INSURANCE_FUNDS, by = "date")

# Interpolate the missing data
interpolated_data <- merged_data %>%
  arrange(date) %>%
  .[-((nrow(.)-2):nrow(.)), ] %>%
  mutate(date = date %m+% months(6)) %>%
  mutate(Social_Insurance_Funds_Admin_Expenses_INTERP  = na.approx(Social_Insurance_Funds_Admin_Expenses)) %>%
  mutate(Social_Insurance_Interest_Received_INTERP  = na.approx(Social_Insurance_Interest_Received))

ts_data <- ts(interpolated_data$Social_Insurance_Funds_Admin_Expenses_INTERP, 
              start = c(year(min(interpolated_data$date)), quarter(min(interpolated_data$date))),
              frequency = 4)

# Create a model
model <- auto.arima(ts_data)

# Create a sequence of future dates
future_dates <- seq(max(interpolated_data$date) %m+% months(3), 
                    as.Date(format(Sys.Date(), "%Y-%m-01")), 
                    by = "quarter")

# Use the model to forecast the future values
forecast_values <- forecast(model, h = length(future_dates))

# Add the forecasted values to your dataframe
extrapolated_data <- data.frame(date = future_dates, 
                                Social_Insurance_Funds_Admin_Expenses_INTERP = forecast_values$mean) %>%
  bind_rows(interpolated_data) %>%
  arrange(date)


GDP_INTEREST_FED_PROFITS_MERGE <- NGDP_1947 %>%
  full_join(INTEREST_PAYMENTS, by = "date") %>%
  full_join(FED_PROFITS_RBIND, by = "date") %>%
  full_join(extrapolated_data, by = "date") %>%
  select(-year) %>%
  full_join(SOCIAL_EXPENSES, by = "date") %>%
  mutate(Social_Insurance_Interest_Received_INTERP = 
           ifelse(is.na(Social_Insurance_Interest_Received_INTERP),
                  Social_Insurance_Funds_Savings + Social_Insurance_Funds_Admin_Expenses_INTERP + Social_Insurance_Funds_Benefits - Social_Insurance_Funds_Contributions,
                  Social_Insurance_Interest_Received_INTERP)) %>%
  select(date, NGDP, Interest_AGG, Interest_Receipts, Social_Insurance_Interest_Received_INTERP, Fed_Profits)


GDP_INTEREST_Graph <- ggplot() + 
  geom_line(data = GDP_INTEREST_FED_PROFITS_MERGE, aes(x=date, y = (Interest_AGG/NGDP), color = "Federal Government Interest Expense as a Share of GDP"), size = 1.25) + 
  geom_line(data = GDP_INTEREST_FED_PROFITS_MERGE, aes(x=date, y = (Interest_AGG-Interest_Receipts)/NGDP, color = "Net of Federal Government Interest Receipts"), size = 1.25) + 
  geom_line(data = GDP_INTEREST_FED_PROFITS_MERGE, aes(x=date, y = ((Interest_AGG-Social_Insurance_Interest_Received_INTERP-Interest_Receipts)/NGDP), color = "& Excluding Interest Paid to Other Parts of the Federal Government (i.e. Social Security Trust Fund)"), size = 1.25) + 
  geom_line(data = GDP_INTEREST_FED_PROFITS_MERGE, aes(x=date, y = ((Interest_AGG-Social_Insurance_Interest_Received_INTERP-Interest_Receipts-Fed_Profits)/NGDP), color = "& Accounting for Federal Reserve Profit/Loss"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.050), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Federal Interest Payments are Soaring") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Rising Rates Are Increasing Debt Servicing Costs—Especially When Accounting for Fed Losses") +
  theme_apricitas + theme(legend.position = c(.51,.15), legend.text = element_text(size = 12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Federal Government Interest Expense as a Share of GDP","Net of Federal Government Interest Receipts","& Excluding Interest Paid to Other Parts of the Federal Government (i.e. Social Security Trust Fund)","& Accounting for Federal Reserve Profit/Loss")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*(today()-as.Date("1947-01-01"))), xmax = as.Date("1947-01-01")-(0.049*(today()-as.Date("1947-01-01"))), ymin = 0-(.3*0.050), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_INTEREST_Graph, "GDP Interest.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PCE_PER_CAPITA_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T70100',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1989, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_PER_CAPITA <- beaGet(PCE_PER_CAPITA_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1989-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  mutate_if(is.numeric, funs((.-lag(.,4))/lag(.,4))) %>%
  clean_names()

PCE_PER_CAPITA_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("hline", y = 0.04, yintercept = 0.04, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", label = "4% Trend Growth",y = 0.052, x = as.Date("2013-09-30"), color = "white", size = 4) +
  geom_line(data = PCE_PER_CAPITA, aes(x=date, y = t70100_a794rc_5_personal_consumption_expenditures_ratio_level_0, color = "Nominal Personal Consumption Expenditures Per Capita,\nYear-on-Year Growth"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-0.1,0.22), breaks = c(0,.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("US Nominal Consumer Spending Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Consumption Growth is Decelerating From Record Highs") +
  theme_apricitas + theme(legend.position = c(.40,.7)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = -.1-(.3*.32), ymax = -.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_PER_CAPITA_Graph, "PCE Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Durables
PCE_DURABLES_PER_CAPITA_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T70100',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2003, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_DURABLES_PER_CAPITA <- beaGet(PCE_DURABLES_PER_CAPITA_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2003-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, value = t70100_a795rx_15_durable_goods_ratio_level_0)

start_val <- PCE_DURABLES_PER_CAPITA$value[PCE_DURABLES_PER_CAPITA$date == as.Date("2007-10-01")]
start_val2 <- PCE_DURABLES_PER_CAPITA$value[PCE_DURABLES_PER_CAPITA$date == as.Date("2019-10-01")]


PCE_DURABLES_PER_CAPITA <- PCE_DURABLES_PER_CAPITA %>%
  mutate(index = row_number()) %>%
  mutate(
  trend07 = ifelse(
    date == as.Date("2007-10-01"),  # on 2007-10-01 the value is the same as in the 'value' column
    start_val,
    start_val * (1.0122347 ^ (index - 20))  # from the second day onwards, the value is increased by 1.022347 from the previous day
  )) %>%
  mutate(
    trend19 = ifelse(
    date == as.Date("2019-10-01"),  # on 2007-10-01 the value is the same as in the 'value' column
    start_val2,
    start_val2 * (1.0122347 ^ (index - 68))  # from the second day onwards, the value is increased by 1.022347 from the previous day
  )
)

PCE_DURABLES_PER_CAPITA_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = subset(PCE_DURABLES_PER_CAPITA, date >= as.Date("2007-10-01")), aes(x=date, y = trend07/1000 , color = "Pre-Great Recession Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = subset(PCE_DURABLES_PER_CAPITA, date >= as.Date("2019-10-01")), aes(x=date, y = trend19/1000 , color = "Pre-COVID Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = PCE_DURABLES_PER_CAPITA, aes(x=date, y = value/1000 , color = "Real Personal Consumption Expenditures Per Capita: Durable Goods"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "k"),limits = c(0,8), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  ylab("Billions of 2012 Dollars") +
  ggtitle("Reversing Hysteresis") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Durables Consumption is Above the Pre-COVID Trend and Was Close to the Pre-GFC Trend") +
  theme_apricitas + theme(legend.position = c(.50,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055"), breaks = c("Real Personal Consumption Expenditures Per Capita: Durable Goods","Pre-Great Recession Trend","Pre-COVID Trend")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*8), ymax = -.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_DURABLES_PER_CAPITA_Graph, "PCE Durables Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PCE_DETAIL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20403',
  'Frequency' = 'M',
  'Year' = paste(seq(from = 2016, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_DETAIL_MONTHLY <- beaGet(PCE_DETAIL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2016-01-01"), length.out = nrow(.), by = "1 month"))) %>%
  clean_names()

PCE_FOOD_DETAIL_MONTHLY_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = PCE_DETAIL_MONTHLY, aes(x=date, y = u20403_dfxara_73_food_and_beverages_purchased_for_off_premises_consumption_fisher_quantity_index_level_0/u20403_dfxara_73_food_and_beverages_purchased_for_off_premises_consumption_fisher_quantity_index_level_0[48]*100, color = "Food and Beverages for Off-Premises Consumption (e.g. Groceries)"), size = 1.25) + 
  geom_line(data = PCE_DETAIL_MONTHLY, aes(x=date, y = u20403_dfsera_235_food_services_fisher_quantity_index_level_0/u20403_dfsera_235_food_services_fisher_quantity_index_level_0[48]*100, color = "Food Services (e.g. Restaurants, Bars)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(50,125), breaks = c(50,75,100,125), expand = c(0,0)) +
  ylab("Index, Dec 2019 = 100") +
  ggtitle("Real Food & Food Services Consumption") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Food Services and Grocery Consumption Have Both Risen in 2023") +
  theme_apricitas + theme(legend.position = c(.43,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 50-(.3*75), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_FOOD_DETAIL_MONTHLY_Graph, "PCE Food Detail Monthly.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



RPCE_PC_RECESSION_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T70100',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1989, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

RPCE_PC_RECESSION <- beaGet(RPCE_PC_RECESSION_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1989-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  transmute(date, value = t70100_a794rx_13_personal_consumption_expenditures_ratio_level_0) %>%
  mutate(quarters = row_number())

RPCE_PC_RECESSION_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = subset(RPCE_PC_RECESSION, date >= as.Date("1990-04-01") & date <= as.Date("2000-10-01")),
            aes(x = quarters-6, 
                y = ((value-(value[1] * ((1 + 0.005701061) ^ (seq_along(value) - 1)))) / value * 100)/100, 
                color = "1990"), 
            size = 1.25) +
  geom_line(data = subset(RPCE_PC_RECESSION, date >= as.Date("2001-01-01") & date <= as.Date("2007-07-01")),
            aes(x = quarters-48, 
                y = ((value-(value[1] * ((1 + 0.005701061) ^ (seq_along(value) - 1)))) / value * 100)/100, 
                color = "2000"), 
            size = 1.25) +
  geom_line(data = subset(RPCE_PC_RECESSION, date >= as.Date("2007-10-01") & date <= as.Date("2019-07-01")),
            aes(x = quarters-76, 
                y = ((value-(value[1] * ((1 + 0.005701061) ^ (seq_along(value) - 1)))) / value * 100)/100, 
                color = "2007"), 
            size = 1.25) +
  geom_line(data = subset(RPCE_PC_RECESSION, date >= as.Date("2019-07-01")),
           aes(x = quarters-123, 
               y = ((value-(value[1] * ((1 + 0.005701061) ^ (seq_along(value) - 1)))) / value * 100)/100, 
               color = "2020"), 
           size = 1.25) +
  xlab("Quarters Since Peak Consumption") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.175,.025), expand = c(0,0)) +
  ylab("Percent Deviation from Pre-Recession Trend") +
  ggtitle("A Historic Consumer Recovery") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Today's Per Capita Consumption Recovery is Still Stronger Than in any other Recent Recession") +
  theme_apricitas + theme(legend.position = c(.74,.43)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures Per Capita\nDeviation From Pre-Recession 2.3% Trend",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*(49)), xmax = 0-(.049*(49)), ymin = -.175-(.3*.20), ymax = -.175) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RPCE_PC_RECESSION_GRAPH, "PCE Per Capita Recession Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


PCE_PRICE_INDEX_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T20804',
  'Frequency' = 'M',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_PRICE_INDEX <- beaGet(PCE_PRICE_INDEX_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "1 month"))) %>%
  clean_names() %>%
  drop_na() %>%
  select(date, t20804_dpcerg_1_personal_consumption_expenditures_pce_fisher_price_index_level_0, t20804_dpccrg_25_pce_excluding_food_and_energy_fisher_price_index_level_0, t20804_ia001260_28_pce_services_excluding_energy_and_housing_fisher_price_index_level_0) %>%
  setNames(c("date","pce","pce_lfe","pce_nhs")) %>%
  arrange(date) %>%
  transmute(date, pce = (pce-lag(pce,12))/lag(pce,12),pce_lfe = (pce_lfe-lag(pce_lfe,12))/lag(pce_lfe,12), pce_nhs = (pce_nhs-lag(pce_nhs,12))/lag(pce_nhs,12)) %>%
  drop_na()

PCE_PRICE_INDEX_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=PCE_PRICE_INDEX, aes(x=date,y= pce, color= "Headline PCE Inflation"), size = 1.25) +
  geom_line(data=PCE_PRICE_INDEX, aes(x=date,y= pce_lfe, color= "PCE Less Food and Energy Inflation"), size = 1.25) +
  geom_line(data=PCE_PRICE_INDEX, aes(x=date,y= pce_nhs, color= "Core PCE Services Ex Housing Inflation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07), limits = c(0,.072), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Inflation Continues Cooling") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "PCE Inflation Has Decelerated—With Headline Data Falling The Fastest") +
  theme_apricitas + theme(legend.position = c(.34,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Headline PCE Inflation","PCE Less Food and Energy Inflation","Core PCE Services Ex Housing Inflation")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(.1861*(today()-as.Date("2018-01-01")))), ymin = 0-(.3*.072), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_PRICE_INDEX_Graph, "PCE Price Index Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_NOMINAL_STRUCTURE_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50405',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2010, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_NOMINAL_STRUCTURE_INVEST <- beaGet(FIXED_NOMINAL_STRUCTURE_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2010-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

ALT_POWER_FIXED_INVEST <- FIXED_NOMINAL_STRUCTURE_INVEST %>%
  select(date,u50405_la001174_18_alternative_electric_current_dollars_level_6,u50405_la001175_19_all_other_electric_current_dollars_level_6) %>%
  setNames(c("date","Alternative Electric Power (Wind, Solar, Dry-Waste and Geothermal)","Conventional Electric Power (Coal, Natural Gas, Nuclear, etc)")) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = rev(c("Alternative Electric Power (Wind, Solar, Dry-Waste and Geothermal)","Conventional Electric Power (Coal, Natural Gas, Nuclear, etc)"))))

ALT_POWER_FIXED_INVEST_graph <- ggplot(data = ALT_POWER_FIXED_INVEST, aes(x = date, y = value/1000, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Dollars") +
  ggtitle("America's Changing Power Investments") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,25,50,75,100), limits = c(0,115), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "New BEA Data Shows that Alternative Energy is Making Up a Higher Share of Power Investment") +
  theme_apricitas + theme(legend.position = c(.425,.89)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Nominal Fixed Investment in Structures",values = c("#EE6055","#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*115), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALT_POWER_FIXED_INVEST_graph, "Alt Power Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


##BEA STATE GRAPH
 
BEA_GDP_STATE_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP9", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE <- beaGet(BEA_GDP_STATE_SPECS, iTableStyle = FALSE) %>%
  rename_with(~ coalesce(stringr::str_extract(., "(?<=\\s)[A-Za-z ]+(?=\\sMillions)"), "Unknown"), .cols = everything()) %>%
  select(-`United States`,-`Rocky Mountain`,-`Unknown`,-`Far West`,-`Rocky Mountain`,-`Southwest`,-`Southeast`,-`Plains`,-`Great Lakes`,-`Mideast`,-`New England`) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  subset(date >= as.Date("2019-07-01")) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "GDP") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(CAGR = (GDP / first(GDP)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  mutate(GROWTH = (GDP / first(GDP)) - 1) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, BEA_GDP_STATE, by = "state_name")

BEA_GDP_STATE_GRADIENT <- ggplot() +
  geom_sf(data = states, aes(fill = CAGR)) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.05,0.05)) +
  ggtitle("       Annualized GDP Growth Since Q3 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

BEA_GDP_STATE_BINS <- states %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  ggplot(aes(fill = CAGR_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("     Annualized Real GDP Growth Since Q3 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank())

states <- states %>%
  mutate(GROWTH_bucket = cut(GROWTH, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+")))

BEA_GDP_STATE_BINS_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids <- states %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states, .) %>%
  st_centroid()

BEA_GDP_STATE_BINS_RAW <- states %>%
  ggplot(aes(fill = GROWTH_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16%+")) +
  ggtitle("             Real GDP Growth Since Q3 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    segment.color = NA,
    hjust = 0.5,
    direction = "y",
    nudge_y = 4000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    box.padding = 0.75,  # Increase box padding
    point.padding = 0.5,
    max.overlaps = 5,
    force = 4,
    force_pull = 1,
    max.iter = 2000000000,
    max.time = 30,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
  #geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_GDP_STATE_BINS_RAW, "BEA GDP STATE BINS RAW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_GDP_STATE_GRADIENT, "BEA GDP STATE GRADIENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_GDP_STATE_BINS, "BEA GDP STATE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

test <- beaParams(beaKey = Sys.getenv("BEA_KEY"), "Regional")
test <- beaParamVals(beaKey = Sys.getenv("BEA_KEY"),"Regional","TableName")

BEA_PCE_PC_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SARPI", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 4, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_PCE_PC_STATE <- beaGet(BEA_PCE_PC_SPECS, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_trim(), starts_with("SARPI-4")) %>%
  rename_with(~str_replace(., " Constant", "")) %>%
  select(-`United States`, -TimePeriod) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "PCE_PER_CAPITA") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(GROWTH = (PCE_PER_CAPITA / first(PCE_PER_CAPITA)) - 1) %>%
  filter(date == max(date)) %>%
  ungroup()

states_PCE_PC <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states_PCE_PC <- left_join(states_PCE_PC, BEA_PCE_PC_STATE, by = "state_name")

states_PCE_PC <- states_PCE_PC %>%
  mutate(states = state_name) %>%
  mutate(GROWTH_bucket = cut(GROWTH, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+")))


BEA_GDP_PCE_PC_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states_PCE_PC, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids_PCE_PC <- states_PCE_PC %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_PCE_PC, .) %>%
  st_centroid()

BEA_PCE_PC_STATE_BINS_RAW <- states_PCE_PC %>%
  ggplot(aes(fill = GROWTH_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", aes(fill = GROWTH_bucket), lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D","#3083DC")))) +
  ggtitle("         Real PCE Per Capita Growth 2019-2022\n                    At State Price Parities") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    segment.color = NA,
    hjust = 0.5,
    direction = "y",
    nudge_y = 4000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    box.padding = 0.75,  # Increase box padding
    point.padding = 0.5,
    max.overlaps = 5,
    force = 4,
    force_pull = 1,
    max.iter = 2000000000,
    max.time = 30,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_PCE_PC_STATE_BINS_RAW, "BEA PCE PER CAPITA STATE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_RPP_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SARPP", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_RPP_STATE <- beaGet(BEA_RPP_SPECS, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_trim(), starts_with("SARPP-1")) %>%
  rename_with(~str_replace(., " Index", "")) %>%
  select(-`United States`, -TimePeriod) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "RPP") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(GROWTH = (RPP - first(RPP))/100) %>%
  filter(date == max(date)) %>%
  ungroup()


states_RPP <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states_RPP <- left_join(states_RPP, BEA_RPP_STATE, by = "state_name")

states_RPP <- states_RPP %>%
  mutate(states = state_name) %>%
  mutate(GROWTH_bucket = cut(GROWTH, breaks = c(-Inf, -0.015, 0,.015, Inf), labels = c("<-1.5%", "-1.5%-0", "0-1.5%", "1.5%+")))


BEA_RPP_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states_RPP, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids_RPP <- states_RPP %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_RPP, .) %>%
  st_centroid()



BEA_RPP_STATE_BINS_RAW <- states_RPP %>%
  ggplot(aes(fill = GROWTH_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#FF8E72","#F5B041","#FFE98F"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<-1.5%", "-1.5-0%", "0-1.5%", "1.5%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#FF8E72","#F5B041","#FFE98F")))) +
  ggtitle("       Change in State Price Parities 2019-2022\n                  Relative to National Average") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_RPP_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    segment.color = NA,
    hjust = 0.5,
    direction = "y",
    nudge_y = 4000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    box.padding = 0.75,  # Increase box padding
    point.padding = 0.5,
    max.overlaps = 5,
    force = 4,
    force_pull = 1,
    max.iter = 2000000000,
    max.time = 30,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_RPP, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_RPP_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_RPP_STATE_BINS_RAW, "BEA RPP STATE BINS 2.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_STATE_INDUSTRIES_SPECS_TOTAL <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_TOTAL <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_TOTAL, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months")))

BEA_GDP_STATE_INDUSTRIES_SPECS_INFO <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 45, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_INFO <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_INFO, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months")))

BEA_GDP_STATE_INDUSTRIES_SPECS_PROF <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 60, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_PROF <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_PROF, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months")))

BEA_GDP_STATE_INDUSTRIES_SPECS_CONS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 11, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_CONS <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_CONS, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months")))


BEA_GDP_STATE_INDUSTRIES_TOTAL_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.15), breaks = c(0,0.05,0.10,0.15), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Total") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_INFO_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.30), breaks = c(0,0.05,0.10,0.15,0.20,0.25,0.30), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Information") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_PROF_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.20), breaks = c(0,0.05,0.10,0.15,0.20,0.25,0.30), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Professional/Scientific/Technical") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_CONS_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.15), breaks = c(0,0.05,0.10,0.15), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Construction") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_GRAPH <- ggarrange(BEA_GDP_STATE_INDUSTRIES_TOTAL_GRAPH, BEA_GDP_STATE_INDUSTRIES_PROF_GRAPH, BEA_GDP_STATE_INDUSTRIES_CONS_GRAPH, BEA_GDP_STATE_INDUSTRIES_INFO_GRAPH,  ncol = 2, nrow = 2, heights = 20, widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

text <- c("State GDP, Percent of US GDP",fontface = "bold")

# Create a text grob
tgrob <- text_grob(expression(bold("                    State GDP, Percent of US GDP")),size = 25, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "top", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-0.15,-0.15,-0.15),"cm"))  
blank <- ""
blankgrob <- text_grob(blank,size = 20)
plot_blank <- as_ggplot(blankgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))
BEA_GDP_STATE_INDUSTRIES_GRAPH <- ggarrange(plot_0,plot_blank,BEA_GDP_STATE_INDUSTRIES_TOTAL_GRAPH, BEA_GDP_STATE_INDUSTRIES_PROF_GRAPH, BEA_GDP_STATE_INDUSTRIES_CONS_GRAPH, BEA_GDP_STATE_INDUSTRIES_INFO_GRAPH,  ncol = 2, nrow = 3, heights = c(5,20,20), widths = 10, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = BEA_GDP_STATE_INDUSTRIES_GRAPH, "BEA GDP STATE INDUSTRIES.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_COUNTIES_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

#Just checking the raw increase in GDP by Metro Since 2019
BEA_GDP_COUNTY_INCREASES <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(INCREASE = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  #slice(-nrow(.)) %>%
  top_n(100, DataValue) %>%
  transmute(GEOID = GeoFips, INCREASE)


BEA_GDP_COUNTIES <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  #mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, CAGR)

counties_map <- get_urbn_map(map = "counties", sf = TRUE)

#NOTE: BEA Data Combines Several Small Counties, Particularly in VA, into Neighboring Counties. This code Manually copies over the CAGR values for those large counties into the Geometry for the smaller, constituent counties 

counties_map <- full_join(counties_map, BEA_GDP_COUNTIES, by = "county_fips") %>%
  mutate(CAGR = if_else(county_fips == 51003, .$CAGR[which(.$county_fips == 51901)], CAGR)) %>% #Albemarle
  mutate(CAGR = if_else(county_fips == 51540, .$CAGR[which(.$county_fips == 51901)], CAGR)) %>% #Charlottesville
  mutate(CAGR = if_else(county_fips == 51005, .$CAGR[which(.$county_fips == 51903)], CAGR)) %>% #Alleghany
  mutate(CAGR = if_else(county_fips == 51580, .$CAGR[which(.$county_fips == 51903)], CAGR)) %>% #Covington
  mutate(CAGR = if_else(county_fips == 51015, .$CAGR[which(.$county_fips == 51907)], CAGR)) %>% #Augusta
  mutate(CAGR = if_else(county_fips == 51790, .$CAGR[which(.$county_fips == 51907)], CAGR)) %>% #Staunton
  mutate(CAGR = if_else(county_fips == 51820, .$CAGR[which(.$county_fips == 51907)], CAGR)) %>% #Waynesboro
  mutate(CAGR = if_else(county_fips == 51031, .$CAGR[which(.$county_fips == 51911)], CAGR)) %>% #Campbell
  mutate(CAGR = if_else(county_fips == 51680, .$CAGR[which(.$county_fips == 51911)], CAGR)) %>% #Lynchburg
  mutate(CAGR = if_else(county_fips == 51035, .$CAGR[which(.$county_fips == 51913)], CAGR)) %>% #Carroll
  mutate(CAGR = if_else(county_fips == 51640, .$CAGR[which(.$county_fips == 51913)], CAGR)) %>% #Galax
  mutate(CAGR = if_else(county_fips == 51053, .$CAGR[which(.$county_fips == 51918)], CAGR)) %>% #Dinwiddie
  mutate(CAGR = if_else(county_fips == 51570, .$CAGR[which(.$county_fips == 51918)], CAGR)) %>% #Colonial Heights
  mutate(CAGR = if_else(county_fips == 51730, .$CAGR[which(.$county_fips == 51918)], CAGR)) %>% #Petersburg
  mutate(CAGR = if_else(county_fips == 51059, .$CAGR[which(.$county_fips == 51919)], CAGR)) %>% #Fairfax
  mutate(CAGR = if_else(county_fips == 51600, .$CAGR[which(.$county_fips == 51919)], CAGR)) %>% #Fairfax city
  mutate(CAGR = if_else(county_fips == 51610, .$CAGR[which(.$county_fips == 51919)], CAGR)) %>% #Falls Church
  mutate(CAGR = if_else(county_fips == 51069, .$CAGR[which(.$county_fips == 51921)], CAGR)) %>% #Frederick
  mutate(CAGR = if_else(county_fips == 51840, .$CAGR[which(.$county_fips == 51921)], CAGR)) %>% #Winchester
  mutate(CAGR = if_else(county_fips == 51081, .$CAGR[which(.$county_fips == 51923)], CAGR)) %>% #Greensville
  mutate(CAGR = if_else(county_fips == 51595, .$CAGR[which(.$county_fips == 51923)], CAGR)) %>% #Emporia
  mutate(CAGR = if_else(county_fips == 51089, .$CAGR[which(.$county_fips == 51929)], CAGR)) %>% #Henry
  mutate(CAGR = if_else(county_fips == 51690, .$CAGR[which(.$county_fips == 51929)], CAGR)) %>% #Martinsville
  mutate(CAGR = if_else(county_fips == 51095, .$CAGR[which(.$county_fips == 51931)], CAGR)) %>% #James City
  mutate(CAGR = if_else(county_fips == 51830, .$CAGR[which(.$county_fips == 51931)], CAGR)) %>% #Williamsburg
  mutate(CAGR = if_else(county_fips == 51121, .$CAGR[which(.$county_fips == 51933)], CAGR)) %>% #Montgomery
  mutate(CAGR = if_else(county_fips == 51750, .$CAGR[which(.$county_fips == 51933)], CAGR)) %>% #Radford
  mutate(CAGR = if_else(county_fips == 51143, .$CAGR[which(.$county_fips == 51939)], CAGR)) %>% #Pittsylvania
  mutate(CAGR = if_else(county_fips == 51590, .$CAGR[which(.$county_fips == 51939)], CAGR)) %>% #Danville
  mutate(CAGR = if_else(county_fips == 51149, .$CAGR[which(.$county_fips == 51941)], CAGR)) %>% #Prince George
  mutate(CAGR = if_else(county_fips == 51670, .$CAGR[which(.$county_fips == 51941)], CAGR)) %>% #Hopewell
  mutate(CAGR = if_else(county_fips == 51683, .$CAGR[which(.$county_fips == 51942)], CAGR)) %>% #Manassas City
  mutate(CAGR = if_else(county_fips == 51685, .$CAGR[which(.$county_fips == 51942)], CAGR)) %>% #Manassas Park City
  mutate(CAGR = if_else(county_fips == 51153, .$CAGR[which(.$county_fips == 51942)], CAGR)) %>% #Prince Williams
  mutate(CAGR = if_else(county_fips == 51161, .$CAGR[which(.$county_fips == 51944)], CAGR)) %>% #Roanoke
  mutate(CAGR = if_else(county_fips == 51775, .$CAGR[which(.$county_fips == 51944)], CAGR)) %>% #Salem
  mutate(CAGR = if_else(county_fips == 51163, .$CAGR[which(.$county_fips == 51945)], CAGR)) %>% #Rockbridge
  mutate(CAGR = if_else(county_fips == 51530, .$CAGR[which(.$county_fips == 51945)], CAGR)) %>% #Buena Vista
  mutate(CAGR = if_else(county_fips == 51678, .$CAGR[which(.$county_fips == 51945)], CAGR)) %>% #Lexington
  mutate(CAGR = if_else(county_fips == 51165, .$CAGR[which(.$county_fips == 51947)], CAGR)) %>% #Rockingham
  mutate(CAGR = if_else(county_fips == 51660, .$CAGR[which(.$county_fips == 51947)], CAGR)) %>% #Harrisonburg
  mutate(CAGR = if_else(county_fips == 51175, .$CAGR[which(.$county_fips == 51949)], CAGR)) %>% #Southampton
  mutate(CAGR = if_else(county_fips == 51620, .$CAGR[which(.$county_fips == 51949)], CAGR)) %>% #Franklin
  mutate(CAGR = if_else(county_fips == 51177, .$CAGR[which(.$county_fips == 51951)], CAGR)) %>% #Spotsylvania
  mutate(CAGR = if_else(county_fips == 51630, .$CAGR[which(.$county_fips == 51951)], CAGR)) %>% #Fredericksburg
  mutate(CAGR = if_else(county_fips == 51191, .$CAGR[which(.$county_fips == 51953)], CAGR)) %>% #Washington
  mutate(CAGR = if_else(county_fips == 51520, .$CAGR[which(.$county_fips == 51953)], CAGR)) %>% #Bristol
  mutate(CAGR = if_else(county_fips == 51195, .$CAGR[which(.$county_fips == 51955)], CAGR)) %>% #Wise
  mutate(CAGR = if_else(county_fips == 51720, .$CAGR[which(.$county_fips == 51955)], CAGR)) %>% #Norton
  mutate(CAGR = if_else(county_fips == 51199, .$CAGR[which(.$county_fips == 51958)], CAGR)) %>% #York
  mutate(CAGR = if_else(county_fips == 51735, .$CAGR[which(.$county_fips == 51958)], CAGR)) %>% #Poquoson
  mutate(CAGR = if_else(county_fips == 15009, .$CAGR[which(.$county_fips == 15901)], CAGR)) %>% #Kalawao
  mutate(CAGR = if_else(county_fips == 15005, .$CAGR[which(.$county_fips == 15901)], CAGR)) %>% #Maui
  drop_na()
  
BEA_GDP_COUNTY_BINS <- counties_map %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  ggplot(aes(fill = CAGR_bucket, color = CAGR_bucket), lwd = 0) +
  geom_sf() +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    #guide = FALSE, 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = FALSE, 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Annualized Real GDP Growth by County, 2019-2022") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26))
  
ggsave(dpi = "retina",plot = BEA_GDP_COUNTY_BINS, "BEA GDP COUNTY BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_COUNTIES_Growth <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, Growth)

counties_map_Growth <- get_urbn_map(map = "counties", sf = TRUE)

counties_map_Growth <- full_join(counties_map_Growth, BEA_GDP_COUNTIES_Growth, by = "county_fips") %>%
  mutate(Growth = if_else(county_fips == 51003, .$Growth[which(.$county_fips == 51901)], Growth)) %>% #Albemarle
  mutate(Growth = if_else(county_fips == 51540, .$Growth[which(.$county_fips == 51901)], Growth)) %>% #Charlottesville
  mutate(Growth = if_else(county_fips == 51005, .$Growth[which(.$county_fips == 51903)], Growth)) %>% #Alleghany
  mutate(Growth = if_else(county_fips == 51580, .$Growth[which(.$county_fips == 51903)], Growth)) %>% #Covington
  mutate(Growth = if_else(county_fips == 51015, .$Growth[which(.$county_fips == 51907)], Growth)) %>% #Augusta
  mutate(Growth = if_else(county_fips == 51790, .$Growth[which(.$county_fips == 51907)], Growth)) %>% #Staunton
  mutate(Growth = if_else(county_fips == 51820, .$Growth[which(.$county_fips == 51907)], Growth)) %>% #Waynesboro
  mutate(Growth = if_else(county_fips == 51031, .$Growth[which(.$county_fips == 51911)], Growth)) %>% #Campbell
  mutate(Growth = if_else(county_fips == 51680, .$Growth[which(.$county_fips == 51911)], Growth)) %>% #Lynchburg
  mutate(Growth = if_else(county_fips == 51035, .$Growth[which(.$county_fips == 51913)], Growth)) %>% #Carroll
  mutate(Growth = if_else(county_fips == 51640, .$Growth[which(.$county_fips == 51913)], Growth)) %>% #Galax
  mutate(Growth = if_else(county_fips == 51053, .$Growth[which(.$county_fips == 51918)], Growth)) %>% #Dinwiddie
  mutate(Growth = if_else(county_fips == 51570, .$Growth[which(.$county_fips == 51918)], Growth)) %>% #Colonial Heights
  mutate(Growth = if_else(county_fips == 51730, .$Growth[which(.$county_fips == 51918)], Growth)) %>% #Petersburg
  mutate(Growth = if_else(county_fips == 51059, .$Growth[which(.$county_fips == 51919)], Growth)) %>% #Fairfax
  mutate(Growth = if_else(county_fips == 51600, .$Growth[which(.$county_fips == 51919)], Growth)) %>% #Fairfax city
  mutate(Growth = if_else(county_fips == 51610, .$Growth[which(.$county_fips == 51919)], Growth)) %>% #Falls Church
  mutate(Growth = if_else(county_fips == 51069, .$Growth[which(.$county_fips == 51921)], Growth)) %>% #Frederick
  mutate(Growth = if_else(county_fips == 51840, .$Growth[which(.$county_fips == 51921)], Growth)) %>% #Winchester
  mutate(Growth = if_else(county_fips == 51081, .$Growth[which(.$county_fips == 51923)], Growth)) %>% #Greensville
  mutate(Growth = if_else(county_fips == 51595, .$Growth[which(.$county_fips == 51923)], Growth)) %>% #Emporia
  mutate(Growth = if_else(county_fips == 51089, .$Growth[which(.$county_fips == 51929)], Growth)) %>% #Henry
  mutate(Growth = if_else(county_fips == 51690, .$Growth[which(.$county_fips == 51929)], Growth)) %>% #Martinsville
  mutate(Growth = if_else(county_fips == 51095, .$Growth[which(.$county_fips == 51931)], Growth)) %>% #James City
  mutate(Growth = if_else(county_fips == 51830, .$Growth[which(.$county_fips == 51931)], Growth)) %>% #Williamsburg
  mutate(Growth = if_else(county_fips == 51121, .$Growth[which(.$county_fips == 51933)], Growth)) %>% #Montgomery
  mutate(Growth = if_else(county_fips == 51750, .$Growth[which(.$county_fips == 51933)], Growth)) %>% #Radford
  mutate(Growth = if_else(county_fips == 51143, .$Growth[which(.$county_fips == 51939)], Growth)) %>% #Pittsylvania
  mutate(Growth = if_else(county_fips == 51590, .$Growth[which(.$county_fips == 51939)], Growth)) %>% #Danville
  mutate(Growth = if_else(county_fips == 51149, .$Growth[which(.$county_fips == 51941)], Growth)) %>% #Prince George
  mutate(Growth = if_else(county_fips == 51670, .$Growth[which(.$county_fips == 51941)], Growth)) %>% #Hopewell
  mutate(Growth = if_else(county_fips == 51683, .$Growth[which(.$county_fips == 51942)], Growth)) %>% #Manassas City
  mutate(Growth = if_else(county_fips == 51685, .$Growth[which(.$county_fips == 51942)], Growth)) %>% #Manassas Park City
  mutate(Growth = if_else(county_fips == 51153, .$Growth[which(.$county_fips == 51942)], Growth)) %>% #Prince Williams
  mutate(Growth = if_else(county_fips == 51161, .$Growth[which(.$county_fips == 51944)], Growth)) %>% #Roanoke
  mutate(Growth = if_else(county_fips == 51775, .$Growth[which(.$county_fips == 51944)], Growth)) %>% #Salem
  mutate(Growth = if_else(county_fips == 51163, .$Growth[which(.$county_fips == 51945)], Growth)) %>% #Rockbridge
  mutate(Growth = if_else(county_fips == 51530, .$Growth[which(.$county_fips == 51945)], Growth)) %>% #Buena Vista
  mutate(Growth = if_else(county_fips == 51678, .$Growth[which(.$county_fips == 51945)], Growth)) %>% #Lexington
  mutate(Growth = if_else(county_fips == 51165, .$Growth[which(.$county_fips == 51947)], Growth)) %>% #Rockingham
  mutate(Growth = if_else(county_fips == 51660, .$Growth[which(.$county_fips == 51947)], Growth)) %>% #Harrisonburg
  mutate(Growth = if_else(county_fips == 51175, .$Growth[which(.$county_fips == 51949)], Growth)) %>% #Southampton
  mutate(Growth = if_else(county_fips == 51620, .$Growth[which(.$county_fips == 51949)], Growth)) %>% #Franklin
  mutate(Growth = if_else(county_fips == 51177, .$Growth[which(.$county_fips == 51951)], Growth)) %>% #Spotsylvania
  mutate(Growth = if_else(county_fips == 51630, .$Growth[which(.$county_fips == 51951)], Growth)) %>% #Fredericksburg
  mutate(Growth = if_else(county_fips == 51191, .$Growth[which(.$county_fips == 51953)], Growth)) %>% #Washington
  mutate(Growth = if_else(county_fips == 51520, .$Growth[which(.$county_fips == 51953)], Growth)) %>% #Bristol
  mutate(Growth = if_else(county_fips == 51195, .$Growth[which(.$county_fips == 51955)], Growth)) %>% #Wise
  mutate(Growth = if_else(county_fips == 51720, .$Growth[which(.$county_fips == 51955)], Growth)) %>% #Norton
  mutate(Growth = if_else(county_fips == 51199, .$Growth[which(.$county_fips == 51958)], Growth)) %>% #York
  mutate(Growth = if_else(county_fips == 51735, .$Growth[which(.$county_fips == 51958)], Growth)) %>% #Poquoson
  mutate(Growth = if_else(county_fips == 15009, .$Growth[which(.$county_fips == 15901)], Growth)) %>% #Kalawao
  mutate(Growth = if_else(county_fips == 15005, .$Growth[which(.$county_fips == 15901)], Growth)) %>% #Maui
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+")))


BEA_GDP_COUNTY_BINS_GROWTH <- counties_map_Growth %>%
  ggplot(aes(fill = Growth_bucket, color = Growth_bucket), lwd = 0) +
  geom_sf() +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = FALSE, 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                     na.value = "grey50", 
                     guide = FALSE, 
                     labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("         Real GDP Growth by County, 2019-2022") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26))

ggsave(dpi = "retina",plot = BEA_GDP_COUNTY_BINS_GROWTH, "BEA GDP COUNTY BINS GROWTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_COUNTIES_INCREASE <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Increase = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, Increase)

counties_map_increase <- get_urbn_map(map = "counties", sf = TRUE)

#NOTE: BEA Data Combines Several Small Counties, Particularly in VA, into Neighboring Counties. This code Manually copies over the CAGR values for those large counties into the Geometry for the smaller, constituent counties 

counties_map_increase <- full_join(counties_map_increase, BEA_GDP_COUNTIES_INCREASE, by = "county_fips") %>%
  mutate(Increase = if_else(county_fips == 51003, .$Increase[which(.$county_fips == 51901)], Increase)) %>% #Albemarle
  mutate(Increase = if_else(county_fips == 51005, .$Increase[which(.$county_fips == 51903)], Increase)) %>% #Alleghany
  mutate(Increase = if_else(county_fips == 51820, .$Increase[which(.$county_fips == 51907)], Increase)) %>% #Waynesboro
  mutate(Increase = if_else(county_fips == 51680, .$Increase[which(.$county_fips == 51911)], Increase)) %>% #Lynchburg
  mutate(Increase = if_else(county_fips == 51640, .$Increase[which(.$county_fips == 51913)], Increase)) %>% #Galax
  mutate(Increase = if_else(county_fips == 51730, .$Increase[which(.$county_fips == 51918)], Increase)) %>% #Petersburg
  mutate(Increase = if_else(county_fips == 51610, .$Increase[which(.$county_fips == 51919)], Increase)) %>% #Falls Church
  mutate(Increase = if_else(county_fips == 51840, .$Increase[which(.$county_fips == 51921)], Increase)) %>% #Winchester
  mutate(Increase = if_else(county_fips == 51595, .$Increase[which(.$county_fips == 51923)], Increase)) %>% #Emporia
  mutate(Increase = if_else(county_fips == 51690, .$Increase[which(.$county_fips == 51929)], Increase)) %>% #Martinsville
  mutate(Increase = if_else(county_fips == 51830, .$Increase[which(.$county_fips == 51931)], Increase)) %>% #Williamsburg
  mutate(Increase = if_else(county_fips == 51750, .$Increase[which(.$county_fips == 51933)], Increase)) %>% #Radford
  mutate(Increase = if_else(county_fips == 51590, .$Increase[which(.$county_fips == 51939)], Increase)) %>% #Danville
  mutate(Increase = if_else(county_fips == 51670, .$Increase[which(.$county_fips == 51941)], Increase)) %>% #Hopewell
  mutate(Increase = if_else(county_fips == 51153, .$Increase[which(.$county_fips == 51942)], Increase)) %>% #Prince Williams
  mutate(Increase = if_else(county_fips == 51775, .$Increase[which(.$county_fips == 51944)], Increase)) %>% #Salem
  mutate(Increase = if_else(county_fips == 51678, .$Increase[which(.$county_fips == 51945)], Increase)) %>% #Lexington
  mutate(Increase = if_else(county_fips == 51165, .$Increase[which(.$county_fips == 51947)], Increase)) %>% #Rockingham
  mutate(Increase = if_else(county_fips == 51620, .$Increase[which(.$county_fips == 51949)], Increase)) %>% #Franklin
  mutate(Increase = if_else(county_fips == 51630, .$Increase[which(.$county_fips == 51951)], Increase)) %>% #Fredericksburg
  mutate(Increase = if_else(county_fips == 51520, .$Increase[which(.$county_fips == 51953)], Increase)) %>% #Bristol
  mutate(Increase = if_else(county_fips == 51720, .$Increase[which(.$county_fips == 51955)], Increase)) %>% #Norton
  mutate(Increase = if_else(county_fips == 51199, .$Increase[which(.$county_fips == 51958)], Increase)) %>% #York
  mutate(Increase = if_else(county_fips == 15005, .$Increase[which(.$county_fips == 15901)], Increase)) %>% #Maui
  drop_na()

counties_map_increase_centroids <- counties_map_increase %>%
  st_centroid()

BEA_GDP_COUNTY_INCREASES_GRAPH <- counties_map %>%
  ggplot() +
  geom_sf(fill = "grey75") +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = Increase > 0, size = Increase/1000000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = Increase/1000000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(name = NULL,
                     values = c("#3083DC","#EE6055"),
                     breaks = c(TRUE, FALSE), 
                     labels = c("Increase", "Decrease"),
                     guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of Change\n2017 Dollars",
                  max_size = 15,
                  breaks = c(0,20,40,60),
                  labels = c("$0","$20B","$40B","$60B"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("         Real GDP Growth by County, 2019-2022") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = BEA_GDP_COUNTY_INCREASES_GRAPH, "BEA GDP COUNTY Increases.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_COUNTIES_NOMINAL_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP2", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_COUNTIES_INCREASE <- beaGet(BEA_GDP_COUNTIES_NOMINAL_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Increase = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, Increase)

counties_map_increase <- get_urbn_map(map = "counties", sf = TRUE)

#NOTE: BEA Data Combines Several Small Counties, Particularly in VA, into Neighboring Counties. This code Manually copies over the CAGR values for those large counties into the Geometry for the smaller, constituent counties 

counties_map_increase <- full_join(counties_map_increase, BEA_GDP_COUNTIES_INCREASE, by = "county_fips") %>%
  mutate(Increase = if_else(county_fips == 51003, .$Increase[which(.$county_fips == 51901)], Increase)) %>% #Albemarle
  mutate(Increase = if_else(county_fips == 51005, .$Increase[which(.$county_fips == 51903)], Increase)) %>% #Alleghany
  mutate(Increase = if_else(county_fips == 51820, .$Increase[which(.$county_fips == 51907)], Increase)) %>% #Waynesboro
  mutate(Increase = if_else(county_fips == 51680, .$Increase[which(.$county_fips == 51911)], Increase)) %>% #Lynchburg
  mutate(Increase = if_else(county_fips == 51640, .$Increase[which(.$county_fips == 51913)], Increase)) %>% #Galax
  mutate(Increase = if_else(county_fips == 51730, .$Increase[which(.$county_fips == 51918)], Increase)) %>% #Petersburg
  mutate(Increase = if_else(county_fips == 51610, .$Increase[which(.$county_fips == 51919)], Increase)) %>% #Falls Church
  mutate(Increase = if_else(county_fips == 51840, .$Increase[which(.$county_fips == 51921)], Increase)) %>% #Winchester
  mutate(Increase = if_else(county_fips == 51595, .$Increase[which(.$county_fips == 51923)], Increase)) %>% #Emporia
  mutate(Increase = if_else(county_fips == 51690, .$Increase[which(.$county_fips == 51929)], Increase)) %>% #Martinsville
  mutate(Increase = if_else(county_fips == 51830, .$Increase[which(.$county_fips == 51931)], Increase)) %>% #Williamsburg
  mutate(Increase = if_else(county_fips == 51750, .$Increase[which(.$county_fips == 51933)], Increase)) %>% #Radford
  mutate(Increase = if_else(county_fips == 51590, .$Increase[which(.$county_fips == 51939)], Increase)) %>% #Danville
  mutate(Increase = if_else(county_fips == 51670, .$Increase[which(.$county_fips == 51941)], Increase)) %>% #Hopewell
  mutate(Increase = if_else(county_fips == 51153, .$Increase[which(.$county_fips == 51942)], Increase)) %>% #Prince Williams
  mutate(Increase = if_else(county_fips == 51775, .$Increase[which(.$county_fips == 51944)], Increase)) %>% #Salem
  mutate(Increase = if_else(county_fips == 51678, .$Increase[which(.$county_fips == 51945)], Increase)) %>% #Lexington
  mutate(Increase = if_else(county_fips == 51165, .$Increase[which(.$county_fips == 51947)], Increase)) %>% #Rockingham
  mutate(Increase = if_else(county_fips == 51620, .$Increase[which(.$county_fips == 51949)], Increase)) %>% #Franklin
  mutate(Increase = if_else(county_fips == 51630, .$Increase[which(.$county_fips == 51951)], Increase)) %>% #Fredericksburg
  mutate(Increase = if_else(county_fips == 51520, .$Increase[which(.$county_fips == 51953)], Increase)) %>% #Bristol
  mutate(Increase = if_else(county_fips == 51720, .$Increase[which(.$county_fips == 51955)], Increase)) %>% #Norton
  mutate(Increase = if_else(county_fips == 51199, .$Increase[which(.$county_fips == 51958)], Increase)) %>% #York
  mutate(Increase = if_else(county_fips == 15005, .$Increase[which(.$county_fips == 15901)], Increase)) %>% #Maui
  drop_na()

counties_map_increase_centroids <- counties_map_increase %>%
  st_centroid()

BEA_NOMINAL_GDP_COUNTY_INCREASES_GRAPH <- counties_map %>%
  ggplot() +
  geom_sf(fill = "grey75") +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = Increase > 0, size = Increase/1000000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = Increase/1000000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of Change",
                  max_size = 10,
                  breaks = c(0,30,60,90),
                  labels = c("$0","$30B","$60B","$90B"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("         Nominal GDP Growth by County, 2019-2022") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = BEA_NOMINAL_GDP_COUNTY_INCREASES_GRAPH, "BEA GDP COUNTY Increases Nominal.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_COUNTIES_CATEGORIES_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" =  paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)


BEA_GDP_COUNTIES_CATEGORIES_CODES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/GDP%20Releases/2023%20Q3/NCHSURCodes2013.csv") %>%
  mutate(GeoFips = str_pad(FIPS, width = 5, pad = "0")) %>%
  mutate(Category = X2013_code) %>%
  select(GeoFips,Category)

BEA_GDP_COUNTIES_CATEGORIES <- beaGet(BEA_GDP_COUNTIES_CATEGORIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  inner_join(BEA_GDP_COUNTIES_CATEGORIES_CODES, by = "GeoFips") %>%
  group_by(Category, TimePeriod) %>%
  summarise(DataValue = sum(DataValue, na.rm = TRUE)) %>%
  mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Index = (DataValue/nth(DataValue, 3)) * 100) %>%
  ungroup() %>%
  mutate(Category = case_when(
    Category == 1 ~ "Large (Pop >1M) Metro, Central Counties",
    Category == 2 ~ "Large (Pop >1M) Metro, Suburban Counties",
    Category == 3 ~ "Medium (Pop 250k-1M) Metro",
    Category == 4 ~ "Small (Pop <250k) Metro",
    Category == 5 ~ "Micropolitan Areas",
    Category == 6 ~ "Non-Core Counties",
    TRUE ~ as.character(Category)  # handles other cases
  )) %>%
  mutate(TimePeriod = as.Date(paste0(TimePeriod,"-01-01")))

BEA_GDP_COUNTIES_CATEGORIES_Graph <- ggplot() +
  geom_line(data = filter(BEA_GDP_COUNTIES_CATEGORIES, Category != "Large (Pop >1M) Metro, Central Counties"), aes(x=TimePeriod, y = Index, color = Category), size = 1.25) + 
  geom_line(data = filter(BEA_GDP_COUNTIES_CATEGORIES, Category == "Large (Pop >1M) Metro, Central Counties"), aes(x=TimePeriod, y = Index, color = Category), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(92,108), breaks = c(92,94,96,98,100,102,104,106,108), expand = c(0,0)) +
  ylab("Index, 2019 = 100") +
  ggtitle("Centers of Major Metros Lead US Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = c(.30,.7)) +
  scale_color_manual(name= "Real GDP, Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("Large (Pop >1M) Metro, Central Counties","Large (Pop >1M) Metro, Suburban Counties","Medium (Pop 250k-1M) Metro","Small (Pop <250k) Metro","Micropolitan Areas","Non-Core Counties")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-730-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-730-as.Date("2017-01-01"))), ymin = 92-(.3*16), ymax = 92) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_GDP_COUNTIES_CATEGORIES_Graph, "BEA GDP COUNTIES CATEGORIES Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_METRO_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_METRO <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(145, DataValue) %>%
  transmute(GEOID = GeoFips, Growth)

#Just checking the raw increase in GDP by Metro Since 2019
BEA_GDP_METRO_INCREASES <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(INCREASE = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  #top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, INCREASE)


MSA_map <- core_based_statistical_areas(cb = TRUE)

MSA_map_US <- merge(MSA_map, BEA_GDP_METRO, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

BEA_GDP_MSA_BINS <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("              Real GDP Growth, 2019-2022\n            50 Largest Metro Areas by GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BEA_GDP_MSA_BINS, "BEA GDP MSA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_RPP_MSA_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "MARPP", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)


TOP_50_METROS_GDP <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  filter(DataValue == max(DataValue)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, DataValue)

BEA_RPP_MSA <- beaGet(BEA_RPP_MSA_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(Growth = (DataValue - first(DataValue))/100) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  #top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, Growth, GeoName) %>%
  merge(.,TOP_50_METROS_GDP, by = "GEOID")

MSA_map <- core_based_statistical_areas(cb = TRUE)

RPP_MSA_map_US <- merge(MSA_map, BEA_RPP_MSA, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, -0.015, 0,.015, Inf), labels = c("<-1.5%", "-1.5%-0", "0-1.5%", "1.5%+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

BEA_RPP_MSA_BINS <- RPP_MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  scale_fill_manual(values = c("#EE6055","#FF8E72","#F5B041","#FFE98F"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<-1.5%", "-1.5-0%", "0-1.5%", "1.5%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#FF8E72","#F5B041","#FFE98F")))) +
  ggtitle("Change in Metro Area Price Parities, 2019-2022\n               Relative to National Average\n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24))


ggsave(dpi = "retina",plot = BEA_RPP_MSA_BINS, "BEA RPP MSA BINS 2.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_RPI_MSA_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "MARPI", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 2, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

TOP_50_METROS_GDP <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  filter(DataValue == max(DataValue)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, DataValue)

BEA_RPI_MSA <- beaGet(BEA_RPI_MSA_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(Growth = (DataValue - first(DataValue))/first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  #top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, Growth, GeoName) %>%
  merge(.,TOP_50_METROS_GDP, by = "GEOID")

MSA_map <- core_based_statistical_areas(cb = TRUE)

RPI_MSA_map_US <- merge(MSA_map, BEA_RPI_MSA, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


BEA_RPI_MSA_BINS <- RPI_MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("Real Per-Capita Personal Income Growth, 2019-2022\n              At Metro Area Price Parities \n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 23))

ggsave(dpi = "retina",plot = BEA_RPI_MSA_BINS, "BEA RPI MSA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_TX <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("TX", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)


MSA_map_TX <- merge(MSA_map, BEA_GDP_METRO_TX, by = "GEOID") %>%
    mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
    mutate(NAME = sub("-.*", "", NAME)) %>%
    st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

MSA_map_TX_centroids <- MSA_map_TX %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_TX, .) %>%
  st_centroid()

BEA_GDP_MSA_TX_BINS <- MSA_map_TX %>%
    ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
    geom_sf(data = filter(states, state_abbv == c("TX")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
    geom_sf() +
    scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                      na.value = "grey50", 
                      #guide = "legend", 
                      labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                      guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
    ggtitle("   Real GDP Growth, 2019-2022\n         Texas Metro Areas") +
    theme(plot.title = element_text(size = 24)) +
    labs(caption = "Graph created by @JosephPolitano using BEA data") +
    labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids, GEOID %in% c("12420")), #Austin
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -350000, # adjust these values as needed
    nudge_x = 450000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids, GEOID %in% c("26420")), #Houston
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000, # adjust these values as needed
    nudge_x = 600000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids, GEOID %in% c("19100")), #Dallas
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 300000, # adjust these values as needed
    nudge_x = 200000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids, GEOID %in% c("41700")), #San Antonio
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -300000, # adjust these values as needed
    nudge_x = -550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
    theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())
  

ggsave(dpi = "retina",plot = BEA_GDP_MSA_TX_BINS, "BEA GDP MSA TX BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
  

BEA_GDP_METRO_CA <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("CA", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_CA <- merge(MSA_map, BEA_GDP_METRO_CA, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")
  

MSA_map_CA_centroids <- MSA_map_CA %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_CA, .) %>%
  st_centroid()

BEA_GDP_MSA_CA_BINS <- MSA_map_CA %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("CA")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  geom_sf(data = filter(counties_map_Growth, Growth_bucket == "0.12-0.16" & state_abbv == "CA"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("           Real GDP Growth, 2019-2022\n                California Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("31080")), #Los Angeles
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("41860")), #San Francisco
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +50000, # adjust these values as needed
    nudge_x = -650000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("41940")), #San Jose
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("41740")), #San Diego
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("40140")), #Riverside
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = +650000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("40900")), #Sacramento
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = +400000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_CA_BINS, "BEA GDP MSA CA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_FL <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("FL", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_FL <- merge(MSA_map, BEA_GDP_METRO_FL, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_FL_centroids <- MSA_map_FL %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_FL, .) %>%
  st_centroid()

BEA_GDP_MSA_FL_BINS <- MSA_map_FL %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("FL")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  geom_sf(data = filter(counties_map_Growth, Growth_bucket %in% c("<0","0.04-0.08") & state_abbv == "FL"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("       Real GDP Growth, 2019-2022\n             Florida Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids, GEOID %in% c("33100")), #Miami
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids, GEOID %in% c("45300")), #Tampa
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -150000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids, GEOID %in% c("36740")), #Orlando
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids, GEOID %in% c("27260")), #Jacksonville
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -175000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_FL_BINS, "BEA GDP MSA FL BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_GDP_METRO_NE <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("MA", "RI", "CT", "NY","NJ","PA","DE","MD","DC","VA","NH","VT","ME"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_NE <- merge(MSA_map, BEA_GDP_METRO_NE, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_NE_centroids <- MSA_map_NE %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_NE, .) %>%
  st_centroid()

BEA_GDP_MSA_NE_BINS <- MSA_map_NE %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("MA", "RI", "CT", "NY","NJ","PA","DE","MD","DC","VA","NH","VT","ME")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("          Real GDP Growth, 2019-2022\n   Northeast and Mid-Atlantic Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("35620")), #New York
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +150000, # adjust these values as needed
    nudge_x = +650000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("47900")), #Washington
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    nudge_x = +550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("14460")), #Boston
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = +750000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("37980")), #Philadelphia
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +80000, # adjust these values as needed
    nudge_x = +600000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("12580")), #Baltimore
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = +500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("38300")), #Pittsburgh
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("25540")), #Hartford
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 250000, # adjust these values as needed
    nudge_x = -700000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) + 
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("47260")), #Virginia Beach
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 40000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("40060")), #Richmond
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("14860")), #Bridgeport
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    nudge_x = -850000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_NE_BINS, "BEA GDP MSA NE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_METRO_RM <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("WA", "OR", "ID", "UT","CO","AZ","NV","NM"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_RM <- merge(MSA_map, BEA_GDP_METRO_RM, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_RM_centroids <- MSA_map_RM %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_RM, .) %>%
  st_centroid()

BEA_GDP_MSA_RM_BINS <- MSA_map_RM %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("WA", "OR", "ID", "UT","CO","AZ","NV","NM")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("          Real GDP Growth, 2019-2022\n          Rocky Mountain Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("42660")), #Seattle
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +150000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("38060")), #Phoenix
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("19740")), #Denver
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +400000, # adjust these values as needed
    nudge_x = +750000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("38900")), #Portland
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +75000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("29820")), #Las Vegas
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("41620")), #Salt Lake City
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 500000, # adjust these values as needed
    nudge_x = +950000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_RM_BINS, "BEA GDP MSA RM BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_MW <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("OH", "MI", "IN", "IL","WI","MN","IA","MO"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_MW <- merge(MSA_map, BEA_GDP_METRO_MW, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_MW_centroids <- MSA_map_MW %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_MW, .) %>%
  st_centroid()

BEA_GDP_MSA_MW_BINS <- MSA_map_MW %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("OH", "MI", "IN", "IL","WI","MN","IA","MO")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("            Real GDP Growth, 2019-2022\n                   Midwest Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("16980")), #Chicago
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +700000, # adjust these values as needed
    nudge_x = +900000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("33460")), #Minneapolis
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -80000, # adjust these values as needed
    nudge_x = -850000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("19820")), #Detroit
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +300000, # adjust these values as needed
    nudge_x = +550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("41180")), #STL
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    nudge_x = -800000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("17140")), #Cincinnati
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = +600000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("26900")), #Indianapolis
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -350000, # adjust these values as needed
    nudge_x = +300000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
    ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("28140")), #Kansas City
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("18140")), #Columbus
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    nudge_x = +750000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) + 
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("17460")), #Cleveland
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000, # adjust these values as needed
    nudge_x = +400000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("33340")), #Milwaukee
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 600000, # adjust these values as needed
    nudge_x = +250000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_MW_BINS, "BEA GDP MSA MW BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_SO <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("NC", "SC", "GA", "AL","MS","TN","KY","AR","LA"), collapse = "|"), GeoName)) %>%
  filter(GeoFips != "47260" & GeoFips != "17140") %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_SO <- merge(MSA_map, BEA_GDP_METRO_SO, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_SO_centroids <- MSA_map_SO %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_SO, .) %>%
  st_centroid()

BEA_GDP_MSA_SO_BINS <- MSA_map_SO %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("NC", "SC", "GA", "AL","MS","TN","KY","AR","LA")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("       Real GDP Growth, 2019-2022\n            South Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids, GEOID %in% c("12060")), #Atlanta
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +750000, # adjust these values as needed
    nudge_x = -100000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids, GEOID %in% c("16740")), #Charlotte
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -700000, # adjust these values as needed
    nudge_x = +450000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids, GEOID %in% c("34980")), #Nashville
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +300000, # adjust these values as needed
    nudge_x = -450000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids, GEOID %in% c("39580")), #Raleigh
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +200000, # adjust these values as needed
    nudge_x = +00000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids, GEOID %in% c("32820")), #Memphis
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +250000, # adjust these values as needed
    nudge_x = -300000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_SO_BINS, "BEA GDP MSA SO BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



ggsave(dpi = "retina",plot = GDPQuarterlyContrib_Graph, "Quarterly GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RFSALEDOMPRIV_Graph, "Real Final Private Sales.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RFSALEDOMPRIV_PCT_Graph, "Real Final Private Sales PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RealPrivateInventories_Graph, "Real Private Inventories.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RGDP_Graph, "RGDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LaborProductivity_Graph, "Labor Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = TradeDeficit_Graph, "Trade Deficit Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Port_Throughput_Graph, "Port Throughput.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_PERSONAL_INCOME_LESS_TRANSFERS_Graph, "Real Personal Income Less Transfers.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Employment_Index_Graph, "Employment Indexed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_GAS_Graph, "Real Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = INDUSTRIAL_PRODUCTION_Index_Graph, "Industrial Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT_Index_Graph, "Real Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = NGDP_Graph, "NGDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RGDO_Graph, "RGDO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = GDI_PCE, "GDI PCE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = STAT_DISC_Graph, "Stat Disc.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = CIPI_Graph, "CIPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Profits_Graph, "Corp Profits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = MANUFACTURING_Graph, "Manufacturing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT, "Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = NGDP_Graph, "NGDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
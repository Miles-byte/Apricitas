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
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment is Declining in Key Sectors") +
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
  ggtitle("Real Consumption and Investment Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.65,.2)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Year on Year Growth" ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = -.20-(.3*.375), ymax = -.20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_INVESTMENT_CONSUMPTION_GRAPH, "Real Investment Consumption.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_INVESTMENT_CONSUMPTION_2018_GRAPH <- ggplot() +
  geom_line(data=filter(REAL_FI_YOY, date >= as.Date("2018-01-01")), aes(x=date,y= value,color= "Real Private Fixed Investment"), size = 1.25)+ 
  geom_line(data=filter(REAL_PCE_YOY, date >= as.Date("2018-01-01")), aes(x=date,y= value/100,color= "Real Personal Consumption Expenditures"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year on Year Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.10,.175), expand = c(0,0), breaks = c(-.10,-0.05,0,0.05,0.1,0.15)) +
  ggtitle("Real Consumption and Investment Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.275,.85)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Year on Year Growth" ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.1-(.3*.275), ymax = -.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_INVESTMENT_CONSUMPTION_2018_GRAPH, "Real Investment Consumption 2018.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



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
  ggtitle("US `Core` GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Growth in Real Final Sales to Private Domestic Purchasers Has Rebounded") +
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

AGG_WAGES_2017 <- fredr(series_id = "A576RC1", observation_start = as.Date("2017-01-01"))

PCE_2017 <- fredr(series_id = "PCE", observation_start = as.Date("2017-01-01"))

PCEPI_2017 <- fredr(series_id = "PCEPI", observation_start = as.Date("2017-01-01"))

REAL_AGG_WAGES_2018 <- merge(AGG_WAGES_2017,PCEPI_2017, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  mutate(value = value/lag(value,12)-1) %>%
  drop_na()

REAL_PCE_2018 <- merge(PCE_2017,PCEPI_2017, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  mutate(value = value/lag(value,12)-1) %>%
  drop_na()

REAL_INVESTMENT_CONSUMPTION_2018_GRAPH <- ggplot() +
  geom_line(data=REAL_PCE_2018, aes(x=date,y= value,color= "Real Personal Consumption Expenditures"), size = 1.25)+ 
  geom_line(data=REAL_AGG_WAGES_2018, aes(x=date,y= value,color= "Real Compensation of Employees: Wage and Salary Disbursements"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year on Year Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.175,.3), expand = c(0,0), breaks = c(-.15,-.1,-0.05,0,0.05,0.10,0.15,0.2,0.25)) +
  ggtitle("Real Consumption and Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.425,.9225)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Year on Year Growth" ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.175-(.3*.475), ymax = -.175) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_INVESTMENT_CONSUMPTION_2018_GRAPH, "Real Investment Consumption 2018.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


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
  ggtitle("US Year-on-Year GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.35,.75)) + theme(legend.spacing.y = unit(0,"cm")) +
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
  theme_apricitas + theme(legend.position = c(.3,.92)) +
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
  subset(date >= as.Date("2022-01-01"))

RGDPQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A191RL 1 Gross domestic product Fisher Quantity Index Percent change, annual rate 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2022-01-01"))

GDPContribBEA_Graph <- ggplot(RGDP_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.0525,0.0625), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Contributions to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = "right") +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-12-01")-(.1861*(today()-as.Date("2021-12-01"))), xmax = as.Date("2021-12-01")-(0.049*(today()-as.Date("2021-12-01"))), ymin = -0.0525-(.3*.115), ymax = -0.0525) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDPContribBEA_Graph, "GDP Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_Investment_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A008RY 9 Nonresidential Quantity Contributions Level 0`,`T10102 A011RY 13 Residential Quantity Contributions Level 0`,`T10102 A014RY 14 Change in private inventories Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Nonresidential Fixed Investment", "Residential Fixed Investment", "Change in Private Inventories","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Nonresidential Fixed Investment`,`Residential Fixed Investment`,`Change in Private Inventories`)) %>%
  subset(date >= as.Date("2022-01-01"))

RGDPInvestmentQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A006RY 7 Gross private domestic investment Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2022-01-01"))

InvestmentQuarterlyContribBEA_Graph <- ggplot(RGDP_Investment_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPInvestmentQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.05), breaks = c(-0.04,-0.03,-.02,-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Investment's Contribution to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.275,.875), plot.title = element_text(size = 24)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Nonresidential Fixed Investment", "Residential Fixed Investment", "Change in Private Inventories")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-12-01")-(.1861*(today()-as.Date("2021-12-01"))), xmax = as.Date("2021-12-01")-(0.049*(today()-as.Date("2021-12-01"))), ymin = -0.04-(.3*.09), ymax = -0.04) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = InvestmentQuarterlyContribBEA_Graph, "Investment Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_Consumption_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 DDURRY 4 Durable goods Quantity Contributions Level 0`,`T10102 DNDGRY 5 Nondurable goods Quantity Contributions Level 0`,`T10102 DSERRY 6 Services Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Durable Goods", "Nondurable Goods", "Services","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Durable Goods`,`Nondurable Goods`,`Services`)) %>%
  subset(date >= as.Date("2022-01-01"))

RGDPConsumptionQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 DPCERY 2 Personal consumption expenditures Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2022-01-01"))

ConsumptionQuarterlyContribBEA_Graph <- ggplot(RGDP_Consumption_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPConsumptionQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,0.0375), breaks = c(-0.04,-0.03,-.02,-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Consumption's Contribution to US RGDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.275,.875), plot.title = element_text(size = 24)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Durable Goods", "Nondurable Goods", "Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-12-01")-(.1861*(today()-as.Date("2021-12-01"))), xmax = as.Date("2021-12-01")-(0.049*(today()-as.Date("2021-12-01"))), ymin = -0.01-(.3*.0475), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ConsumptionQuarterlyContribBEA_Graph, "Consumption Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_Govt_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A824RY 24 National defense Quantity Contributions Level 0`,`T10102 A825RY 25 Nondefense Quantity Contributions Level 0`,`T10102 A829RY 26 State and local Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  `colnames<-`(c("Federal Defense", "Federal Nondefense", "State & Local","date")) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Federal Defense`,`Federal Nondefense`,`State & Local`)) %>%
  subset(date >= as.Date("2022-01-01"))

RGDPGovtnQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A822RY 22 Government consumption expenditures and gross investment Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2022-01-01"))

GovtQuarterlyContribBEA_Graph <- ggplot(RGDP_Govt_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPGovtnQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .25),limits = c(-0.0075,0.016), breaks = c(-0.0075,-0.005,-0.0025,0,.0025,.005,.0075,0.01,0.0125,0.015), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Government's Contribution to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.275,.875), plot.title = element_text(size = 23)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Federal Defense", "Federal Nondefense", "State & Local")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-12-01")-(.1861*(today()-as.Date("2021-12-01"))), xmax = as.Date("2021-12-01")-(0.049*(today()-as.Date("2021-12-01"))), ymin = -0.0075-(.3*.0235), ymax = -0.0075) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GovtQuarterlyContribBEA_Graph, "Govt Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RGDP_NetExp_Contributions <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A253RY 17 Goods Quantity Contributions Level 0`,`T10102 A646RY 18 Services Quantity Contributions Level 0`,`T10102 A255RY 20 Goods Quantity Contributions Level 0`,`T10102 A656RY 21 Services Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  transmute(`Net Exports of Goods` = `T10102 A253RY 17 Goods Quantity Contributions Level 0` + `T10102 A255RY 20 Goods Quantity Contributions Level 0`, `Net Exports of Services` = `T10102 A656RY 21 Services Quantity Contributions Level 0` + `T10102 A646RY 18 Services Quantity Contributions Level 0`, date) %>%
  drop_na() %>%
  pivot_longer(cols = c(`Net Exports of Goods`,`Net Exports of Services`)) %>%
  subset(date >= as.Date("2022-01-01"))

RGDPNetExpQuarterly <- beaGet(RGDP_Contributions_Specs, iTableStyle = FALSE) %>%
  select(`T10102 A019RY 15 Net exports of goods and services Quantity Contributions Level 0`) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  `colnames<-`(c("value", "date")) %>%
  drop_na() %>%
  subset(date >= as.Date("2022-01-01"))

NetExptQuarterlyContribBEA_Graph <- ggplot(RGDP_NetExp_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPNetExpQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.05,0.05), breaks = c(-0.05,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Net Exports' Contribution to US Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.6,.85), plot.title = element_text(size = 23)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= "Contributions to Quarterly Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Net Exports of Goods", "Net Exports of Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-12-01")-(.1861*(today()-as.Date("2021-12-01"))), xmax = as.Date("2021-12-01")-(0.049*(today()-as.Date("2021-12-01"))), ymin = -0.05-(.3*.10), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NetExptQuarterlyContribBEA_Graph, "Net Exp Quarterly Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


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

REAL_FINAL_QUARTERLY_BAR <- ggplot(PRIVATE_SALES %>% filter(date >= as.Date("2022-01-01")), aes(fill="Growth in Real Final Sales to Private Domestic Purchasers", x=date, y=t10401_pb000003_8_final_sales_to_private_domestic_purchasers_fisher_quantity_index_percent_change_annual_rate_0/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  ylab("Quarterly Growth, Seasonally Adjusted at Annnualized Rates") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.005,.055), expand = c(0,0)) +
  ggtitle("US 'Core' GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Growth in Real Final Sales to Private Domestic Purchasers Has Remained Steady") +
  theme_apricitas + theme(legend.position = c(.5,.96)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_fill_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -.005-(.3*.06), ymax = -.005) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  clean_names()

FIXED_INVESTMENT_BULK_MF_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST_BULK, aes(x=date, y = u50406_c292rx_40_multifamily_structures_chained_dollars_level_6/1000, color = "Real Fixed Investment, Multi-Family Structures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,110), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Real Multifamily Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Multifamily Housing is at a Multi-Decade High") +
  theme_apricitas + theme(legend.position = c(.60,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*110), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_INVESTMENT_BULK_MANU_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST_BULK, aes(x=date, y = u50406_c307rx_14_manufacturing_chained_dollars_level_6/1000, color = "Real Fixed Investment, Manufacturing Structures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,160), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Real Manufacturing Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Manufacturing is at Multi-Decade High") +
  theme_apricitas + theme(legend.position = c(.60,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*160), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_INVESTMENT_BULK_ALTE_Graph <- ggplot() + #indexed employment rate
  geom_line(data = filter(FIXED_RESI_INVEST_BULK, date > as.Date("2010-01-01")), aes(x=date, y = u50406_lb001174_18_alternative_electric_chained_dollars_level_6/1000, color = "Real Fixed Investment, Alternative Electric Power\n(Wind, Solar, Geothermal & Dry Waste)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,ceiling(max(FIXED_RESI_INVEST_BULK$u50406_lb001174_18_alternative_electric_chained_dollars_level_6, na.rm = TRUE)/10000)*10), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Real Clean Alt Electricity Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Alternative Clean Sources of Electric Power is at a Record High") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*(ceiling(max(FIXED_RESI_INVEST_BULK$u50406_lb001174_18_alternative_electric_chained_dollars_level_6, na.rm = TRUE)/10000)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_BULK_MF_Graph, "Fixed Multifamily Investment Bulk Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT_BULK_MANU_Graph, "Fixed Manufacturing Investment Bulk Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT_BULK_ALTE_Graph, "Fixed Alt Electric Investment Bulk Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


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
  mutate(date = (seq(as.Date("2007-01-01"), length.out = nrow(.), by = "3 months"))) %>%
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
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*80), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.20,.80)) +
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

BROKERS_COMMISSIONS_RESIDENTIAL_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST_BULK, aes(x=date, y = u50406_a758rx_45_brokers_commissions_and_other_ownership_transfer_costs_chained_dollars_level_6/1000, color = "Real Brokers' Commissions & Other Ownership Transfer Costs, Residential Structures"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(75,200), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Real Brokers' Commissions") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(.525,.975)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 75-(.3*125), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BROKERS_COMMISSIONS_RESIDENTIAL_Graph, "Brokers Commissions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FIXED_IP_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T50306',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_IP_INVEST <- beaGet(FIXED_IP_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na() %>%
  mutate(RandD_Growth = t50306_y006rx_18_research_and_development_chained_dollars_level_6/lag(t50306_y006rx_18_research_and_development_chained_dollars_level_6,4)-1,Software_Growth = t50306_b985rx_17_software_chained_dollars_level_6/lag(t50306_b985rx_17_software_chained_dollars_level_6,4)-1) %>%
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

FIXED_IP_INVEST_GROWTH_GRAPH <- ggplot() + #growth in IP investment
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = FIXED_IP_INVEST, aes(x=date, y = RandD_Growth, color = "Research and Development"), size = 1.25) + 
  geom_line(data = FIXED_IP_INVEST, aes(x=date, y = Software_Growth, color = "Software"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.025,0.175), breaks = c(0,0.05,0.1,0.15), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Real IP Investment, Year-on-Year Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Software is Rebounding Amidst the AI Boom") +
  theme_apricitas + theme(legend.position = c(.225,.925)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Software","Research and Development")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.025-(.3*.2), ymax = -.025) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_IP_INVEST_GROWTH_GRAPH, "Fixed IP Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


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


PCEPI_DETAIL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20404',
  'Frequency' = 'M',
  'Year' = paste(seq(from = 2015, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCEPI_DETAIL_MONTHLY <- beaGet(PCEPI_DETAIL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2015-01-01"), length.out = nrow(.), by = "1 month"))) %>%
  clean_names() %>%
  mutate(across(where(is.numeric),~ .x / lag(.x, 12) - 1)) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  select(date, u20404_ia000062_375_pce_goods_excluding_food_and_energy_fisher_price_index_level_0)

PCE_PRICE_INDEX_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=PCEPI_DETAIL_MONTHLY, aes(x=date,y= u20404_dgdsrg_2_goods_fisher_price_index_level_0, color= "Goods"), size = 1.25) +
  geom_line(data=PCEPI_DETAIL_MONTHLY, aes(x=date,y= u20404_ia000062_375_pce_goods_excluding_food_and_energy_fisher_price_index_level_0, color= "Goods ex Food & Energy"), size = 1.25) +
  geom_line(data=PCEPI_DETAIL_MONTHLY, aes(x=date,y= u20404_ddurrg_3_durable_goods_fisher_price_index_level_0, color= "Durable Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10,0.12), limits = c(-0.04,.12), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Tariffs Cause a Rebound in Goods Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Goods Inflation Is Rebounding From 2024 Lows as Tariffs Raise the Price of Manufactured Imports") +
  theme_apricitas + theme(legend.position = c(.34,.79)) +
  scale_color_manual(name= "PCE Price Increases, Year-on-Year",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Goods","Goods ex Food & Energy","Durable Goods")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(.1861*(today()-as.Date("2016-01-01")))), ymin = -0.04-(.3*0.16), ymax = -0.04) +
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

US_COMPUTER_FIXED_EQUIP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6/1000, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  #geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral_yoy-1, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(FIXED_EQUIP_INVEST_REAL$u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6)/50000)*50), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Computer Investment At Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Computer Investment Has Grown Significantly Since the AI Boom, Breaking Years of Stagnation") +
  theme_apricitas + theme(legend.position = c(.3,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(plot.title = element_text(size = 26.5)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*(ceiling(max(FIXED_EQUIP_INVEST_REAL$u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6)/50000)*50)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_COMPUTER_FIXED_EQUIP_INVEST_GRAPH, "US Computer Fixed Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


REAL_IMPORTS_EXPORTS_GDP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T40206B',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2014, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_IMPORTS_EXPORTS_GDP <- beaGet(REAL_IMPORTS_EXPORTS_GDP_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2014-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

REAL_IMPORTS_GDP_Graph <- ggplot() + #indexed employment rate
  geom_line(data = REAL_IMPORTS_EXPORTS_GDP, aes(x=date, y = t40206b_a255rx_95_imports_of_goods_chained_dollars_level_6/1000000, color = "Real Imports of Goods"), size = 1.25) + 
  #geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral_yoy-1, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "T"), limits = c(1.5,ceiling(max(REAL_IMPORTS_EXPORTS_GDP$t40206b_a255rx_95_imports_of_goods_chained_dollars_level_6)/500000)*.5), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars, Seasonally Adjusted Annualized Rate") +
  ggtitle("The Rush to Import Before Trump's Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Imports of Goods Jumped to a Record High in Q1 2025 Amidst a Rush to Front-Run Tariffs") +
  theme_apricitas + theme(legend.position = c(.3,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(plot.title = element_text(size = 26.5)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 1.5-(.3*(ceiling(max(REAL_IMPORTS_EXPORTS_GDP$t40206b_a255rx_95_imports_of_goods_chained_dollars_level_6)/500000)*.5-1.5)), ymax = 1.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_IMPORTS_GDP_Graph, "Real Imports GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

REAL_IMPORTS_GDP_Category_Graph <- ggplot() + #indexed employment rate
  geom_line(data = REAL_IMPORTS_EXPORTS_GDP, aes(x=date, y = t40206b_lb001160_145_medicinal_dental_and_pharmaceutical_preparations_including_vitamins_chained_dollars_level_6/1000, color = "Pharmaceuticals & Related"), size = 1.25) + 
  geom_line(data = REAL_IMPORTS_EXPORTS_GDP, aes(x=date, y = t40206b_b852rx_119_computers_peripherals_and_parts_chained_dollars_level_6/1000, color = "Computers & Related"), size = 1.25) + 
  #geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral_yoy-1, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(REAL_IMPORTS_EXPORTS_GDP$t40206b_lb001160_145_medicinal_dental_and_pharmaceutical_preparations_including_vitamins_chained_dollars_level_6)/500000)*500), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars, Seasonally Adjusted Annualized Rate") +
  ggtitle("The Rush to Import Before Trump's Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Imports of Drugs & Computers Surged as Companies Rushed to Get Ahead of Trump's Tariffs") +
  theme_apricitas + theme(legend.position = c(.3,.8)) +
  scale_color_manual(name= "Real Imports of Goods, Select Categories",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(plot.title = element_text(size = 26.5)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 1.5-(.3*(ceiling(max(REAL_IMPORTS_EXPORTS_GDP$t40206b_lb001160_145_medicinal_dental_and_pharmaceutical_preparations_including_vitamins_chained_dollars_level_6)/500000)*500)-1.5), ymax = 1.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_IMPORTS_GDP_Category_Graph, "Real Imports Category GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


REAL_IMPORTS_COMPUTER_Graph <- ggplot() + #indexed employment rate
  geom_line(data = REAL_IMPORTS_EXPORTS_GDP, aes(x=date, y = (t40206b_b852rx_119_computers_peripherals_and_parts_chained_dollars_level_6)/1000, color = "Real Imports of Computers & Related Parts/Peripherals"), size = 1.25) + 
  #geom_line(data = REAL_IMPORTS_EXPORTS_GDP, aes(x=date, y = t40206b_b850rx_26_computers_peripherals_and_parts_chained_dollars_level_6/1000, color = "Real Exports of Computers & Related Parts/Peripherals"), size = 1.25) + 
  #geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral_yoy-1, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(REAL_IMPORTS_EXPORTS_GDP$t40206b_b852rx_119_computers_peripherals_and_parts_chained_dollars_level_6)/100000)*100), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars, Seasonally Adjusted Annualized Rate") +
  ggtitle("Computer Imports are Surging Amid the AI Boom") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Imports of Computers Have Surged to Record Highs as Companies Rush to Build US Data Centers") +
  theme_apricitas + theme(legend.position = c(.4,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 1.5-(.3*(ceiling(max(REAL_IMPORTS_EXPORTS_GDP$t40206b_b852rx_119_computers_peripherals_and_parts_chained_dollars_level_6)/100000)*100)-1.5), ymax = 1.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = REAL_IMPORTS_COMPUTER_Graph, "Real Imports Computer Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CHANGE_IN_INVENTORIES_GDP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50705BU1',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2014, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

CHANGE_IN_INVENTORIES_GDP <- beaGet(CHANGE_IN_INVENTORIES_GDP_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2014-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

CHANGE_IN_INVENTORIES_GDP_GRAPH <- ggplot() + #indexed employment rate
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = CHANGE_IN_INVENTORIES_GDP, aes(x=date, y = u50705bu1_c4222_56_drugs_and_druggists_sundries_wholesalers_current_dollars_level_6/1000, color = "Change in Private Inventories: Drug Wholesalers"), size = 1.25) + 
  #geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral_yoy-1, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(-40,ceiling(max(CHANGE_IN_INVENTORIES_GDP$u50705bu1_c4222_56_drugs_and_druggists_sundries_wholesalers_current_dollars_level_6)/50000)*50), expand = c(0,0)) +
  ylab("Billions of Dollars, Seasonally Adjusted Annualized Rate") +
  ggtitle("Pharma Companies are Stockpiling\nAhead of Trump's Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Inventories for Drug Wholesalers Skyrocketed Alongside Imports in Q1 2025") +
  theme_apricitas + theme(legend.position = c(.4,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(plot.title = element_text(size = 26.5)) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = -40-(.3*140), ymax = -40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = -40-(.3*((ceiling(max(CHANGE_IN_INVENTORIES_GDP$u50705bu1_c4222_56_drugs_and_druggists_sundries_wholesalers_current_dollars_level_6)/50000)*50)+40)), ymax = -40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = CHANGE_IN_INVENTORIES_GDP_GRAPH, "Change in Inventories GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



test <- beaSets(Sys.getenv("BEA_KEY"))

beaParams(Sys.getenv("BEA_KEY"))

test <- beaParamVals(Sys.getenv("BEA_KEY"),"NIUnderlyingDetail","TableName")

test <- test$ParamValue

?beaParams

test <- beaSearch("Real Exports",beaKey = Sys.getenv("BEA_KEY"))

FIXED_IP_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T40206',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)


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
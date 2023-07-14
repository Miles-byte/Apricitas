pacman::p_load(bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(300,700), breaks = c(300,400,500,600,700), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("The Trillion Dollar Mystery") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Spending on Tech Investment has Been Strong, But Not Spectacular") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 300-(.3*400), ymax = 300) +
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
  geom_line(data = GDI_Employees, aes(x=date, y = value/94.77595, color = "GDI: Compensation of Employees, Paid: Wages and Salaries"), size = 1.25) + 
  geom_line(data = PCE, aes(x=date, y = value/146.53949, color = "GDP: Personal Consumption Expenditures"), size = 1.25) + 
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
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(17,21), breaks = c(17,18,19,20,21), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "'Equivalent' Official Measures of Aggregate Output Are Diverging") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Real GDP","Real GDI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 17-(.3*4), ymax = 17) +
  coord_cartesian(clip = "off")

RGDO_Graph <- ggplot() +
  geom_line(data = RGDP, aes(x=date, y = value/1000, color = "Real GDP"), size = 1.25) + 
  geom_line(data = RGDI, aes(x=date, y = value/1000, color = "Real GDI"), size = 1.25) + 
  geom_line(data = RGDO, aes(x=date, y = value/1000, color = "Real GDO (Average of GDP and GDI)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(17,21), breaks = c(17,18,19,20,21), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Measures of Output Show Extremely Weak Growth So Far This Year") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055"), breaks = c("Real GDP","Real GDI","Real GDO (Average of GDP and GDI)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 17-(.3*4), ymax = 17) +
  coord_cartesian(clip = "off")

REAL_GAS_Graph <- ggplot() +
  geom_line(data = REAL_GAS, aes(x=date, y = value, color = "Real Personal Consumption Expenditures: Gasoline And Other Energy Goods"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(340,480), breaks = c(350,400,450), expand = c(0,0)) +
  ylab("Billions of 2012 US Dollars") +
  ggtitle("Demand Destruction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Gasoline Consumption Has Fallen Below Pre-COVID Lows") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 340-(.3*140), ymax = 340) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(14.5,18), breaks = c(15,16,17,18), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Growth In Real Final Private Domestic Consumption and Investment Has Weakened Considerably") +
  theme_apricitas + theme(legend.position = c(.40,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 14.5-(.3*3.5), ymax = 14.5) +
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
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(9,15), breaks = c(9,10,11,12,13,14,15), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Personal Income is Stalling-But Not Yet Shrinking") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 9-(.3*6), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Private Inventories
RealPrivateInventories_Graph <- ggplot() + 
  geom_line(data = RealPrivateInventories, aes(x=date, y = value/1000, color = "Real Private Inventories"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = .05),limits = c(2.75,2.95), breaks = c(2.75,2.8,2.85,2.9,2.95), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Taking Inventory") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Inventory Growth Was High-But Lower Than Last Quarter") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 2.75-(.3*.2), ymax = 2.75) +
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
  geom_line(data = FIXED_RESIDENTIAL, aes(x=date, y = value/5.96, color = "Real Fixed Investment: Residential"), size = 1.25) + 
  geom_line(data = FIXED_INDUSTRIAL, aes(x=date, y = value/2.42, color = "Real Fixed Investment: Industrial Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(85,90,95,100,105,110,115,120,125), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("Unfixed Problems") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Fixed Investment is Declining in Key Sectors") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 80-(.3*45), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_RESIDENTIAL_2015 <- fredr(series_id = "PRFIC1",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)

FIXED_INVESTMENT_Residential_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESIDENTIAL_2015, aes(x=date, y = value/5.96, color = "Real Fixed Investment: Residential"), size = 1.25) + 
  #geom_line(data = FIXED_INDUSTRIAL, aes(x=date, y = value/2.42, color = "Real Fixed Investment: Industrial Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(85,90,95,100,105,110,115,120,125), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("Unfixed Problems") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Fixed Investment is Declining in Key Sectors") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 80-(.3*45), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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

#Data Using BEA API

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
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "US NGDP Growth is Still Well Above Pre Pandemic Levels—As Real Growth Slows") +
  theme_apricitas + theme(legend.position = c(.47,.75)) + theme(legend.spacing.y = unit(0,"cm")) +
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
  annotate("text", label = "4% Pre-COVID Norm",y = 0.044, x = as.Date("2020-02-20"), color = "white", size = 3.5) +
  annotate("text", label = "*Note: Q1-Q3 2020 Excluded Because of Volatility",y = 0.08, x = as.Date("2019-02-01"), color = "white", size = 3.5) +
  #geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  #guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.15), breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("US NGDP Growth is Still Elevated") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Spending is Declining But Still Too High To Maintain Target Inflation") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
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


GDPMonthlyContribBEA_Graph <- ggplot(RGDP_Contributions, aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  #guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.09), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Contributions to GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = c(.67,.88)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.04-(.3*.13), ymax = -0.04) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDPMonthlyContribBEA_Graph, "GDP Contributions BEA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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
REAL_FI_YOY <- fredr(series_id = "FPIC1",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")

REAL_INVESTMENT_CONSUMPTION_GRAPH <- ggplot() +
  geom_line(data=REAL_FI_YOY, aes(x=date,y= value/100,color= "Real Private Fixed Investment"), size = 1.25)+ 
  geom_line(data=REAL_PCE_YOY, aes(x=date,y= value/100,color= "Real Personal Consumption Expenditures"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year on Year Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.20,.175), expand = c(0,0)) +
  ggtitle("The Investment Crash") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Consumption Growth Has Slowed—While Aggregate Private Investment Has Collapsed") +
  theme_apricitas + theme(legend.position = c(.65,.2)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
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

#
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
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dfdhra_21_furnishings_and_durable_household_equipment_fisher_quantity_index_level_0/u20403_dfdhra_21_furnishings_and_durable_household_equipment_fisher_quantity_index_level_0[1]*100, color = "Furnishings and Durable Household Equipment"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dwhlra_51_sports_and_recreational_vehicles_79_fisher_quantity_index_level_0/u20403_dwhlra_51_sports_and_recreational_vehicles_79_fisher_quantity_index_level_0[1]*100, color = "Sports and Recreational Vehicles"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(75,165), breaks = c(80,90,100,110,120,130,140,150,160,170), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Outside of Motor Vehicles, Consumption of Large Durable Goods Still Remains Elevated") +
  theme_apricitas + theme(legend.position = c(.315,.825)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*90), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

REAL_PCE_BREAKDOWN_TECH_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dvaara_38_video_and_audio_equipment_fisher_quantity_index_level_0/u20403_dvaara_38_video_and_audio_equipment_fisher_quantity_index_level_0[1]*100, color = "Video and Audio Equipment"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dipera_46_information_processing_equipment_fisher_quantity_index_level_0/u20403_dipera_46_information_processing_equipment_fisher_quantity_index_level_0[1]*100, color = "Information Processing Equipment (Including Computers and Software)"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dtcera_69_telephone_and_related_communication_equipment_fisher_quantity_index_level_0/u20403_dtcera_69_telephone_and_related_communication_equipment_fisher_quantity_index_level_0[1]*100, color = "Telephone and Related Communication Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(75,265), breaks = c(100,150,200,250), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Tech-Related Durables Surged During The Pandemic, But Has Slowed Recently") +
  theme_apricitas + theme(legend.position = c(.515,.85)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*175), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

REAL_PCE_BREAKDOWN_NONDURABLES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dclora_102_clothing_and_footwear_fisher_quantity_index_level_0/u20403_dclora_102_clothing_and_footwear_fisher_quantity_index_level_0[1]*100, color = "Clothing and Footwear"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dreira_124_recreational_items_parts_of_80_92_and_93_fisher_quantity_index_level_0/u20403_dreira_124_recreational_items_parts_of_80_92_and_93_fisher_quantity_index_level_0[1]*100, color = "Recreational Items"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dopcra_135_personal_care_products_part_of_118_fisher_quantity_index_level_0/u20403_dopcra_135_personal_care_products_part_of_118_fisher_quantity_index_level_0[1]*100, color = "Personal Care Products"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dfxara_71_food_and_beverages_purchased_for_off_premises_consumption_fisher_quantity_index_level_0/u20403_dfxara_71_food_and_beverages_purchased_for_off_premises_consumption_fisher_quantity_index_level_0[1]*100, color = "Food and Beverages for Off-Premises Consumption"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(55,165), breaks = c(60,70,80,90,100,110,120,130,140,150,160,170), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Outside of Food, Consumption of Many Nondurable Goods Still Remains Elevated") +
  theme_apricitas + theme(legend.position = c(.35,.825)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Food and Beverages for Off-Premises Consumption","Recreational Items","Clothing and Footwear","Personal Care Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 55-(.3*115), ymax = 55) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

REAL_PCE_BREAKDOWN_SERVICES_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dhlcra_170_health_care_fisher_quantity_index_level_0/u20403_dhlcra_170_health_care_fisher_quantity_index_level_0[1]*100, color = "Health Care Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dtrsra_188_transportation_services_fisher_quantity_index_level_0/u20403_dtrsra_188_transportation_services_fisher_quantity_index_level_0[1]*100, color = "Transportation Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_drcara_207_recreation_services_fisher_quantity_index_level_0/u20403_drcara_207_recreation_services_fisher_quantity_index_level_0[1]*100, color = "Recreation Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dfsera_233_food_services_fisher_quantity_index_level_0/u20403_dfsera_233_food_services_fisher_quantity_index_level_0[1]*100, color = "Food Services"), size = 1.25) + 
  geom_line(data = REAL_PCE_BREAKDOWN, aes(x=date, y = u20403_dperra_305_personal_care_and_clothing_services_14_and_parts_of_17_and_118_fisher_quantity_index_level_0/u20403_dperra_305_personal_care_and_clothing_services_14_and_parts_of_17_and_118_fisher_quantity_index_level_0[1]*100, color = "Personal Care and Clothing Services"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(0,115), breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The Slow Renormalization") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Services Consumption Is Climbing Back But Still Below Pre-Pandemic Levels in Many Areas") +
  theme_apricitas + theme(legend.position = c(.75,.25)) +
  scale_color_manual(name= "Real Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*115), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_RESI_INVEST <- beaGet(FIXED_RESI_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVESTMENT_RESIDENTIAL_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a944rx_37_single_family_structures_chained_dollars_level_6/u50406_a944rx_37_single_family_structures_chained_dollars_level_6[1]*100, color = "Single-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_c292rx_38_multifamily_structures_chained_dollars_level_6/u50406_c292rx_38_multifamily_structures_chained_dollars_level_6[1]*100, color = "Multi-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a946rx_42_improvements_chained_dollars_level_6/u50406_a946rx_42_improvements_chained_dollars_level_6[1]*100, color = "Residential Improvements"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,130), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Unfixed Problems") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Single-Family Homes and Home Improvements are Declining") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= "Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Single-Family Structures","Multi-Family Structures","Residential Improvements")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*45), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


FIXED_INVESTMENT_NONRESIDENTIAL_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w031rx_4_office_chained_dollars_level_6/u50406_w031rx_4_office_chained_dollars_level_6[1]*100, color = "Offices"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w032rx_5_health_care_chained_dollars_level_6/u50406_w032rx_5_health_care_chained_dollars_level_6[1]*100, color = "Healthcare"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w038rx_12_warehouses_chained_dollars_level_6/u50406_w038rx_12_warehouses_chained_dollars_level_6[1]*100, color = "Warehouses"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_c307rx_14_manufacturing_chained_dollars_level_6/u50406_c307rx_14_manufacturing_chained_dollars_level_6[1]*100, color = "Manufacturing"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_w036rx_10_multimerchandise_shopping_chained_dollars_level_6/u50406_w036rx_10_multimerchandise_shopping_chained_dollars_level_6[1]*100, color = "Multimerchandise Shopping"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(0,130), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Structural Shortfall") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Healthcare, Offices, and Shopping Malls are Well Below Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.21,.22)) +
  scale_color_manual(name= "Real Investment: Structures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*130), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  scale_y_continuous(limits = c(90,170), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("The Tech Push") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Software and R&D Surged During the Pandemic—Though R&D is Cooling Now") +
  theme_apricitas + theme(legend.position = c(.21,.72)) +
  scale_color_manual(name= "Real Private Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*80), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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

PCE_PC_YEAR <- fredr(series_id = "A794RC0Q052SBEA", observation_start = as.Date("1995-01-01"), units = "pc1")

PCE_PC_YEAR_Graph <- ggplot() + #plotting Wage Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=PCE_PC_YEAR, aes(x=date,y= value/100,color= "Personal Consumption Expenditures Per Capita, Annual Growth"), size = 1.25) +
  annotate("hline", y = 0.04, yintercept = 0.04, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", label = "4% Growth Roughly Consistent With 2% Inflation",y = 0.056, x = as.Date("2013-09-30"), color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.10,0.225), breaks = c(-.10,-0.05,0,0.05,0.10,0.15,0.20), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("US Spending Growth Remains High") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "Spending Growth Has Decelerated But Remains Well Above Normal Levels") +
  theme_apricitas + theme(legend.position = c(.42,.72)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = -.10-(.3*0.325), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_PC_YEAR_Graph, "PCE PC Growth Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

FIXED_EQUIP_INVEST_SPECS_REAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50506',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST_REAL <- beaGet(FIXED_EQUIP_INVEST_SPECS_REAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_EQUIP_INVEST_SPECS_NOMINAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50505',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST_NOMINAL <- beaGet(FIXED_EQUIP_INVEST_SPECS_NOMINAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

US_CHIP_FIXED_EQUIP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = (u50506_c272rx_18_special_industry_machinery_n_e_c_chained_dollars_level_6/u50506_c272rx_18_special_industry_machinery_n_e_c_chained_dollars_level_6[1]*FIXED_EQUIP_INVEST_NOMINAL$u50505_c272rc_18_special_industry_machinery_n_e_c_current_dollars_level_6[1])/1000, color = "Real 2018 Dollars"), size = 1.25) + 
  geom_line(data = FIXED_EQUIP_INVEST_NOMINAL, aes(x=date, y = (u50505_c272rc_18_special_industry_machinery_n_e_c_current_dollars_level_6/1000), color = "Nominal"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,90), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("US Chip Equipment Spending Has Surged") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Nominal Business Investment in Special Industry Equipment Has Doubled Since 2018") +
  theme_apricitas + theme(legend.position = c(.51,.23)) +
  scale_color_manual(name= "Private Fixed Investment, Special Industry Machinery N.E.C. (Mostly Semiconductor Equipment)",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*90), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


RegionalSearch <- beaParamVals(beaKey = Sys.getenv("BEA_KEY"), "Regional", "LineCode")$ParamValue

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
  subset(date >= as.Date("2019-10-01")) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "GDP") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(CAGR = (GDP / first(GDP)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, BEA_GDP_STATE, by = "state_name")

BEA_GDP_STATE <- ggplot() +
  geom_sf(data = states, aes(fill = CAGR)) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.05,0.05)) +
  ggtitle("       Annualized GDP Growth Since Q4 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

BEA_GDP_STATE <- states %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  ggplot(aes(fill = CAGR_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#FFE98F","#F5B041", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("     Annualized Real GDP Growth Since Q4 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_STATE, "BEA GDP STATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = US_CHIP_FIXED_EQUIP_INVEST_GRAPH, "US Chip Equipment Fixed Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
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
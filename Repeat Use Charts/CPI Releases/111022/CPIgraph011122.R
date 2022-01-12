pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

CPI <- bls_api("CUSR0000SA0", startyear = 2019, endyear = 2021, Sys.getenv("BLS_KEY")) #headline cpi data
CPIUSEDCARS <- bls_api("CUSR0000SETA02", startyear = 2019, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi used cars data
CPINEWCARS <- bls_api("CUSR0000SETA01", startyear = 2019, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi new cars data
CPIRENT <- bls_api("CUSR0000SEHA", startyear = 2019, endyear = 2021, calculations = TRUE, Sys.getenv("BLS_KEY")) #cpi rent data
CPIORENT <- bls_api("CUSR0000SEHC", startyear = 2019, endyear = 2021, calculations = TRUE, Sys.getenv("BLS_KEY")) #cpi owners equivalent rent
CPIGAS <- bls_api("CUSR0000SETB01", startyear = 2019, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi gasoline data
CPIPIPEDGAS <- bls_api("CUSR0000SEHF02", startyear = 2019, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi piped gas data
CPILODGINGHOME <- bls_api("CUSR0000SEHB", startyear = 2019, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi lodging away from home data
CPIDURABLE <- bls_api("CUSR0000SAD", startyear = 2005, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi durables data
CPISERVICE <- bls_api("CUSR0000SAS", startyear = 2005, endyear = 2021, Sys.getenv("BLS_KEY")) #cpi services data


CPIRENT$calculations <- str_sub(CPIRENT$calculations, start= -3) #correcting percent growth calculations to remove excess data and convert to numeric
CPIRENT$calculations <- as.numeric(CPIRENT$calculations)
CPIORENT$calculations <- str_sub(CPIORENT$calculations, start= -3)
CPIORENT$calculations <- as.numeric(CPIORENT$calculations)


CPI=CPI[order(nrow(CPI):1),] #blscraper puts rows in reverse order, so I am reversing them again here
CPIUSEDCARS=CPIUSEDCARS[order(nrow(CPIUSEDCARS):1),]
CPINEWCARS=CPINEWCARS[order(nrow(CPINEWCARS):1),]
CPIRENT=CPIRENT[order(nrow(CPIRENT):1),]
CPIORENT=CPIORENT[order(nrow(CPIORENT):1),]
CPIGAS=CPIGAS[order(nrow(CPIGAS):1),]
CPIPIPEDGAS=CPIPIPEDGAS[order(nrow(CPIPIPEDGAS):1),]
CPILODGINGHOME=CPILODGINGHOME[order(nrow(CPILODGINGHOME):1),]
CPIDURABLE=CPIDURABLE[order(nrow(CPIDURABLE):1),]
CPISERVICE=CPISERVICE[order(nrow(CPISERVICE):1),]


CPI$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months") #adding a "date" sequence. Note: you will have to change the end data if you want to perform this analysis for data ending aftere 12/1/2022
CPIUSEDCARS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPINEWCARS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPIRENT$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPIORENT$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPIGAS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPIPIPEDGAS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPILODGINGHOME$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
CPIDURABLE$date <- seq(as.Date("2005-01-01"), as.Date("2021-12-01"), "months")
CPISERVICE$date <- seq(as.Date("2005-01-01"), as.Date("2021-12-01"), "months")

#FRED Backups
#CPIUSEDCARS <- fredr(series_id = "CUSR0000SETA02",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#CPINEWCARS <- fredr(series_id = "CUSR0000SETA01",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") 
#CPIOERENT <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") 
#CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
#PPIIDC <- fredr(series_id = "PPIIDC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#PROFIT_MARGIN <- fredr(series_id = "A463RD3Q052SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

PCE <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #personal consumption expenditured data
PCEGD <- fredr(series_id = "DGDSRC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pce goods data
PCESV <- fredr(series_id = "PCES",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pce services data
WTI <- fredr(series_id = "DCOILWTICO",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #WTI oil prices data
FIVEYEARBREAKEVEN <- fredr(series_id = "T5YIE",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN <- fredr(series_id = "T5YIFR",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data
WTI <- drop_na(WTI) #dropping NAs
FIVEYEARBREAKEVEN <- drop_na(FIVEYEARBREAKEVEN)
FIVEYEARFWDBREAKEVEN <- drop_na(FIVEYEARFWDBREAKEVEN)

ECISERV <- fredr(series_id = "CIS201S000000000I",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading Employment Cost Index (ECI) services data
ECIGOOD <- fredr(series_id = "CIU201G000000000I",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading ECI goods data
PCESERV <- fredr(series_id = "DSERRG3M086SBEA",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading PCE services
PCEGOOD <- fredr(series_id = "DGDSRG3M086SBEA",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading PCE goods

Wage_Price_Merge <- do.call("rbind", list(ECISERV,ECIGOOD,PCESERV,PCEGOOD)) #binding ECI and PCE data for service and goods sector
#renaming series IDs in the merged data set to plain language explanations
Wage_Price_Merge$series_id <- gsub("CIS201S000000000I","Services Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("CIU201G000000000I","Goods Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DSERRG3M086SBEA","Services Prices (PCE)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DGDSRG3M086SBEA","Goods Prices (PCE)",Wage_Price_Merge$series_id)


PCEPIPCT <- fredr(series_id = "PCEPI",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #PCEPI yoy percent growth data
COREPCEPI <- fredr(series_id = "PCEPILFE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #Core PCEPI yoy percent growth data
TRIMMEDPCEPI <- fredr(series_id = "PCETRIM12M159SFRBDAL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Trimmed Mean PCEPI data

PCEPIIND <- fredr(series_id = "PCEPI",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pcepi index data

#manually adding 2% CPI growth trend for later chart on above-trend CPI
CPI$CPITREND <- c(seq(0,0,length.out = 13), 258.824*1.001652^(0:22)) #the sequence of zeroes is for the part of the chart where the trendline is excluded, and the second sequence is compounding CPI monthly at a 2% annual rate

#manually adding 4% personal income and outlays growth trend line for later chart on personal income and outlays
DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-11-01"), "months")), trend = 16622.8*1.003274^(0:22)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-11-01"), "months")), trend = 15328.8*1.003274^(0:22))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI_SERV_DURABLE <- merge(CPIDURABLE,CPISERVICE, by = "date") #merging cpi services and durables data

PCE_Graph <- ggplot() + #plotting Personal Consumption Expenditures as well as PCE Goods/Services
  geom_line(data=PCE, aes(x=date,y= (value/141.04) ,color= "Total Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCEGD, aes(x=date,y= (value/43.46) ,color= "Goods Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCESV, aes(x=date,y= (value/97.73) ,color= "Services Personal Consumption Expenditures"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(83,135), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  ylab("Personal Consumption Expenditures: January 2019 = 100") +
  ggtitle("Good to Go?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Goods Shot Up after the Pandemic Hit, but is Now Stalling") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 83-(.3*47), ymax = 83) +
  coord_cartesian(clip = "off")

Wage_Price_Graph <- ggplot() + #plotting service/goods industry wages/prices using ECI/PCE
  geom_line(data = Wage_Price_Merge, aes(x=date, y = value/100, color = series_id, alpha = series_id), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.025,0.065), breaks = c(-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Spiral? Not so Fast!") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "Price Growth is in Goods, but Wage Growth is in Services") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  scale_alpha_manual(name= NULL,values = c(1,.5,1,.5)) + #scale alpha is making some of the lines have less opacity
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = -.025-(.3*.09), ymax = -0.025) +
  coord_cartesian(clip = "off")


CPI_Durable_Services <- ggplot() + #plotting durables v services inflation
  geom_line(data=CPI_SERV_DURABLE, aes(x=date,y= (value.x/value.y)/.5132*100 ,color= "CPI Durables/CPI Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,100), breaks = c(60,70,80,90,100), expand = c(0,0)) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Relative Price of Durable Goods Has Rocketed Upwards in 2021") +
  theme_apricitas + theme(legend.position = c(.50,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*6200), xmax = as.Date("2005-01-01")-(0.049*6200), ymin = 60-(.3*40), ymax = 60) +
  coord_cartesian(clip = "off")

T5YIE <- ggplot() + #plotting inflation breakevens
  geom_line(data=FIVEYEARBREAKEVEN, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARFWDBREAKEVEN, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "In a Recent Reversal, Short Term Inflation Expectations Are Higher Than Long-Term Expectations") +
  theme_apricitas + theme(legend.position = c(.60,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6950), xmax = as.Date("2003-01-01")-(0.049*6950), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

CPI_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=CPI, aes(x=date,y= (value/2.52) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPI, aes(x=date,y= CPITREND/2.52 ,color= "2% Target"), size = 1.25, linetype = "dashed") +
  geom_line(data=PCEPIIND, aes(x=date,y= (value/1.0814) ,color= "PCEPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(100,112), breaks = c(100,105,110), expand = c(0,0)) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "The CPI and PCE Price Indexes are Way Above Trend") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= NULL,breaks = c("CPI","PCEPI","2% Target"),values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 100-(.3*12), ymax = 100) +
  coord_cartesian(clip = "off")

PCE_Inflation_Rates_Graph <- ggplot() + #plotting PCE Inflation Rates
  geom_line(data=PCEPIPCT, aes(x=date,y= value/100 ,color= "PCE Inflation"), size = 1.25) +
  geom_line(data=COREPCEPI, aes(x=date,y= value/100 ,color= "Core PCE Inflation"), size = 1.25) +
  geom_line(data=TRIMMEDPCEPI, aes(x=date,y= value/100 ,color= "Trimmed Mean PCE Inflation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.055), breaks = c(0,.02,.04), expand = c(0,0)) +
  ylab("Percent Change from One Year Ago") +
  ggtitle("Is Inflation Transitory?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Traditional 'Core' Inflation Metric Does Not Eliminate Pandemic Volatility") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E"))+
  annotate(geom = "hline",y = 0.02,yintercept = 0.02, size = 1.25,linetype = "dashed",color = "white") +#annotating 2% target inflation
  annotate(geom = "text", label = as.character("2% Inflation Target"),x = as.Date("2019-05-01"),y = 0.023,color = "white") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.055), ymax = 0) +
  coord_cartesian(clip = "off")

CPI_New_Used_Car_Vehicles_Graph <- ggplot() + #plotting "Used Cars and Trucks" and "New Vehicles" price Indexes
  geom_line(data=CPIUSEDCARS, aes(x=date,y= (value/141)*100 ,color= "Used Cars and Trucks"), size = 1.25) +
  geom_line(data=CPINEWCARS, aes(x=date,y= (value/146)*100 ,color= "New Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,160), breaks = c(90,120,150), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Used Cars and Trucks Experienced Unprecedented Price Increases") +
  theme_apricitas + theme(legend.position = c(.60,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 80-(.3*80), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_Rent <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  geom_line(data=CPIRENT, aes(x=date,y= (calculations/100) ,color= "CPI Rent: Annual Percentage Growth"), size = 1.25) +
  geom_line(data=CPIORENT, aes(x=date,y= (calculations/100) ,color= "CPI Owner's Equivalent Rent: Annual Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Housing Price Growth Had Slowed, But is Rebounding") +
  theme_apricitas + theme(legend.position = c(.40,.30)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent: Annual Percentage Growth","CPI Owner's Equivalent Rent: Annual Percentage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0.00) +
  coord_cartesian(clip = "off")

CPI_Lodging_Graph <- ggplot() + #plotting lodging away from home
  geom_line(data=CPILODGINGHOME, aes(x=date,y= (value/162)*100 ,color= "CPI: Lodging Away From Home"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(85,115), breaks = c(90,100,110), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Lodging Away From Home (Hotels, Motels, etc) Have Bounced Back") +
  theme_apricitas + theme(legend.position = c(.60,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 85-(.3*30), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_PIPED_GAS <- ggplot() + #plotting piped gas price index
  geom_line(data=CPIPIPEDGAS, aes(x=date,y= (value/176)*100 ,color= "CPI: Utility (Piped) Gas Service in U.S. City Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(94,140), breaks = c(100,120,140), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("The Energy Crunch") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Utility Gas Service Have Jumped for the First Time in a Decade") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 94-(.3*46), ymax = 94) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_GAS <- ggplot() + #plotting gasoline price index against WTI prices
  geom_line(data=CPIGAS, aes(x=date,y= (value/213)*100 ,color= "CPI: Gasoline"), size = 1.25) +
  geom_line(data=WTI, aes(x=date,y= (value/46.3)*100 ,color= "Crude Oil (WTI)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,190), breaks = c(50,75,100,125,150,175), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("The Energy Crunch") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Omicron May Not Be Enough to Keep Gas Prices Down") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 50-(.3*140), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Personal Income and Outlays are on Trend, But Consumers Have Significant Excess Savings") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

#Saving png images of all graphs
ggsave(dpi = "retina",plot = CPI_Graph, "CPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = PCE_Inflation_Rates_Graph, "PCE Inflation.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPI_New_Used_Car_Vehicles_Graph, "CPI CARS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPI_Rent, "CPI Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_GAS, "CPI Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_PIPED_GAS, "CPI Piped Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Lodging_Graph, "CPI Lodging.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Durable_Services, "Durable Services.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5YIE, "Inflation Expectations.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Wage_Price_Graph, "Wage Price Spiral.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PCE_Graph, "PCE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

fredr_set_key("96786b5abc3a13aa838f928bf645a572") #setting FRED API key for data download. Normally I would set this in the R environment, but I am manually setting it here for ease of use 

#Downloading Data from FRED
CPIUSEDCARS <- fredr(series_id = "CUSR0000SETA02",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading Consumer Price Index (CPI) Used Cars and Trucks data 
CPINEWCARS <- fredr(series_id = "CUSR0000SETA01",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading CPI New Vehicles data 
CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading CPI Rent of Primary Residence data in percent change format
CPIOERENT <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading CPI Owners' Equivalent Rent of Residences data in percent change format
CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading CPI All Items data 
PCE <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading Personal Consumption Expenditures (PCE) data
PCEGD <- fredr(series_id = "DGDSRC1",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading PCE Goods data
PCESV <- fredr(series_id = "PCES",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading PCE Services data
PPIIDC <- fredr(series_id = "PPIIDC",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading Producer Price Index: Industrial Commodities data
NFC_PROFIT <- fredr(series_id = "A463RD3Q052SBEA",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading Profit per unit of Gross Value Added for Nonfinancial Corporations data
NFC_PRICE <- fredr(series_id = "A455RD3Q052SBEA",observation_start = as.Date("2019-01-01"),observation_end = as.Date("2021-10-30")) #downloading Price per unit of Gross Value Added for Nonfinancial Corporations data
ECISERV <- fredr(series_id = "CIS201S000000000I",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading Employment Cost Index (ECI) services data
ECIGOOD <- fredr(series_id = "CIU201G000000000I",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading ECI goods data
PCESERV <- fredr(series_id = "DSERRG3M086SBEA",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading PCE services
PCEGOOD <- fredr(series_id = "DGDSRG3M086SBEA",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading PCE goods
DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays


#manually adding 2% CPI growth trend for later chart on above-trend CPI
CPI$CPITREND <- c(seq(0,0,length.out = 13), 258.824*1.001652^(0:20)) #the sequence of zeroes is for the part of the chart where the trendline is excluded, and the second sequence is compounding CPI monthly at a 2% annual rate

#formatting data for the later chart on wage-price spirals
Wage_Price_Merge <- do.call("rbind", list(ECISERV,ECIGOOD,PCESERV,PCEGOOD)) #binding ECI and PCE data for service and goods sector
#renaming series IDs in the merged data set to plain language explanations
Wage_Price_Merge$series_id <- gsub("CIS201S000000000I","Services Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("CIU201G000000000I","Goods Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DSERRG3M086SBEA","Services Prices (PCE)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DGDSRG3M086SBEA","Goods Prices (PCE)",Wage_Price_Merge$series_id)

#formatting data for the later chart on corporate profit margins
PROFIT_MARGIN <- merge(NFC_PROFIT, NFC_PRICE, by = "date") #merging price and profit margin data per unit of gross value added for nonfinancial corporations
PROFIT_MARGIN <- subset(PROFIT_MARGIN, select = c("date","value.x","value.y")) #deleting unneeded variables from the data frame
colnames(PROFIT_MARGIN) <- c("date","NFC_PROFIT","NFC_PRICE") #renaming vairables for ease of use

#manually adding 4% personal income and outlays growth trend line for later chart on personal income and outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-10-01"), "months")), trend = 16622.8*1.003274^(0:21)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-10-01"), "months")), trend = 15328.8*1.003274^(0:21))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI_Graph <- ggplot() + #plotting CPI against 2% CPI trend
  geom_line(data=CPI, aes(x=date,y= (value/2.52) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPI, aes(x=date,y= CPITREND/2.52 ,color= "2% CPI Trend"), size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(limits = c(100,110), breaks = c(100,105,110), expand = c(0,0)) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "CPI is Way Above Trend") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= "January 2019 = 100",breaks = c("CPI","2% CPI Trend"),values = c("#FFE98F","#FFE98F","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 95-(.3*17), ymax = 95) +
  coord_cartesian(clip = "off")

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

PPI_Commodities <- ggplot() + #plotting PPI Industrial Commodities Index
  geom_line(data=PPIIDC, aes(x=date,y= (value/2) ,color= "PPI: Industrial Commodities"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,125), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  ylab("Producer Price Index: Jan 2019 = 100") +
  ggtitle("The Price of Production") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Industrial Commodities Have Shot Up in Price as the Demand for Production Inputs Grows") +
  theme_apricitas + theme(legend.position = c(.40,.40)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 90-(.3*35), ymax = 90) +
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
  geom_line(data=CPIRENT, aes(x=date,y= (value/100) ,color= "CPI Rent: Annual Percentage Growth"), size = 1.25) +
  geom_line(data=CPIOERENT, aes(x=date,y= (value/100) ,color= "CPI Owner's Equivalent Rent: Annual Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Housing Price Growth Had Slowed, But is Rebounding") +
  theme_apricitas + theme(legend.position = c(.40,.30)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent: Annual Percentage Growth","CPI Owner's Equivalent Rent: Annual Percentage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0.00) +
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

PROFIT_MARGIN_GRAPH <- ggplot() + #plotting corporate profit margins
  geom_line(data=PROFIT_MARGIN, aes(x=date,y= NFC_PROFIT/NFC_PRICE ,color= "Nonfinancial Corporate Business Profit Margins"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.11,.165), breaks = c(.11,.12,.13,.14,0.15,0.16), expand = c(0,0)) +
  ylab("Margin, %") +
  ggtitle("More Than Marginal Improvements") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "With Prices Rising Faster Than Wages, Corporate Profit Margins Have Jumped") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*900), xmax = as.Date("2019-01-01")-(0.049*900), ymin = .11-(.3*.055), ymax = .11) +
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
ggsave(dpi = "retina",plot = CPI_New_Used_Car_Vehicles_Graph, "CPI CARS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPI_Graph, "CPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPI_Rent, "CPI RENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Wage_Price_Graph, "Wage Price Spiral.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = PCE_Graph, "PCE Trend.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = PPI_Commodities, "PPI Commodities.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = PROFIT_MARGIN_GRAPH, "Profit Margins.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

p_unload(all)  # Remove all packages using tha package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

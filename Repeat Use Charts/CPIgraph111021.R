pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

CPIUSEDCARS <- fredr(series_id = "CUSR0000SETA02",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data
CPINEWCARS <- fredr(series_id = "CUSR0000SETA01",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #u1Rate Extended Unemployment Data
CPIOERENT <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #u1Rate Extended Unemployment Data
CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
PCE <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data
PCEGD <- fredr(series_id = "DGDSRC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
PCESV <- fredr(series_id = "PCES",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data
PPIIDC <- fredr(series_id = "PPIIDC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data
PROFIT_MARGIN <- fredr(series_id = "A463RD3Q052SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data


ECISERV <- fredr(series_id = "CIS201S000000000I",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI services
ECIGOOD <- fredr(series_id = "CIU201G000000000I",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI goods
PCESERV <- fredr(series_id = "DSERRG3M086SBEA",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI goods
PCEGOOD <- fredr(series_id = "DGDSRG3M086SBEA",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI goods

#add obersvation_ends before sending data for application

Wage_Price_Merge <- do.call("rbind", list(ECISERV,ECIGOOD,PCESERV,PCEGOOD))
Wage_Price_Merge$series_id <- gsub("CIS201S000000000I","Services Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("CIU201G000000000I","Goods Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DSERRG3M086SBEA","Services Prices (PCE)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DGDSRG3M086SBEA","Goods Prices (PCE)",Wage_Price_Merge$series_id)

#manually adding CPI trend
CPI$CPITREND <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,102.7079365,102.8791506,103.0506502,103.2224356,103.3945074,103.5668661,103.739512,103.9124458,104.0856678,104.2591786,104.4329787,104.6070685,104.7814485,104.9561191,105.131081,105.3063345,105.4818802,105.6577184,105.8338499,106.0102749,106.186994)#,106.3640077,106.5413165,106.7189209,106.8968214,107.0750184,107.2535124,107.432304,107.6113937)

#MANUAL EDITOR - DO NOT USE UNLESS FRED DOES NOT UPDATE
#Remember to update the date each time!
CPI[nrow(EPop) + 1,] = list(as.Date("2021-09-01"),"X", 25, as.Date("2021-10-07"),as.Date("2021-10-07"))
CPIOERENT[nrow(LAH) + 1,] = list(as.Date("2021-09-01"),"X", 25, as.Date("2021-10-07"),as.Date("2021-10-07"))
CPIRENT[nrow(U1RATE) + 1,] = list(as.Date("2021-09-01"),"X", 25, as.Date("2021-10-07"),as.Date("2021-10-07"))

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI_New_Used_Car_Vehicles_Graph <- ggplot() + #plotting Used Cars and Truck Price Indexes
  geom_line(data=CPIUSEDCARS, aes(x=date,y= (value/141)*100 ,color= "Used Cars and Trucks"), size = 1.25) +
  geom_line(data=CPINEWCARS, aes(x=date,y= (value/146)*100 ,color= "New Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,160), breaks = c(90,120,150), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Used Cars and Trucks Experienced Unprecedented Price Increases") +
  theme_apricitas + theme(legend.position = c(.60,.70)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 80-(.3*80), ymax = 80) +
  coord_cartesian(clip = "off")

CPI_Rent <- ggplot() + #plotting Rent Price Growth
  geom_line(data=CPIRENT, aes(x=date,y= (value/100) ,color= "CPI Rent: Annual Percentage Growth"), size = 1.25) +
  geom_line(data=CPIOERENT, aes(x=date,y= (value/100) ,color= "CPI Owner's Equivalent Rent: Annual Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Housing Price Growth Had Slowed, But is Rebounding") +
  theme_apricitas + theme(legend.position = c(.40,.30)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent: Annual Percentage Growth","CPI Owner's Equivalent Rent: Annual Percentage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0.00) +
  coord_cartesian(clip = "off")

CPI_Graph <- ggplot() + #plotting CPI
  geom_line(data=CPI, aes(x=date,y= (value/2.52) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPI, aes(x=date,y= CPITREND ,color= "2% CPI Trend"), size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(limits = c(100,110), breaks = c(100,105,110), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "CPI is Way Above Trend") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= "January 2019 = 100",breaks = c("CPI","2% CPI Trend"),values = c("#FFE98F","#FFE98F","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 95-(.3*17), ymax = 95) +
  coord_cartesian(clip = "off")

Wage_Price_Graph <- ggplot() +
  geom_line(data = Wage_Price_Merge, aes(x=date, y = value/100, color = series_id, alpha = series_id), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.025,0.065), breaks = c(-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Spiral? Not so Fast!") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "Price Growth is in Goods, but Wage Growth is in Services") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  scale_alpha_manual(name= NULL,values = c(1,.5,1,.5)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = -.025-(.3*.09), ymax = -0.025) +
  coord_cartesian(clip = "off")

PCETREND_Graph <- ggplot() + #plotting CPI
  geom_line(data=PCE, aes(x=date,y= (value/141.04) ,color= "Total Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCEGD, aes(x=date,y= (value/43.46) ,color= "Goods Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCESV, aes(x=date,y= (value/97.73) ,color= "Services Personal Consumption Expenditures"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(83,130), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Personal Consumption Expenditures: January 2019 = 100") +
  ggtitle("Good to Go?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Goods Shot Up after the Pandemic Hit, but is Now Stalling") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 83-(.3*47), ymax = 83) +
  coord_cartesian(clip = "off")

PPI_Commodities <- ggplot() + #plotting PPI Commodities Index
  geom_line(data=PPIIDC, aes(x=date,y= (value/2) ,color= "PPI: Industrial Commodities"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,125), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Producer Price Index: Jan 2019 = 100") +
  ggtitle("The Price of Production") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Industrial Commodities Have Shot Up in Price as the Demand for Production Inputs Grows") +
  theme_apricitas + theme(legend.position = c(.40,.40)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 90-(.3*35), ymax = 90) +
  coord_cartesian(clip = "off")

PROFIT_MARGIN_GRAPH <- ggplot() + #plotting PPI Commodities Index
  geom_line(data=PROFIT_MARGIN, aes(x=date,y= value ,color= "Profit Per Unit of Real Gross Value Added: Nonfinancial Corporate Business"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.125,.185), breaks = c(.13,.14,0.15,0.16,0.17,0.18), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Margin, %") +
  ggtitle("More Than Marginal Improvements") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "With Prices Rising Faster Than Wages, Corporate Profit Margins Have Jumped") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*900), xmax = as.Date("2019-01-01")-(0.049*900), ymin = .125-(.3*.06), ymax = .125) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_New_Used_Car_Vehicles_Graph, "CPI CARS.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = CPI_Graph, "CPI.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = CPI_Rent, "CPI RENT.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Wage_Price_Graph, "Wage Price Spiral v2.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PCETREND_Graph, "PCE Trend.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PPI_Commodities, "PPI Commodities.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PROFIT_MARGIN_GRAPH, "Profit Margins.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

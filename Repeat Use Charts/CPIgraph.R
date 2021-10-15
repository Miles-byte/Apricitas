pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

CPIUSEDCARS <- fredr(series_id = "CUSR0000SETA02",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data
CPINEWCARS <- fredr(series_id = "CUSR0000SETA01",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #u1Rate Extended Unemployment Data
CPIOERENT <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #u1Rate Extended Unemployment Data
CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
#manually adding CPI trend
CPI$CPITREND <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,102.7079365,102.8791506,103.0506502,103.2224356,103.3945074,103.5668661,103.739512,103.9124458,104.0856678,104.2591786,104.4329787,104.6070685,104.7814485,104.9561191,105.131081,105.3063345,105.4818802,105.6577184,105.8338499)#,106.0102749,106.186994,106.3640077,106.5413165,106.7189209,106.8968214,107.0750184,107.2535124,107.432304,107.6113937)

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
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0.00) +
  coord_cartesian(clip = "off")

CPI <- ggplot() + #plotting CPI
  geom_line(data=CPI, aes(x=date,y= (value/2.52) ,color= "CPI: January 2019 = 100"), size = 1.25) +
  geom_line(data=CPI, aes(x=date,y= CPITREND ,color= "2% CPI Trend"), size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(limits = c(95,112), breaks = c(95,100,105,110), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "CPI is Above Trend, but Slowing Down") +
  theme_apricitas + theme(legend.position = c(.40,.30)) +
  scale_color_manual(name= NULL,breaks = c("CPI: January 2019 = 100","2% CPI Trend"),values = c("#FFE98F","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 95-(.3*17), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_New_Used_Car_Vehicles_Graph, "CPI CARS.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = CPI, "CPI.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = CPI_Rent, "CPI RENT.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

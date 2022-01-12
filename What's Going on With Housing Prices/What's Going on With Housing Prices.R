pacman::p_load(remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

ThirtyYearMortgage <- fredr(series_id = "MORTGAGE30US",observation_start = as.Date("2003-01-02"),realtime_start = NULL, realtime_end = NULL) #downloading thirty year mortgage rates
AllTransaction <- fredr(series_id = "USSTHPI",observation_start = as.Date("2003-01-02"),realtime_start = NULL, realtime_end = NULL) #downloading thirty year mortgage rates
CaseShiller <- fredr(series_id = "CSUSHPISA",observation_start = as.Date("2003-01-02"),realtime_start = NULL, realtime_end = NULL) #downloading thirty year mortgage rates

CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi Rent
CPIOERENT <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi OERent

Vacancy_Rate <- fredr(series_id = "RRVRUSQ156N",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi OERent

HousingInventory <- fredr(series_id = "ACTLISCOUUS",observation_start = as.Date("2016-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi OERent

HousingStarts <- fredr(series_id = "HOUST",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi OERent


ZORI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/What's%20Going%20on%20With%20Housing%20Prices/Metro_ZORI_AllHomesPlusMultifamily_SSA.csv")
AL_National <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/What's%20Going%20on%20With%20Housing%20Prices/Apartment_List_Rent_Estimates_National_2021_11.csv")
AL_MSA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/What's%20Going%20on%20With%20Housing%20Prices/Apartment_List_Rent_Estimates_CBSA_2021_11.csv")


ZORI <- transpose(ZORI)
colnames(ZORI) <- ZORI[2,]
ZORI <- ZORI[-1:-3,]
ZORI <- as.data.frame(lapply(ZORI,as.numeric))
ZORI$date <- seq(as.Date("2014-01-01"), as.Date("2021-10-01"), "months")

AL_National <- transpose(AL_National)
AL_National <- data.frame(date = seq(as.Date("2017-01-01"), as.Date("2021-11-01"), "months"),national = AL_National[-1:-4,-2:-6])
AL_National$national <- as.numeric(AL_National$national)

AL_MSA <- subset(AL_MSA, AL_MSA$Bedroom_Size == "_Overall")
AL_MSA <- subset(AL_MSA, AL_MSA$CBSA_Name == "New York-Newark-Jersey City, NY-NJ-PA"| AL_MSA$CBSA_Name == "Atlanta-Sandy Springs-Alpharetta, GA")
AL_MSA <- AL_MSA[,-2:-4]
AL_MSA <- transpose(AL_MSA)
colnames(AL_MSA) <- AL_MSA[1,]
AL_MSA <- data.frame(sapply(AL_MSA, as.numeric))
AL_MSA <- as.data.frame(AL_MSA[-1,])
AL_MSA$date <- seq(as.Date("2017-01-01"), as.Date("2021-11-01"), "months")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Mortgage_Graph <- ggplot() + #plotting Mortgage Rates
  geom_line(data=ThirtyYearMortgage, aes(x=date,y= value/100 ,color= "30-Year Fixed Rate Mortgage Average in the United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07), expand = c(0,0)) +
  ylab("Mortgage Rate, %") +
  ggtitle("Lower For Longer") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Mortgage Rates Are at Historic Lows, both in Nominal and Real Terms") +
  theme_apricitas + theme(legend.position = c(.40,.45)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6918), xmax = as.Date("2003-01-01")-(0.049*6918), ymin = 0-(.3*.07), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Home_Price_Graph <- ggplot() + #plotting Home Prices
  geom_line(data=CaseShiller, aes(x=date,y= value/1.28 ,color= "S&P/Case-Shiller U.S. National Home Price Index"), size = 1.25) +
  geom_line(data=AllTransaction, aes(x=date,y= value/2.80 ,color= "All-Transactions House Price Index for the United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(75,230), breaks = c(75,100,125,150,175,200,225), expand = c(0,0)) +
  ylab("Index, 2003=100") +
  ggtitle("The Home Price Boom") +
  labs(caption = "Graph created by @JosephPolitano using S&P and Census data",subtitle = "Home Prices Took Off Like A Rocket When the Pandemic Started") +
  theme_apricitas + theme(legend.position = c(.40,.75)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6918), xmax = as.Date("2003-01-01")-(0.049*6918), ymin = 75-(.3*155), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

RENT_TREND <- data.frame(date = c(seq(as.Date("2020-03-01"), as.Date("2021-11-01"), "months")), trend = 100*1.003073^(0:20))

Rent_Price_Graph <- ggplot() + #plotting Home Prices
  geom_line(data=CPIRENT, aes(x=date,y= value/3.39 ,color= "CPI Rent of Primary Residence"), size = 1.25) +
  geom_line(data=CPIOERENT, aes(x=date,y= value/3.32 ,color= "CPI Owner's Equivalent Rent"), size = 1.25) +
  geom_line(data=RENT_TREND, aes(x=date,y= trend ,color= "3.75% Annual Pre-COVID Growth Trend"), size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(limits = c(95,107.5), breaks = c(100,105,110), expand = c(0,0)) +
  ylab("Index, March 2020=100") +
  ggtitle("A Rental Slump?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Official Measures Show Rent Growth Below-Trend") +
  theme_apricitas + theme(legend.position = c(.40,.75)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent of Primary Residence","CPI Owner's Equivalent Rent","3.75% Annual Pre-COVID Growth Trend"), guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 95-(.3*12.7), ymax = 95) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  theme(legend.key.width =  unit(.82, "cm")) +
  coord_cartesian(clip = "off")

Inventory_Graph <- ggplot() + #plotting Home Prices
  geom_line(data=HousingInventory, aes(x=date,y= value/1000000,color= "Housing Inventory: Active Listing Count in the United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "M"),limits = c(0.4,1.6), breaks = c(0.4,0.8,1.2,1.6), expand = c(0,0)) +
  ylab("Homes, Millions") +
  ggtitle("The Inventory Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Realtor.com data",subtitle = "Total Home Inventory Has Sunk to Record Lows") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-07-01")-(.1861*1642), xmax = as.Date("2016-07-01")-(0.049*1642), ymin = 0.4-(.3*1.2), ymax = 0.4) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  theme(legend.key.width =  unit(.82, "cm")) +
  coord_cartesian(clip = "off")

Vacancy_Graph <- ggplot() + #plotting Home Prices
  geom_line(data= Vacancy_Rate, aes(x=date,y= value/100,color= "Rental Vacancy Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.04,.12), breaks = c(0.04,0.08,.12), expand = c(0,0)) +
  ylab("Rental Vacancy Rate, Percent") +
  ggtitle("The Inventory Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Rental Vacancy Rates Have Sunk to Record Lows") +
  theme_apricitas + theme(legend.position = c(.50,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*5475), xmax = as.Date("2006-01-01")-(0.049*5475), ymin = 0.04-(.3*0.08), ymax = 0.04) + 
  coord_cartesian(clip = "off")


CPIRENT2017 <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi Rent
CPIRENT2017NYC <- fredr(series_id = "CUURA101SEHA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi Rent
CPIRENT2017ATL <- fredr(series_id = "CUURA319SEHA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #cpi Rent


Rent_Indexes_Graph <- ggplot() + #plotting Home Prices
  geom_line(data=ZORI, aes(x=date,y= United.States/15.27,color= "Zillow Observed Rent Index"), size = 1.25) +
  geom_line(data=CPIRENT2017, aes(x=date,y= value/3.03 ,color= "CPI Rent of Primary Residence"), size = 1.25) +
  geom_line(data=AL_National, aes(x=date,y= national/10.54 ,color= "Apartment List National Rent Report"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(90,130), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2017-01-01"),as.Date("2021-11-01"))) +
  ylab("Index, January 2017 = 100") +
  ggtitle("The Index Issue") +
  labs(caption = "Graph created by @JosephPolitano using Zillow, Apartment List, and BLS data",subtitle = "Official Rent Measures Have Diverged From Private Indexes Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.77)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent of Primary Residence","Zillow Observed Rent Index","Apartment List National Rent Report")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*1642), xmax = as.Date("2017-01-01")-(0.049*1642), ymin = 90-(.3*40), ymax = 90) + 
  coord_cartesian(clip = "off")

Rent_Indexes_Graph_NYC <- ggplot() + #plotting Home Prices
  geom_line(data=ZORI, aes(x=date,y= New.York..NY/26.22,color= "Zillow Observed Rent Index"), size = 1.25) +
  geom_line(data=CPIRENT2017NYC, aes(x=date,y= value/3.77 ,color= "CPI Rent of Primary Residence"), size = 1.25) +
  geom_line(data=AL_MSA, aes(x=date,y= New.York.Newark.Jersey.City..NY.NJ.PA/16.73 ,color= "Apartment List National Rent Report"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(90,130), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2017-01-01"),as.Date("2021-11-01"))) +
  ylab("Index, January 2017 = 100") +
  ggtitle("The Index Issue") +
  labs(caption = "Graph created by @JosephPolitano using Zillow, Apartment List, and BLS data",subtitle = "Official Rent Measures Have Diverged From Private Indexes Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.77)) +
  scale_color_manual(name= "New York-Newark-Jersey City CBSA" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("CPI Rent of Primary Residence","Zillow Observed Rent Index","Apartment List National Rent Report")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*1642), xmax = as.Date("2017-01-01")-(0.049*1642), ymin = 90-(.3*40), ymax = 90) + 
  coord_cartesian(clip = "off")

Rent_Indexes_Graph_ATL <- ggplot() + #plotting Home Prices
  geom_line(data=ZORI, aes(x=date,y= Atlanta..GA/12.59,color= "Zillow Observed Rent Index"), size = 1.25) +
  geom_line(data=CPIRENT2017ATL, aes(x=date,y= value/2.56 ,color= "CPI Rent of Primary Residence"), size = 1.25) +
  geom_line(data=AL_MSA, aes(x=date,y= Atlanta.Sandy.Springs.Alpharetta..GA/11.28 ,color= "Apartment List National Rent Report"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(90,150), breaks = c(90,110,130,150), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2017-01-01"),as.Date("2021-11-01"))) +
  ylab("Index, January 2017 = 100") +
  ggtitle("The Index Issue") +
  labs(caption = "Graph created by @JosephPolitano using Zillow, Apartment List, and BLS data",subtitle = "Official Rent Measures Have Diverged From Private Indexes Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.77)) +
  scale_color_manual(name= "Atlanta-Sandy Springs-Roswell CBSA" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("CPI Rent of Primary Residence","Zillow Observed Rent Index","Apartment List National Rent Report")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*1642), xmax = as.Date("2017-01-01")-(0.049*1642), ymin = 90-(.3*60), ymax = 90) + 
  coord_cartesian(clip = "off")

Starts_Graph <- ggplot() + #plotting Housing Starts
  geom_line(data=HousingStarts, aes(x=date,y= value/1000,color= "New Privately-Owned Housing Units Started"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "M"),limits = c(0.4,2.5), breaks = c(0.5,1,1.5,2,2.5), expand = c(0,0)) +
  ylab("Units, Millions") +
  ggtitle("Breaking New Ground") +
  labs(caption = "Graph created by @JosephPolitano using HUD data",subtitle = "Housing Starts Have Hit Their Highest Level Since the GFC") +
  theme_apricitas + theme(legend.position = c(.50,.77)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6918), xmax = as.Date("2003-01-01")-(0.049*6918), ymin = 0.4-(.3*2.1), ymax = 0.4) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = Mortgage_Graph, "Mortgage.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Rent_Price_Graph, "CPI Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Home_Price_Graph, "Home Price.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Inventory_Graph, "Inventory.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Vacancy_Graph, "Vacancy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Rent_Indexes_Graph, "National Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Rent_Indexes_Graph_NYC, "NYC Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Rent_Indexes_Graph_ATL, "ATL Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Starts_Graph, "Housing Starts.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

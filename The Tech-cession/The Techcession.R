pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FB <- tq_get("META",from = "2019-01-01")
AMZN <- tq_get("AMZN",from = "2019-01-01")
AAPL <- tq_get("AAPL",from = "2019-01-01")
NFLX <- tq_get("NFLX",from = "2019-01-01")
GOOG <- tq_get("GOOG",from = "2019-01-01")

#FAANG
FAANG_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=FB, aes(x=date,y= ((adjusted/1.3568)-100)/100,color= "Facebook/Meta"), size = 1.25)+ 
  geom_line(data=AMZN, aes(x=date,y= ((adjusted/.769565)-100)/100,color= "Amazon"), size = 1.25)+ 
  geom_line(data=AAPL, aes(x=date,y= ((adjusted/.3810514)-100)/100,color= "Apple"), size = 1.25)+ 
  geom_line(data=NFLX, aes(x=date,y= ((adjusted/2.6766)-100)/100,color= "Netflix"), size = 1.25)+ 
  geom_line(data=GOOG, aes(x=date,y= ((adjusted/.522925)-100)/100,color= "Google/Alphabet"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Total Return Since Jan 2019") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.40,0,.40,.80,1.2,1.6,2,2.4,2.8,3.2,3.6), limits = c(-.40,3.75), expand = c(0,0)) +
  ggtitle("De-FAANGed") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "FAANG Stocks (Excluding Apple) Have Tanked This Year") +
  theme_apricitas + theme(legend.position = c(.2,.64)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Facebook/Meta","Apple","Amazon","Netflix","Google/Alphabet")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.40-(.3*4.15), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FAANG_Graph, "FAANG Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Ecommerce Sales
ECOM_PCT <- fredr(series_id = "ECOMPCTSA",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

ECOM_PCT_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=ECOM_PCT, aes(x=date,y= value/100,color= "E-Commerce Retail Sales as a Percent of Total Sales"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Retail Sales") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10,0.15,.2), limits = c(0,.20), expand = c(0,0)) +
  ggtitle("Window Shopping") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "The Pandemic Supercharged E-Commerce's Share of Spending, but Then Growth Stagnated") +
  theme_apricitas + theme(legend.position = c(.4,.64)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECOM_PCT_Graph, "ECOM PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Ad Costs
ADS_PPI <- fredr(series_id = "WPU365",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL)

ADS_PPI_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=ADS_PPI, aes(x=date,y= value,color= "Internet Advertising, Exluding Those Sold by Print Publishers"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Retail Sales") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(50,60,70,80,90,100,110), limits = c(50,110), expand = c(0,0)) +
  ggtitle("Strong Impressions") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Ad Prices Have Been Increasing for the First Time in a Decade") +
  theme_apricitas + theme(legend.position = c(.6,.84)) +
  scale_color_manual(name= "Producer Price Index",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 50-(.3*60), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ADS_PPI_Graph, "ADS PPI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Tech Employment
DATA_PROCESSING <- bls_api("CES5051800001", startyear = 2005, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SOFTWARE_PUBLISHERS <- bls_api("CES5051120001", startyear = 2005, registrationKey = "BLS_KEY") %>% #software employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
INTERNET_PUBLISHERS <- bls_api("CES5051913001", startyear = 2005, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

TECH_EMPLOYMENT_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=DATA_PROCESSING, aes(x=date,y= value,color= "Data Processing, Hosting, and Related Activities"), size = 1.25)+ 
  geom_line(data=SOFTWARE_PUBLISHERS, aes(x=date,y= value,color= "Software Publishers"), size = 1.25) + 
  geom_line(data=INTERNET_PUBLISHERS, aes(x=date,y= value,color= "Internet Publishing, Broadcasting, and Web Search Portals"), size = 1.25) + 
  xlab("Date") +
  ylab("Initial Claims") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,650), expand = c(0,0)) +
  ggtitle("Tech-Cession?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment is Still Rising in Key Digital Tech Sectors") +
  theme_apricitas + theme(legend.position = c(.38,.85)) +
  scale_color_manual(name= "All Employees",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Software Publishers","Data Processing, Hosting, and Related Activities","Internet Publishing, Broadcasting, and Web Search Portals")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOYMENT_Graph, "Tech Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#
DATA_AVERAGE_COMPENSATION <- bls_api("ENUUS000505518", startyear = 2007, registrationKey = "BLS_KEY") %>% #data processing compensation
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SOFTWARE_AVERAGE_COMPENSATION <- bls_api("ENUUS0005055112", startyear = 2007, registrationKey = "BLS_KEY") %>% #software compensation
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
INTERNET_PUBLISHER_COMPENSATION <- bls_api("ENUUS00050551913", startyear = 2007, registrationKey = "BLS_KEY") %>% #internet compensation
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

TECH_AVERAGE_COMPENSATION_GRAPH <- ggplot() + #plotting bank consumer loan data
  geom_line(data=DATA_AVERAGE_COMPENSATION, aes(x=year,y= value/1000,color= "Data Processing, Hosting, and Related Activities"), size = 1.25) + 
  geom_line(data=SOFTWARE_AVERAGE_COMPENSATION, aes(x=year,y= value/1000,color= "Software Publishers"), size = 1.25) + 
  geom_line(data=INTERNET_PUBLISHER_COMPENSATION, aes(x=year,y= value/1000,color= "Internet Publishing, Broadcasting, and Web Search Portals"), size = 1.25) + 
  xlab("Date") +
  ylab("Average Compensation, Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200,300), limits = c(0,350), expand = c(0,0)) +
  ggtitle("Vested In") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Compensation Growth Was Rapid in 2020-2021 as the Tech Boom Boosted Stock Options") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= "Average Annual Compensation",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 2007-(.1861*(2021-2007)), xmax = 2007-(0.049*(2021-2007)), ymin = 0-(.3*350), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_AVERAGE_COMPENSATION_GRAPH, "Tech Average Compensation.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#US Semiconductor production
US_Semiconductor_Production <- fredr(series_id = "IPB53122S", observation_start = as.Date("2000-01-01"))

US_Semiconductor_Production_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=US_Semiconductor_Production, aes(x=date,y= value,color= "Semiconductors, Printed Circuit Boards, and Other Electronic Components"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Retail Sales") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,20,40,60,80,100,120,140,160), limits = c(0,160), expand = c(0,0)) +
  ggtitle("Demand Dropoffs") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "US Semiconductor Output is Dropping Again") +
  theme_apricitas + theme(legend.position = c(.475,.92)) +
  scale_color_manual(name= "Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*160), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Semiconductor_Production_Graph, "US Semiconductor Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#Information Layoffs
Information_Layoffs <- fredr(series_id = "JTU5100LDL", observation_start = as.Date("2019-01-01"))

Information_Sector_Layoffs <- ggplot() + #plotting total quits and layoffs
  geom_line(data=Information_Layoffs, aes(x=date,y= value,color= "Layoffs, Information Sector"), size = 1.25)+
  annotate(geom = "text", label = "Note: Discontinuity at March 2020, When Layoffs hit 190k", x = as.Date("2020-01-01"), y = 15, color ="white", size = 4, alpha = 0.75) +
  xlab("Date") +
  ylab("Thousands of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,30,60,90), limits = c(0,90), expand = c(0,0)) +
  ggtitle("The Tech Downturn") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Layoffs were Low,but Cuts at Facebook, Twitter, and Others Will Show up in Data Soon") +
  theme_apricitas + theme(legend.position = c(.30,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*90), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Information_Sector_Layoffs, "Information_Sector.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Tech Layoffs
TECH_LAYOFFS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Tech-cession/LAYOFFS_FYI.csv") %>%
  mutate(Date = as.Date(Date))

TECH_LAYOFFS_Graph <- ggplot() + #plotting total quits and layoffs
  geom_line(data=TECH_LAYOFFS, aes(x=Date,y= Layoffs/1000,color= "Layoffs, Tech Sector"), size = 1.25)+
  annotate(geom = "text", label = "Note: November Data as of 11/19", x = as.Date("2022-09-01"), y = 40, color ="white", size = 4, alpha = 0.75) +
  xlab("Date") +
  ylab("Thousands of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,10,20,30,40,50), limits = c(0,50), expand = c(0,0)) +
  ggtitle("The Tech Downturn") +
  labs(caption = "Graph created by @JosephPolitano using Layoffs.FYI data via @Roger_Lee", subtitle = "Tech Sector Layoffs Have Spiked This Month") +
  theme_apricitas + theme(legend.position = c(.30,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_LAYOFFS_Graph, "Tech Layoffs.png", type = "cairo-png") #cairo gets rid of anti aliasing



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
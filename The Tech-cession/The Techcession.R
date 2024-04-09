pacman::p_load(sf,censusapi,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)
install_github("mikeasilva/blsAPI")
library(blsAPI)

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
  geom_line(data=FB, aes(x=date,y= ((adjusted/adjusted[1])*100-100)/100,color= "Facebook/Meta"), size = 1.25)+ 
  geom_line(data=AMZN, aes(x=date,y= ((adjusted/adjusted[1])*100-100)/100,color= "Amazon"), size = 1.25)+ 
  geom_line(data=AAPL, aes(x=date,y= ((adjusted/adjusted[1])*100-100)/100,color= "Apple"), size = 1.25)+ 
  geom_line(data=NFLX, aes(x=date,y= ((adjusted/adjusted[1])*100-100)/100,color= "Netflix"), size = 1.25)+ 
  geom_line(data=GOOG, aes(x=date,y= ((adjusted/adjusted[1])*100-100)/100,color= "Google/Alphabet"), size = 1.25)+ 
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
SOFTWARE_PUBLISHERS <- bls_api("CES5051320001", startyear = 2005, registrationKey = "BLS_KEY") %>% #software employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SEARCH_PORTALS <- bls_api("CES5051929001", startyear = 2005, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
MEDIA_SOCIAL <- bls_api("CES5051620001", startyear = 2005, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


TECH_EMPLOYMENT_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=DATA_PROCESSING, aes(x=date,y= value,color= "Computing Infrastructure, Data Processing, Web Hosting, & Related"), size = 1.25)+ 
  geom_line(data=SOFTWARE_PUBLISHERS, aes(x=date,y= value,color= "Software Publishers"), size = 1.25) + 
  geom_line(data=SEARCH_PORTALS, aes(x=date,y= value,color= "Web Search Portals and All Other Information Services"), size = 1.25) + 
  geom_line(data=MEDIA_SOCIAL, aes(x=date,y= value,color= "Media Streaming Distribution Services, Social Networks, & Related"), size = 1.25) + 
  xlab("Date") +
  ylab("All Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,675), expand = c(0,0)) +
  ggtitle("Tech-Cession?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment is Stalling in Key Digital Tech Sectors") +
  theme_apricitas + theme(legend.position = c(.385,.835), legend.text = element_text(size = 13)) +
  scale_color_manual(name= "All Employees",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Software Publishers","Computing Infrastructure, Data Processing, Web Hosting, & Related","Media Streaming Distribution Services, Social Networks, & Related","Web Search Portals and All Other Information Services")) +
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
  ylab("Index, 2017 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,20,40,60,80,100,120,140,160), limits = c(0,160), expand = c(0,0)) +
  ggtitle("Demand Dropoffs") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "US Semiconductor Output is Dropping Again") +
  theme_apricitas + theme(legend.position = c(.475,.92)) +
  scale_color_manual(name= "Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*160), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Semiconductor_Production_Graph, "US Semiconductor Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Information Layoffs
Information_Layoffs <- fredr(series_id = "JTU5100LDL", observation_start = as.Date("2018-07-01"), frequency = "sa", aggregation_method = "sum")
Information_Quits <- fredr(series_id = "JTU5100QUL", observation_start = as.Date("2018-07-01"), frequency = "sa", aggregation_method = "sum")

Information_Sector_Layoffs <- ggplot() + #plotting total quits and layoffs
  geom_line(data=Information_Layoffs, aes(x=date+180,y= value,color= "Layoffs, Information Sector"), size = 1.25)+
  geom_line(data=Information_Quits, aes(x=date+180,y= value,color= "Quits, Information Sector"), size = 1.25)+
  xlab("Date") +
  ylab("Thousands of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,100,200,300,400,500), limits = c(0,500), expand = c(0,0)) +
  ggtitle("Understanding The Tech Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Layoffs Rose and Quits Declined as the Tech Labor Market Contracted") +
  theme_apricitas + theme(legend.position = c(.70,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*500), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Information_Sector_Layoffs, "Information_Sector.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Tech Layoffs
TECH_LAYOFFS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Tech-cession/LAYOFFS_FYI_0723.csv") %>%
  mutate(Date = as.Date(Date))

TECH_LAYOFFS_Graph <- ggplot(data = TECH_LAYOFFS, aes(x = Date, y = Layoffs/1000, fill = "Layoff Announcements, Tech Sector")) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Thousands of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0,100), expand = c(0,0)) +
  ggtitle("The End of the Tech-cession?") +
  labs(caption = "Graph created by @JosephPolitano using Layoffs.FYI data via @Roger_Lee", subtitle = "Tech Sector Layoffs Have Broadly Returned to 2022 Levels") +
  theme_apricitas + theme(legend.position = c(.30,.87)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*100), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_LAYOFFS_Graph, "Tech Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


INFORMATION_EXPANSION <- bls_api("BDS0000000000200050110002LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000200050110002LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

INFORMATION_OPENINGS <- bls_api("BDS0000000000200050110003LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000200050110003LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

INFORMATION_CONTRACTIONS <- bls_api("BDS0000000000200050110005LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000200050110005LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

INFORMATION_CLOSINGS <- bls_api("BDS0000000000200050110006LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000200050110006LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))


COMPUTING_INFRASTRUCTURE_EXPANSION <- bls_api("BDS0000000000300518110002LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300518110002LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

COMPUTING_INFRASTRUCTURE_OPENINGS <- bls_api("BDS0000000000300518110003LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300518110003LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

COMPUTING_INFRASTRUCTURE_CONTRACTIONS <- bls_api("BDS0000000000300518110005LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300518110005LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

COMPUTING_INFRASTRUCTURE_CLOSINGS <- bls_api("BDS0000000000300518110006LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300518110006LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))


WEB_SEARCH_EXPANSION <- bls_api("BDS0000000000300519110002LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300519110002LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

WEB_SEARCH_OPENINGS <- bls_api("BDS0000000000300519110003LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300519110003LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

WEB_SEARCH_CONTRACTIONS <- bls_api("BDS0000000000300519110005LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300519110005LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))

WEB_SEARCH_CLOSINGS <- bls_api("BDS0000000000300519110006LQ5", startyear = 1992, registrationKey = "BLS_KEY") %>%
  rbind(.,bls_api("BDS0000000000300519110006LQ5", startyear = 2012, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q%Y")))


Information_Gross_Expansions_Contractions <- ggplot() + #plotting total quits and layoffs
  geom_line(data=INFORMATION_CONTRACTIONS, aes(x=date,y= value,color= "Gross Job Losses From Establishment Contractions"), size = 1.25)+
  geom_line(data=INFORMATION_CLOSINGS, aes(x=date,y= value,color= "Gross Job Losses From Establishment Closures"), size = 1.25)+
  geom_line(data=INFORMATION_EXPANSION, aes(x=date,y= value,color= "Gross Job Gains From Establishment Expansions"), size = 1.25)+
  geom_line(data=INFORMATION_OPENINGS, aes(x=date,y= value,color= "Gross Job Gains From Establishment Openings"), size = 1.25)+
  annotate(geom = "text", label = "Note: Discontinuity at Q2 2020, When Gross Job Losses from Contractions hit 353k", x = as.Date("2008-01-01"), y = 12, color ="white", size = 4, alpha = 0.75) +
  xlab("Date") +
  ylab("Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,100,200,300,400,500), limits = c(0,300), expand = c(0,0)) +
  ggtitle("The Tech Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Gross Job Losses in Information Rose Above Gross Job Gains in Q4 2022") +
  theme_apricitas + theme(legend.position = c(.68,.83)) +
  scale_color_manual(name= "Quarterly Gross Employment Changes, Information Sector",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-07-01")-(.1861*(today()-as.Date("1992-07-01"))), xmax = as.Date("1992-07-01")-(0.049*(today()-as.Date("1992-07-01"))), ymin = 0-(.3*275), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Information_Gross_Expansions_Contractions, "Information Gross Expansions Contractions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


Computing_Infrastructure_Gross_Expansions_Contractions <- ggplot() + #plotting total quits and layoffs
  geom_line(data=COMPUTING_INFRASTRUCTURE_CONTRACTIONS, aes(x=date,y= value/1000,color= "Gross Job Losses From Establishment Contractions"), size = 1.25)+
  geom_line(data=COMPUTING_INFRASTRUCTURE_CLOSINGS, aes(x=date,y= value/1000,color= "Gross Job Losses From Establishment Closures"), size = 1.25)+
  geom_line(data=COMPUTING_INFRASTRUCTURE_EXPANSION, aes(x=date,y= value/1000,color= "Gross Job Gains From Establishment Expansions"), size = 1.25)+
  geom_line(data=COMPUTING_INFRASTRUCTURE_OPENINGS, aes(x=date,y= value/1000,color= "Gross Job Gains From Establishment Openings"), size = 1.25)+
  xlab("Date") +
  ylab("Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,25,50,75), limits = c(0,50), expand = c(0,0)) +
  ggtitle("The Tech Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Computing Infrastructure Gross Job Losses Rose to Meet Gross Job Gains in Q4 2022") +
  theme_apricitas + theme(legend.position = c(.7,.8)) +
  scale_color_manual(name= "Quarterly Gross Employment Changes\nComputing Infrastructure, Data Processing, & Web Hosting",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-07-01")-(.1861*(today()-as.Date("1992-07-01"))), xmax = as.Date("1992-07-01")-(0.049*(today()-as.Date("1992-07-01"))), ymin = 0-(.3*50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Computing_Infrastructure_Gross_Expansions_Contractions, "Computing Infrastructure Gross Expansions Contractions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Web_Search_Gross_Expansions_Contractions <- ggplot() + #plotting total quits and layoffs
  geom_line(data=WEB_SEARCH_CONTRACTIONS, aes(x=date,y= value/1000,color= "Gross Job Losses From Establishment Contractions"), size = 1.25)+
  geom_line(data=WEB_SEARCH_CLOSINGS, aes(x=date,y= value/1000,color= "Gross Job Losses From Establishment Closures"), size = 1.25)+
  geom_line(data=WEB_SEARCH_EXPANSION, aes(x=date,y= value/1000,color= "Gross Job Gains From Establishment Expansions"), size = 1.25)+
  geom_line(data=WEB_SEARCH_OPENINGS, aes(x=date,y= value/1000,color= "Gross Job Gains From Establishment Openings"), size = 1.25)+
  xlab("Date") +
  ylab("Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,27.5), expand = c(0,0)) +
  ggtitle("The Tech Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Web Search & Other Information Services Gross Job Losses Reached a Record High in Q4 2022") +
  theme_apricitas + theme(legend.position = c(.45,.8)) +
  scale_color_manual(name= "Quarterly Gross Employment Changes\nWeb Search Portals, Libraries, Archives, and Other Information Services",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-07-01")-(.1861*(today()-as.Date("1992-07-01"))), xmax = as.Date("1992-07-01")-(0.049*(today()-as.Date("1992-07-01"))), ymin = 0-(.3*27.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Web_Search_Gross_Expansions_Contractions, "Web Search Gross Expansions Contractions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

test <- listCensusApis()

QSS_Data <- getCensus(
  name = "timeseries/eits/qss",
  vars = c("data_type_code","category_code","cell_value", "seasonally_adj","time_slot_id"),
  time = paste("from 1990 to", format(Sys.Date(), "%Y")),
  seasonally_adj = "yes",
  data_type_code = "QREV"
) %>%
  filter(category_code %in% c("5112T","518T","5415T","519T")) %>%
  transmute(name = category_code, date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(cell_value)) %>%
  pivot_wider(values_from = value) %>%
  setNames(c("date","Software Publishers","Data Processing, Hosting, and Related Services","Computer Systems Design and Related Services","Web Search Portals, Libraries, Archives, and Other Information Services")) %>%
  arrange(date) %>%
  mutate(across(where(is.numeric), ~ (.x-lag(.x,4))/lag(.x,4))) %>%
  drop_na() %>%
  filter(date >= as.Date("2016-01-01"))

QSS_Data_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=QSS_Data, aes(x=date,y= `Web Search Portals, Libraries, Archives, and Other Information Services`,color= "Web Search Portals, Libraries, Archives,\nand Other Information Services"), size = 1.25)+ 
  geom_line(data=QSS_Data, aes(x=date,y= `Computer Systems Design and Related Services`,color= "Computer Systems Design and Related Services"), size = 1.25)+ 
  geom_line(data=QSS_Data, aes(x=date,y= `Software Publishers`,color= "Software Publishers"), size = 1.25)+ 
  geom_line(data=QSS_Data, aes(x=date,y= `Data Processing, Hosting, and Related Services`,color= "Data Processing, Hosting, and Related Services"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Revenue Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10,0.15,.2,.25,.3,.35,.40,.45), limits = c(0,.45), expand = c(0,0)) +
  ggtitle("Tech Sector Revenue Growth") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "Revenue Growth Has Reaccelerated Across Tech Sectors in 2023 After Slowing in 2022") +
  theme_apricitas + theme(legend.position = c(.32,.80)) +
  scale_color_manual(name= "Year-on-Year Revenue Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Data Processing, Hosting, and Related Services","Software Publishers","Computer Systems Design and Related Services","Web Search Portals, Libraries, Archives,\nand Other Information Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*.45), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QSS_Data_Graph, "QSS Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


QCEW_2021_Q4 <- blsQCEW('Industry', year='2021', quarter='4', industry='10') %>%
  filter(own_code == "0") %>%
  select(area_fips,total_qtrly_wages)

QCEW_2022_Q4 <- blsQCEW('Industry', year='2022', quarter='4', industry='10') %>%
  filter(own_code == "0") %>%
  select(area_fips,oty_total_qtrly_wages_chg,oty_total_qtrly_wages_pct_chg)

QCEW_MERGE <- merge(QCEW_2021_Q4,QCEW_2022_Q4, by = "area_fips") %>%
  transmute(fips = area_fips,value = oty_total_qtrly_wages_chg/total_qtrly_wages)


devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  mutate(fips = paste0(state_fips, "000"))

states <- left_join(states, QCEW_MERGE, by = "fips") %>%
  mutate(value_cut = cut(value, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+")))

# states <- get_urbn_map("states", sf = TRUE) %>%
#   st_as_sf()

QCEW_GROWTH_States <- ggplot() +
  geom_sf(data = states, aes(fill = value_cut), color = NA) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("Change in Aggregate Worker Compensation\n    Including Bonuses and Stock Options\n                    Q4 2021 to Q4 2022") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = QCEW_GROWTH_States, "QCEW Growth Counties.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

counties <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(fips = county_fips) %>%
  left_join(., QCEW_MERGE, by = "fips") %>%
  mutate(value_cut = cut(value, breaks = c(-Inf,-0.15,-0.02, 0, 0.01, 0.02, 0.03, Inf), labels = c("<-15%","-.15-0.02","-.02-0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  filter(state_abbv == "CA")

QCEW_GROWTH_California <- ggplot() +
  geom_sf(data = counties, aes(fill = value_cut), color = NA) +
  geom_sf(data = counties, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#E63946","#F44336","#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("< -15%","-15%--2%","-2%-0","0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("Change in Aggregate Worker Compensation\n    Including Bonuses and Stock Options\n                    Q4 2021 to Q4 2022") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Zillow data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))



US_PRIVATE_WAGES_GROWTH <- bls_api("ENUUS00030510", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()
CA_PRIVATE_WAGES_GROWTH <- bls_api("ENU0600030510", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()
SF_PRIVATE_WAGES_GROWTH <- bls_api("ENU0607530510", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()
SM_PRIVATE_WAGES_GROWTH <- bls_api("ENU0608130510", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()
SC_PRIVATE_WAGES_GROWTH <- bls_api("ENU0608530510", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()

QCEW_GROWTH_LINE_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=US_PRIVATE_WAGES_GROWTH, aes(x=date,y= value,color= "US Total"), size = 1.25)+ 
  geom_line(data=CA_PRIVATE_WAGES_GROWTH, aes(x=date,y= value,color= "California Total"), size = 1.25)+ 
  geom_line(data=SF_PRIVATE_WAGES_GROWTH, aes(x=date,y= value,color= "San Francisco County"), size = 1.25)+ 
  geom_line(data=SM_PRIVATE_WAGES_GROWTH, aes(x=date,y= value,color= "San Mateo County"), size = 1.25)+ 
  geom_line(data=SC_PRIVATE_WAGES_GROWTH, aes(x=date,y= value,color= "Santa Clara County"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Aggregate Compensation Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.20,0,0.20,0.40), limits = c(-.22,.40), expand = c(0,0)) +
  ggtitle("Aggregate Private-Sector Worker Compensation\n(Including Bonuses and Stock Options) Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Aggregate Compensation Growth is Now Deeply Negative in Much of the Bay Area") +
  theme_apricitas + theme(legend.position = c(.15,.86), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("US Total","California Total","San Francisco County","San Mateo County","Santa Clara County")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.22-(.3*.62), ymax = -0.22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QCEW_GROWTH_LINE_Graph, "QCEW Growth Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DATA_PROCESSING_WAGES_GROWTH <- bls_api("ENUUS000305518", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()

WEB_SEARCH_WAGES_GROWTH <- bls_api("ENUUS000305519", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()

CUSTOM_COMPUTER_PROGRAMMING_WAGES_GROWTH <- bls_api("ENUUS000305541511", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()

COMPUTER_SYSTEM_DESIGN_SERVICES_WAGES_GROWTH <- bls_api("ENUUS000305541512", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()

INFORMATION_WAGES_GROWTH <- bls_api("ENUUS00030551", startyear = 2017, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearqtr(paste(period, year), "Q%q %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value, 4))/lag(value,4)) %>%
  select(date, value) %>%
  drop_na()

QCEW_GROWTH_LINE_SECTOR_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=COMPUTER_SYSTEM_DESIGN_SERVICES_WAGES_GROWTH, aes(x=date,y= value,color= "Computer Systems Design Services"), size = 1.25)+ 
  geom_line(data=CUSTOM_COMPUTER_PROGRAMMING_WAGES_GROWTH, aes(x=date,y= value,color= "Custom Computer Programming Services"), size = 1.25)+ 
  geom_line(data=DATA_PROCESSING_WAGES_GROWTH, aes(x=date,y= value,color= "Computing Infrastructure, Data Processing,\nWeb Hosting, & Related"), size = 1.25)+ 
  geom_line(data=INFORMATION_WAGES_GROWTH, aes(x=date,y= value,color= "Information Total"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Aggregate Compensation Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.20,0,0.20,0.40), limits = c(-.22,.40), expand = c(0,0)) +
  ggtitle("Aggregate Private-Sector Worker Compensation\n(Including Bonuses and Stock Options) Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Compensation Growth Slowed Down Across Tech Sectors in 2022 after Booming During Early COVID") +
  theme_apricitas + theme(legend.position = c(.4,.2), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Information Total","Computer Systems Design Services","Custom Computer Programming Services","Computing Infrastructure, Data Processing,\nWeb Hosting, & Related")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.22-(.3*.62), ymax = -0.22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QCEW_GROWTH_LINE_SECTOR_Graph, "QCEW Growth Data Sector Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


RGDP_CA <- fredr(series_id = "CARQGSP", observation_start = as.Date("2015-01-01"), units = "pc1")
RGDP_US <- fredr(series_id = "GDPC1", observation_start = as.Date("2015-01-01"), units = "pc1")

CALIFORNIA_GDP_LINE_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=RGDP_US, aes(x=date,y= value/100,color= "United States"), size = 1.25)+ 
  geom_line(data=RGDP_CA, aes(x=date,y= value/100,color= "California"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Real GDP Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.10,-0.05,0,0.05,0.10,.15), limits = c(-.10,.16), expand = c(0,0)) +
  ggtitle("California and the Tech-cession") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "California GDP Growth Briefly Turned Negative in 2022") +
  theme_apricitas + theme(legend.position = c(.15,.86), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Real GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("United States","California")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.10-(.3*.26), ymax = -0.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CALIFORNIA_GDP_LINE_Graph, "CALIFORNIA GDP Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

NEWS_EMPLOYMENT <- data.frame(value = c(1, 7, 8, 6, 4, 12, 3, 12, 12, 12, 10, -4, 3, 6, 5, 8, 7, 7, 8, -1, 2, 7, -2, 10, 6, 1, -4, -61, -82, -62, -60, -58, -48, -44, -44, -47, -51, -32, -12, 11, 4, 5, 5, -4, -16, -8, -17, -18, -14, -15, -7, -4, -6, -6, -17, -12, -9, -9, -12, -11, -14, -16, -11, -21, -24, -9),
      date = seq(as.Date("2018-01-01"), length.out = 66, by = "month"))

NEWS_EMPLOYMENT_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=NEWS_EMPLOYMENT, aes(x=date,y= value/100,color= "Net Percent of People Hearing Good News About Employment\n(% Hearing About Lower Unemployment Minus % Hearing About Higher Unemployment)\nUMich Consumer Survey"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Real GDP Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.80,-.60,-.40,-0.20,0,.20,.40), limits = c(-.90,.40), expand = c(0,0)) +
  ggtitle("Unemployment Perception is Biased") +
  labs(caption = "Graph created by @JosephPolitano using UMich data", subtitle = "Americans Hear More News About Higher Unemployment Even as Unemployment Hits Record Lows") +
  theme_apricitas + theme(legend.position = c(.52,.92), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.90-(.3*1.1), ymax = -0.90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NEWS_EMPLOYMENT_Graph, "NEWS EMPLOYMENT GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

Information_Layoffs <- fredr(series_id = "JTU5100LDL", observation_start = as.Date("2018-07-01"), frequency = "sa", aggregation_method = "sum")
Business_Layoffs <- fredr(series_id = "JTU540099LDL", observation_start = as.Date("2018-07-01"), frequency = "sa", aggregation_method = "sum")
Total_Layoffs <- fredr(series_id = "JTULDL", observation_start = as.Date("2018-07-01"), frequency = "sa", aggregation_method = "sum")

Layoffs_Rbind <- rbind(Information_Layoffs,Business_Layoffs,Total_Layoffs) %>%
  select(date,series_id, value) %>%
  pivot_wider(names_from = series_id) %>%
  mutate(JTULDL = JTULDL-JTU5100LDL-JTU540099LDL) %>%
  setNames(c("date", "Information", "Professional and Business Services","All Other Sectors")) %>%
  pivot_longer(cols = Information:`All Other Sectors`) %>%
  mutate(value = if_else(date == as.Date("2020-01-01"), 0, value)) %>%
  mutate(name = factor(name,levels = c("All Other Sectors","Professional and Business Services","Information")))

TECH_LAYOFFS_Graph <- ggplot(data = Layoffs_Rbind, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  annotate(geom = "text", label = "*Note: Discontinuity at H1 2020, When Total Layoffs hit 30M", x = as.Date("2019-08-01"), y = 14, color ="white", size = 4, alpha = 0.75) +
  xlab("Date") +
  ylab("Tech Layoffs in Context") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,5,10,15), limits = c(0,15), expand = c(0,0)) +
  ggtitle("Tech Layoffs in Context") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Rising Layoffs in Information & Professional Services Have Had a Small Impact on Total Layoffs") +
  theme_apricitas + theme(legend.position = c(.76,.75)) +
  scale_fill_manual(name= "Layoffs and Discharges",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Information","Professional and Business Services","All Other Sectors")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-07-01")-(.1861*(today()-as.Date("2018-07-01"))), xmax = as.Date("2018-07-01")-(0.049*(today()-as.Date("2018-07-01"))), ymin = 0-(.3*15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_LAYOFFS_Graph, "Tech Layoffs Stacked.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

# AREA <- blsQCEW('Area', year='2018', quarter='1', area = "01000")
# 
# SOFTWARE_PUBLISHERS_2021 <- blsQCEW('Industry', year='2021', quarter='4', industry='5112')
# SOFTWARE_PUBLISHERS_2022 <- blsQCEW('Industry', year='2022', quarter='4', industry='5132')
# 
# CUSTOM_COMPUTER_PROGRAMMING_2021 <- blsQCEW('Industry', year='2021', quarter='4', industry='541511')
# CUSTOM_COMPUTER_PROGRAMMING_2022 <- blsQCEW('Industry', year='2022', quarter='4', industry='541511')
# 
# COMPUTING_INFRASTRUCTURE_2021 <- blsQCEW('Industry', year='2021', quarter='4', industry='541512')
# COMPUTING_INFRASTRUCTURE_2022 <- blsQCEW('Industry', year='2022', quarter='4', industry='541512')
# 
# COMPUTER_SYSTEMS_DESIGN_2021 <- blsQCEW('Industry', year='2021', quarter='4', industry='518')
# COMPUTER_SYSTEMS_DESIGN_2022 <- blsQCEW('Industry', year='2022', quarter='4', industry='518')
# 
# WEB_SEARCH_2021 <- blsQCEW('Industry', year='2021', quarter='4', industry='519')
# WEB_SEARCH_2022 <- blsQCEW('Industry', year='2022', quarter='4', industry='519')
# 
# STREAMING_2021 <- blsQCEW('Industry', year='2022', quarter='4', industry='5162')
# STREAMING_2022 <- blsQCEW('Industry', year='2022', quarter='4', industry='5162')

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
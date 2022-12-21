pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

NTRR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/NTRR/NTRR.csv") %>%
  mutate(date = as.Date(date))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2005-04-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") # CPI rent
#NTRR CPI
NTRR_Graph <- ggplot() + #plotting NTRR & ATRR
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= NTRR/100,color= "New Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= ATRR/100,color= "All Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=subset(CPIRENT,date > as.Date("2005-01-01")), aes(x=date,y= value/100,color= "CPI: Rent of Primary Residence"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,.125), breaks = c(-0.05,0,.05,0.1), expand = c(0,0)) +
  ggtitle("The New Key Inflation Indicator") +
  labs(caption = "Graph created by @JosephPolitano using Cleveland Fed & BLS data via Adams, Loewenstein, Montag, and Vebrugge (2022)", subtitle = "Rent Inflation for New Tenants is High But Rapidly Declining") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("New Tenant Repeat Rent Index","All Tenant Repeat Rent Index","CPI: Rent of Primary Residence")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-04-01")-(.1861*(today()-as.Date("2005-04-01"))), xmax = as.Date("2005-04-01")-(0.049*(today()-as.Date("2005-04-01"))), ymin = -0.05-(.3*.175), ymax = -.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NTRR_Graph, "NTRR Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#NTRR ZORI and Apartment List

ZORI <- read.csv("https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_month.csv?t=1665666510") %>%
  select(-RegionID, -SizeRank) %>%
  subset(RegionName == "United States") %>%
  transpose() %>%
  `colnames<-`(.[1, ]) %>%
  mutate(date = c(seq(as.Date("2014-12-01"), as.Date("2022-11-01"), "months"))) %>%
  .[-1, ] %>%
  mutate(`United States` = as.numeric(`United States`)) %>%
  mutate(`United States` = (`United States`-lag(`United States`,12))/lag(`United States`,12))

ApartmentList <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/091322/apartmentlist.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

CPI_Rent_Zillow <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(CPIRENT,date > as.Date("2019-01-01")), aes(x=date,y= (value/100) ,color= "CPI: Rent of Primary Residence"), size = 1.25) +
  geom_line(data=subset(ZORI, date > as.Date("2018-03-01")), aes(x=date+365,y= (`United States`) ,color= "Zillow Observed Rent Index, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(ApartmentList, date > as.Date("2018-03-01")), aes(x=date+365,y= annualpct ,color= "ApartmentList Median New Lease, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(NTRR,date > as.Date("2018-01-01")), aes(x=date+365,y= NTRR/100,color= "New Tenant Repeat Rent Index, Lagged 1 Year"), size = 1.25)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,.20), breaks = c(0,.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS, Zillow, and ApartmentList data",subtitle = "When Rent Growth Actually Peaks Will Be Critical to Future Inflation Prints") +
  theme_apricitas + theme(legend.position = c(.35,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI: Rent of Primary Residence","New Tenant Repeat Rent Index, Lagged 1 Year","Zillow Observed Rent Index, Lagged 1 Year","ApartmentList Median New Lease, Lagged 1 Year")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+365-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+365-as.Date("2019-01-01"))), ymin = -0.025-(.3*0.225), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Rent_Zillow, "CPI Rent Zillow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#GLI YoY growth
GLI_CES <- fredr(series_id = "CES0500000017",observation_start = as.Date("2005-04-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1", frequency = "q") # CPI rent
ELEV2554 <- fredr(series_id = "LNS12000060",observation_start = as.Date("2004-04-01"),realtime_start = NULL, realtime_end = NULL, frequency = "q", aggregation_method = "avg") # CPI rent
ECIWAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2004-04-01"),realtime_start = NULL, realtime_end = NULL) # CPI rent

GLI_ECI <- merge(ELEV2554,ECIWAG, by = "date") %>%
  select(date, value.x, value.y) %>%
  mutate(pct = (value.x*value.y)) %>%
  mutate(pct = (pct-lag(pct, 4))/lag(pct,4))
  
GLI_NTRR <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= NTRR/100,color= "New Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=GLI_CES, aes(x=date+90,y= value/100,color= "Gross Labor Income, Nonfarm Payrolls"), size = 1.25)+ 
  geom_line(data=subset(GLI_ECI,date > as.Date("2005-01-01")), aes(x=date+90,y= pct,color= "Gross Labor Income, Prime Age Employment * ECI"), size = 1.25)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.1,.15), breaks = c(-.10,-0.05,0,0.05,0.10,0.15), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using Cleveland Fed & BLS data via Adams, Loewenstein, Montag, and Vebrugge (2022)",subtitle = "New Tenant Rents are Partially Driven by Gross Labor Income Growth") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Tenant Repeat Rent Index","Gross Labor Income, Nonfarm Payrolls","Gross Labor Income, Prime Age Employment * ECI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-04-01")-(.1861*(today()-as.Date("2005-04-01"))), xmax = as.Date("2005-04-01")-(0.049*(today()-as.Date("2005-04-01"))), ymin = -0.10-(.3*.25), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_NTRR, "GLI NTRR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
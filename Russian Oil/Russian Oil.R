pacman::p_load(eia,eurostat,restatapi,stringi,jsonlite,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EU_RU_CRUDE_REFINED_IMPORTS <- get_eurostat_data("NRG_TI_OILM",
                                       filters=c("EU27_2020","RU","O4100_TOT","O4300","O4600"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  subset(partner == "RU") %>%
  select(partner, time, values, siec) %>%
  pivot_wider(names_from = siec, values_from = values)

EU_RU_CRUDE_REFINED_IMPORTS_graph <- ggplot() + #plotting EU Crude Imports
  geom_line(data=EU_RU_CRUDE_REFINED_IMPORTS, aes(x=time,y= O4100_TOT/1000, color= "Crude Oil"), size = 1.25) +
  geom_line(data=EU_RU_CRUDE_REFINED_IMPORTS, aes(x=time,y= O4300/1000, color= "Refinery Feedstocks"), size = 1.25) +
  geom_line(data=EU_RU_CRUDE_REFINED_IMPORTS, aes(x=time,y= O4600/1000, color= "Refined Petroleum Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " Mt", accuracy = 1), limits = c(0,16),breaks = c(0,5,10,15), expand = c(0,0)) +
  ylab("Megatonnes") +
  ggtitle("Russia's Oil Trade With Europe") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Petroleum Imports from Russia Have Declined as Sanctions Take Effect") +
  theme_apricitas + theme(legend.position = c(.67,.85)) +
  scale_color_manual(name= "EU-27 Imports from Russia" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*16), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_RU_CRUDE_REFINED_IMPORTS_graph, "EU RU Crude Refined.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#EU Crude Import Data
EU_CRUDE_REFINED_IMPORTS_EURO <- get_eurostat_data("EXT_ST_EU27_2020SITC ",
                                                   filters=c("RU","TRD_VAL","SITC33","IMP","O4600"),
                                                   date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) 

EU_CRUDE_REFINED_IMPORTS_EURO_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=EU_CRUDE_REFINED_IMPORTS_EURO, aes(x=time,y= values/1000,color= "EU-27 Imports of Russian Petroleum, Petroleum Products, and Related Materials"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = "€"), limits = c(0,10), breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Russia's Oil Trade With Europe") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Imports of Russian Oil Have Decreased as Sanctions Start Taking Effect and Prices Decrease") +
  theme_apricitas + theme(legend.position = c(.51,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = EU_CRUDE_REFINED_IMPORTS_EURO_Graph, "EU RU Crude Refined Euros.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#RU Oil Graph
RU_OIL <- eia_series("INTL.57-1-RUS-TBPD.M", start = "2018", end = "2026") 
RU_OIL <- as.data.frame(RU_OIL$data)

RU_OIL_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=RU_OIL, aes(x=date, y = value/1000,color= "Russian Crude Oil (Including Lease Condensate) Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "Mbbl"), limits = c(8,12), breaks = c(8,9,10,11,12), expand = c(0,0)) +
  ylab("Millions of Barrels Per Day") +
  ggtitle("Russia's Crude Oil Production") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Russian Oil Production Has Only Declined by About 700kbpd Compared to Pre-Pandemic") +
  theme_apricitas + theme(legend.position = c(.51,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*4), ymax = 8) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_OIL_Graph, "Russian Oil Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#India China Energy Imports

INDIA_CHINA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russian%20Oil/Russian_Oil_Imports_China_Russia.csv") %>%
  mutate(date = as.Date(date))

INDIA_CHINA_RUSSIA_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=INDIA_CHINA, aes(x=date,y= India/1000,color= "India"), size = 1.25) +
  geom_line(data=INDIA_CHINA, aes(x=date,y= China/1000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,10), breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Where is Russian Energy Going?") +
  labs(caption = "Graph created by @JosephPolitano using Indian and Chinese Customs data",subtitle = "Indian and Chinese Imports of Russian Energy Have Shot Up This Year") +
  theme_apricitas + theme(legend.position = c(.51,.85)) +
  scale_color_manual(name= "Imports of Russian Oil, Gas, Coal and other Energy Commodities (HS Code 27)",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INDIA_CHINA_RUSSIA_Graph, "India China Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Value of Shipped Production Russia

RUSSIA_OIL_GAS_REV <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/Oil_Gas_Revenues.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Oil_Gas_Rev = gsub(",","",Oil_Gas_Rev)) %>%
  mutate(Oil_Gas_Rev = as.numeric(Oil_Gas_Rev))

RUSSIA_OIL_GAS_REV_graph <- ggplot() + #plotting russian gas data
  geom_line(data=RUSSIA_OIL_GAS_REV, aes(x=Date,y= Oil_Rev/1000000,color= "Oil"), size = 1.25)+ 
  geom_line(data=RUSSIA_OIL_GAS_REV, aes(x=Date,y= Gas_Rev/1000000,color= "Natural Gas"), size = 1.25)+ 
  geom_line(data=RUSSIA_OIL_GAS_REV, aes(x=Date,y= Products_Rev/1000000,color= "Oil Products"), size = 1.25)+ 
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Rubles") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, prefix = "₽", suffix = "T"), breaks = c(0,0.5,1,1.5,2,2.5), limits = c(0,2.25), expand = c(0,0)) +
  ggtitle("Cutting off Russian Revenue") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data", subtitle = "Ruble-Denominated Crude and Product Shipments Have Fallen Near Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.42,.90)) +
  scale_color_manual(name= "Volume of Shipped Production",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Oil","Oil Products","Natural Gas")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-02-01")-(.1861*(today()-as.Date("2017-02-01"))), xmax = as.Date("2017-02-01")-(0.049*(today()-as.Date("2017-02-01"))), ymin = 0-(.3*2.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_OIL_GAS_REV_graph, "Volume of Russian Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#urals brent
CRUDE_BRENT <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russian%20Oil/XBR_USD%20Historical%20Data.csv") %>%
  mutate(Date = as.Date(Date))

CRUDE_URALS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russian%20Oil/Crude%20Oil%20Urals%20Europe%20CFR%20Spot%20Historical%20Data.csv") %>%
  mutate(Date = as.Date(Date))

OIL_CRUDE_BRENT_URALS_graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=CRUDE_URALS, aes(x=Date,y= Price,color= "Urals Crude"), size = 1.25) +
  geom_line(data=CRUDE_BRENT, aes(x=Date,y= Price,color= "Brent Crude"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,130), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Dollars Per Barrel") +
  ggtitle("Cutting off Russian Revenue") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo Finance data",subtitle = "Russian Oil is Trading at a Significant Discount to International Market Prices") +
  theme_apricitas + theme(legend.position = c(.51,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*130), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = OIL_CRUDE_BRENT_URALS_graph, "Russia Brent Urals.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


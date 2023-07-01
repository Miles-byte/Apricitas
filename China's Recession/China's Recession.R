pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Using an updated version of the Chinese national stats bureau rstatscn package that fixes a json error in the old database
install_github("pcdi/rstatscn")
library(rstatscn)

statscnQueryZb(dbcode='hgyd', lang = "en") #lists all datasets with monthly national data
statscnQueryZb(dbcode='hgjd', lang = "en") #lists all datasets with quarterly national data
statscnQueryZb('A08',dbcode='hgyd', lang = "en")
statscnQueryZb('A01',dbcode='hgjd', lang = "en")
CPI <- statscnQueryData('A010301',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") #headline inflation data (A01)
#please note: the package is weird in that it will only let me retrieve a certain n of previous results, so I just used 60 here
statscnQueryLastN(60, lang = "en")

IND_PRO <- statscnQueryData('A020901',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb")

EMPLOY <- statscnQueryData('A0E01',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) 

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Employment Graph
statscnQueryLastN(60, lang = "en")

EMPLOY_Graph <- ggplot() + #plotting china unemployment rate
  geom_line(data=EMPLOY[2:( nrow(EMPLOY) - 1),], aes(x=date,y= `The Urban Surveyed Unemployment Rate`/100,color= "China Urban Surveyed Unemployment Rate"), size = 1.25)+ 
  geom_line(data=EMPLOY[2:( nrow(EMPLOY) - 1),], aes(x=date,y= `The Urban Surveyed Unemployment Rate of the Population Aged from 16 to 24`/100,color= "China Youth (16-24) Urban Surveyed Unemployment Rate"), size = 1.25)+ 
  xlab("Date") +
  ylab("Unemployment Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.20),breaks = c(0,0.05,0.1,0.15,0.20), expand = c(0,0)) +
  ggtitle("China's Employment Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "The Unemployment Rate—Especially for Young People—is Rising in China") +
  theme_apricitas + theme(legend.position = c(.37,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.20), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_Graph, "China Employ.png", type = "cairo-png") #cairo gets rid of anti aliasing
#index of services production
statscnQueryLastN(60, lang = "en")


ISP_INDEX <- statscnQueryData('A0501',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(date != as.Date("2019-01-01") & date !=as.Date("2018-01-01") & date !=as.Date("2020-01-01") & date !=as.Date("2021-01-01") & date !=as.Date("2022-01-01")) #jan data is often excluded because of Chinese New Year

ISP_Graph <- ggplot() + #plotting index of service production
  geom_line(data=ISP_INDEX[2:( nrow(ISP_INDEX) - 1),], aes(x=date,y= (`Index of Service Production(ISP), Growth Rate _(The same period last year=100)`)/100,color= "China Index of Service Production, Year-on-Year Growth"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Unemployment Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.125,.30),breaks = c(-.15,0,0.15,.30), expand = c(0,0)) +
  ggtitle("China's Service Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Service Output in China is Declining Again Amidst Lockdown") +
  theme_apricitas + theme(legend.position = c(.37,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-02-01")-(.1861*(today()-as.Date("2018-02-01"))), xmax = as.Date("2018-02-01")-(0.049*(today()-as.Date("2018-02-01"))), ymin = -.125-(.3*.425), ymax = -.125) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ISP_Graph, "China ISP.png", type = "cairo-png") #cairo gets rid of anti aliasing

#PMI
statscnQueryLastN(60, lang = "en")

PMI_MANUFACT <- statscnQueryData('A0B01',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.))))
PMI_SERVICES <- statscnQueryData('A0B02',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.))))
PMI_COMPREHENSIVE <- statscnQueryData('A0B03',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.))))

PMI_Graph <- ggplot() + #plotting index of service production
  geom_line(data=PMI_MANUFACT, aes(x=date,y=`Manufacturing Purchasing Managers' Index `,color= "Manufacturing"), size = 1.25)+ 
  geom_line(data=PMI_SERVICES, aes(x=date,y=`Non-Manufacturing Business Index `,color= "Services (Non-Manufacturing)"), size = 1.25)+ 
  geom_line(data=PMI_COMPREHENSIVE, aes(x=date,y=`Comprehensive PMI output index`,color= "Comprehensive"), size = 1.25)+ 
  annotate("hline", y = 50, yintercept = 50, color = "white", size = .5) +
  xlab("Date") +
  ylab("Diffusion Index, Monthly, 50+ Indicates Growth") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(25,60),breaks = c(30,40,50,60,70), expand = c(0,0)) +
  ggtitle("China's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "PMIs in China Have Turned Negative Again Amidst Intensifying Lockdowns") +
  theme_apricitas + theme(legend.position = c(.225,.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 25-(.3*35), ymax = 25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PMI_Graph, "PMI Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Domestic Retail Trade
statscnQueryLastN(10, lang = "en")

RETAIL_SALES <- statscnQueryData('A0701',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  .[order(nrow(.):1),] %>%
  subset(`Total Retail Sales of Consumer Goods, Current Period` != 0) %>%
  mutate(`Total Retail Sales of Consumer Goods, Current Period` = c(0,0,0,0,0,0,0,0,0,rollmean(`Total Retail Sales of Consumer Goods, Current Period`,10)))

RETAIL_SALES_graph <- ggplot() + #plotting index of service production
  geom_line(data=subset(RETAIL_SALES,`Total Retail Sales of Consumer Goods, Current Period` != 0), aes(x=date,y=`Total Retail Sales of Consumer Goods, Current Period`/10000,color= "China Retail Sales, Rolling 1-Year Average"), size = 1.25)+ 
  xlab("Date") +
  ylab("Trillions of Yuan, Monthly") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "¥", suffix = "T"),limits = c(1.5,4),breaks = c(2,3,4), expand = c(0,0)) +
  ggtitle("China's Malaise") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Retail Sales In China Have Stagnated Since the Start of COVID") +
  theme_apricitas + theme(legend.position = c(.375,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 1.5-(.3*2.5), ymax = 1.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_SALES_graph, "Retail Sales.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Passenger and Ton-KM
statscnQueryLastN(100, lang = "cn")

PASSENGER_KM <- statscnQueryData('A0904',dbcode='hgyd',lang = "cn", rowcode = "sj", colcode = "zb") %>%
  mutate(Passenger_km = 旅客周转量_当期值) %>%
  mutate(date = rownames(.)) %>%
  mutate(date = gsub("月", "",date)) %>%
  mutate(date = gsub("年", "-",date)) %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  subset(.,Passenger_km != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(Passenger_km = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(Passenger_km,12)))

TON_KM <- statscnQueryData('A0902',dbcode='hgyd',lang = "cn",rowcode = "sj", colcode = "zb") %>%
  mutate(Freight_tons_km = 货物周转量_当期值) %>%
  mutate(date = rownames(.)) %>%
  mutate(date = gsub("月", "",date)) %>%
  mutate(date = gsub("年", "-",date)) %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  subset(.,Freight_tons_km != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(Freight_tons_km = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(Freight_tons_km,12)))

PASSENGER_TON_KM_graph <- ggplot() + #plotting index of service production
  geom_line(data=subset(TON_KM,Freight_tons_km != 0), aes(x=date,y=Freight_tons_km/164.8884,color= "Freight Ton-kms, Rolling 1-Year Average"), size = 1.25)+ 
  geom_line(data=subset(PASSENGER_KM,Passenger_km != 0), aes(x=date,y=Passenger_km/28.82607,color= "Passenger-kms, Rolling 1-Year Average"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2019 = 100") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(35,120),breaks = c(40,60,80,100,120), expand = c(0,0)) +
  ggtitle("Only Half Locked") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Freight Travel Remains as Strong as Ever even as Passenger Travel Collapses") +
  theme_apricitas + theme(legend.position = c(.65,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 35-(.3*85), ymax = 35) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PASSENGER_TON_KM_graph, "Passenger Ton KM.png", type = "cairo-png") #cairo gets rid of anti aliasing

#GDP Growth
statscnQueryLastN(70, lang = "en")

RGDP_Growth <- statscnQueryData('A0103',dbcode='hgjd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = rownames(.)) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = as.Date(as.yearqtr(date,format="%qQ %Y"))) %>%
  mutate(`Indices of Gross Domestic Product (preceding _year=100), Current Quarter` = `Indices of Gross Domestic Product (preceding _year=100), Current Quarter`-100)
  
RGDP_Growth_Graph <- ggplot() + #plotting RGDP Growth
  geom_line(data=RGDP_Growth, aes(x=date,y= `Indices of Gross Domestic Product (preceding _year=100), Current Quarter`/100,color= "China, Year-on-Year Real GDP Growth"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("RGDP Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.075,.20),breaks = c(-0.05,0,0.05,0.1,0.15,0.20), expand = c(0,0)) +
  ggtitle("China's Malaise") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Chinese GDP Growth Continues to Slow Amidst Economic Woes") +
  theme_apricitas + theme(legend.position = c(.37,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -0.075-(.3*.275), ymax = -0.075) +
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = RGDP_Growth_Graph, "RGDP Growth Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Floor Space Started
statscnQueryLastN(60, lang = "en")

FLOOR_SPACE <- statscnQueryData('A0604',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = rownames(.)) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y")) %>%
  dplyr::select(date,year,`Floor Space of Real Estate Started This Year, _Accumulated`) %>%
  group_by(year) %>%
  mutate(monthly = `Floor Space of Real Estate Started This Year, _Accumulated`-lag(`Floor Space of Real Estate Started This Year, _Accumulated`)) %>%
  drop_na() %>%
  subset(`Floor Space of Real Estate Started This Year, _Accumulated` != 0)
  
FLOOR_SPACE_graph <- ggplot() + #plotting floor space
  geom_line(data=FLOOR_SPACE, aes(x=date,y=monthly/10000,color= "Chinese Construction: Total Floor Space Started this Month"), size = 1.25)+ 
  xlab("Date") +
  ylab("Square Meters") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B"),limits = c(0,3),breaks = c(0,1,2,3), expand = c(0,0)) +
  ggtitle("The Property Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Chinese New Construction Has Dropped by 50% Compared to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-02-01")-(.1861*(today()-as.Date("2018-02-01"))), xmax = as.Date("2018-02-01")-(0.049*(today()-as.Date("2018-02-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")  

ggsave(dpi = "retina",plot = FLOOR_SPACE_graph, "Floor Space.png", type = "cairo-png") #cairo gets rid of anti aliasing

#NEX Graph
statscnQueryLastN(60, lang = "en")

NEX <- statscnQueryData('A0801',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = rownames(.)) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(`Balance of Imports and Exports, Current Period`,12)))

NEX_graph <- ggplot() + #plotting index of service production
  #geom_line(data=subset(NEX,`Balance of Imports and Exports, Current Period` != 0), aes(x=date,y=`Balance of Imports and Exports, Current Period`/1000000,color= "China Net Exports"), size = 1.25)+ 
  geom_line(data=subset(NEX,rollmean != 0), aes(x=date,y=rollmean/1000000,color= "China Net Exports, Rolling 1-Year Monthly Average"), size = 1.25)+ 
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,90),breaks = c(0,25,50,75), expand = c(0,0)) +
  ggtitle("Solo Circulation") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "China's Net Exports Are Rising—Offsetting Weakness in Domestic Consumption") +
  theme_apricitas + theme(legend.position = c(.375,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*95), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NEX_graph, "NEX Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
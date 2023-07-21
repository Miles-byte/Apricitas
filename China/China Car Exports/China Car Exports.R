pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Using an updated version of the Chinese national stats bureau rstatscn package that fixes a json error in the old database
install_github("pcdi/rstatscn")
library(rstatscn)

#statscnQueryZb(dbcode='hgyd', lang = "en") #lists all datasets with monthly national data
#statscnQueryZb(dbcode='hgjd', lang = "en") #lists all datasets with quarterly national data
#statscnQueryZb('A08',dbcode='hgyd', lang = "en")
#statscnQueryZb('A01',dbcode='hgjd', lang = "en")
#CPI <- statscnQueryData('A010301',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") #headline inflation data (A01)
#please note: the package is weird in that it will only let me retrieve a certain n of previous results, so I just used 60 here
#statscnQueryLastN(60, lang = "en")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Domestic Retail Trade
statscnQueryLastN(10, lang = "en")

statscnQueryZb('A0704',dbcode='hgyd', lang = "en")

RETAIL_SALES_AUTO <- statscnQueryData('A07040F',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 
  
RETAIL_SALES_AUTO <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  .[order(nrow(.):1),] %>%
  subset(`Retail Sales of Enterprises above Designated Size, _Automobile, Current Period` != 0) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,rollmean(`Retail Sales of Enterprises above Designated Size, _Automobile, Current Period`,10))) %>%
  subset(date >= as.Date("2011-01-01"))

RETAIL_SALES_AUTO_graph <- ggplot() + #plotting index of service production
  geom_line(data=subset(RETAIL_SALES_AUTO,`Retail Sales of Enterprises above Designated Size, _Automobile, Current Period` != 0), aes(x=date,y=`Retail Sales of Enterprises above Designated Size, _Automobile, Current Period`/10,color= "Chinese Retail Sales, Automobiles"), size = 1.25)+ 
  geom_line(data=subset(RETAIL_SALES_AUTO,rollmean != 0), aes(x=date,y=rollmean/10,color= "1-Year Rolling Average"), size = 1.25)+ 
  xlab("Date") +
  ylab("Billions of Yuan, Monthly") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "¥", suffix = "B"),limits = c(000,520),breaks = c(0,50,100,150,200,250,300,350,400,450,500), expand = c(0,0)) +
  ggtitle("China's Domestic Auto Sales Slump") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Domestic Chinese Automobile Retail Sales Growth Has Stagnated Since the Start of COVID") +
  theme_apricitas + theme(legend.position = c(.5,.35)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9"), breaks = c("Chinese Retail Sales, Automobiles","1-Year Rolling Average")) +
  annotation_custom(apricitas_logo_rast, xmin = min(RETAIL_SALES_AUTO$date)-(.1861*(max(RETAIL_SALES_AUTO$date)-min(RETAIL_SALES_AUTO$date))), xmax = min(RETAIL_SALES_AUTO$date)-(0.049*(max(RETAIL_SALES_AUTO$date)-min(RETAIL_SALES_AUTO$date))), ymin = 0-(.3*500), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_SALES_AUTO_graph, "Retail Sales Auto.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Passenger and Ton-KM
statscnQueryLastN(500, lang = "cn")

PASSENGER_KM <- statscnQueryData('A0904',dbcode='hgyd',lang = "cn", rowcode = "sj", colcode = "zb") %>%
  mutate(Road_Passenger_km = 公路旅客周转量_当期值) %>%
  mutate(date = rownames(.)) %>%
  mutate(date = gsub("月", "",date)) %>%
  mutate(date = gsub("年", "-",date)) %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  subset(.,Road_Passenger_km != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(Passenger_km_roll = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(Road_Passenger_km,12)))

PASSENGER_KM <- statscnQueryLastN(500, lang = "cn") %>%
  mutate(Road_Passenger_km = 公路旅客周转量_当期值) %>%
  mutate(date = rownames(.)) %>%
  mutate(date = gsub("月", "",date)) %>%
  mutate(date = gsub("年", "-",date)) %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  subset(.,Road_Passenger_km != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(Passenger_km_roll = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(Road_Passenger_km,12)))

PASSENGER_KM_graph <- ggplot() + #plotting index of service production
  geom_line(data=subset(PASSENGER_KM,Road_Passenger_km != 0 & date>= as.Date("2015-01-01")), aes(x=date,y=Road_Passenger_km/7.748843,color= "China Highway Passenger-kms"), size = 1.25)+ 
  geom_line(data=subset(PASSENGER_KM,Passenger_km_roll != 0 & date>= as.Date("2015-01-01")), aes(x=date,y=Passenger_km_roll/7.748843,color= "Rolling 1-Year Average"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, 2019 Average = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,160),breaks = c(0,20,40,60,80,100,120,140,160), expand = c(0,0)) +
  ggtitle("China's Car Travel Crash") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China data", subtitle = "Chinese Highway Travel is Still Below 40% of Pre-Pandemic Averages") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*160), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PASSENGER_KM_graph, "Passenger KM.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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

statscnQueryZb('A0209',dbcode='hgyd', lang = "en")

IND_PRO_MV <- statscnQueryData('A020923',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_MV <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(date >= as.Date("1992-01-01")) %>%
  subset(.,`Output of Motor Vehicles, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Motor Vehicles, Current Period`,12)))
  
IND_PRO_MV_NEV <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= IND_PRO_MV, aes(x=date,y=`Output of Motor Vehicles, Current Period`/100 ,color= "Chinese Industrial Production of Motor Vehicles, Monthly"), size = 1.25) +
  geom_line(data= IND_PRO_MV, aes(x=date,y=rollmean/100,color= "Rolling 1-year Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,3.50), breaks = c(0,1.00,2.00,3.00,4.00), expand = c(0,0)) +
  ylab("Units, Monthly") +
  ggtitle("Chinese Motor Vehicle Production") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Motor Vehicle Production Remains Below Pre-Pandemic Averages Despite an Export Boom") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_MV$date)-(.1861*(max(IND_PRO_MV$date)-min(IND_PRO_MV$date))), xmax = min(IND_PRO_MV$date)-(0.049*(max(IND_PRO_MV$date)-min(IND_PRO_MV$date))), ymin = 0-(.3*3.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_MV_NEV, "China Ind Pro Car Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IND_PRO_NEV <- statscnQueryData('A02092W',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_NEV <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(date >= as.Date("1992-01-01")) %>%
  subset(.,`Output of New Energy Vehicles, Current Period` != 0)

IND_PRO_NEV_2022 <- subset(IND_PRO_NEV, date < as.Date("2024-01-01")) %>%
  mutate(date = as.Date(date) - years(1),
         `Output of New Energy Vehicles, Current Period` = `Output of New Energy Vehicles, Current Period` / ((`Output of New Energy Vehicles, Growth Rate (The same period last _year=100)`/ 100)+1),
         `Output of New Energy Vehicles, Growth Rate (The same period last _year=100)` = NA)

IND_PRO_NEV_RBIND <- rbind(IND_PRO_NEV,IND_PRO_NEV_2022)

IND_PRO_MV_NEV_merge <- merge(IND_PRO_MV,IND_PRO_NEV_RBIND, by = "date") %>%
  mutate(pct_NEV = `Output of New Energy Vehicles, Current Period`/`Output of Motor Vehicles, Current Period`)

IND_PRO_MV_NEV_SHARE_GRAPH <- ggplot() + #plotting Chinese Semiconductor Production
  geom_line(data=IND_PRO_MV_NEV_merge, aes(x=date,y= pct_NEV, color= "China, New Energy Vehicle Production as a Share of Total Motor Vehicle Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ylab("Percent of Total Vehicle Production") +
  ggtitle("The Chinese EV Revolution") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "New Energy Vehicle Production is Growing Rapidly—and Now Makes Up 1/3 of Total Vehicle Output") +
  theme_apricitas + theme(legend.position = c(.515,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_MV_NEV_merge$date)-(.1861*(max(IND_PRO_MV_NEV_merge$date)-min(IND_PRO_MV_NEV_merge$date))), xmax = min(IND_PRO_MV_NEV_merge$date)-(0.049*(max(IND_PRO_MV_NEV_merge$date)-min(IND_PRO_MV_NEV_merge$date))), ymin = 0-(.3*.50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_MV_NEV_SHARE_GRAPH, "China Ind Pro NEV Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


FIXED_INVESTMENT_CAR <- statscnQueryData('A0403',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.))))

FIXED_INVESTMENT_CAR <- statscnQueryLastN(100, lang = "en") %>%
  subset(`Investment in Fixed Assets, Manufacture of Cars, _Accumulated Growth Rate` != 0) %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  filter(month(.[1, 'date']) == month(.$date))

CHINA_FIXED_INVESTMENT_ELECTRONICS <- ggplot() + #plotting Chinese Semiconductor Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(FIXED_INVESTMENT_CAR,`Investment in Fixed Assets, Manufacture of Cars, _Accumulated Growth Rate` != 0), aes(x=date,y= `Investment in Fixed Assets, Manufacture of Cars, _Accumulated Growth Rate`/100, color= "Growth in Chinese Investment in Fixed Assets, Manufacture of Cars"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.075,.375), breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Chinese Electronic Investment") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "Chinese Electronic Manufacturing Investment Slowed as Software/Services Investment Rebounds") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= paste0("China, Investment in Fixed Assets, Jan to ", month(FIXED_INVESTMENT$date[1], label = TRUE, abbr = TRUE), ", Growth"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Manufacture of Communication Equipment, Computers, and Other Electronic Equipment","Information Transmission, Computer Services, and Software")) +
  annotation_custom(apricitas_logo_rast, xmin = min(FIXED_INVESTMENT$date)-(.1861*(max(FIXED_INVESTMENT$date)-min(FIXED_INVESTMENT$date))), xmax = min(FIXED_INVESTMENT$date)-(0.049*(max(FIXED_INVESTMENT$date)-min(FIXED_INVESTMENT$date))), ymin = -0.075-(.3*.45), ymax = -0.075) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_FIXED_INVESTMENT_ELECTRONICS, "China Fixed Investment Electronics Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
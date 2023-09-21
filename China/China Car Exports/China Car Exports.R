pacman::p_load(wiesbaden,keyring,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*160), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PASSENGER_KM_graph, "Passenger KM.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Ind Pro MV Graph
IND_PRO_MV <- statscnQueryData('A020923',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_MV <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(date >= as.Date("1992-01-01")) %>%
  subset(.,`Output of Motor Vehicles, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Motor Vehicles, Current Period`,11)))
  
IND_PRO_MV_GRAPH <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= IND_PRO_MV, aes(x=date,y=`Output of Motor Vehicles, Current Period`/100 ,color= "Chinese Industrial Production of Motor Vehicles, Monthly"), size = 1.25) +
  geom_line(data= filter(IND_PRO_MV, date > as.Date("1993-01-01")), aes(x=date,y=rollmean/100,color= "Rolling 1-year Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,3.50), breaks = c(0,1.00,2.00,3.00,4.00), expand = c(0,0)) +
  ylab("Units, Monthly") +
  ggtitle("Chinese Motor Vehicle Production") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Motor Vehicle Production Remains Below Pre-Pandemic Averages Despite an Export Boom") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_MV$date)-(.1861*(max(IND_PRO_MV$date)-min(IND_PRO_MV$date))), xmax = min(IND_PRO_MV$date)-(0.049*(max(IND_PRO_MV$date)-min(IND_PRO_MV$date))), ymin = 0-(.3*3.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_MV_GRAPH, "China Ind Pro Car Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IND_PRO_NEV <- statscnQueryData('A02092W',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_NEV <- statscnQueryLastN(100, lang = "en") %>%
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

CHINA_FIXED_INVESTMENT_CARS <- ggplot() + #plotting Chinese Semiconductor Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(FIXED_INVESTMENT_CAR,`Investment in Fixed Assets, Manufacture of Cars, _Accumulated Growth Rate` != 0), aes(x=date,y= `Investment in Fixed Assets, Manufacture of Cars, _Accumulated Growth Rate`/100, color= "Manufacturing of Cars"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.21,.21), breaks = c(-.2,-.1,0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("Growth, Year-on-Year") +
  ggtitle("Chinese Car Manufacturing Investment") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Car Manufacturing Investment is Rebounding From Massive Pandemic-era Contractions") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= paste0("China, Investment in Fixed Assets, Jan to ", month(FIXED_INVESTMENT_CAR$date[1], label = TRUE, abbr = TRUE), ", Growth"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(FIXED_INVESTMENT_CAR$date)-(.1861*(max(FIXED_INVESTMENT_CAR$date)-min(FIXED_INVESTMENT_CAR$date))), xmax = min(FIXED_INVESTMENT_CAR$date)-(0.049*(max(FIXED_INVESTMENT_CAR$date)-min(FIXED_INVESTMENT_CAR$date))), ymin = -0.21-(.3*.42), ymax = -0.21) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_FIXED_INVESTMENT_CARS, "China Fixed Investment Cars Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", 
                  "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                  "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", 
                  "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", 
                  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden","Czechia(Czech Republic)","Czechia(Czech Republic)","United Kingdom")

Asia_countries <- c("Afghanistan", "Armenia", "Azerbaijan", "Bangladesh", "Bhutan", 
                    "Brunei", "Cambodia", "China", "Georgia", "India", "Indonesia", 
                    "Japan", "Kazakhstan", "Kyrgyzstan", "Laos", "Malaysia", 
                    "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", 
                    "Pakistan", "Philippines","Singapore", "South Korea", 
                    "Sri Lanka", "Taiwan", "Tajikistan", "Thailand", "Turkmenistan", 
                    "Uzbekistan", "Vietnam","T\xa8\xb9rkiye","Hong Kong,China", "Taiwan,China","Lao","Macao,China","Republic of Korea","Viet Nam","Timor-Leste","Armenia(Before 2023)","Azerbai jan(Before 2023)")

Middle_East_countries <- c("Bahrain", "Cyprus", "Egypt", "Iran", "Iraq", "Israel", 
                           "Jordan", "Kuwait", "Lebanon", "Oman", "Palestine", 
                           "Qatar", "Saudi Arabia", "Syria", "Turkey", 
                           "United Arab Emirates", "Yemen")

Oceania_countries <- c("Australia", "Fiji", "Kiribati", "Marshall Islands", 
                       "Micronesia (Federated States of)", "Nauru", "New Zealand", 
                       "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", 
                       "Tonga", "Tuvalu", "Vanuatu", "Cook Islands", "Niue", 
                       "American Samoa", "French Polynesia", "Guam", "New Caledonia", 
                       "Northern Mariana Islands", "Pitcairn", "Tokelau", 
                       "Wallis and Futuna", "Easter Island", "Hawaiian Islands", 
                       "Norfolk Island", "Rotuma", "West Papua", "West New Guinea")

North_America_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                             "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", 
                             "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", 
                             "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", 
                             "Saint Lucia", "Saint Vincent and the Grenadines", 
                             "Trinidad and Tobago", "United States")



CHINA_IMPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/China/China%20Car%20Exports/CAR_IMPORTS_COUNTRY.csv") %>%
  transmute(date = as.Date(as.yearmon(as.character(Date.of.data), "%Y%m")), country = Trading.partner, imports= as.numeric(gsub(",","",US.dollar)))
  
CHINA_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/China/China%20Car%20Exports/CAR_EXPORTS_COUNTRY.csv") %>%
  transmute(date = as.Date(as.yearmon(as.character(Date.of.data), "%Y%m")), country = Trading.partner, exports= as.numeric(gsub(",","",US.dollar)))

CHINA_EV_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/China/China%20Car%20Exports/EV_EXPORTS_COUNTRY.csv") %>%
  subset(Commodity.code != 87036000) %>%
  transmute(date = as.Date(as.yearmon(as.character(Date.of.data), "%Y%m")), country = Trading.partner, exports= as.numeric(gsub(",","",US.dollar)))

CHINA_EXPORTS_SPLIT <- CHINA_EXPORTS %>%
  mutate(country = if_else(country %in% EU_countries, "European Union & United Kingdom", country)) %>%
  mutate(country = if_else(country %in% Asia_countries, "Asia Ex-Russia & Middle East", country)) %>%
  mutate(country = if_else(country %in% Middle_East_countries, "Middle East", country)) %>%
  mutate(country = if_else(country %in% Oceania_countries, "Oceania", country)) %>%
  mutate(country = if_else(country %in% North_America_countries, "North America", country)) %>%
  mutate(country = if_else(country %in% c("Russia", "European Union & United Kingdom","Middle East","Asia Ex-Russia & Middle East","Oceania","North America"), country, "All Other Countries")) %>%
  group_by(country, date) %>%
  summarize(exports = sum(exports, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(country = factor(country, levels = rev(c("European Union & United Kingdom","Russia","Middle East","Asia Ex-Russia & Middle East","North America","Oceania","All Other Countries"))))

CHINA_EXPORTS_SPLIT_GRAPH <- ggplot(data = CHINA_EXPORTS_SPLIT, aes(x = date, y = exports/1000000000, fill = country)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.5, suffix = "B", prefix = "$"), breaks = c(0,2.50,5.00,7.50), limits = c(0,7.50), expand = c(0,0)) +
  ggtitle("Chinese Passenger Vehicle Exports") +
  labs(caption = "Graph created by @JosephPolitano using GACC", subtitle = "Chinese Vehicle Exports Have Grown Globally, But Particularly in the EU and Russia") +
  theme_apricitas + theme(legend.position = c(.38,.7)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#3083DC","#FF8E72","#A7ACD9","#6A4C93"), breaks = c("European Union & United Kingdom","Russia","Middle East","Asia Ex-Russia & Middle East","Oceania","North America","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_EXPORTS_SPLIT_GRAPH, "CHINA EXPORTS SPLIT GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHINA_EV_EXPORTS_SPLIT <- CHINA_EV_EXPORTS %>%
  group_by(date) %>%
  summarize(exports = sum(exports, na.rm = TRUE)) %>%
  merge(.,CHINA_EXPORTS %>%
          group_by(date) %>%
          summarize(exports = sum(exports, na.rm = TRUE)), by = "date") %>%
  transmute(date, `Electric Vehicles` = exports.x, `Non-Electric Vehicles` = exports.y-exports.x) %>%
  pivot_longer(cols = `Electric Vehicles`:`Non-Electric Vehicles`) %>%
  mutate(name = factor(name, levels = rev(c("Electric Vehicles","Non-Electric Vehicles"))))
  
CHINA_EV_EXPORTS_SPLIT_GRAPH <- ggplot(data = CHINA_EV_EXPORTS_SPLIT, aes(x = date, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.5, suffix = "B", prefix = "$"), breaks = c(0,2.50,5.00,7.50), limits = c(0,7.50), expand = c(0,0)) +
  ggtitle("Chinese Passenger Vehicle Exports") +
  labs(caption = "Graph created by @JosephPolitano using GACC", subtitle = "Chinese Electric Vehicle Exports Have Surged Dramatically Over The Last 3 Years") +
  theme_apricitas + theme(legend.position = c(.38,.7)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#3083DC","#FF8E72","#A7ACD9","#6A4C93"), breaks = c("Electric Vehicles","Non-Electric Vehicles")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*7.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_EV_EXPORTS_SPLIT_GRAPH, "CHINA EV EXPORTS SPLIT GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RU_IND_PRO <- read.xlsx("https://rosstat.gov.ru/storage/mediabank/ind_baza_2018.xlsx", sheet = 2)

RU_IND_PRO_CAR <- RU_IND_PRO %>%
  subset(X2 == "29.1") %>%
  select(-К.содержанию,-X2) %>%
  transpose() %>%
  transmute(Monthly_growth = as.numeric(V1)) %>%
  mutate(Vehicle_IND_PRO = cumprod(1 + (Monthly_growth-100)/100)*(10000/Monthly_growth[1])) %>%
  select(Vehicle_IND_PRO) %>%
  ts(., start = c(2015,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2015-01-01"), by = "month", length = nrow(.)), value = x)

RU_IND_PRO_CAR_GRAPH <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= subset(RU_IND_PRO_CAR, date >= as.Date("2018-01-01")), aes(x=date,y=value/2,color= "Russian Industrial Production of Motor Vehicles"), size = 1.25) +
  geom_line(data= subset(CHINA_EXPORTS_SPLIT, country == "Russia"), aes(x=date,y=exports/exports[49]*100,color= "Chinese Exports of Motor Vehicles to Russia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,500), breaks = c(0,100,200,300,400,500), expand = c(0,0)) +
  ylab("Index, Jan 2022 = 100") +
  ggtitle("Russian Car Production & Trade") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data seasonally adjusted usint X-13ARIMA & GACC Data",subtitle = "Sanctions Crushed Russian Car Output, But Chinese Imports are Helping Make Up the Loss") +
  theme_apricitas + theme(legend.position = c(.415,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Russian Industrial Production of Motor Vehicles","Chinese Exports of Motor Vehicles to Russia")) +
  annotation_custom(apricitas_logo_rast, xmin = min(CHINA_EXPORTS_SPLIT$date)-(.1861*(max(CHINA_EXPORTS_SPLIT$date)-min(CHINA_EXPORTS_SPLIT$date))), xmax = min(CHINA_EXPORTS_SPLIT$date)-(0.049*(max(CHINA_EXPORTS_SPLIT$date)-min(CHINA_EXPORTS_SPLIT$date))), ymin = 0-(.3*500), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_IND_PRO_CAR_GRAPH, "RU Ind Pro Car Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHINA_NET_EXPORTS <- CHINA_IMPORTS %>%
  group_by(date) %>%
  summarize(imports = sum(imports, na.rm = TRUE)) %>%
  merge(.,CHINA_EXPORTS %>%
          group_by(date) %>%
          summarize(exports = sum(exports, na.rm = TRUE)), by = "date")

GER_EXP_list <- retrieve_datalist(tableseries = "51*", genesis=c(db='de'), language = "en")

test_login(genesis=c(db='de', user="DEM56460DY", password="XSYhTyP4JV4!Q4b"))
save_credentials(db= 'de',user="DEM56460DY", password="XSYhTyP4JV4!Q4b")

GERMANY_CAR_TRADE <- retrieve_data(tablename = "51000BM140", startyear = 2018, genesis=c(db='de'), language = "en")

GERMANY_CAR_NET_EXP <- GERMANY_CAR_TRADE %>%
  subset(WAM4 == "WA8703") %>%
  mutate(MONAT = gsub("MONAT","",MONAT)) %>%
  mutate(date = as.Date(paste0(JAHR,"-", MONAT,"-01"))) %>%
  transmute(date, exports = WERTAS_val, imports = WERTES_val)
  
JPN_EXPORT_DATA_2016_2020 <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003313967",
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = "70503010"
)

JPN_EXPORT_DATA_2021_Plus <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003425295",
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = "70503010"
)

JPN_IMPORT_DATA_2016_2020 <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003313968",
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = "70501010"
)

JPN_IMPORT_DATA_2021_Plus <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003425296",
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = "70501010"
)

JAPAN_EXPORT_CARS <- rbind(JPN_EXPORT_DATA_2016_2020,JPN_EXPORT_DATA_2021_Plus) %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Value", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Value-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Value-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

JAPAN_IMPORTS_CARS <- rbind(JPN_IMPORT_DATA_2016_2020,JPN_IMPORT_DATA_2021_Plus) %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Value", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Value-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Value-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), imports = x)

YEN_EXCHANGE_RATE <- fredr("EXJPUS", observation_start = as.Date("2016-01-01"))

JPN_CAR_NET_EXP <- merge(JAPAN_IMPORTS_CARS,JAPAN_EXPORT_CARS,by = "date") %>%
  merge(.,YEN_EXCHANGE_RATE, by = "date") %>%
  subset(date >= as.Date("2018-01-01"))

NET_EXPORTS_GRAPH <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data= CHINA_NET_EXPORTS, aes(x=date,y=(exports-imports)/1000000000,color= "China"), size = 1.25) +
  geom_line(data= GERMANY_CAR_NET_EXP, aes(x=date,y=(exports-imports)/1000000,color= "Germany"), size = 1.25) +
  geom_line(data= JPN_CAR_NET_EXP, aes(x=date,y=((exports-imports)/value)/1000000,color= "Japan"), size = 1.25) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.5, suffix = "B", prefix = "$"), breaks = c(-7.5,-5,-2.5,0,2.50,5.00,7.50,10), limits = c(-7.5,10), expand = c(0,0)) +
  ggtitle("Net Passenger Vehicle Exports") +
  labs(caption = "Graph created by @JosephPolitano using GACC, Destatis, and E-Stat Japan Data", subtitle = "New Chinese Vehicle Exports Have Risen Dramatically, But Are Still Below Germany/Japan") +
  theme_apricitas + theme(legend.position = c(.2,.6)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#3083DC","#FF8E72","#A7ACD9","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -7.5-(.3*17.5), ymax = -7.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

GROSS_EXPORTS_GRAPH <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data= CHINA_NET_EXPORTS, aes(x=date,y=(exports)/1000000000,color= "China"), size = 1.25) +
  geom_line(data= GERMANY_CAR_NET_EXP, aes(x=date,y=(exports)/1000000,color= "Germany"), size = 1.25) +
  geom_line(data= JPN_CAR_NET_EXP, aes(x=date,y=((exports)/value)/1000000,color= "Japan"), size = 1.25) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(-7.5,-5,-2.5,0,5.00,10,15,20), limits = c(0,20), expand = c(0,0)) +
  ggtitle("Gross Passenger Vehicle Exports") +
  labs(caption = "Graph created by @JosephPolitano using GACC, Destatis, and E-Stat Japan Data", subtitle = "New Chinese Vehicle Exports Have Risen Dramatically, But Are Still Below Germany/Japan") +
  theme_apricitas + theme(legend.position = c(.2,.6)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#3083DC","#FF8E72","#A7ACD9","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NET_EXPORTS_GRAPH, "Net Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GROSS_EXPORTS_GRAPH, "Gross Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(seasonal,eurostat,rsdmx,wiesbaden,keyring,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD")))
save_credentials(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD"))

IPMAN <- retrieve_data(tablename = "42153BM001", genesis=c(db='de'))

IPMAN_ENERGY <- IPMAN %>%
  subset(WZ08V1 %in% c("WZ08-C","WZ08-B-10")) %>% #taking manufacturing and energy intensive manufacturing data 
  subset(WERT03 == "X13JDKSB") %>%#calendar and seasonally adjusted
  mutate(MONAT = gsub("MONAT","",MONAT)) %>%
  mutate(date = as.Date(paste0(JAHR,"-", MONAT,"-01"))) %>%
  transmute(date, value = PRO101_val, category = WZ08V1) %>%
  pivot_wider(names_from = category)

ENERGY_MANUFACTURING_graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=subset(IPMAN_ENERGY, date >= as.Date("2018-01-01")), aes(x=date,y= `WZ08-B-10`/`WZ08-B-10`[1]*100,color="Energy-Intensive Manufacturing"), size = 1.25) +
  geom_line(data=subset(IPMAN_ENERGY, date >= as.Date("2018-01-01")), aes(x=date,y= `WZ08-C`/`WZ08-C`[1]*100,color="Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,110), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The German Industrial Crunch") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "The Energy Crisis Has Crushed Energy-Intensive German Manufacturing") +
  theme_apricitas + theme(legend.position = c(.6,.87)) +
  scale_color_manual(name= "Germany, Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Manufacturing","Energy-Intensive Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 65-(.3*45), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ENERGY_MANUFACTURING_graph, "Energy Intensive Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

IP_CHEM <- as.data.frame(readSDMX("https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBDE1.M.DE.Y.BAA1.N2C200000.G.C.I15.A&its_fileFormat=sdmx&mode=its")) %>%
  transmute(date = as.Date(as.yearmon(TIME_PERIOD)),value = as.numeric(OBS_VALUE)) %>%
  subset(date >= as.Date("2018-01-01"))
IP_BASIC_METAL <- as.data.frame(readSDMX("https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBDE1.M.DE.Y.BAA1.N2C240000.G.C.I15.A&its_fileFormat=sdmx&mode=its"))  %>%
  transmute(date = as.Date(as.yearmon(TIME_PERIOD)),value = as.numeric(OBS_VALUE)) %>%
  subset(date >= as.Date("2018-01-01"))
IP_COKE_PETROLEUM <- as.data.frame(readSDMX("https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBDE1.M.DE.Y.BAA1.N2C190000.G.C.I15.A&its_fileFormat=sdmx&mode=its"))  %>%
  transmute(date = as.Date(as.yearmon(TIME_PERIOD)),value = as.numeric(OBS_VALUE)) %>%
  subset(date >= as.Date("2018-01-01"))
IP_GLASS <- as.data.frame(readSDMX("https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBDE1.M.DE.Y.BAA1.N2C230000.G.C.I15.A&its_fileFormat=sdmx&mode=its"))  %>%
  transmute(date = as.Date(as.yearmon(TIME_PERIOD)),value = as.numeric(OBS_VALUE)) %>%
  subset(date >= as.Date("2018-01-01"))
IP_PAPER <- as.data.frame(readSDMX("https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBDE1.M.DE.Y.BAA1.N2C170000.G.C.I15.A&its_fileFormat=sdmx&mode=its"))  %>%
  transmute(date = as.Date(as.yearmon(TIME_PERIOD)),value = as.numeric(OBS_VALUE)) %>%
  subset(date >= as.Date("2018-01-01"))

ENERGY_MANUFACTURING_COMPONENT_graph <- ggplot() + #plotting regular vs non-regular employment
  #geom_line(data=IP_COKE_PETROLEUM, aes(x=date,y= value/value[nrow(IP_COKE_PETROLEUM)]*100,color="Coke and Refined Petroleum Products"), size = 1.25) +
  geom_line(data=IP_PAPER, aes(x=date,y= value/value[nrow(IP_PAPER)]*100,color="Paper & Paper Products"), size = 1.25) +
  geom_line(data=IP_GLASS, aes(x=date,y= value/value[nrow(IP_GLASS)]*100,color="Non-Metallic Mineral Products"), size = 1.25) +
  geom_line(data=IP_BASIC_METAL, aes(x=date,y= value/value[nrow(IP_BASIC_METAL)]*100,color="Basic Metals"), size = 1.25) +
  geom_line(data=IP_CHEM, aes(x=date,y= value/value[nrow(IP_CHEM)]*100,color="Chemicals & Chemical Products"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,110), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The German Industrial Crunch") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "The Energy Crisis Has Crushed Energy-Intensive German Manufacturing") +
  theme_apricitas + theme(legend.position = c(.225,.3)) +
  scale_color_manual(name= "Germany, Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Chemicals & Chemical Products","Basic Metals","Paper & Paper Products","Non-Metallic Mineral Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 65-(.3*45), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ENERGY_MANUFACTURING_COMPONENT_graph, "Energy Intensive Manufacturing Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


ELECTRIC <- as.data.frame(readSDMX("https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBDE1.D.DE.Y.VERS.P2XD35165.C.S.ABA.A&its_fileFormat=sdmx&mode=its"))  %>%
  .[order(nrow(.):1),] %>%
  transmute(date = as.Date(TIME_PERIOD),value = as.numeric(OBS_VALUE)) %>%
  mutate(value = c(rep(0,29),rollmean(value,30))) %>%
  subset(date >= as.Date("2018-01-01")) 

ELECTRIC_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=ELECTRIC, aes(x=date,y= value/1000000,color="Germany Realized General Electricity Consumption, Rolling 30 Day Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .01, suffix = "TWh"),limits = c(1.2,1.45), breaks = c(1.2,1.25,1.3,1.35,1.4,1.45), expand = c(0,0)) +
  ylab("Rolling 30 Day Average, TWh") +
  ggtitle("Germany and The Energy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "Electricity Consumption in Germany Has Fallen 10% Amidst The Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.5,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 1.2-(.3*.25), ymax = 1.2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ELECTRIC_graph, "Germany Electricity Consumption.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Gas Supply Change 

household_nat_gas <- read.csv("https://www.bundesnetzagentur.de/_tools/SVG/js2/_functions/csv_export.html?view=renderCSV&id=1093592",sep = ";") %>%
  select(c(1,2,4,5)) %>%
  `colnames<-`(c("month","average","2022","2023")) %>%
  mutate(`2022` = `2022` - average) %>%
  mutate(`2023` = `2023` - average) %>%
  select(-average) %>%
  pivot_longer(cols = c(`2022`,`2023`)) %>%
  transmute(date = as.Date(paste0(name,"-",month,"-01")), value = value) %>%
  drop_na()

industry_nat_gas <- read.csv("https://www.bundesnetzagentur.de/_tools/SVG/js2/_functions/csv_export.html?view=renderCSV&id=1093026",sep = ";") %>%
  select(c(1,3,4,5)) %>%
  `colnames<-`(c("month","2022","2023","average")) %>%
  mutate(`2022` = `2022` - average) %>%
  mutate(`2023` = `2023` - average) %>%
  select(-average) %>%
  pivot_longer(cols = c(`2022`,`2023`)) %>%
  transmute(date = as.Date(paste0(name,"-",month,"-01")), value = value) %>%
  drop_na()

NAT_GAS_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=industry_nat_gas, aes(x=date,y= value/1000,color="Industry (Including Electricity Generation)"), size = 1.25) +
  geom_line(data=household_nat_gas, aes(x=date,y= value/1000,color="Households and Non-Industrial Businesses"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .2, suffix = "TWh"),limits = c(-0.6,0.2), expand = c(0,0), breaks = c(-0.6,-0.4,-0.2,0,0.2)) +
  ylab("Deviation From 2018-2021 Average, TWh/Day") +
  ggtitle("Germany and The Energy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Bundesnetzagentur Data",subtitle = "Gas Consumption For German Industry and Households Has Fallen Amidst the Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.65,.875)) +
  scale_color_manual(name= "German Gas Consumption, Deviation from 2018-2021 Average",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -0.6-(.3*0.8), ymax = -0.6) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NAT_GAS_graph, "Germany Nat Gas Consumption.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GDP_list <- retrieve_datalist(tableseries = "81000*", genesis=c(db='de'), language = "en")

GDP <- retrieve_data(tablename = "81000BV007", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPKM") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, BIP004_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = BIP004_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2000-01-01"))

US_GDP <- fredr(series_id = "A939RX0Q048SBEA", observation_start = as.Date("2000-01-01"))

GDP_graph <- ggplot() + #plotting GDP For US vs Germany
  geom_line(data=GDP, aes(x=date,y= value/value[1]*100,color="Germany"), size = 1.25) +
  geom_line(data=US_GDP, aes(x=date,y= value/value[1]*100,color="United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,135), expand = c(0,0)) +
  ylab("Index, Q1 2000 = 100") +
  ggtitle("Germany's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis and BEA Data",subtitle = "Since 2018, German Economic Growth Has Been Especially Weak") +
  theme_apricitas + theme(legend.position = c(.6,.87)) +
  scale_color_manual(name= "Real GDP Per Capita",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Germany","United States")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 95-(.3*40), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_graph, "Germany GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#IP_CAR <- retrieve_datalist(tableseries = "6111*",genesis=c(db='de'), language = "en")

IP_CAR <- retrieve_data(tablename = "42153BM003", genesis=c(db='de')) %>%
  subset(WZ08V3 == "WZ08-291") %>% #taking manufacturing and energy intensive manufacturing data 
  subset(WERT03 == "X13JDKSB") %>%#calendar and seasonally adjusted
  mutate(MONAT = gsub("MONAT","",MONAT)) %>%
  mutate(date = as.Date(paste0(JAHR,"-", MONAT,"-01"))) %>%
  transmute(date, value = PRO101_val, category = WZ08V3) %>%
  pivot_wider(names_from = category)

CAR_MANUFACTURING_graph <- ggplot() + #plotting car manufacturing
  geom_line(data=subset(IP_CAR, date >= as.Date("2018-01-01")), aes(x=date,y= `WZ08-291`/90.08*100,color="Motor Vehicles"), size = 1.25) +
  annotate(geom = "hline", y = 100, yintercept = 100, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "2019 Average", x = as.Date("2021-07-01"), y = 105, color ="#FFE98F", size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,130), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The German Industrial Crunch") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "German Car Manufacturing Has Still Not Recovered to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.8,.27)) +
  scale_color_manual(name= "Germany, Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*130), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAR_MANUFACTURING_graph, "Germany car Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Downloading Quarterly 9 Digit Bulk Industrial Production Data
IP_9DIGIT_BULK <- retrieve_data(tablename = "42131BV203", genesis=c(db='de')) 

IP_EV_EURO <- IP_9DIGIT_BULK %>%
  subset(GP19A9 %in% c("GP19-291024500","GP19-291024300")) %>%
  mutate(GP19A9 = gsub("GP19-291024500", "Battery Electric Vehicles",GP19A9)) %>%
  mutate(GP19A9 = gsub("GP19-291024300", "Plug-in Hybrids",GP19A9)) %>%
  transmute(category = factor(GP19A9, levels = c("Plug-in Hybrids","Battery Electric Vehicles")), value = PRODAW_val, date = as.Date(as.yearqtr(paste0(JAHR, '-', gsub("QUART", "", QUARTG)), format = "%Y-%q")))

IP_EV_NUMBER <- IP_9DIGIT_BULK %>%
  subset(GP19A9 %in% c("GP19-291024500","GP19-291024300")) %>%
  mutate(GP19A9 = gsub("GP19-291024500", "Battery Electric Vehicles",GP19A9)) %>%
  mutate(GP19A9 = gsub("GP19-291024300", "Plug-in Hybrids",GP19A9)) %>%
  transmute(category = factor(GP19A9, levels = c("Plug-in Hybrids","Battery Electric Vehicles")), value = PRO008_val, date = as.Date(as.yearqtr(paste0(JAHR, '-', gsub("QUART", "", QUARTG)), format = "%Y-%q")))

EV_STACKED_EURO_graph <- ggplot(data = IP_EV_EURO, aes(x = date, y = value/1000000000, fill = category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Euros") +
  ggtitle("The German EV Surge") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B", prefix = "€"), breaks = c(0,5,10,15), limits = c(0,15), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using DeStatis data", subtitle = "The Value of German EV Output is Rapidly Growing as the Industry Retools") +
  theme_apricitas + theme(legend.position = c(.425,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Value of German Quarterly Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Battery Electric Vehicles","Plug-in Hybrids")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

EV_STACKED_NUMBER_graph <- ggplot(data = IP_EV_NUMBER, aes(x = date, y = value/1000, fill = category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Thousands of Vehicles") +
  ggtitle("The German EV Surge") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,200,400), limits = c(0,400), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using DeStatis data", subtitle = "The Number of German EVs Produced is Rapidly Growing as the Industry Retools") +
  theme_apricitas + theme(legend.position = c(.425,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "German Quarterly Production, Thousands of Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Battery Electric Vehicles","Plug-in Hybrids")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*400), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = EV_STACKED_EURO_graph, "Germany EV Stacked.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EV_STACKED_NUMBER_graph, "Germany EV Stacked Number.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HEATPUMP_NUMBER <- IP_9DIGIT_BULK %>%
  subset(GP19A9 %in% c("GP19-282513801","GP19-282513809")) %>%
  mutate(GP19A9 = gsub("GP19-282513801","Heat Pump",GP19A9)) %>%
  mutate(GP19A9 = gsub("GP19-282513809","Heat Pump",GP19A9)) %>%
  transmute(category = GP19A9, value = PRO008_val, date = as.Date(as.yearqtr(paste0(JAHR, '-', gsub("QUART", "", QUARTG)), format = "%Y-%q")))

HEATPUMP_NUMBER_graph <- ggplot(data = HEATPUMP_NUMBER, aes(x = date, y = value/1000, fill = "German Quarterly Production, Thousands of Heat Pumps")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Thousands of Heat Pumps") +
  ggtitle("The German Heat Pump Surge") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,50,100,150), limits = c(0,150), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using DeStatis data", subtitle = "The Number of German Heat Pumps Produced is Rapidly Growing amidst the Energy Transition") +
  theme_apricitas + theme(legend.position = c(.425,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*150), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HEATPUMP_NUMBER_graph, "Germany Heat Pump Number.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HICP <- get_eurostat("prc_hicp_manr")

HICP_DE <- HICP %>%
  subset(geo == "DE") %>%
  subset(time>= as.Date("2000-01-01")) %>%
  subset(coicop == "CP00")

HICP_DE_LFE <- HICP %>%
  subset(geo == "DE") %>%
  subset(time>= as.Date("2000-01-01")) %>%
  subset(coicop == "TOT_X_NRG_FOOD")

HICP_graph <- ggplot() + #plotting car manufacturing
  geom_line(data=HICP_DE, aes(x=time,y= values/100,color="Harmonized Index of Consumer Prices (HICP)"), size = 1.25) +
  geom_line(data=HICP_DE_LFE, aes(x=time,y= values/100,color="HICP Excluding Food, Energy, Alcohol and Tobacco"), size = 1.25) +
  annotate(geom = "hline", y = 0, yintercept = 0, color = "white", size = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.007,.12), expand = c(0,0)) +
  ylab("Year-on-Year Growth") +
  ggtitle("The German Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat Data",subtitle = "German Inflation is at a Historic High, and Core Inflation is Still Increasing") +
  theme_apricitas + theme(legend.position = c(.4,.67)) +
  scale_color_manual(name= "Germany, Year-on-Year Inflation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.007-(.3*0.127), ymax = -0.007) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HICP_graph, "HICP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CONSTRUCTION_PROD <- get_eurostat("sts_copr_m") %>%
  subset(geo == "DE") %>%
  subset(time>= as.Date("2018-01-01")) %>%
  subset(nace_r2 == "F") %>%
  subset(unit == "I15") %>%
  subset(s_adj == "SCA")
MANU_PROD <- get_eurostat("sts_inpr_m", legacy_bulk_download = FALSE) %>%
  subset(geo == "DE") %>%
  subset(TIME_PERIOD >= as.Date("2018-01-01")) %>%
  subset(nace_r2 == "C") %>%
  subset(unit == "I15") %>%
  subset(s_adj == "SCA")
SERVICES_PROD <- get_eurostat("sts_sepr_m") %>%
  subset(geo == "DE") %>%
  subset(time>= as.Date("2018-01-01")) %>%
  subset(nace_r2 == "H-N_X_K") %>%
  subset(unit == "I15") %>%
  subset(s_adj == "SCA") %>%
  arrange(desc(row_number()))
  
CONSTRUCT_MANU_SERV_graph <- ggplot() + #plotting GDP For US vs Germany
  #geom_line(data=CONSTRUCTION_PROD, aes(x=time,y= values/values[1]*100,color="Construction"), size = 1.25) +
  geom_line(data=MANU_PROD, aes(x=TIME_PERIOD,y= values/values[1]*100,color="Manufacturing"), size = 1.25) +
  geom_line(data=SERVICES_PROD, aes(x=time,y= values/values[1]*100,color="Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,110), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Germany's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "Falling Manufacturing Output Has Slowed Germany Down, and Services Output is Now Also Falling") +
  theme_apricitas + theme(legend.position = c(.7,.27)) +
  scale_color_manual(name= "Real Output Index, Jan 2018 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Services","Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 65-(.3*45), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCT_MANU_SERV_graph, "Manu Serv Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#EU Russia Gas Imports

EUROSTAT_NATURAL_GAS_DATA_BULK <- get_eurostat("nrg_ti_gasm") 

EUROSTAT_NATURAL_GAS_DATA <- EUROSTAT_NATURAL_GAS_DATA_BULK %>%
  #subset(time >= as.Date("2018-01-01")) %>%
  subset(geo == "EU27_2020") %>%
  subset(unit == "MIO_M3") %>%
  subset(siec == "G3000")

EU_RU_GAS_IMPORTS <- EUROSTAT_NATURAL_GAS_DATA %>%
  subset(partner %in% c("RU","UA","BY")) %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(BY:UA))) %>%
  select(time,values) %>%
  mutate(partner = "Russia, Ukraine, and Belarus")

EU_US_GAS_IMPORTS <- EUROSTAT_NATURAL_GAS_DATA %>%
  subset(partner == "US") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  transmute(time,values = US) %>%
  mutate(partner = "United States")

EU_NO_GAS_IMPORTS <- EUROSTAT_NATURAL_GAS_DATA %>%
  subset(partner == "NO") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  transmute(time,values = NO) %>%
  mutate(partner = "Norway")

EU_QA_GAS_IMPORTS <- EUROSTAT_NATURAL_GAS_DATA %>%
  subset(partner %in% c("QA","NG")) %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(QA:NG))) %>%
  select(time,values) %>%
  mutate(partner = "Qatar and Nigeria")

EU_AL_GAS_IMPORTS <- EUROSTAT_NATURAL_GAS_DATA %>%
  subset(partner %in% c("DZ","MA","TN","LY")) %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(DZ:TN))) %>%
  select(time,values) %>%
  mutate(partner = "Algeria, Tunisia, Morocco, and Libya")

EU_OTHER_GAS_IMPORTS <- EUROSTAT_NATURAL_GAS_DATA %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  select(-TOTAL,-EUR_OTH,-BE,-BG,-CZ,-DK,-DE,-EE,-IE,-EL,-ES,-FR,-HR,-IT,-CY,-LV,-LT,-LU,-HU,-MT,-NL,-AT,-PL,-PT,-RO,-SI,-SK,-FI,-SE,-NO,-DZ,-US,-QA,-RU,-UA,-BY,-CH,-MA,-TN,-LY,-NG) %>%
  rowwise() %>%
  mutate(values = sum(c_across(AD:ZA))) %>%
  select(time,values) %>%
  mutate(partner = "Other (Including Re-Exports from UK/Turkey/etc)")

EU_STACKED_GAS_IMPORTS <- rbind(EU_OTHER_GAS_IMPORTS,EU_AL_GAS_IMPORTS,EU_QA_GAS_IMPORTS,EU_NO_GAS_IMPORTS,EU_US_GAS_IMPORTS,EU_RU_GAS_IMPORTS) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  pivot_longer(cols = c(`Other (Including Re-Exports from UK/Turkey/etc)`:`Russia, Ukraine, and Belarus`)) %>%
  mutate(name = factor(name,levels = c("Other (Including Re-Exports from UK/Turkey/etc)","United States","Qatar and Nigeria","Algeria, Tunisia, Morocco, and Libya","Norway","Russia, Ukraine, and Belarus")))

EU_STACKED_GAS_IMPORTS_graph <- ggplot(data = EU_STACKED_GAS_IMPORTS, aes(x = time, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Cubic Meters") +
  ggtitle("EU-27 Natural Gas Imports") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50), limits = c(0,57.5), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "Imports Through Russia are Down Significantly, But the EU is Making Up the Difference") +
  theme_apricitas + theme(legend.position = c(.325,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Russia, Ukraine, and Belarus","Norway","Algeria, Tunisia, Morocco, and Libya","Qatar and Nigeria","United States","Other (Including Re-Exports from UK/Turkey/etc)")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*57.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_STACKED_GAS_IMPORTS_graph, "EU Stacked Gas Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#has to be manually updated from DeStatis downloads
IFO_MATERIAL_SHORTAGE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Germany/IFO_Shortage.csv.csv", sep = ";") %>%
  drop_na() %>%
  mutate(date = as.Date(Tag))

IFO_MATERIAL_SHORTAGE_graph <- ggplot() + #plotting car manufacturing
  geom_line(data=IFO_MATERIAL_SHORTAGE, aes(x=date,y= shortage.indicator/100,color="% of German Manufacturers Reporting Shortage of Material Inputs, IFO Institute"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), expand = c(0,0)) +
  ylab("Percent of Firms") +
  ggtitle("The German Suppy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using IFO Data",subtitle = "German Manufacturers are Still Finding it Difficult to Source Key Inputs, Though Supply is Improving") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.007-(.3*0.127), ymax = -0.007) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IFO_MATERIAL_SHORTAGE_graph, "IFO Shortage Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EU_MANU_SURVEY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Germany/SECTOR_SHORTAGE.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  subset(Date > as.Date("2000-01-01"))
  
EU_MANU_SURVEY_graph <- ggplot() + #plotting car manufacturing
  geom_line(data=EU_MANU_SURVEY, aes(x=Date,y= Machinery/100,color="Machinery and Other Equipment"), size = 1.25) +
  geom_line(data=EU_MANU_SURVEY, aes(x=Date,y= Computer/100,color="Computer, Electronic, and Optical Products"), size = 1.25) +
  geom_line(data=EU_MANU_SURVEY, aes(x=Date,y= Electrical/100,color="Electrical Equipment"), size = 1.25) +
  geom_line(data=EU_MANU_SURVEY, aes(x=Date,y= Motor_Vehicles/100,color="Motor Vehicles, Trailers, and Semi-Trailers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), expand = c(0,0)) +
  ylab("Percent of Firms") +
  ggtitle("The German Suppy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat Data",subtitle = "Key German Industries are Still in a Supply-Chain Crisis") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_color_manual(name= "% of German Manufacturers With Material Shortages Restricting Output",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Motor Vehicles, Trailers, and Semi-Trailers","Computer, Electronic, and Optical Products","Electrical Equipment","Machinery and Other Equipment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MANU_SURVEY_graph, "EU MANU SURVEY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#SMDX 3.0 FULL DATASET NOT COMPRESSED
EMP_EXP <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/EI_BSEE_M_R2/1.0?compress=false"))
#EMP_EXP <- get_eurostat("ei_bsee_m_r2") %>%
  
EMP_EXP <- EMP_EXP %>%
  subset(geo == "DE" & unit == "BAL") %>%
  transmute(indic, time = as.Date(as.yearmon(TIME_PERIOD, format = "%Y-%m")), value = as.numeric(OBS_VALUE)) %>%
  subset(time >= as.Date("2018-01-01")) %>%
  pivot_wider(names_from = indic, values_from = value)
  
  
EMP_EXP_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=EMP_EXP, aes(x=time,y= `BS-CEME-BAL`,color="Construction"), size = 1.25) +
  geom_line(data=EMP_EXP, aes(x=time,y= `BS-IEME-BAL`,color="Industry"), size = 1.25) +
  geom_line(data=EMP_EXP, aes(x=time,y= `BS-REM-BAL`,color="Retail Trade"), size = 1.25) +
  geom_line(data=EMP_EXP, aes(x=time,y= `BS-SEEM-BAL`,color="Services"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .2),limits = c(-35,25), expand = c(0,0), breaks = c(-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25)) +
  ylab("Balance, Increase minus Decrease") +
  ggtitle("Germany's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "German Employment Expectations Are Weak—Expecially in Industry") +
  theme_apricitas + theme(legend.position = c(.80,.20)) +
  scale_color_manual(name= "Employment Expectations, Next 3M",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -35-(.3*60), ymax = -35) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_graph, "Emp Exp.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HOUSING_list <- retrieve_datalist(tableseries = "31*", genesis=c(db='de'), language = "en")

HOUSING_STARTS <- retrieve_data(tablename = "31111BM001", genesis=c(db='de')) %>%
  filter(BAUGB1 %in% c("WOHNGEBAEUDE","GEBW01")) %>% #ALL CONSTRUCTION AND SINGLE-FAMILY CONSTRUCTION
  filter(BAUTK1 == "ARTBT5") %>% #NEW CONSTRUCTION
  transmute(Type = BAUGB1,date = as.Date(as.yearmon(paste0(JAHR, '-', gsub("MONAT", "", MONAT)), format = "%Y-%m")),value = WOHN01_val) %>%
  pivot_wider(names_from = Type) %>%
  arrange(date) %>%
  transmute(`Single-Family` = GEBW01, `Multi-Family` = WOHNGEBAEUDE-GEBW01) %>%
  ts(., start = c(2003,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2003-01-01"), by = "month", length = nrow(.))) %>%
  setNames(c("Single-Family","Multi-Family", "date")) %>%
  pivot_longer(cols = c(`Single-Family`:`Multi-Family`))

GER_STACKED_HOUSING_STARTS_graph <- ggplot(data = HOUSING_STARTS, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  ylab("Units, Thousands, Monthly") +
  ggtitle("German Housing Starts are Falling") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,5,10,15,20,25,30,35), limits = c(0,37.5), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using DeStatis data", subtitle = "German Housing Starts are Down Significantly, With Single Family Starts Near Record Lows") +
  theme_apricitas + theme(legend.position = c(.325,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*37.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GER_STACKED_HOUSING_STARTS_graph, "GER STACKED HOUSING STARTS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FACTORY_CONSTRUCTION_BULK <- retrieve_data(tablename = "31111BM001", genesis=c(db='de'))

FACTORY_CONSTRUCTION <- FACTORY_CONSTRUCTION_BULK %>%
  filter(BAUGB1 == c("GEBFABRIK01")) %>% #ALL FACTORY CONSTRUCTION
  filter(BAUTK1 == "ARTBT5") %>% #NEW CONSTRUCTION
  transmute(date = as.Date(as.yearmon(paste0(JAHR, '-', gsub("MONAT", "", MONAT)), format = "%Y-%m")),volume = FLC003_val,value = VKB001_val) %>%
  arrange(date) %>%
  select(-date) %>%
  # ts(., start = c(2003,1), frequency = 12) %>%
  # seas() %>%
  # final() %>%
  # as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2003-01-01"), by = "month", length = nrow(.))) 

  
GER_FACTORY_CONSTRUCTION_graph <- ggplot() + #plotting permanent and temporary job losers
  #geom_line(data=FACTORY_CONSTRUCTION, aes(x=date,y= value,color="value"), size = 1.25) +
  geom_line(data=FACTORY_CONSTRUCTION, aes(x=date,y= volume,color="volume"), size = 1.25) +
  #geom_line(data=EMP_EXP, aes(x=date,y= `BS-IEME-BAL`,color="Industry"), size = 1.25) +
  ylab("Units, Thousands, Monthly") +
  ggtitle("German Factory Construction is Falling") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,5,10,15,20,25,30,35), limits = c(0,37.5), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using DeStatis data", subtitle = "German Housing Starts are Down Significantly, With Single Family Starts Near Record Lows") +
  theme_apricitas + theme(legend.position = c(.325,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*37.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = GER_STACKED_HOUSING_STARTS_graph, "GER STACKED HOUSING STARTS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GERMAN_GFCF_PRIVATE_EQUIPMENT <- retrieve_data(tablename = "81000BV015", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, VGR105_val,BAU020_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = VGR105_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2000-01-01"))

GERMAN_GFCF_PRIVATE_EQUIPMENT_graph <- ggplot() + #plotting GDP For US vs Germany
  geom_line(data=GERMAN_GFCF_PRIVATE_EQUIPMENT, aes(x=date,y= value,color="Real Private Fixed Investment in Equipment, Germany"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = "€"),limits = c(30,60), expand = c(0,0)) +
  ylab("Chained Billions of Dollars") +
  ggtitle("Germany's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis and BEA Data",subtitle = "Since 2018, German Economic Growth Has Been Especially Weak") +
  theme_apricitas + theme(legend.position = c(.6,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 95-(.3*40), ymax = 95) +
  coord_cartesian(clip = "off")

GERMAN_GFCF_EQUIPMENT_CATEGORIES <- retrieve_data(tablename = "81000BV009", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, VGR008_val, INV006_val, INV012_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), Equipment = VGR008_val, Machines = INV006_val, Vehicles = INV012_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2016-01-01")) %>%
  mutate(across(where(is.numeric), ~if_else(.x == 0, NA_real_, .x))) %>%
  mutate(across(where(is.numeric), ~ .x / .x[9]*100))

GERMAN_GFCF_EQUIPMENT_CATEGORIES_graph <- ggplot() + #plotting Fixed Investment
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Machines,color="Equipment: Machinery and Other Devices"), size = 1.25) +
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Vehicles,color="Equipment: Vehicles"), size = 1.25) +
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Equipment,color="Equipment"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,120), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Germany's Investment Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "German Investment In Fixed Manufacturing Assets Has Not Recovered to Pre-Pandemic Lvels") +
  theme_apricitas + theme(legend.position = c(.3,.27)) +
  scale_color_manual(name= "Germany: Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 60-(.3*60), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GERMAN_GFCF_EQUIPMENT_CATEGORIES_graph, "GERMAN GFCF EQUIPMENT CATEGORIES GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#ADD GRAPH OF ORDER BACKLOGS


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(eurostat,rsdmx,wiesbaden,keyring,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user="DEM56460DY", password="XSYhTyP4JV4!Q4b"))
save_credentials(db= 'de',user="DEM56460DY", password="XSYhTyP4JV4!Q4b")

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
  scale_y_continuous(labels = scales::number_format(accuracy = .01, suffix = "TWh"),limits = c(1.2,1.45), expand = c(0,0)) +
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
  scale_y_continuous(labels = scales::number_format(accuracy = .2),limits = c(-0.6,0.2), expand = c(0,0), breaks = c(-0.6,-0.4,-0.2,0,0.2)) +
  ylab("Deviation From 2018-2021 Average, TWh/Day") +
  ggtitle("Germany and The Energy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Bundesnetzagentur Data",subtitle = "Gas Consumption For German Industry and Households Has Fallen Amidst the Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.65,.875)) +
  scale_color_manual(name= "German Gas Consumption, Deviation from 2018-2021 Average",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -0.6-(.3*0.6), ymax = -0.6) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NAT_GAS_graph, "Germany Nat Gas Consumption.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GDP_list <- retrieve_datalist(tableseries = "81000*", genesis=c(db='de'), language = "en")

GDP <- retrieve_data(tablename = "81000BV007", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPKM") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, BIP004_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = BIP004_val) %>%
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

WZ08-2910

IP_CAR <- retrieve_datalist(tableseries = "6111*",genesis=c(db='de'), language = "en")

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
  annotate(geom = "text", label = "2019 Average", x = as.Date("2021-07-01"), y = 92, color ="#FFE98F", size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,130), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The German Industrial Crunch") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "German Car Manufacturing Has Still Not Recovered to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.8,.27)) +
  scale_color_manual(name= "Germany, Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*110), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAR_MANUFACTURING_graph, "Germany car Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CPI_list <- retrieve_datalist(tableseries = "6111*", genesis=c(db='de'), language = "en")

HICP <- get_eurostat("prc_hicp_manr")


CPI_LFE <- retrieve_data(tablename = "61111BM006", genesis=c(db='de'), startyear=2000, endyear=2023) %>%
  subset(CC13B1 == "CC13-63E") %>% #taking manufacturing and energy intensive manufacturing data 
  mutate(MONAT = gsub("MONAT","",MONAT)) %>%
  mutate(date = as.Date(paste0(JAHR,"-", MONAT,"-01"))) %>%
  transmute(date, value = (PREIS1_val-100)/100, category = CC13B1)


CPI_LFE <- retrieve_data(tablename = "61111BM006", genesis=c(db='de'), startyear=2000, endyear=2023) %>%
  subset(CC13B1 == "CC13-63E") %>% #taking manufacturing and energy intensive manufacturing data 
  mutate(MONAT = gsub("MONAT","",MONAT)) %>%
  mutate(date = as.Date(paste0(JAHR,"-", MONAT,"-01"))) %>%
  transmute(date, value = (PREIS1_val-100)/100, category = CC13B1)

spec()

?retrieve_data()

61111-0003:



42153BM003

Impediments to Production Graph
Index of Services Output vs Manufacturing and Construction Output Graph
Inflation Components Graph
Employment and Employment Expectations Graph


str_sub(x, start= -6)

?retrieve_datalist
?retrieve_valuelabel

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
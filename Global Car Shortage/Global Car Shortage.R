pacman::p_load(seasonal,eurostat,restatapi,bea.R,sidrar,htmltools,devtools,onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

devtools::install_github("warint/statcanR")
library(statcanR)
devtools::install_github("rOpenGov/eurostat")
library(eurostat)
install_github("seokhoonj/ecos", force = TRUE)
library("ecos")
ecos.setKey('2DNSQWJY32YGLL8EM95R')


CANADA_INDPRO <- statcan_data("36-10-0434-04", "eng") %>%
  subset(`North American Industry Classification System (NAICS)`=="Motor vehicle manufacturing [3361]") %>%
  subset(`Prices`=="Chained (2017) dollars") %>%
  subset(`Seasonal adjustment`=="Seasonally adjusted at annual rates") %>%
  subset(REF_DATE >= as.Date("2018-01-01"))

US_INDPRO <- fredr(series_id = "IPG3361S",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, aggregation_method = "m")

MEXICO_INDPO <- read.csv("C:/Users/josep/Documents/Global Car Shortage/Mex_TEP_INDPRO.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  subset(Date >= as.Date("2018-01-01"))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

US_CANADA_INDPRO_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = CANADA_INDPRO, aes(x = REF_DATE, y = VALUE/VALUE[1]*100, color = "Canada"), size = 1.25) +
  geom_line(data = US_INDPRO, aes(x = date, y = value/value[1]*100, color = "United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,130), breaks = c(0,25,50,75,100,125,150), expand = c(0,0)) +
  ylab("Index, Jan 2018=100") +
  ggtitle("Canada, Chips, and the Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and Statistics Canada data",subtitle = "Canadian Auto Production Has Suffered More Thanks to The Chip Shortage & EV Retooling") +
  theme_apricitas + theme(legend.position = c(.72,.2)) +
  scale_color_manual(name= "Industrial Production of Motor Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*130), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CANADA_INDPRO_graph, "US Canada Indpro.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MEXICO_INDPO <- read.csv("C:/Users/josep/Documents/Global Car Shortage/Mex_TEP_INDPRO.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  subset(Date >= as.Date("2018-01-01"))

MEXICO_INDPO_graph <- ggplot() + #plotting mexico indpro
  geom_line(data = MEXICO_INDPO, aes(x = Date, y = MX_INDPRO_TEP/MX_INDPRO_TEP[1]*100, color = "Mexican Industrial Production of Transportation Equipment Manufacturers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,130), breaks = c(0,25,50,75,100,125,150), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Mexico, Chips, and the Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Mexican Transportation Production Is Slowly Recovering from the Chip Shortage") +
  theme_apricitas + theme(legend.position = c(.50,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*130), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_INDPO_graph, "Mexico Indpro.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Assemblies <- fredr(series_id = "MVATOTASSS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(Shortfall = 10.91-value) %>%
  mutate(Cumulative_Shortfall = (cumsum(Shortfall)+4.5665)/12)

Assemblies_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=Assemblies, aes(x=date,y= value, color = "US Total Motor Vehicle Assemblies"), size = 1.25)+ 
  annotate(geom = "hline", y = 10.91, yintercept = 10.91, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "2019 Average", x = as.Date("2021-10-01"), y = 11.5, color ="#FFE98F",size = 5) +
  #geom_line(data=AssembliesNSA, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Motor Vehicle Assemblies, Millions, Annual Rate") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,14), breaks = c(0,4,8,12), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Fixing the Assembly Line") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Motor Vehicle Assemblies Popped Above Their Pre-Pandemic Average") +
  theme_apricitas + theme(legend.position = c(0.75,0.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

Cumulative_Shortfall_Graph <- ggplot(subset(Assemblies, date > as.Date("2020-01-01")), aes(x = date, y = Cumulative_Shortfall, fill = "US Cumulative Shortfall in US Motor Vehicle Production")) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  xlab("Date") +
  ylab("Millions of Motor Vehicles") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3,4,5), limits = c(0,5), expand = c(0,0)) +
  ggtitle("Falling Short") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Cumulative Shortfall in US Vehicle Production Sits at More Than 4.5 Million Units") +
  theme_apricitas + theme(legend.position = c(.35,.95)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-03-01")-(.1861*(today()-as.Date("2020-03-01"))), xmax = as.Date("2020-03-01")-(0.049*(today()-as.Date("2020-03-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Assemblies_Graph, "US Assemblies Indpro.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Cumulative_Shortfall_Graph, "US Cumulative Shortfall.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

read.csv("https://apisidra.ibge.gov.br/values/t/7511/n1/all/v/11599/p/all/c542/129268,129269,129270,129271/d/v11599%205")

BRAZIL_INDPRO <- get_sidra(api = "/t/7511/n1/all/v/11599/p/all/c542/129268,129269,129270,129271/d/v11599%205") %>%
  select(Valor,`Mês (Código)`,`Grupos e classes industriais (Código)`) %>%
  `colnames<-`(c("value","date","name")) %>%
  mutate(date = as.Date(as.yearmon(date,"%Y%m"))) %>%
  pivot_wider() %>%
  subset(date >= as.Date("2018-01-01"))

BRAZIL_INDPO_graph <- ggplot() + #plotting mexico indpro
  geom_line(data = BRAZIL_INDPRO, aes(x = date, y = `129268`/`129268`[3]*100, color = "Cars, Vans, and Utility Vehicles"), size = 1.25) +
  geom_line(data = BRAZIL_INDPRO, aes(x = date, y = `129269`/`129269`[3]*100, color = "Trucks and Buses"), size = 1.25) +
  #geom_line(data = BRAZIL_INDPRO, aes(x = date, y = `129270`/`129270`[1]*100, color = "Cabins, Bodies, and Trailers for Motor Vehicles"), size = 1.25) +
  #geom_line(data = BRAZIL_INDPRO, aes(x = date, y = `129271`/`129271`[1]*100, color = "Parts and Accessories for Motor Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,175), breaks = c(0,50,100,150), expand = c(0,0)) +
  ylab("Index, Mar 2018 = 100") +
  ggtitle("Brazil, Chips, and the Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using IBGE data",subtitle = "Brazilian Car Production is Down—But Trucks and Buses Have More Than Recovered") +
  theme_apricitas + theme(legend.position = c(.20,.2)) +
  scale_color_manual(name= "Brazil: Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*175), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BRAZIL_INDPO_graph, "Brazil.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

# EU_MOTOR_VEHICLE <- get_eurostat_data("STS_INPR_M",
#                                         filters=c("DE","ES","FR","TR","IT","SCA","C291","I15"),
#                                         date_filter=">2018-01-01") %>%
#   mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
#   select(geo, time, values) %>%
#   pivot_wider(names_from = geo, values_from = values)

EU_IND_PRO_VEHICLE_graph <- ggplot() + #plotting INDPRO Motor Vehicles
  geom_line(data=EU_MOTOR_VEHICLE, aes(x=time,y= IT/IT[1]*100,color= "Italy"), size = 1.25) +
  geom_line(data=EU_MOTOR_VEHICLE, aes(x=time,y= ES/ES[1]*100,color= "Spain"), size = 1.25) +
  geom_line(data=EU_MOTOR_VEHICLE, aes(x=time,y= TR/TR[1]*100,color= "Turkey"), size = 1.25) +
  geom_line(data=EU_MOTOR_VEHICLE, aes(x=time,y= FR/FR[1]*100,color= "France"), size = 1.25) +
  geom_line(data=EU_MOTOR_VEHICLE, aes(x=time,y= DE/DE[1]*100,color= "Germany"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),breaks = c(0,25,50,75,100,125), limits = c(0,130), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The European Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "European Auto Output Has Struggled Amidst the Chip Shortage and Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.22,.22)) +
  scale_color_manual(name= "Industrial Production of Motor Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Germany","France","Spain","Turkey","Italy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(.1861*(today()-as.Date("2018-01-01")))), ymin = 0-(.3*125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_IND_PRO_VEHICLE_graph, "EU Vehicle IndPro.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_MINOR_IND_PRO_VEHICLE <- get_eurostat_data("STS_INPR_M",
                                              filters=c("SCA","RO","CZ","PL","C29","I15"),
                                              date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(geo, time, values) %>%
  pivot_wider(names_from = geo, values_from = values)

UK_IND_PRO_VEHICLE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k23u/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1) %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2018-01-01"))

EU_MINOR_VEHICLE_graph <- ggplot() + #plotting INDPRO Motor Vehicles
  geom_line(data=EU_MINOR_IND_PRO_VEHICLE, aes(x=time,y= CZ/CZ[1]*100,color= "Czechia"), size = 1.25) +
  geom_line(data=EU_MINOR_IND_PRO_VEHICLE, aes(x=time,y= PL/PL[1]*100,color= "Poland"), size = 1.25) +
  geom_line(data=EU_MINOR_IND_PRO_VEHICLE, aes(x=time,y= RO/RO[1]*100,color= "Romania"), size = 1.25) +
  geom_line(data=UK_IND_PRO_VEHICLE, aes(x=date,y= value/value[1]*100,color= "United Kingdom"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),breaks = c(0,25,50,75,100,125), limits = c(0,140), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The European Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "UK Vehicle Output Has Fallen—While Polish Output Has Risen") +
  theme_apricitas + theme(legend.position = c(.22,.22)) +
  scale_color_manual(name= "Production: Vehicles, Trailers, & Semis",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United Kingdom","Czechia","Poland","Romania")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(.1861*(today()-as.Date("2018-01-01")))), ymin = 0-(.3*125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MINOR_VEHICLE_graph, "EU Minor Vehicle IndPro.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EA_IND_PRO_VEHICLE <- get_eurostat_data("STS_INPR_M",
                                        filters=c("EA19","SCA","C291","C2931","C2932","I15"),
                                        date_filter=">2019-01-01") %>%
  mutate(time = as.Date(as.yearmon(time,"%Y-%m"))) %>%
  select(nace_r2, time, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values)

EA_IND_PRO_VEHICLE_graph <- ggplot() + #plotting BIE
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C2932/C2932[1]*100,color= "Other Parts & Accessories for Motor Vehicles"), size = 1.25) +
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C2931/C2931[1]*100,color= "Electrical & Electronic Parts for Motor Vehicles"), size = 1.25) +
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C291/C291[1]*100,color= "Motor Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),breaks = c(0,20, 40,60,80,100), limits = c(0,110), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("The Eurozone Car Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "European Auto Output Has Struggled Amidst the Chip Shortage and Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.69,.15)) +
  scale_color_manual(name= "EA-19 Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Motor Vehicles","Electrical & Electronic Parts for Motor Vehicles","Other Parts & Accessories for Motor Vehicles")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(.1861*(today()-as.Date("2019-01-01")))), ymin = 0-(.3*110), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_IND_PRO_VEHICLE_graph, "EA INDPRO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Asia
South_Korea <- read.csv("C:/Users/josep/Documents/Global Car Shortage/SK_MV_INDPRO.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  subset(Date >= as.Date("2018-01-01"))
INDIA <- read.csv2("https://datasource.kapsarc.org/api/explore/v2.1/catalog/datasets/india-index-of-industrial-production-iip-monthly-update/exports/csv?lang=en&facet=facet(name%3D%22date%22%2C%20disjunctive%3Dtrue)&facet=facet(name%3D%22frequency%22%2C%20disjunctive%3Dtrue)&facet=facet(name%3D%22industry_name%22%2C%20disjunctive%3Dtrue)&facet=facet(name%3D%22variable_name%22%2C%20disjunctive%3Dtrue)&refine=industry_name%3A%22Manufacture%20of%20motor%20vehicles%2C%20trailers%20and%20semi-trailers%22&refine=variable_name%3A%22Index%20of%20Industrial%20Production%20(IIP)%22&timezone=America%2FNew_York&use_labels=true&csv_separator=%3B")%>%
  mutate(Date = as.Date(as.yearmon(Date,"%Y-%m"))) %>%
  subset(Date >= as.Date("2018-01-01")) %>%
  mutate(Value = as.numeric(Value))
JAPAN_IP <- read.xlsx("https://www.meti.go.jp/english/statistics/tyo/iip/xls/b2020_gsm1e.xlsx") %>%
  select(-`Seasonally.adjusted.Index.by.Industry.:.Industrial.Production.(2020=100.0)`,-X3) %>%
  transpose() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  #mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = nrow(.))) %>%
  mutate_if(is.character,as.numeric)

ASIA_IP_CARS_graph <- ggplot() + #plotting MOVE
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= motor_vehicles/1.008,color= "Japan"), size = 1.25) +
  geom_line(data=INDIA, aes(x=Date,y= Value/Value[1]*100,color= "India"), size = 1.25) +
  geom_line(data=South_Korea, aes(x=Date,y= Motor_Vehicles/Motor_Vehicles[1]*100,color= "South Korea"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,135), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Auto Production in Asia") +
  labs(caption = "Graph created by @JosephPolitano using METI, NBSS, MOSPI, and KOSTAT Data",subtitle = "South Korean Production Has Recovered—While India and Japan Lag Behind") +
  theme_apricitas + theme(legend.position = c(.225,.25)) +
  scale_color_manual(name= "Industrial Production of Motor Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

JAPAN_IP_CARS_graph <- ggplot() + #plotting MOVE
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= trucks/trucks[1]*100,color= "Trucks"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= passenger_cars/passenger_cars[1]*100,color= "Passenger Cars"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= motorcycles/motorcycles[1]*100,color= "Motorcycles"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= buses/buses[1]*100,color= "Buses"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(20,170), breaks = c(0,20,40,60,80,100,120,140,160), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Auto Production in Japan") +
  labs(caption = "Graph created by @JosephPolitano using METI Data",subtitle = "The Fall in Japanese Production Has Been Driven By Buses and Cars, But Buoyed by Motorcycles") +
  theme_apricitas + theme(legend.position = c(.2,.20)) +
  scale_color_manual(name= "Japan: Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 20-(.3*150), ymax = 20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = JAPAN_IP_CARS_graph, "Japan INDPRO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


ggsave(dpi = "retina",plot = ASIA_IP_CARS_graph, "Asia INDPRO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


install_github("pcdi/rstatscn")
library(rstatscn)

statscnQueryLastN(70, lang = "en")
CHINA_IND_PRO_MV <- statscnQueryData('A020923',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  .[order(nrow(.):1),] %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y")) %>%
  #dplyr::select(date,year,`Output of Motor Vehicles, Accumulated`) %>%
  group_by(year) %>%
  mutate(monthly = `Output of Motor Vehicles, Accumulated`-lag(`Output of Motor Vehicles, Accumulated`)) %>%
  drop_na() %>%
  mutate(monthly = ifelse(month(ymd(date)) == 2, monthly*1/2, monthly)) %>%
  subset(`Output of Motor Vehicles, Accumulated` != 0) %>%
  mutate(`Output of Motor Vehicles, Current Period` = ifelse(`Output of Motor Vehicles, Current Period` == 0, monthly, `Output of Motor Vehicles, Current Period`))
  
CHINA_IND_PRO_CAR <- statscnQueryData('A020924',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  .[order(nrow(.):1),] %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y")) %>%
  #dplyr::select(date,year,`Output of Motor Vehicles, Accumulated`) %>%
  group_by(year) %>%
  mutate(monthly = `Output of Cars, Accumulated`-lag(`Output of Cars, Accumulated`)) %>%
  drop_na() %>%
  mutate(monthly = ifelse(month(ymd(date)) == 2, monthly*1/2, monthly)) %>%
  subset(`Output of Cars, Accumulated` != 0) %>%
  mutate(`Output of Cars, Current Period` = ifelse(`Output of Cars, Current Period` == 0, monthly, `Output of Cars, Current Period`))

CHINA_IND_PRO_SUV <- statscnQueryData('A020925',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  .[order(nrow(.):1),] %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y")) %>%
  #dplyr::select(date,year,`Output of Motor Vehicles, Accumulated`) %>%
  group_by(year) %>%
  mutate(monthly = `Output of SUV, Accumulated`-lag(`Output of SUV, Accumulated`)) %>%
  drop_na() %>%
  mutate(monthly = ifelse(month(ymd(date)) == 2, monthly*1/2, monthly)) %>%
  subset(`Output of SUV, Accumulated` != 0) %>%
  mutate(`Output of SUV, Current Period` = ifelse(`Output of SUV, Current Period` == 0, monthly, `Output of SUV, Current Period`))

CHINA_IND_PRO_TRUCK <- statscnQueryData('A020926',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  .[order(nrow(.):1),] %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(year = format(as.Date(date, format="%d/%m/%Y"),"%Y")) %>%
  #dplyr::select(date,year,`Output of Motor Vehicles, Accumulated`) %>%
  group_by(year) %>%
  mutate(monthly = `Output of Trucks, Accumulated`-lag(`Output of Trucks, Accumulated`)) %>%
  drop_na() %>%
  mutate(monthly = ifelse(month(ymd(date)) == 2, monthly*1/2, monthly)) %>%
  subset(`Output of Trucks, Accumulated` != 0) %>%
  mutate(`Output of Trucks, Current Period` = ifelse(`Output of Trucks, Current Period` == 0, monthly, `Output of Trucks, Current Period`))

CHINA_IP_CARS_graph <- ggplot() + #plotting MOVE
  geom_line(data=CHINA_IND_PRO_MV, aes(x=date,y= `Output of Motor Vehicles, Current Period`/`Output of Motor Vehicles, Current Period`[1]*100,color= "Motor Vehicles"), size = 1.25) +
  geom_line(data=CHINA_IND_PRO_CAR, aes(x=date,y= `Output of Cars, Current Period`/`Output of Cars, Current Period`[1]*100,color= "Cars"), size = 1.25) +
  geom_line(data=CHINA_IND_PRO_SUV, aes(x=date,y= `Output of SUV, Current Period`/`Output of SUV, Current Period`[1]*100,color= "SUVs"), size = 1.25) +
  geom_line(data=CHINA_IND_PRO_TRUCK, aes(x=date,y= `Output of Trucks, Current Period`/`Output of Trucks, Current Period`[1]*100,color= "Trucks"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,200), breaks = c(0,20,40,60,80,100,120,140,160,180), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Auto Production in China") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "Chinese Vehicle Production Has Essentially Recovered—Despite Notable Recent Falls") +
  theme_apricitas + theme(legend.position = c(.15,.20)) +
  scale_color_manual(name= "Industrial Output",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-02-01")-(.1861*(today()-as.Date("2018-02-01"))), xmax = as.Date("2018-02-01")-(0.049*(today()-as.Date("2018-02-01"))), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_IP_CARS_graph, "CHINA INDPRO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

QSPC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/Compiled_QSPC.csv") %>%
  mutate(date = paste(Year,"Q",Quarter)) %>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q %q"))) %>%
  mutate(Value = as.numeric(Value))

write.csv(QSPC, "QSPC.csv")

QSPC <- read.csv("C:/Users/josep/Documents/Global Car Shortage/QSPC.csv") %>%
  mutate(date = as.Date(date))

QSPC_Supply_Selected_Graph <- ggplot() + #plotting BIE
  geom_line(data=subset(QSPC, Sector == "336" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Insufficient Supply of Materials"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "336" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "336" & Measure == "Logistics/transportation constraints"), aes(x=date,y= Value/100,color= "Logistics/Transportation Constraints"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.85), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Still Stressed Out") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "The US Car Industry Cites Materials Shortages as the Biggest Supply-Side Production Constraint") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= "Main Production Constraint, US Transportation Equipment Manufacturers",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Insufficient Supply of Materials","Insufficient Supply of Labor","Logistics/Transportation Constraints")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(.1861*(today()-as.Date("2014-04-01")))), ymin = 0-(.3*.85), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QSPC_Supply_Selected_Graph, "QSPC Selected.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EU_QSPC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Global%20Car%20Shortage/EURO_QSPC.csv") %>%
  mutate(Date = as.Date(date)) %>%
  subset(Date >= as.Date("2014-01-01"))

EU_QSPC_Supply_Selected_Graph <- ggplot() + #plotting BIE
  geom_line(data=EU_QSPC, aes(x=Date,y= Materials/100,color= "Shortage of Materials or Equipment"), size = 1.25) +
  geom_line(data=EU_QSPC, aes(x=Date,y= Labor/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EU_QSPC, aes(x=Date,y= Other/100,color= "Other Supply Issues"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.85), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("The Chip Shortage is Easing") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Car Makers are Citing Material Shortages Much Less Than at Peak Chip Shortage") +
  theme_apricitas + theme(legend.position = c(.41,.85)) +
  scale_color_manual(name= "Main Production Constraint For EU-27 Motor Vehicle Manufacturers",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Materials or Equipment","Shortage of Labor","Other Supply Issues")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(.1861*(today()-as.Date("2014-04-01")))), ymin = 0-(.3*.85), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_QSPC_Supply_Selected_Graph, "EU QSPC Selected.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

INDIA <- INDIA[order(as.Date(INDIA$Date, format="%m/%d/%Y")),]
#Reordering here

Global_Production <- ggplot() + #plotting MOVE
  #geom_line(data=CHINA_IND_PRO_MV, aes(x=date,y= `Output of Motor Vehicles, Current Period`/(sum(`Output of Motor Vehicles, Current Period`[1:11])/11)*100,color= "China"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= motor_vehicles/(sum(motor_vehicles[1:12])/12)*100,color= "Japan"), size = 1.25) +
  geom_line(data=INDIA, aes(x=Date,y= Value/(sum(Value[1:12])/12)*100,color= "India"), size = 1.25) +
  geom_line(data=EA_IND_PRO_VEHICLE, aes(x=time,y= C291/(sum(C291[1:12])/12)*100,color= "Euro Area"), size = 1.25) +
  geom_line(data=US_INDPRO, aes(x = date, y = value/sum((value[1:12])/12)*100, color = "United States"), size = 1.25) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,135), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Index, 2018 Avg = 100") +
  ggtitle("Cars and The Global Chip Shorage") +
  labs(caption = "Graph created by @JosephPolitano using METI, MOSPI, NBSS, Eurostat, and Federal Reserve Data",subtitle = "American and Chinese Auto Production Has Recovered—But Other Major Countries Lag Behind") +
  theme_apricitas + theme(legend.position = c(.2,.25)) +
  scale_color_manual(name= "Production of Motor Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United States","China","India","Euro Area","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Global_Production, "Global Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EU_MOTOR_VEHICLE_TOTAL <- get_eurostat("sts_inpr_m")

EU_MOTOR_VEHICLE_SUBSET <- EU_MOTOR_VEHICLE_TOTAL %>%
  subset(nace_r2 == "C291") %>%
  subset(unit == "I15") %>%
  subset(s_adj == "SCA") %>%
  subset(time >= as.Date("2018-01-01")) %>%
  subset(geo == "EU27_2020") %>%
  arrange(time)

US_EU_JPN_Production <- ggplot() + #plotting MOVE
  #geom_line(data=CHINA_IND_PRO_MV, aes(x=date,y= `Output of Motor Vehicles, Current Period`/(sum(`Output of Motor Vehicles, Current Period`[1:11])/11)*100,color= "China"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= motor_vehicles/(sum(motor_vehicles[1:12])/12)*100,color= "Japan"), size = 1.25) +
  #geom_line(data=INDIA, aes(x=Date,y= Value/(sum(Value[1:12])/12)*100,color= "India"), size = 1.25) +
  geom_line(data=EU_MOTOR_VEHICLE_SUBSET, aes(x=time,y= values/(sum(values[1:12])/12)*100,color= "European Union"), size = 1.25) +
  geom_line(data=US_INDPRO, aes(x = date, y = value/sum((value[1:12])/12)*100, color = "United States"), size = 1.25) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,135), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Index, 2018 Avg = 100") +
  ggtitle("Cars and The Global Chip Shorage") +
  labs(caption = "Graph created by @JosephPolitano using METI, MOSPI, NBSS, Eurostat, and Federal Reserve Data",subtitle = "The Global Recovery From the Chips Shortage is Continuing") +
  theme_apricitas + theme(legend.position = c(.2,.25)) +
  scale_color_manual(name= "Production of Motor Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United States","European Union","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EU_JPN_Production, "US EU JPN Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

KOR_INDPRO <- statSearch(lang = "en",stat_code = "901Y032", item_code1 = "I11ACU",item_code2 = "2", start_time = "201801", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))


EU_JPN_CAN_MEX_KOR_Production <- ggplot() + #plotting MOVE
  #geom_line(data=CHINA_IND_PRO_MV, aes(x=date,y= `Output of Motor Vehicles, Current Period`/(sum(`Output of Motor Vehicles, Current Period`[1:11])/11)*100,color= "China"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP, date >= as.Date("2018-01-01")), aes(x=date,y= motor_vehicles/(sum(motor_vehicles[1:12])/12)*100,color= "Japan"), size = 1.25) +
  #geom_line(data=INDIA, aes(x=Date,y= Value/(sum(Value[1:12])/12)*100,color= "India"), size = 1.25) +
  geom_line(data=EU_MOTOR_VEHICLE_SUBSET, aes(x=time,y= values/(sum(values[1:12])/12)*100,color= "European Union"), size = 1.25) +
  geom_line(data = CANADA_INDPRO, aes(x = REF_DATE, y = VALUE/(sum(VALUE[1:12])/12)*100, color = "Canada"), size = 1.25) +
  geom_line(data = KOR_INDPRO, aes(x = time, y = data_value/(sum(data_value[1:12])/12)*100, color = "South Korea"), size = 1.25) +
  geom_line(data = MEXICO_INDPO, aes(x = Date, y = MX_INDPRO_TEP/(sum(MX_INDPRO_TEP[1:12])/12)*100, color = "Mexico\n(Transportation Equipment)"), size = 1.25) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,135), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Index, 2018 Avg = 100") +
  ggtitle("Global Car Production is Recovering") +
  labs(caption = "Graph created by @JosephPolitano using METI, MOSPI, NBSS, Eurostat, and Federal Reserve Data",subtitle = "The Global Recovery From the Chips Shortage is Continuing") +
  theme_apricitas + theme(legend.position = c(.2,.25)) +
  scale_color_manual(name= "Production of Motor Vehicles",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Mexico\n(Transportation Equipment)","Canada","European Union","Japan","South Korea")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_JPN_CAN_MEX_KOR_Production, "EU JPN CAN MEX KOR Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


Manheim_Bulk <- read.xlsx("https://site.manheim.com/wp-content/uploads/sites/2/2023/11/Oct-2023-ManheimUsedVehicleValueIndex-web-table-data.xlsx") %>%
  mutate(date = seq.Date(from = as.Date("1997-01-01"), by = "month", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-11-01"))

Used_NSA_prices <- fredr("CUUR0000SETA02", observation_start = as.Date("2019-01-01"))

CPI_Manheim_Used_Car_Vehicles_NSA_Graph <- ggplot() + #plotting "Used Cars and Trucks" and "Mannheim" price Indexes
  geom_line(data=Used_NSA_prices, aes(x=date,y= (value/value[1])*100 ,color= "CPI: Used Cars and Trucks NSA"), size = 1.25) +
  geom_line(data=subset(Manheim_Bulk), aes(x=date + 60,y= (`Manheim.Index.$.amount.NSA`/`Manheim.Index.$.amount.NSA`[1])*100 ,color= "Manheim Used Vehicles Value Index NSA, 2 Months Forward"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,200), breaks = c(90,120,150,180), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Manheim data",subtitle = "Removing Seasonal Adjustments, it's Clear Manheim Consistently Leads CPI by About 2 Months") +
  theme_apricitas + theme(legend.position = c(.40,.875)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 90-(.3*110), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Manheim_Used_Car_Vehicles_NSA_Graph, "Manheim Used NSA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

Motor_Vehicle_Sales <- fredr("ALTSALES", observation_start = as.Date("2018-01-01"))

Sales_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=Motor_Vehicle_Sales, aes(x=date,y= value, color = "US Total Automobile & Light-Duty Truck Sales"), size = 1.25)+ 
  annotate(geom = "hline", y = 16.961, yintercept = 16.961, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "2019 Average", x = as.Date("2022-07-01"), y = 17.5, color ="#FFE98F") +
  #geom_line(data=AssembliesNSA, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Motor Vehicle Sales, Millions, Annual Rate") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,20), breaks = c(0,5,10,15,20), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Shoring up the Showroom") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Motor Vehicle Sales Have Almost Climbed Back to Their Pre-Pandemic Average") +
  theme_apricitas + theme(legend.position = c(0.65,0.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*20), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Sales_Graph, "Motor Vehicle Sales.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

Inventory <- fredr(series_id = "N864RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

Vehicle_Inventory_Graph <- ggplot() + #plotting real private auto inventories
  geom_line(data=Inventory, aes(x=date,y= value,color= "Real Private Inventories: Retail trade: Motor vehicle and Parts Dealers"), size = 1.25)+ 
  xlab("Date") +
  ylab("Inventories, US Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(150,275), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Dealership Inventories are Recovering") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Vehicle Inventories Have Risen 12% From 2021 Lows") +
  theme_apricitas + theme(legend.position = c(.46,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 150-(.3*125), ymax = 150) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Vehicle_Inventory_Graph, "Motor Vehicle Inventory.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CON_AUTO <- fredr(series_id = "STDSAUTO",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

CON_AUTO_LOANS_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=CON_AUTO, aes(x=date,y= value/100,color= "Net Percentage of Domestic Banks Tightening Standards, Auto Loans"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.20,0,.20,.40,.60), limits = c(-.30,.65), expand = c(0,0)) +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Banks are Rapidly Tightening Lending Standards for Auto Loans") +
  theme_apricitas + theme(legend.position = c(.475,.98)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-04-01")-(.1861*(today()-as.Date("2011-04-01"))), xmax = as.Date("2011-04-01")-(0.049*(today()-as.Date("2011-04-01"))), ymin = -0.30-(.3*0.95), ymax = -0.30) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CON_AUTO_LOANS_Graph, "CON Auto Loan Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

LOAN_60 <- fredr("RIFLPBCIANM60NM", observation_start = as.Date("2018-01-01")) %>%
  drop_na()

LOANS_RATES_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=LOAN_60, aes(x=date,y= value/100,color= "Finance Rate on 60-Month New Auto Loans at Commercial Banks"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent Interest") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.02,0.04,0.06,0.08,0.10), limits = c(0,0.10), expand = c(0,0)) +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Auto Loan Interest Rates Have Risen as the Fed Tightened Monetary Policy") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-02-01")-(.1861*(today()-as.Date("2018-02-01"))), xmax = as.Date("2018-02-01")-(0.049*(today()-as.Date("2018-02-01"))), ymin = 0-(.3*0.10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LOANS_RATES_Graph, "Loan Rates Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

RETAIL_IS_MOTOR <- fredr(series_id = "MRTSIR441USS", observation_start = as.Date("2018-01-01"))
RETAIL_IS_EX_MOTOR <- fredr(series_id = "MRTSIR4400AUSS", observation_start = as.Date("2018-01-01"))

RETAIL_IS_graph <- ggplot() + #plotting components of annual inflation
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 0.5) +
  geom_line(data = RETAIL_IS_MOTOR, aes(x = date, y = value/value[1]*100, color = "Motor Vehicles and Parts"), size = 1.25) +
  geom_line(data = RETAIL_IS_EX_MOTOR, aes(x = date, y = value/value[1]*100, color = "Excluding Motor Vehicles and Parts"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(50,151), breaks = c(0,25,50,75,100,125,150), expand = c(0,0)) +
  ylab("Index, Jan 2018=100") +
  ggtitle("Slow Recovery in Dealer Inventories") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Inventory/Sales Have Normalized For Most Retailers, But are Still Slowly Recovering For Dealerships") +
  theme_apricitas + theme(legend.position = c(.75,.75)) +
  scale_color_manual(name= "Retail Inventory/Sales Ratio\nIndexed to Jan 2018",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Motor Vehicles and Parts","Excluding Motor Vehicles and Parts")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 50-(.3*101), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_IS_graph, "Retail IS Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

NET_IMPORTS <- fredr(series_id = "AB63RX1Q020SBEA", observation_start = as.Date("2018-01-01"))

NET_IMPORTS_Graph <- ggplot() + #plotting real private auto inventories
  geom_line(data=NET_IMPORTS, aes(x=date,y= value*-1,color= "US Net Real Imports of Motor Vehicles"), size = 1.25)+ 
  xlab("Date") +
  ylab("Billions of Chained 2017 US Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,175), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("US Vehicle Imports Have Recovered") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "US Vehicle Imports Hit a New Record High in Q1 2023") +
  theme_apricitas + theme(legend.position = c(.46,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*175), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NET_IMPORTS_Graph, "Net Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

AUTO_LOAN_ORIGINS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Global%20Car%20Shortage/AUTO_LOAN_ORIGIN.csv") %>%
  select(origin) %>%
  mutate(origin = as.numeric(origin)) %>%
  ts(., frequency = 4, start = c(2004, 1)) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2004-04-01"), to = as.Date("2023-04-01"),by = "3 months"))

LOAN_ORIGINS_graph <- ggplot() + #plotting real private auto inventories
  geom_line(data=AUTO_LOAN_ORIGINS, aes(x=date,y= x,color= "US Quarterly Auto Loan Originations"), size = 1.25)+ 
  xlab("Date") +
  ylab("Billions of US Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,200), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("US Auto Lending is Declining") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY consumer credit data seasonally adjusted with X-13ARIMA", subtitle = "US Auto Loan Originations Have Fallen From Pandemic-era Highs") +
  theme_apricitas + theme(legend.position = c(.46,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-04-01")-(.1861*(today()-as.Date("2004-04-01"))), xmax = as.Date("2004-04-01")-(0.049*(today()-as.Date("2004-04-01"))), ymin = 0-(.3*200), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LOAN_ORIGINS_graph, "Loan Origins Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

SERIOUS_DELINQUENCY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Global%20Car%20Shortage/transition_to_serious_delinquency.csv") %>%
  select(srs_delinq) %>%
  mutate(srs_delinq = as.numeric(srs_delinq)) %>%
  mutate(date = seq(from = as.Date("2003-04-01"), to = as.Date("2023-04-01"),by = "3 months"))

SERIOUS_DELINQUENCY_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=SERIOUS_DELINQUENCY, aes(x=date,y= srs_delinq/100,color= "% of Auto Loans Transitioning to Serious (90+ Day) Delinquency, 4Q Moving Sum"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of Total Balances") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03), limits = c(0,.036), expand = c(0,0)) +
  ggtitle("Car Loan Delinquencies are Normalizing") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Consumer Credit data", subtitle = "Consumer Auto Loan Delinquency Rates are Returning to Pre-Pandemic Norms") +
  theme_apricitas + theme(legend.position = c(.52,.29)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-04-01")-(.1861*(today()-as.Date("2003-04-01"))), xmax = as.Date("2003-04-01")-(0.049*(today()-as.Date("2003-04-01"))), ymin = 0-(.3*0.036), ymax = -0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SERIOUS_DELINQUENCY_Graph, "Serious Delinquency Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
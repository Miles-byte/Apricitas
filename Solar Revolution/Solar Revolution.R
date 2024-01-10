pacman::p_load(readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Using an updated version of the Chinese national stats bureau rstatscn package that fixes a json error in the old database
install_github("pcdi/rstatscn")
library(rstatscn)

devtools::install_github("jameelalsalam/eia2")
library("eia2")


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

IND_PRO_PV <- statscnQueryData('A02092E',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_PV <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(.,`Output of Photovoltaic Cells, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Photovoltaic Cells, Current Period`,11))) %>%
  subset(date >= as.Date("2016-01-01"))

IND_PRO_PV1 <- IND_PRO_PV %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(.,`Output of Photovoltaic Cells, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Photovoltaic Cells, Current Period`,11))) %>%
  subset(date >= as.Date("2016-01-01"))

IND_PRO_PV_GRAPH <- ggplot() + #plotting Chinese PV Production
  geom_line(data= IND_PRO_PV1, aes(x=date,y=rollmean/100,color= "Rolling 1-year Average"), size = 1.25) +
  geom_line(data= IND_PRO_PV1, aes(x=date,y=`Output of Photovoltaic Cells, Current Period`/100 ,color= "Chinese Industrial Production of Photovoltaic Cells, Monthly"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"),limits = c(0, ceiling(max(IND_PRO_PV1$`Output of Photovoltaic Cells, Current Period`/1000))*10), expand = c(0,0)) +
  ylab("GW of Capacity, Monthly") +
  ggtitle("Chinese Solar Panel Production") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Solar Production is Growing Exponentially and Has Surged Post-Pandemic") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_PV1$date)-(.1861*(max(IND_PRO_PV1$date)-min(IND_PRO_PV1$date))), xmax = min(IND_PRO_PV1$date)-(0.049*(max(IND_PRO_PV1$date)-min(IND_PRO_PV1$date))), ymin = 0-(.3*(ceiling(max(IND_PRO_PV1$`Output of Photovoltaic Cells, Current Period`/1000))*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_PV_GRAPH, "China Ind Pro PV Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Must update links
CHINA_SOLAR_EXPORTS <- read.csv("https://ember-climate.org/app/uploads/2023/06/china_solar_exports_data.csv") %>%
  setNames(c("Area","date","Areatype","Region","Items","KG","USD","Price_per_MW","Capacity_MW")) %>%
  group_by(Area) %>%
  filter(n() >= 12) %>%
  mutate(date = as.Date(date)) %>%
  arrange("date") %>%
  mutate(rollmean_USD = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(USD,12)), rollmean_Capacity = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(Capacity_MW,12))) %>%
  ungroup()

CHINA_PV_EXPORTS_INDIA_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  geom_line(data= filter(CHINA_SOLAR_EXPORTS, Area == "India"), aes(x=date,y=Capacity_MW/1000,color= "GW of Capacity, Monthly"), size = 1.25) +
  annotate("vline", x = as.Date("2022-04-01"), xintercept = as.Date("2022-04-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "India Enacts Tariffs",x = as.Date("2023-01-01"), y = 4, size = 4,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"),limits = c(0, 5.1), expand = c(0,0)) +
  ylab("GW of Capacity, Monthly") +
  ggtitle("Chinese Solar Panel Exports to India") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate Data",subtitle = "Chinese Solar Exports to India Have Fallen Dramatically After Tariffs Came Into Effect") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*((today()-as.Date("2017-01-01")))), ymin = 0-(.3*(5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_PV_EXPORTS_INDIA_GRAPH, "China PV Exports India.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CHINA_PV_EXPORTS_Stacked <- ggplot(CHINA_SOLAR_EXPORTS %>%
                                            filter(Area %in% c("Africa","Asia","Europe","North America","Oceania","South America","Latin America and Caribbean","Middle East")) %>%
                                            filter(rollmean_Capacity != 0) %>%
                                            mutate(Area = factor(Area, rev(c("Europe","Asia","Latin America and Caribbean","Middle East","Africa","Oceania","North America")))), aes(fill=Area, x=date, y=rollmean_USD/1000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  ylab("GW of Capacity, Monthly Average") + 
  scale_y_continuous(labels = scales::number_format(suffix = "GW"), breaks = c(0,5,10,15), limits = c(0,17.5), expand = c(0,0)) +
  ggtitle("China's Booming Solar Panel Exports") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data", subtitle = "China Solar Exports are Rising Rapidly, Especially to Europe") +
  theme_apricitas + theme(legend.position = c(.4,.68), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "GW of Capacity, 12M Moving Average",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#F5B041","#9A348E","#00A99D","#EE6055","#FFE98F")),breaks = c("Europe","Asia","Latin America and Caribbean","Middle East","Africa","Oceania","North America")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*17.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = CHINA_PV_EXPORTS_Stacked, "China PV Exports Stacked.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Must Update Links
MONTHLY_ELECTRICITY_GLOBAL <- read.csv("https://ember-climate.org/app/uploads/2022/07/monthly_full_release_long_format-4.csv")

WORLD_GENERATION_SOLAR_MONTHLY <- MONTHLY_ELECTRICITY_GLOBAL %>%
  filter(Area == "World" & Category == "Electricity generation" & Subcategory == "Fuel" & Unit == "TWh" & Variable == "Solar") %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(Value, k= 12))) %>%
  mutate(date = as.Date(Date))

YEARLY_ELECTRICITY_GLOBAL <- read.csv("https://ember-climate.org/app/uploads/2022/07/yearly_full_release_long_format.csv") %>%
  filter(Area == "World" & Category == "Electricity generation" & Subcategory == "Fuel" & Unit == "TWh" & Variable == "Solar")

WORLD_GENERATION_SOLAR_YEARLY <- YEARLY_ELECTRICITY_GLOBAL %>%
  filter(Area == "World" & Category == "Electricity generation" & Subcategory == "Fuel" & Unit == "TWh" & Variable == "Solar") %>%
  mutate(date = as.Date(paste0(Year,"-01-01")))

WORLD_GENERATION_SOLAR_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  geom_line(data= filter(WORLD_GENERATION_SOLAR_MONTHLY, rollsum > 0), aes(x=date,y=rollsum/1000,color= "Rolling 12M Total of Monthly Data"), size = 1.25) +
  geom_line(data= WORLD_GENERATION_SOLAR_YEARLY, aes(x=date+365,y=Value/1000,color= "Yearly Totals"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "PWh"),limits = c(0, 1.5), expand = c(0,0)) +
  ylab("Global Solar Power Production, PWh") +
  ggtitle("Solar's Exponential Moment") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate Data.\nNote: Monthly Data Covers Geographies Representing 90% of Global Power Demand",subtitle = "Global Solar Output is Growing at an Exponential Rate") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  scale_color_manual(name= "Global Solar Electricity Generation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Yearly Totals","Rolling 12M Total of Monthly Data")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*((today()-as.Date("2000-01-01")))), ymin = 0-(.3*(1.5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WORLD_GENERATION_SOLAR_GRAPH, "World Generation Solar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_EV_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "870380",
)

US_EV_IMPORTS <- US_EV_IMPORTS_BULK %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  pivot_wider()

US_EV_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "870380",
)

US_EV_EXPORTS <- US_EV_EXPORTS_BULK %>%
  mutate(value = as.numeric(ALL_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  pivot_wider()

US_NET_EV_EXPORTS <- merge(US_EV_EXPORTS %>% select(`Total For All Countries`,`European Union`,`Mexico`,`Korea, South`,`Japan`,`China`,`Hong Kong`,`Macau`,`date`),US_EV_IMPORTS %>% select(`Total For All Countries`,`European Union`,`Mexico`,`Korea, South`,`Japan`,`China`,`date`), by = "date") %>%
  mutate(Mexico.y = replace_na(Mexico.y, 0)) %>%
  mutate(date, `Net Exports`=`Total For All Countries.x`-`Total For All Countries.y`, `European Union`=`European Union.x`-`European Union.y`, `South Korea`=`Korea, South.x`-`Korea, South.y`, `Mexico` = `Mexico.x`-`Mexico.y`, `Japan` = `Japan.x`-`Japan.y`, `China` = `China.x`-`China.y`) %>%
  mutate(Mexico.y = replace_na(Mexico.y, 0)) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollEU = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`European Union.y`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Korea, South.y`,12))) %>%
  mutate(rollMX = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Mexico.y`,12))) %>%
  mutate(rollJP = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Japan.y`,12))) %>%
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China.y`,12)))


US_NET_EV_IMPORTS_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 8.45, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`Net Exports`*12)/1000000000,color= "US Net Exports of Electric Vehicles, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollnetexports`)/1000000000,color= "US Net Exports of Electric Vehicles, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(-15, 12.5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Trade") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The US has Become a Major Net Importer of Finished Electric Vehicles") +
  theme_apricitas + theme(legend.position = c(.375,.15)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Net Exports of Electric Vehicles, Rolling 12M Total","US Net Exports of Electric Vehicles, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -15-(.3*(27.5)), ymax = -15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = US_NET_EV_IMPORTS_GRAPH, "US EV Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

  
US_EV_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 8.45, size = 4,color = "white", lineheight = 0.8) +
  annotate(geom = "vline",x = as.Date("2022-12-29"), xintercept = as.Date("2022-12-29"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "EV Leasing\nCredit\nAnnounced",x = as.Date("2023-05-15"), y = 8.45, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`China.y`*12/1000000000,color= "China, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union.y`*12/1000000000,color= "EU, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico.y`*12/1000000000,color= "Mexico, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South.y`*12/1000000000,color= "South Korea, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan.y`*12/1000000000,color= "Japan, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollCN/1000000000,color= "China, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollJP/1000000000,color= "Japan, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollSK/1000000000,color= "South Korea, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollMX/1000000000,color= "Mexico, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollEU/1000000000,color= "EU, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,9),breaks = c(0,3,6,9), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data Via Chad Bown",subtitle = "US Imports of Finished Electric Vehicles Have Increased Significantly Even After the IRA") +
  theme_apricitas + theme(legend.position = c(.35,.675)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#EE6055","#A7ACD9","#A7ACD9","#9A348E","#9A348E"), breaks = c("EU, Rolling 12M Total","EU, Monthly Annualized","South Korea, Rolling 12M Total","South Korea, Monthly Annualized","Mexico, Rolling 12M Total","Mexico, Monthly Annualized","Japan, Rolling 12M Total","Japan, Monthly Annualized","China, Rolling 12M Total","China, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2,1,2,1,2,1,2), lwd = c(1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75), alpha = c(1,0.5,1,0.5,1,0.5,1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(9)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EV_IMPORTS_BREAKDOWN_GRAPH, "US EV Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#US SOLAR PRODUCTION
US_SOLAR_SHIPMENTS_ANNUAL <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/US_SOLAR_SHIPMENTS_ANNUAL.csv") %>%
  mutate(date = as.Date(date))
US_SOLAR_SHIPMENTS_MONTHLY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/US_SOLAR_SHIPMENTS_MONTHLY.csv") %>%
  mutate(date = as.Date(date)) %>%
  drop_na() %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(shipments,12)))


US_SHIPMENTS_PV_GRAPH <- ggplot() + #plotting Chinese PV Production
  geom_line(data= US_SOLAR_SHIPMENTS_ANNUAL, aes(x=date,y=shipments/1000000,color= "Monthly Average of Annual Data"), size = 1.25) +
  geom_line(data= US_SOLAR_SHIPMENTS_MONTHLY, aes(x=date,y=rollmean/1000000,color= "Rolling 1-Year Average of Monthly Data"), size = 1.25) +
  geom_line(data= US_SOLAR_SHIPMENTS_MONTHLY, aes(x=date,y=shipments/1000000,color= "Monthly Data"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"),limits = c(0, 3.5), expand = c(0,0)) +
  ylab("GW of Capacity, Monthly") +
  ggtitle("US Solar Panel Shipments") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "US Solar Shipments is Hitting New Highs as the Country Continues to Add Capacity") +
  theme_apricitas + theme(legend.position = c(.35,.62)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= "Solar Panel Shipments in the US\nIncluding Imports\nGW of Capacity",values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Monthly Average of Annual Data","Rolling 1-Year Average of Monthly Data","Monthly Data"), guide = guide_legend(override.aes = list(linetype = c(1,1,2), lwd = c(1.25,1.25,0.75), alpha = c(1,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-12-01")-(.1861*(today()-as.Date("2006-12-01"))), xmax = as.Date("2006-12-01")-(0.049*((today()-as.Date("2006-12-01")))), ymin = 0-(.3*(3.5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_SHIPMENTS_PV_GRAPH, "US Shipments PV Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")





US_BATTERY_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8507",
)

US_BATTERY_IMPORTS <- US_BATTERY_IMPORTS_BULK %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  pivot_wider()

US_BATTERY_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "8507",
)

US_BATTERY_EXPORTS <- US_BATTERY_EXPORTS_BULK %>%
  mutate(value = as.numeric(ALL_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  pivot_wider()

US_NET_BATTERY_EXPORTS <- merge(US_BATTERY_EXPORTS %>% select(`Total For All Countries`,`European Union`,`Mexico`,`Korea, South`,`Japan`,`China`,`Hong Kong`,`Macau`,`date`),US_BATTERY_IMPORTS %>% select(`Total For All Countries`,`European Union`,`Mexico`,`Korea, South`,`Japan`,`China`,`Hong Kong`,`Macau`,`date`), by = "date") %>%
  mutate(Mexico.y = replace_na(Mexico.y, 0)) %>%
  mutate(`Hong Kong.y` = replace_na(`Hong Kong.y`,0)) %>%
  mutate(`Hong Kong.x` = replace_na(`Hong Kong.x`,0)) %>%
  mutate(`Macau.x` = replace_na(`Macau.x`,0)) %>%
  mutate(`Macau.y` = replace_na(`Macau.y`,0)) %>%
  mutate(date, `Net Exports`=`Total For All Countries.x`-`Total For All Countries.y`, `European Union`=`European Union.x`-`European Union.y`, `South Korea`=`Korea, South.x`-`Korea, South.y`, `Mexico` = `Mexico.x`-`Mexico.y`, `Japan` = `Japan.x`-`Japan.y`, `China` = `China.x` + `Hong Kong.x` + `Macau.x` - `China.y` - `Hong Kong.y` - `Macau.y`) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollEU = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`European Union`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`South Korea`,12))) %>%
  mutate(rollMX = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Mexico`,12))) %>%
  mutate(rollJP = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Japan`,12))) %>%
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China`,12)))

US_NET_BATTERY_IMPORTS_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 8.45, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`Net Exports`*12)/1000000000,color= "US Net Imports of Rechargeable Batteries, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollnetexports`)/1000000000,color= "US Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0, 31), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The US has Become a Major Net Importer of Batteries Post-COVID & Post-IRA") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Net Imports of Rechargeable Batteries, Rolling 12M Total","US Net Imports of Rechargeable Batteries, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(31)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NET_BATTERY_IMPORTS_GRAPH, "US Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_NET_BATTERY_EXPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 15, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`European Union`*12/1000000000,color= "EU, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Mexico`*12/1000000000,color= "Mexico, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`South Korea`*12/1000000000,color= "South Korea, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Japan`*12/1000000000,color= "Japan, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`China`*12/1000000000,color= "China, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollJP/1000000000,color= "Japan, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollSK/1000000000,color= "South Korea, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollMX/1000000000,color= "Mexico, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollEU/1000000000,color= "EU, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollCN/1000000000,color= "China, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,20),breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "US Imports of Rechargeable Batteries Have Surged, Especially From China") +
  theme_apricitas + theme(legend.position = c(.25,.60)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= "US Net Imports of Rechargeable Batteries",values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#EE6055","#A7ACD9","#A7ACD9","#9A348E","#9A348E"), breaks = c("EU, Rolling 12M Total","EU, Monthly Annualized","South Korea, Rolling 12M Total","South Korea, Monthly Annualized","Mexico, Rolling 12M Total","Mexico, Monthly Annualized","Japan, Rolling 12M Total","Japan, Monthly Annualized","China, Rolling 12M Total","China, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2,1,2,1,2,1,2), lwd = c(1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75), alpha = c(1,0.5,1,0.5,1,0.5,1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(20)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NET_BATTERY_EXPORTS_BREAKDOWN_GRAPH, "US Net Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_EV_BATTERY_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8507204000",
  I_COMMODITY = "8507304000",
  I_COMMODITY = "8507404000",
  I_COMMODITY = "8507600010",
  I_COMMODITY = "8507804000",
  I_COMMODITY = "8507804100",
)

US_EV_BATTERY_IMPORTS <- US_EV_BATTERY_IMPORTS_BULK %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  group_by(date, name) %>%
  summarise(value = sum(as.numeric(value))) %>%
  ungroup() %>%
  pivot_wider()

US_EV_BATTERY_IMPORTS_BREAKDOWN <- US_EV_BATTERY_IMPORTS %>%
  mutate(Mexico = replace_na(Mexico, 0)) %>%
  mutate(`Hong Kong` = replace_na(`Hong Kong`,0)) %>%
  mutate(`Macau` = replace_na(`Macau`,0)) %>%
  mutate(date, `Net Exports`=`Total For All Countries`, `China` = `China` + `Hong Kong` + `Macau`) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollEU = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`European Union`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Korea, South`,12))) %>%
  mutate(rollMX = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Mexico`,12))) %>%
  mutate(rollJP = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Japan`,12))) %>%
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China`,12)))

US_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 2, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union`*12/1000000000,color= "EU, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico`*12/1000000000,color= "Mexico, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South`*12/1000000000,color= "South Korea, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan`*12/1000000000,color= "Japan, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`China`*12/1000000000,color= "China, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollJP/1000000000,color= "Japan, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollSK/1000000000,color= "South Korea, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollMX/1000000000,color= "Mexico, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollEU/1000000000,color= "EU, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollCN/1000000000,color= "China, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"),limits = c(0,3.5),breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "Gross Imports of EV Batteries Have Surged Over the Last Few Years") +
  theme_apricitas + theme(legend.position = c(.25,.65)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#EE6055","#A7ACD9","#A7ACD9","#9A348E","#9A348E"), breaks = c("EU, Rolling 12M Total","EU, Monthly Annualized","South Korea, Rolling 12M Total","South Korea, Monthly Annualized","Mexico, Rolling 12M Total","Mexico, Monthly Annualized","Japan, Rolling 12M Total","Japan, Monthly Annualized","China, Rolling 12M Total","China, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2,1,2,1,2,1,2), lwd = c(1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75), alpha = c(1,0.5,1,0.5,1,0.5,1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(3.5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH, "US EV Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_BATTERY_TRADE_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.EU27_2020_EXTRA.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[product]=8507&c[flow]=1,2&compress=false"))

EU_BATTERY_TRADE <- EU_BATTERY_TRADE_BULK %>%
  transmute(name = flow, value = as.numeric(OBS_VALUE), date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_NET_BATTERY_IMPORTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_BATTERY_TRADE, date >= as.Date("2017-12-01")), aes(x=date,y=-(NET_EXPORTS*12)/1000000000,color= "EU Net Imports of Rechargeable Batteries, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_BATTERY_TRADE, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollsum`)/1000000000,color= "EU Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(0, 30), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("Europe's Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "The EU has a Yawning Deficit in the Battery Trade Despite the EV Surplus") +
  theme_apricitas + theme(legend.position = c(.415,.82)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU Net Imports of Rechargeable Batteries, Rolling 12M Total","EU Net Imports of Rechargeable Batteries, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(30)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_NET_BATTERY_IMPORTS_GRAPH, "EU NET Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EUR_USD_EXCHANGE <- fredr(series_id = "DEXUSEU", observation_start = as.Date("2016-01-01"), frequency = "m", aggregation_method = "avg")

EUR_USD_BATTERY_EXPORTS <- merge(EU_BATTERY_TRADE %>% select(date,NET_EXPORTS, rollsum),EUR_USD_EXCHANGE, by = "date") %>%
  transmute(date, `Net Exports` = NET_EXPORTS*value, rollnetexports = rollsum*value)

US_EU_NET_BATTERY_IMPORTS_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`Net Exports`*12)/1000000000,color= "US Net Imports of Rechargeable Batteries, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollnetexports`)/1000000000,color= "US Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(EUR_USD_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`Net Exports`*12)/1000000000,color= "EU Net Imports of Rechargeable Batteries, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EUR_USD_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollnetexports`)/1000000000,color= "EU Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0, 31), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("EU & US Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census and Eurostat Data",subtitle = "The US and EU are Both Now Major Net Importers of Batteries") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Net Imports of Rechargeable Batteries, Rolling 12M Total","US Net Imports of Rechargeable Batteries, Monthly Annualized","EU Net Imports of Rechargeable Batteries, Rolling 12M Total","EU Net Imports of Rechargeable Batteries, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2), lwd = c(1.25,0.75,1.25,0.75), alpha = c(1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(31)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EU_NET_BATTERY_IMPORTS_GRAPH, "US EU Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



NOMINAL_BATTERY_MANU <- fredr(series_id = "A35DVS", observation_start = as.Date("2018-01-01"))
PPI_BATTERY_MANU <- fredr(series_id = "PCU335911335911", observation_start = as.Date("2018-01-01"))

REAL_US_BATTERY_MANU <- merge(NOMINAL_BATTERY_MANU,PPI_BATTERY_MANU, by = "date") %>%
  transmute(date, value = (value.x/value.y)/(value.x[1]/value.y[1])*100)

REAL_US_BATTERY_MANU_graph <- ggplot() + #plotting real battery shipments
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 125, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data=REAL_US_BATTERY_MANU, aes(x=date,y= value,color="Real Shipments: US Battery Manufacturing"), size = 1.25) +
  annotate(geom = "text", label = "Note: Real Shipments Derived by Deflating Nominal Shipments by PPI: Battery Manufacturing",x = as.Date("2020-03-01"), y = 65, size = 3,color = "white", lineheight = 0.8, alpha = 0.5) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,150), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("America's Battery Manufacturing Boom") +
  labs(caption = "Graph created by @JosephPolitano using Census and BLS Data",subtitle = "US Battery Manufacturing Has Surged in the Wake of the Inflation Reduction Act") +
  theme_apricitas + theme(legend.position = c(.3,.67)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*90), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_US_BATTERY_MANU_graph, "Real US Battery Manufacturing graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EU_BATTERY_MANUFACTURING <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/STS_INPR_M/1.0/M.PROD.C272.SCA.I15.*?c[geo]=EU27_2020&compress=false&")) %>%
  transmute(value = as.numeric(OBS_VALUE), date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[1]*100)

REAL_EU_BATTERY_MANU_graph <- ggplot() + #plotting real battery shipments
  geom_line(data=EU_BATTERY_MANUFACTURING, aes(x=date,y= value,color="EU Industrial Production: Batteries and Accumulators"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,310), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The EU's Battery Manufacturing Boom") +
  labs(caption = "Graph created by @JosephPolitano using Census and BLS Data",subtitle = "EU Battery Manufacturing Has Grown Significantly Over the Last 5 Years") +
  theme_apricitas + theme(legend.position = c(.4,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*240), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_EU_BATTERY_MANU_graph, "Real EU Battery Manufacturing graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

US_BATTERY_CAPACITY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/US_BATTERY_CAPACITY.csv") %>%
  transmute(date = as.Date(date), capacity_MWh = as.numeric(capacity_MWh))

US_BATTERY_CAPACITY_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  geom_line(data= US_BATTERY_CAPACITY, aes(x=date,y=capacity_MWh/1000,color= "US Large-Scale Battery Storage Energy Capacity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GWh"),limits = c(0, 25), expand = c(0,0)) +
  ylab("GWh of Energy Capacity") +
  ggtitle("Battery Power's Exponential Moment") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "US Large-Scale Battery Storage Capacity is Growing Exponentially") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*((today()-as.Date("2003-01-01")))), ymin = 0-(.3*(25)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_BATTERY_CAPACITY_GRAPH, "US Battery Capacity graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GLOBAL_EV_SALES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/EV_SALES.csv") %>%
  mutate(date = as.Date(date)) %>%
  pivot_longer(cols = -date, names_to = "name", values_to = "value") %>%
  mutate(name = factor(name, levels = rev(c("China","Europe","US","Other"))))

GLOBAL_EV_SALES_graph <- ggplot(data = GLOBAL_EV_SALES, aes(x = date, y = value, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Millions of Vehicles") +
  ggtitle("EV's Exponential Moment") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(0,5,10,15), limits = c(0,15), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using IEA data", subtitle = "Global EV Sales Have Rapidly Risen Over the Last Three Years") +
  theme_apricitas + theme(legend.position = c(.425,.65)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Millions of New Electric Vehicle Sales",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("China","Europe","US","Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLOBAL_EV_SALES_graph, "Global EV Sales graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#Chile & Australia Lithium Exports
#https://www.bcentral.cl/en/web/banco-central/areas/statistics/external-sector/foreign-trade-of-goods
CHILE_LITHIUM_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/Chile_Lithium_Exports.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(exports = gsub(",","",exports)) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(exports = as.numeric(exports))

AUS_LITHIUM_EXPORTS <- read_abs(series_id = "A1827825W") %>%
  subset(date >= as.Date("2016-01-01"))

AUS_USD_EXCHANGE <- fredr(series_id = "DEXUSAL", observation_start = as.Date("2016-01-01"), frequency = "m", aggregation_method = "avg")

AUS_LITHIUM_EXPORTS <- merge(AUS_LITHIUM_EXPORTS,AUS_USD_EXCHANGE, by = "date") %>%
  transmute(date, value = value.x*value.y)

Lithium_Exports_Graph <- ggplot() + #plotting Australian Lithium Exports
  geom_line(data=AUS_LITHIUM_EXPORTS, aes(x=date,y= value/1000*12, color= "Australia, Exports of Lithium & Crude Minerals/Fertilizers n.e.s (SITC 27)"), size = 1.25) +
  geom_line(data=CHILE_LITHIUM_EXPORTS, aes(x=date,y= exports/1000*12, color= "Chile, Exports of Lithium Carbonate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,25),breaks = c(0,5,10,15,20,25,30,35), expand = c(0,0)) +
  ylab("US Dollars, Annual Rate") +
  ggtitle("Powering the Energy Transition") +
  labs(caption = "Graph created by @JosephPolitano using ABS data",subtitle = "Chilean and Australian Lithium Exports Have Skyrocketed Amidst Rising Demand") +
  theme_apricitas + theme(legend.position = c(.45,.88)) +
  scale_color_manual(name= "Exports, Billions of Dollars, Annual Rate" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Lithium_Exports_Graph, "Lithium Exports AUS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_COAL <- eia1_series("ELEC.GEN.COW.US.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Coal", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
US_NATGAS <- eia1_series("ELEC.GEN.NG.US.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Natural Gas", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
US_NUCLEAR <- eia1_series("ELEC.GEN.NUC.US.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
US_HYDRO <- eia1_series("ELEC.GEN.HYC.US.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
US_WIND <- eia1_series("ELEC.GEN.WND.US.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Wind", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
US_SOLAR <- eia1_series("ELEC.GEN.TSN.US.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
  
US_ELECTRICITY_PRODUCTION_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(US_COAL, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Coal"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NATGAS, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Natural Gas"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NUCLEAR, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Nuclear"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_HYDRO, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Hydro"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_WIND, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Wind"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Solar"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_COAL, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Coal"), size = 1.25) +
  geom_line(data= filter(US_NATGAS, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Natural Gas"), size = 1.25) +
  geom_line(data= filter(US_NUCLEAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Nuclear"), size = 1.25) +
  geom_line(data= filter(US_HYDRO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Hydro"), size = 1.25) +
  geom_line(data= filter(US_WIND, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Wind"), size = 1.25) +
  geom_line(data= filter(US_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Solar"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(US_NATGAS$value)/10000)*10), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("America's Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Natural Gas, Wind, and Solar Now Make Up A Larger Share of America's Grid") +
  theme_apricitas + theme(legend.position = c(.315,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "US Net Electricity Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = c("Coal","Natural Gas","Nuclear","Hydro","Wind","Solar"), guide = guide_legend(ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(US_NATGAS$value)/10000)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ELECTRICITY_PRODUCTION_GRAPH, "US Electricity Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_ELECTRICITY_PRODUCTION_LONG_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(US_COAL, date >= as.Date("2002-01-01")), aes(x=date,y=rollmean/1000,color= "Coal"), size = 1.25) +
  geom_line(data= filter(US_NATGAS, date >= as.Date("2002-01-01")), aes(x=date,y=rollmean/1000,color= "Natural Gas"), size = 1.25) +
  geom_line(data= filter(US_NUCLEAR, date >= as.Date("2002-01-01")), aes(x=date,y=rollmean/1000,color= "Nuclear"), size = 1.25) +
  geom_line(data= filter(US_HYDRO, date >= as.Date("2002-01-01")), aes(x=date,y=rollmean/1000,color= "Hydro"), size = 1.25) +
  geom_line(data= filter(US_WIND, date >= as.Date("2002-01-01")), aes(x=date,y=rollmean/1000,color= "Wind"), size = 1.25) +
  geom_line(data= filter(US_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Solar"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(US_NATGAS$value)/10000)*10), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("America's Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Natural Gas, Wind, and Solar Now Make Up A Larger Share of America's Grid") +
  theme_apricitas + theme(legend.position = c(.715,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "US Net Electricity Generation\n12M Moving Average",values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = c("Coal","Natural Gas","Nuclear","Hydro","Wind","Solar"), guide = guide_legend(ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*((today()-as.Date("2002-01-01")))), ymin = 0-(.3*(ceiling(max(US_NATGAS$value)/10000)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ELECTRICITY_PRODUCTION_LONG_GRAPH, "US Electricity Production Long Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



TX_COAL <- eia1_series("ELEC.GEN.COW.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Coal", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
TX_NATGAS <- eia1_series("ELEC.GEN.NG.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Natural Gas", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
TX_NUCLEAR <- eia1_series("ELEC.GEN.NUC.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
TX_HYDRO <- eia1_series("ELEC.GEN.HYC.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
TX_WIND <- eia1_series("ELEC.GEN.WND.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Wind", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
TX_SOLAR <- eia1_series("ELEC.GEN.TSN.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

TX_ELECTRICITY_PRODUCTION_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(TX_COAL, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Coal"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_NATGAS, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Natural Gas"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_NUCLEAR, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Nuclear"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_HYDRO, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Hydro"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_WIND, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Wind"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Solar"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_COAL, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Coal"), size = 1.25) +
  geom_line(data= filter(TX_NATGAS, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Natural Gas"), size = 1.25) +
  geom_line(data= filter(TX_NUCLEAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Nuclear"), size = 1.25) +
  geom_line(data= filter(TX_HYDRO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Hydro"), size = 1.25) +
  geom_line(data= filter(TX_WIND, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Wind"), size = 1.25) +
  geom_line(data= filter(TX_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Solar"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(TX_NATGAS$value)/5000)*5), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("Texas's Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Wind and Solar Now Make Up A Much Larger Share of Texas' Grid") +
  theme_apricitas + theme(legend.position = c(.315,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "Texas Net Electricity Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = c("Coal","Natural Gas","Nuclear","Hydro","Wind","Solar"), guide = guide_legend(ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(TX_NATGAS$value)/5000)*5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_ELECTRICITY_PRODUCTION_GRAPH, "TX Electricity Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CA_SOLAR <- eia1_series("ELEC.GEN.TSN.CA.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

TX_CA_SOLAR_PRODUCTION_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(TX_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "Texas"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "Texas"), size = 1.25) +
  geom_line(data= filter(CA_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=value/1000,color= "California"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(CA_SOLAR, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean/1000,color= "California"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(CA_SOLAR$value)/5000)*5), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("Solar Production in TX and CA") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Texas is Catching Up, But California Still Leads the Nation in Solar Generation") +
  theme_apricitas + theme(legend.position = c(.315,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "Solar Net Electricity Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#FFE98F","#00A99D"), breaks = c("California","Texas"), guide = guide_legend(ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(CA_SOLAR$value)/5000)*5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_CA_SOLAR_PRODUCTION_GRAPH, "TX CA Electricity Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")





ERCOT_NUC <- eia1_series("STEO.NUEPGEN_TX.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
ERCOT_HYD <- eia1_series("STEO.HVEPGEN_TX.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
ERCOT_REN <- eia1_series("STEO.RNEPGEN_TX.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

ERCOT_TOTAL <- eia1_series("STEO.TOEPGEN_TX.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

ERCOT_RBIND <- rbind(ERCOT_NUC,ERCOT_HYD,ERCOT_REN,ERCOT_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


CAISO_NUC <- eia1_series("STEO.NUEPGEN_CA.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
CAISO_HYD <- eia1_series("STEO.HVEPGEN_CA.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
CAISO_REN <- eia1_series("STEO.RNEPGEN_CA.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

CAISO_TOTAL <- eia1_series("STEO.TOEPGEN_CA.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

CAISO_RBIND <- rbind(CAISO_NUC,CAISO_HYD,CAISO_REN,CAISO_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


NW_NUC <- eia1_series("STEO.NUEPGEN_NW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
NW_HYD <- eia1_series("STEO.HVEPGEN_NW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
NW_REN <- eia1_series("STEO.RNEPGEN_NW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

NW_TOTAL <- eia1_series("STEO.TOEPGEN_NW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

NW_RBIND <- rbind(NW_NUC,NW_HYD,NW_REN,NW_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)



SW_NUC <- eia1_series("STEO.NUEPGEN_SW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
SW_HYD <- eia1_series("STEO.HVEPGEN_SW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
SW_REN <- eia1_series("STEO.RNEPGEN_SW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

SW_TOTAL <- eia1_series("STEO.TOEPGEN_SW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

SW_RBIND <- rbind(SW_NUC,SW_HYD,SW_REN,SW_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


SPP_NUC <- eia1_series("STEO.NUEPGEN_SP.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
SPP_HYD <- eia1_series("STEO.HVEPGEN_SP.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
SPP_REN <- eia1_series("STEO.RNEPGEN_SP.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

SPP_TOTAL <- eia1_series("STEO.TOEPGEN_SP.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

SPP_RBIND <- rbind(SPP_NUC,SPP_HYD,SPP_REN,SPP_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


MSO_NUC <- eia1_series("STEO.NUEPGEN_MW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
MSO_HYD <- eia1_series("STEO.HVEPGEN_MW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
MSO_REN <- eia1_series("STEO.RNEPGEN_MW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

MSO_TOTAL <- eia1_series("STEO.TOEPGEN_MW.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

MSO_RBIND <- rbind(MSO_NUC,MSO_HYD,MSO_REN,MSO_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


NY_NUC <- eia1_series("STEO.NUEPGEN_NY.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
NY_HYD <- eia1_series("STEO.HVEPGEN_NY.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
NY_REN <- eia1_series("STEO.RNEPGEN_NY.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

NY_TOTAL <- eia1_series("STEO.TOEPGEN_NY.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

NY_RBIND <- rbind(NY_NUC,NY_HYD,NY_REN,NY_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


NE_NUC <- eia1_series("STEO.NUEPGEN_NE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
NE_HYD <- eia1_series("STEO.HVEPGEN_NE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
NE_REN <- eia1_series("STEO.RNEPGEN_NE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

NE_TOTAL <- eia1_series("STEO.TOEPGEN_NE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

NE_RBIND <- rbind(NE_NUC,NE_HYD,NE_REN,NE_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)



PJM_NUC <- eia1_series("STEO.NUEPGEN_PJ.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
PJM_HYD <- eia1_series("STEO.HVEPGEN_PJ.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
PJM_REN <- eia1_series("STEO.RNEPGEN_PJ.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

PJM_TOTAL <- eia1_series("STEO.TOEPGEN_PJ.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

PJM_RBIND <- rbind(PJM_NUC,PJM_HYD,PJM_REN,PJM_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)



SERC_NUC <- eia1_series("STEO.NUEPGEN_SE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
SERC_HYD <- eia1_series("STEO.HVEPGEN_SE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
SERC_REN <- eia1_series("STEO.RNEPGEN_SE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

SERC_TOTAL <- eia1_series("STEO.TOEPGEN_SE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

SERC_RBIND <- rbind(SERC_NUC,SERC_HYD,SERC_REN,SERC_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)



FL_NUC <- eia1_series("STEO.NUEPGEN_FL.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
FL_HYD <- eia1_series("STEO.HVEPGEN_FL.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
FL_REN <- eia1_series("STEO.RNEPGEN_FL.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

FL_TOTAL <- eia1_series("STEO.TOEPGEN_FL.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

FL_RBIND <- rbind(FL_NUC,FL_HYD,FL_REN,FL_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Total > 0) %>%
  mutate(Clean_Share = (Nuclear + Hydro + Renewables)/Total)


US_NUC <- eia1_series("STEO.NUEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))
US_REN <- eia1_series("STEO.RTEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Renewables", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_TOTAL <- eia1_series("STEO.TOEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_RBIND <- rbind(US_NUC,US_REN,US_TOTAL) %>%
  select(-value) %>%
  pivot_wider(names_from = category, values_from = rollmean) %>%
  filter(Renewables > 0) %>%
  mutate(Clean_Share = (Nuclear + Renewables)/Total)

#Chart has to be reformatted 
US_ELECTRICITY_REGION_STEO_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= ERCOT_RBIND, aes(x=date,y=Clean_Share,color= "Texas (ERCOT)"), size = 1.25) +
  geom_line(data= CAISO_RBIND, aes(x=date,y=Clean_Share,color= "California"), size = 1.25) +
  geom_line(data= NW_RBIND, aes(x=date,y=Clean_Share,color= "Northwest and Rockies"), size = 1.25) +
  geom_line(data= SW_RBIND, aes(x=date,y=Clean_Share,color= "Southwest"), size = 1.25) +
  geom_line(data= SPP_RBIND, aes(x=date,y=Clean_Share,color= "Southwest Power Pool (SPP)"), size = 1.25) +
  geom_line(data= MSO_RBIND, aes(x=date,y=Clean_Share,color= "Midcontinent ISO (MISO)"), size = 1.25) +
  geom_line(data= NY_RBIND, aes(x=date,y=Clean_Share,color= "New York (NYISO)"), size = 1.25) +
  geom_line(data= NE_RBIND, aes(x=date,y=Clean_Share,color= "New England (ISO-NE)"), size = 1.25) +
  geom_line(data= PJM_RBIND, aes(x=date,y=Clean_Share,color= "Mid Atlantic (PJM)"), size = 1.25) +
  geom_line(data= SERC_RBIND, aes(x=date,y=Clean_Share,color= "Southeast (SERC)"), size = 1.25) +
  geom_line(data= FL_RBIND, aes(x=date,y=Clean_Share,color= "Florida (FRCC)"), size = 1.25) +
  geom_line(data= US_RBIND, aes(x=date,y=Clean_Share,color= "US Total"), size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0, 0.70), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("America's Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Natural Gas, Wind, and Solar Now Make Up A Larger Share of America's Grid") +
  theme_apricitas + theme(legend.position = c(.315,.85), legend.key.height = unit(0, "cm")) #+
  # scale_color_manual(name= "US Net Electricity Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = c("Coal","Natural Gas","Nuclear","Hydro","Wind","Solar"), guide = guide_legend(ncol = 2)) +
  # annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(US_NATGAS$value)/10000)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  # coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ELECTRICITY_REGION_STEO_GRAPH, "US Electricity Region STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

  
US_GAS_STEO <- eia1_series("STEO.NGEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nat Gas", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_COL_STEO <- eia1_series("STEO.CLEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Coal", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_NUC_STEO <- eia1_series("STEO.NUEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Nuclear", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_HYD_STEO <- eia1_series("STEO.HVEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Hydro", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_WND_STEO <- eia1_series("STEO.WNEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Wind", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_SOL_STEO <- eia1_series("STEO.SOEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_ELECTRICITY_PRODUCTION_STEO_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(NAT_GAS_PRODUCTION$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -475), "month"), y = 205, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data= filter(US_COL_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Coal"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_GAS_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Natural Gas"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NUC_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Nuclear"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_HYD_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Hydro"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_WND_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Wind"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_SOL_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Solar"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_COL_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Coal"), size = 1.25) +
  geom_line(data= filter(US_GAS_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Natural Gas"), size = 1.25) +
  geom_line(data= filter(US_NUC_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Nuclear"), size = 1.25) +
  geom_line(data= filter(US_HYD_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Hydro"), size = 1.25) +
  geom_line(data= filter(US_WND_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Wind"), size = 1.25) +
  geom_line(data= filter(US_SOL_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Solar"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(US_GAS$value)/10)*10+10), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("America's Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Natural Gas, Wind, and Solar Now Make Up A Larger Share of America's Grid") +
  theme_apricitas + theme(legend.position = c(.3,.86), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "US Net Utility-Scale Electricity Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = c("Coal","Natural Gas","Nuclear","Hydro","Wind","Solar"), guide = guide_legend(ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(US_GAS$value)/10)*10+10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ELECTRICITY_PRODUCTION_STEO_GRAPH, "US Electricity Production STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_BATTERY_STEO <- eia1_series("STEO.BAEPCGW_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_BATTERY_STORAGE_STEO_GRAPH <- ggplot() + 
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(US_BATTERY_STEO$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -475), "month"), y = 30, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data= filter(US_BATTERY_STEO, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "US Battery Storage Power Capacity, GW"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "GW"),limits = c(0, ceiling(max(US_BATTERY_STEO$value)/10)*10), expand = c(0,0)) +
  ylab("GW") +
  ggtitle("America's Battery Boom") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "US Battery Storage Capacity is Booming and Projected to Nearly-Triple by 2025") +
  theme_apricitas + theme(legend.position = c(.3,.86), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= NULL,values = rev(c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(US_BATTERY_STEO$value)/10)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_BATTERY_STORAGE_STEO_GRAPH, "US Battery Storage STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

pacman::p_load(censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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

IND_PRO_PV <- statscnQueryData('A02092E',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_PV <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(.,`Output of Photovoltaic Cells, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Photovoltaic Cells, Current Period`,11))) %>%
  subset(date >= as.Date("2016-01-01"))

IND_PRO_PV_GRAPH <- ggplot() + #plotting Chinese PV Production
  geom_line(data= IND_PRO_PV, aes(x=date,y=rollmean/100,color= "Rolling 1-year Average"), size = 1.25) +
  geom_line(data= IND_PRO_PV, aes(x=date,y=`Output of Photovoltaic Cells, Current Period`/100 ,color= "Chinese Industrial Production of Photovoltaic Cells, Monthly"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"),limits = c(0, round(max(IND_PRO_PV$`Output of Photovoltaic Cells, Current Period`/1000))*10), expand = c(0,0)) +
  ylab("GW of Capacity, Monthly") +
  ggtitle("Chinese Solar Panel Production") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Solar Production is Growing Exponentially and Has Surged Post-Pandemic") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_PV$date)-(.1861*(max(IND_PRO_PV$date)-min(IND_PRO_PV$date))), xmax = min(IND_PRO_PV$date)-(0.049*(max(IND_PRO_PV$date)-min(IND_PRO_PV$date))), ymin = 0-(.3*(round(max(IND_PRO_PV$`Output of Photovoltaic Cells, Current Period`/1000))*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  geom_line(data= WORLD_GENERATION_SOLAR_YEARLY, aes(x=date+365,y=Value/1000,color= "Yearly Totals"), size = 1.25) +
  geom_line(data= filter(WORLD_GENERATION_SOLAR_MONTHLY, rollsum > 0), aes(x=date,y=rollsum/1000,color= "Rolling 12M Total of Monthly Data"), size = 1.25) +
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

EU_EV_TRADE_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.EU27_2020_EXTRA.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[product]=870380&c[flow]=1,2&compress=false"))

GER_EV_TRADE_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.WORLD.*.*.VALUE_IN_EUROS?c[reporter]=DE&c[product]=870380&c[flow]=1,2&compress=false"))

#1 = IMPORT
#2 = EXPORT


EU_NET_EV_EXPORTS <- EU_EV_TRADE_BULK %>%
  transmute(name = flow, value = as.numeric(OBS_VALUE), date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))
  
GER_NET_EV_EXPORTS <- GER_EV_TRADE_BULK %>%
  transmute(name = flow, value = as.numeric(OBS_VALUE), date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_NET_EV_EXPORTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "Monthly Net Exports, Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(GER_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "Monthly Net Exports, Annualized "), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(GER_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsum`)/1000000000,color= "German Net Exports of Electric Vehicles, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(EU_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsum`)/1000000000,color= "EU Net Exports of Electric Vehicles, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(-7.5, 30), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Europe's Growing EV Trade") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "EU EV Exports are Booming, Especially in Germany") +
  theme_apricitas + theme(legend.position = c(.415,.82)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU Net Exports of Electric Vehicles, Rolling 12M Total","Monthly Net Exports, Annualized","German Net Exports of Electric Vehicles, Rolling 12M Total","Monthly Net Exports, Annualized "), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2), lwd = c(1.25,0.75, 1.25,0.75), alpha = c(1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -7.5-(.3*(37.5)), ymax = -7.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_NET_EV_EXPORTS_GRAPH, "EU NET EV Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_IMPORTS_EXPORTS_CHINA <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=CN,HK,MO&c[product]=870380&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_CHINA_MOD <- EU_IMPORTS_EXPORTS_CHINA %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12)))

EU_EV_EXPORTS_IMPORTS_CHINA_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`1`*12)/1000000000,color= "Monthly Imports, Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`2`*12)/1000000000,color= "Monthly Exports, Annualized "), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumimports`)/1000000000,color= "EU Imports of Electric Vehicles From China, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumexports`)/1000000000,color= "EU Exports of Electric Vehicles to China, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(0, 16), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Europe's Growing EV Trade") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data  Note: China Includes HK and MO",subtitle = "EU EV Exports are Booming, Especially in Germany") +
  theme_apricitas + theme(legend.position = c(.4,.89)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU Imports of Electric Vehicles From China, Rolling 12M Total","Monthly Imports, Annualized","EU Exports of Electric Vehicles to China, Rolling 12M Total","Monthly Exports, Annualized "), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2), lwd = c(1.25,0.75, 1.25,0.75), alpha = c(1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(16)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_EV_EXPORTS_IMPORTS_CHINA_GRAPH, "EU EV Imports Exports China Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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

US_NET_EV_EXPORTS <- merge(US_EV_EXPORTS %>% select(`Total For All Countries`,`European Union`,`Mexico`,`Korea, South`,`Japan`,`date`),US_EV_IMPORTS %>% select(`Total For All Countries`,`European Union`,`Mexico`,`Korea, South`,`Japan`,`date`,`date`), by = "date") %>%
  mutate(date, `Net Exports`=`Total For All Countries.x`-`Total For All Countries.y`, `European Union`=`European Union.x`-`European Union.y`, `South Korea`=`Korea, South.x`-`Korea, South.y`, `Mexico` = `Mexico.x`-`Mexico.y`, `Japan` = `Japan.x`-`Japan.y`) %>%
  mutate(Mexico.y = replace_na(Mexico.y, 0)) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollEU = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`European Union.y`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Korea, South.y`,12))) %>%
  mutate(rollMX = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Mexico.y`,12))) %>%
  mutate(rollJP = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Japan.y`,12)))


US_NET_EV_IMPORTS_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`Net Exports`*12)/1000000000,color= "Monthly Net Exports, Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollnetexports`)/1000000000,color= "US Net Exports of Electric Vehicles, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"),limits = c(-12.5, 15), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Trade") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The US has Become a Major Net Importer of Finished Electric Vehicles") +
  theme_apricitas + theme(legend.position = c(.375,.15)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Net Exports of Electric Vehicles, Rolling 12M Total","Monthly Net Exports, Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -12.5-(.3*(27.5)), ymax = -12.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = US_NET_EV_IMPORTS_GRAPH, "US EV Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

  
US_EV_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union.y`*12/1000000000,color= "Monthly Imports, Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico.y`*12/1000000000,color= "Monthly Imports, Annualized "), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South.y`*12/1000000000,color= "Monthly Imports, Annualized  "), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan.y`*12/1000000000,color= "Monthly Imports, Annualized   "), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollJP/1000000000,color= "Imports From Japan, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollSK/1000000000,color= "Imports From South Korea, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollMX/1000000000,color= "Imports From Mexico, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollEU/1000000000,color= "Imports From the European Union, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"),limits = c(0,9),breaks = c(0,3,6,9), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The US has Become a Major Net Importer of Finished Electric Vehicles Even After the IRA") +
  theme_apricitas + theme(legend.position = c(.35,.675)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= "US EV Imports",values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#EE6055","#A7ACD9","#A7ACD9","#9A348E"), breaks = c("Imports From the European Union, Rolling 12M Total","Monthly Imports, Annualized","Imports From South Korea, Rolling 12M Total","Monthly Imports, Annualized  ","Imports From Mexico, Rolling 12M Total","Monthly Imports, Annualized ","Imports From Japan, Rolling 12M Total","Monthly Imports, Annualized   "), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2,1,2,1,2), lwd = c(1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75), alpha = c(1,0.5,1,0.5,1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(9)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EV_IMPORTS_BREAKDOWN_GRAPH, "US EV Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#US SOLAR PRODUCTION

8506
8507
BATTERIES

#Add Lithium Ion Batteries
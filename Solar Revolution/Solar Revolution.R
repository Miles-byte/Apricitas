pacman::p_load(purrr,sf,seasonal,tigris,maps,readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Using an updated version of the Chinese national stats bureau rstatscn package that fixes a json error in the old database
install_github("pcdi/rstatscn")
library(rstatscn)

devtools::install_github("jameelalsalam/eia2")
library("eia2")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


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

IND_PRO_PV <- statscnQueryLastN(700, lang = "en") #%>%
#   mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
#   subset(.,`Output of Photovoltaic Cells, Current Period` != 0) %>%
#   .[order(nrow(.):1),] %>%
#   mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Photovoltaic Cells, Current Period`,11))) %>%
#   subset(date >= as.Date("2016-01-01"))

IND_PRO_PV1 <- IND_PRO_PV %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(.,`Output of Photovoltaic Cells, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,rollmean(`Output of Photovoltaic Cells, Current Period`,10))) %>%
  subset(date >= as.Date("2016-01-01"))

IND_PRO_PV_GRAPH <- ggplot() + #plotting Chinese PV Production
  geom_line(data= IND_PRO_PV1, aes(x=date,y=rollmean/100,color= "Rolling 1-year Average"), size = 1.25) +
  geom_line(data= IND_PRO_PV1, aes(x=date,y=`Output of Photovoltaic Cells, Current Period`/100 ,color= "Chinese Industrial Production of Photovoltaic Cells, Monthly"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"),limits = c(0, ceiling(max(IND_PRO_PV1$`Output of Photovoltaic Cells, Current Period`/1000))*10), expand = c(0,0)) +
  ylab("GW of Capacity, Monthly") +
  ggtitle("Chinese Solar Production") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Solar Production is Growing Exponentially and Has Surged Post-Pandemic") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_PV1$date)-(.1861*(max(IND_PRO_PV1$date)-min(IND_PRO_PV1$date))), xmax = min(IND_PRO_PV1$date)-(0.049*(max(IND_PRO_PV1$date)-min(IND_PRO_PV1$date))), ymin = 0-(.3*(ceiling(max(IND_PRO_PV1$`Output of Photovoltaic Cells, Current Period`/1000))*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_PV_GRAPH, "China Ind Pro PV Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Must update links
CHINA_SOLAR_EXPORTS <- read.csv("https://storage.googleapis.com/emb-prod-bkt-publicdata/dagster/solar_exports/published/mart_solar_exports_full_release_monthly.csv") %>%
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
                                            mutate(Area = factor(Area, rev(c("Europe","Asia","Latin America and Caribbean","Middle East","Africa","Oceania","North America")))), aes(fill=Area, x=date, y=rollmean_Capacity/1000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  ylab("GW of Capacity, Monthly Average") + 
  scale_y_continuous(labels = scales::number_format(suffix = "GW"), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ggtitle("China's Booming Solar Panel Exports") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data", subtitle = "China Solar Exports are Rising Rapidly, Especially to Europe") +
  theme_apricitas + theme(legend.position = c(.4,.68), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "GW of Capacity, 12M Moving Average",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#F5B041","#9A348E","#00A99D","#EE6055","#FFE98F")),breaks = c("Europe","Asia","Latin America and Caribbean","Middle East","Africa","Oceania","North America")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_PV_EXPORTS_Stacked, "China PV Exports Stacked.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHINA_PV_EXPORTS_MONTHLY_Stacked <- ggplot(CHINA_SOLAR_EXPORTS %>%
                                     filter(Area %in% c("Africa","Asia","Europe","North America","Oceania","South America","Latin America and Caribbean","Middle East")) %>%
                                     filter(rollmean_Capacity != 0) %>%
                                     mutate(Area = factor(Area, rev(c("Europe","Asia","Latin America and Caribbean","Middle East","Africa","Oceania","North America")))), aes(fill=Area, x=date, y=Capacity_MW/1000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  ylab("GW of Capacity, Monthly") + 
  scale_y_continuous(labels = scales::number_format(suffix = "GW"), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ggtitle("China's Booming Solar Panel Exports") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data", subtitle = "China Solar Exports are Rising Rapidly, Especially to Europe") +
  theme_apricitas + theme(legend.position = c(.4,.68), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "GW of Capacity, Monthly",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#F5B041","#9A348E","#00A99D","#EE6055","#FFE98F")),breaks = c("Europe","Asia","Latin America and Caribbean","Middle East","Africa","Oceania","North America")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_PV_EXPORTS_MONTHLY_Stacked, "China PV Exports Monthly Stacked.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "PWh"),limits = c(0, 1.75), expand = c(0,0)) +
  ylab("Global Solar Power Production, PWh") +
  ggtitle("Solar's Exponential Moment") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate Data.\nNote: Monthly Data Covers Geographies Representing 90% of Global Power Demand",subtitle = "Global Solar Output is Growing at an Exponential Rate") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  scale_color_manual(name= "Global Solar Electricity Generation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Yearly Totals","Rolling 12M Total of Monthly Data")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*((today()-as.Date("2000-01-01")))), ymin = 0-(.3*(1.75)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China.y`,12))) %>%
  mutate(rollgrossimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Total For All Countries.y`,12)))
  


US_NET_EV_IMPORTS_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 8.45, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`Net Exports`*12)/1000000000,color= "US Net Exports of Electric Vehicles, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollnetexports`)/1000000000,color= "US Net Exports of Electric Vehicles, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(-25, 12.5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Trade") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The US has Become a Major Net Importer of Finished Electric Vehicles") +
  theme_apricitas + theme(legend.position = c(.375,.15)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Net Exports of Electric Vehicles, Rolling 12M Total","US Net Exports of Electric Vehicles, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -25-(.3*(32.5)), ymax = -25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = US_NET_EV_IMPORTS_GRAPH, "US EV Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

  
US_EV_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 11, size = 4,color = "white", lineheight = 0.8) +
  annotate(geom = "vline",x = as.Date("2022-12-29"), xintercept = as.Date("2022-12-29"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "EV Leasing\nCredit\nAnnounced",x = as.Date("2023-06-15"), y = 11, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`China.y`*12/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union.y`*12/1000000000,color= "EU"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico.y`*12/1000000000,color= "Mexico"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South.y`*12/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan.y`*12/1000000000,color= "Japan"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollCN/1000000000,color= "China"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollJP/1000000000,color= "Japan"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollSK/1000000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollMX/1000000000,color= "Mexico"), size = 1.25) +
  geom_line(data= filter(US_NET_EV_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=rollEU/1000000000,color= "EU"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,13),breaks = c(0,3,6,9,12), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data Via Chad Bown",subtitle = "US Imports of Finished Electric Vehicles Have Increased Significantly Even After the IRA") +
  theme_apricitas + theme(legend.position = c(.33,.76)) +
  #theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= "Gross EV Imports\nSolid = Rolling 12M Total, Dashed = Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU","South Korea","Mexico","Japan","China"))+ #), guide = guide_legend(override.aes = list(linetype = c(1,2,1,2,1,2,1,2,1,2), lwd = c(1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75,1.25,0.75), alpha = c(1,0.5,1,0.5,1,0.5,1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(12)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`European Union`*12/1000000000,color= "EU"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Mexico`*12/1000000000,color= "Mexico"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`South Korea`*12/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Japan`*12/1000000000,color= "Japan"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`China`*12/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollJP/1000000000,color= "Japan"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollSK/1000000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollMX/1000000000,color= "Mexico"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollEU/1000000000,color= "EU"), size = 1.25) +
  geom_line(data= filter(US_NET_BATTERY_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-rollCN/1000000000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,22.5),breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data. Note: China Includes HK & MO",subtitle = "US Imports of Rechargeable Batteries Have Surged, Especially From China") +
  theme_apricitas + theme(legend.position = c(.33,.70)) +
  scale_color_manual(name= "US Net Imports of Rechargeable Batteries\nSolid = Rolling 12M Total, Dashed = Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU","South Korea","Mexico","Japan","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(22.5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-05-01"), y = 3, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union`*12/1000000000,color= "EU"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico`*12/1000000000,color= "Mexico"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South`*12/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan`*12/1000000000,color= "Japan"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`China`*12/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollJP/1000000000,color= "Japan"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollSK/1000000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollMX/1000000000,color= "Mexico"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollEU/1000000000,color= "EU"), size = 1.25) +
  geom_line(data= filter(US_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollCN/1000000000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"),limits = c(0,5),breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's EV Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "Gross Imports of EV Batteries Have Surged Over the Last Few Years, Led by China") +
  theme_apricitas + theme(legend.position = c(.33,.70)) +
  scale_color_manual(name= "US Gross Imports of EV Batteries\nSolid = Rolling 12M Total, Dashed = Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU","South Korea","Mexico","Japan","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH, "US EV Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Solar

US_SOLAR_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "854142",
  I_COMMODITY = "854143"
)

US_SOLAR_IMPORTS <- US_SOLAR_IMPORTS_BULK %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  group_by(date, name) %>%
  summarise(value = sum(as.numeric(value))) %>%
  ungroup() %>%
  pivot_wider()

US_SOLAR_CELLS_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "854142"
)

US_SOLAR_MODULES_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "854143"
)

US_SOLAR_EXPORTS_BULK <- rbind(US_SOLAR_CELLS_EXPORTS_BULK,US_SOLAR_MODULES_BULK)

US_SOLAR_EXPORTS <- US_SOLAR_EXPORTS_BULK %>%
  mutate(value = as.numeric(ALL_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  group_by(date, name) %>%
  summarise(value = sum(as.numeric(value))) %>%
  ungroup() %>%
  pivot_wider()

US_NET_SOLAR_EXPORTS <- merge(US_SOLAR_EXPORTS %>% select(`Total For All Countries`,`Malaysia`,`Vietnam`,`Korea, South`,`Thailand`,`China`,`Hong Kong`,`date`),US_SOLAR_IMPORTS %>% select(`Total For All Countries`,`Malaysia`,`Vietnam`,`Korea, South`,`Cambodia`,`Thailand`,`China`,`Hong Kong`,`date`), by = "date") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(date, `Net Exports`=`Total For All Countries.x`-`Total For All Countries.y`, `Malaysia`=`Malaysia.x`-`Malaysia.y`, `South Korea`=`Korea, South.x`-`Korea, South.y`, `Vietnam`=`Vietnam.x`-`Vietnam.y`, `Cambodia`= -`Cambodia`, `Thailand` = `Thailand.x`-`Thailand.y`, `China` = `China.x` + `Hong Kong.x` - `China.y` - `Hong Kong.y`) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollKH = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Cambodia`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`South Korea`,12))) %>%
  mutate(rollVN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Vietnam`,12))) %>%
  mutate(rollMY = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Malaysia`,12))) %>%
  mutate(rollTH = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Thailand`,12))) %>%
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China`,12)))

US_NET_SOLAR_IMPORTS_GRAPH <- ggplot() + #plotting US Net Imports of Solar
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-07-01"), y = 15, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`Net Exports`*12)/1000000000,color= "US Net Imports of Solar Cells and Modules\nMonthly Annualized"), size = 1.25) +
  #geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollnetexports`)/1000000000,color= "US Net Imports of Solar Cells and Modules, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0, 25), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Net Solar Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The US is a Major Net Importer of Solar Cells and Panels") +
  theme_apricitas + theme(legend.position = c(.7,.35)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*((today()-as.Date("2022-01-01")))), ymin = 0-(.3*(25)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NET_SOLAR_IMPORTS_GRAPH, "US Solar Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_NET_SOLAR_EXPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-07-01"), y = 6, size = 4,color = "white", lineheight = 0.8) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Thailand`*12/1000000000,color= "Thailand"), size = 1.25) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Cambodia`*12/1000000000,color= "Cambodia"), size = 1.25) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`South Korea`*12/1000000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Malaysia`*12/1000000000,color= "Malaysia"), size = 1.25) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`China`*12/1000000000,color= "China"), size = 1.25) +
  geom_line(data= filter(US_NET_SOLAR_EXPORTS, date >= as.Date("2017-12-01")), aes(x=date,y=-`Vietnam`*12/1000000000,color= "Vietnam"), size = 1.25) +
  #geom_line(data= filter(US_NET_SOLAR_IMPORTS_GRAPH, date >= as.Date("2017-12-01")), aes(x=date,y=-rollJP/1000000000,color= "Japan"), size = 1.25) +
  #geom_line(data= filter(US_NET_SOLAR_IMPORTS_GRAPH, date >= as.Date("2017-12-01")), aes(x=date,y=-rollSK/1000000000,color= "South Korea"), size = 1.25) +
  #geom_line(data= filter(US_NET_SOLAR_IMPORTS_GRAPH, date >= as.Date("2017-12-01")), aes(x=date,y=-rollMX/1000000000,color= "Mexico"), size = 1.25) +
  #geom_line(data= filter(US_NET_SOLAR_IMPORTS_GRAPH, date >= as.Date("2017-12-01")), aes(x=date,y=-rollEU/1000000000,color= "EU"), size = 1.25) +
  #geom_line(data= filter(US_NET_SOLAR_IMPORTS_GRAPH, date >= as.Date("2017-12-01")), aes(x=date,y=-rollCN/1000000000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(-0.1,8),breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Net Solar Cell and Module Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data. Note: China Includes HK & MO",subtitle = "US Imports of Solar Cells and Modules Primarily Come From Southeast Asia, Not China") +
  theme_apricitas + theme(legend.position = c(.14,.69)) +
  scale_color_manual(name= "Net Imports\nMonthly Annualized",values = c("#FFE98F","#EE6055","#A7ACD9","#3083DC","#00A99D","#9A348E"), breaks = c("Vietnam","Thailand","Malaysia","Cambodia","South Korea","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*((today()-as.Date("2022-01-01")))), ymin = 0-(.3*(8)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NET_SOLAR_EXPORTS_BREAKDOWN_GRAPH, "US Net Solar Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_STEEL_CHINA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "72",#, #Machines Principally Used to Manufacture Semiconductors
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  #CTY_CODE = "5880", #Japan
  #CTY_CODE = "0003", #EU
  #CTY_CODE = "5570", #Malaysia
  #CTY_CODE = "5800", #South Korea
  #CTY_CODE = "5830", #Taiwan
  #CTY_CODE = "5590", #Singapore
  #CTY_CODE = "-", #Total
) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  select(GEN_VAL_MO) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))

US_STEEL_PRODUCTS_CHINA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "73",#, #Machines Principally Used to Manufacture Semiconductors
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  #CTY_CODE = "5880", #Japan
  #CTY_CODE = "0003", #EU
  #CTY_CODE = "5570", #Malaysia
  #CTY_CODE = "5800", #South Korea
  #CTY_CODE = "5830", #Taiwan
  #CTY_CODE = "5590", #Singapore
  #CTY_CODE = "-", #Total
) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  select(GEN_VAL_MO) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))


US_ALUM_CHINA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "76",#, #Machines Principally Used to Manufacture Semiconductors
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  #CTY_CODE = "5880", #Japan
  #CTY_CODE = "0003", #EU
  #CTY_CODE = "5570", #Malaysia
  #CTY_CODE = "5800", #South Korea
  #CTY_CODE = "5830", #Taiwan
  #CTY_CODE = "5590", #Singapore
  #CTY_CODE = "-", #Total
) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  select(GEN_VAL_MO) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))


US_STEEL_ALUM_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(US_STEEL_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Iron & Steel"), size = 1.25) + 
  #geom_line(data=filter(US_STEEL_PRODUCTS_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Iron & Steel Products"), size = 1.25) + 
  geom_line(data=filter(US_ALUM_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Aluminum & Related"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,5.75), breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Iron, Steel, & Aluminum Imports From China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA. Note: China Includes HK & MO",subtitle = "US Aluminum Imports From China are Significantly More Important Than Iron & Steel Imports") +
  theme_apricitas + theme(legend.position = c(.57,.75), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "US Gross Imports from China\nSeasonally Adjusted at Annual Rates",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*5.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_STEEL_ALUM_CHINA_Graph, "US Steel Alum China Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_NAT_GRAPH_CHINA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "2504",#, #Machines Principally Used to Manufacture Semiconductors
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  #CTY_CODE = "5880", #Japan
  #CTY_CODE = "0003", #EU
  #CTY_CODE = "5570", #Malaysia
  #CTY_CODE = "5800", #South Korea
  #CTY_CODE = "5830", #Taiwan
  #CTY_CODE = "5590", #Singapore
  #CTY_CODE = "-", #Total
) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  select(GEN_VAL_MO) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))

US_PERMANENT_MAGNETS_CHINA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "850511",#, #Machines Principally Used to Manufacture Semiconductors
  I_COMMODITY = "850519",#, #Machines Principally Used to Manufacture Semiconductors
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  #CTY_CODE = "5880", #Japan
  #CTY_CODE = "0003", #EU
  #CTY_CODE = "5570", #Malaysia
  #CTY_CODE = "5800", #South Korea
  #CTY_CODE = "5830", #Taiwan
  #CTY_CODE = "5590", #Singapore
  #CTY_CODE = "-", #Total
) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  select(GEN_VAL_MO) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))

US_CRITICAL_MINERALS_CHINA_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "251110",
  I_COMMODITY = "251910",
  I_COMMODITY = "251990",
  I_COMMODITY = "252921",
  I_COMMODITY = "252922",
  I_COMMODITY = "253020",
  I_COMMODITY = "253090",
  I_COMMODITY = "260200",
  I_COMMODITY = "260400",
  I_COMMODITY = "260500",
  I_COMMODITY = "260600",
  I_COMMODITY = "260800",
  I_COMMODITY = "260900",
  I_COMMODITY = "261000",
  I_COMMODITY = "261100",
  I_COMMODITY = "261400",
  I_COMMODITY = "261510",
  I_COMMODITY = "261590",
  I_COMMODITY = "261690",
  I_COMMODITY = "261710",
  I_COMMODITY = "261790",
  I_COMMODITY = "262011",
  I_COMMODITY = "262019",
  I_COMMODITY = "262099",
  I_COMMODITY = "711230",
  I_COMMODITY = "711292",
  I_COMMODITY = "750300",
  I_COMMODITY = "760200",
  I_COMMODITY = "790200",
  I_COMMODITY = "800200",
  I_COMMODITY = "810197",
  I_COMMODITY = "810330",
  I_COMMODITY = "810420",
  I_COMMODITY = "810530",
  I_COMMODITY = "810610",
  I_COMMODITY = "810690",
  I_COMMODITY = "810830",
  I_COMMODITY = "810931",
  I_COMMODITY = "810939",
  I_COMMODITY = "811020",
  I_COMMODITY = "811100",
  I_COMMODITY = "811213",
  I_COMMODITY = "811222",
  I_COMMODITY = "811241",
  I_COMMODITY = "811252",
  I_COMMODITY = "811292",
  I_COMMODITY = "811300",
  I_COMMODITY = "262040",
  I_COMMODITY = "262060",
  I_COMMODITY = "262091",
  I_COMMODITY = "280130",
  I_COMMODITY = "280450",
  I_COMMODITY = "280480",
  I_COMMODITY = "280519",
  I_COMMODITY = "280530",
  I_COMMODITY = "281111",
  I_COMMODITY = "281119",
  I_COMMODITY = "281129",
  I_COMMODITY = "281390",
  I_COMMODITY = "281610",
  I_COMMODITY = "281640",
  I_COMMODITY = "281700",
  I_COMMODITY = "281820",
  I_COMMODITY = "281830",
  I_COMMODITY = "281910",
  I_COMMODITY = "281990",
  I_COMMODITY = "282010",
  I_COMMODITY = "282090",
  I_COMMODITY = "282200",
  I_COMMODITY = "282300",
  I_COMMODITY = "282520",
  I_COMMODITY = "282530",
  I_COMMODITY = "282540",
  I_COMMODITY = "282560",
  I_COMMODITY = "282580",
  I_COMMODITY = "282590",
  I_COMMODITY = "282612",
  I_COMMODITY = "282630",
  I_COMMODITY = "282690",
  I_COMMODITY = "282731",
  I_COMMODITY = "282735",
  I_COMMODITY = "282739",
  I_COMMODITY = "282749",
  I_COMMODITY = "282759",
  I_COMMODITY = "282760",
  I_COMMODITY = "283090",
  I_COMMODITY = "283321",
  I_COMMODITY = "283324",
  I_COMMODITY = "283327",
  I_COMMODITY = "283329",
  I_COMMODITY = "283429",
  I_COMMODITY = "283660",
  I_COMMODITY = "283691",
  I_COMMODITY = "283699",
  I_COMMODITY = "284130",
  I_COMMODITY = "284150",
  I_COMMODITY = "284161",
  I_COMMODITY = "284169",
  I_COMMODITY = "284180",
  I_COMMODITY = "284190",
  I_COMMODITY = "284610",
  I_COMMODITY = "284690",
  I_COMMODITY = "284990",
  I_COMMODITY = "285000",
  I_COMMODITY = "291529",
  I_COMMODITY = "320611",
  I_COMMODITY = "320619",
  I_COMMODITY = "320642",
  I_COMMODITY = "340590",
  I_COMMODITY = "360690",
  I_COMMODITY = "381511",
  I_COMMODITY = "381512",
  I_COMMODITY = "381519",
  I_COMMODITY = "381590",
  I_COMMODITY = "381800",
  I_COMMODITY = "382499",
  I_COMMODITY = "711011",
  I_COMMODITY = "711019",
  I_COMMODITY = "711021",
  I_COMMODITY = "711029",
  I_COMMODITY = "711031",
  I_COMMODITY = "711039",
  I_COMMODITY = "711041",
  I_COMMODITY = "711049",
  I_COMMODITY = "720211",
  I_COMMODITY = "720219",
  I_COMMODITY = "720230",
  I_COMMODITY = "720241",
  I_COMMODITY = "720249",
  I_COMMODITY = "720250",
  I_COMMODITY = "720260",
  I_COMMODITY = "720280",
  I_COMMODITY = "720291",
  I_COMMODITY = "720292",
  I_COMMODITY = "720293",
  I_COMMODITY = "720299",
  I_COMMODITY = "722720",
  I_COMMODITY = "722820",
  I_COMMODITY = "740500",
  I_COMMODITY = "750110",
  I_COMMODITY = "750120",
  I_COMMODITY = "750210",
  I_COMMODITY = "750220",
  I_COMMODITY = "750400",
  I_COMMODITY = "750511",
  I_COMMODITY = "750512",
  I_COMMODITY = "750890",
  I_COMMODITY = "760110",
  I_COMMODITY = "760120",
  I_COMMODITY = "760410",
  I_COMMODITY = "760421",
  I_COMMODITY = "760429",
  I_COMMODITY = "780191",
  I_COMMODITY = "790111",
  I_COMMODITY = "790112",
  I_COMMODITY = "790120",
  I_COMMODITY = "790310",
  I_COMMODITY = "790390",
  I_COMMODITY = "790700",
  I_COMMODITY = "800110",
  I_COMMODITY = "800120",
  I_COMMODITY = "800700",
  I_COMMODITY = "810110",
  I_COMMODITY = "810194",
  I_COMMODITY = "810199",
  I_COMMODITY = "810320",
  I_COMMODITY = "810391",
  I_COMMODITY = "810399",
  I_COMMODITY = "810411",
  I_COMMODITY = "810419",
  I_COMMODITY = "810430",
  I_COMMODITY = "810490",
  I_COMMODITY = "810520",
  I_COMMODITY = "810590",
  I_COMMODITY = "810820",
  I_COMMODITY = "810890",
  I_COMMODITY = "810921",
  I_COMMODITY = "810929",
  I_COMMODITY = "810991",
  I_COMMODITY = "810999",
  I_COMMODITY = "811010",
  I_COMMODITY = "811090",
  I_COMMODITY = "811212",
  I_COMMODITY = "811219",
  I_COMMODITY = "811221",
  I_COMMODITY = "811229",
  I_COMMODITY = "811231",
  I_COMMODITY = "811239",
  I_COMMODITY = "811249",
  I_COMMODITY = "811251",
  I_COMMODITY = "811299",
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  #CTY_CODE = "5880", #Japan
  #CTY_CODE = "0003", #EU
  #CTY_CODE = "5570", #Malaysia
  #CTY_CODE = "5800", #South Korea
  #CTY_CODE = "5830", #Taiwan
  #CTY_CODE = "5590", #Singapore
  CTY_CODE = "-", #Total
) 

US_CRITICAL_MINERALS_BREAKDOWN <- US_CRITICAL_MINERALS_CHINA_BULK %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(YEAR, CTY_CODE, I_COMMODITY) %>%
  summarise(I_COMMODITY, CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  filter(YEAR == 2023) %>%
  select(-CTY_CODE) %>%
  pivot_wider(names_from = CTY_NAME, values_from = GEN_VAL_MO) %>%
  mutate(share = CHINA/`TOTAL FOR ALL COUNTRIES`)


US_CRITICAL_MINERALS_CHINA <- US_CRITICAL_MINERALS_CHINA_BULK %>%
  filter(CTY_CODE != "-") %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  select(GEN_VAL_MO) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))

#FOR CREATING BREAKDOWNS
  # mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  # mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  # mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  # mutate(I_COMMODITY = substr(I_COMMODITY, 1, 4)) %>%
  # mutate(I_COMMODITY = if_else(I_COMMODITY %in% c("3824", "2825", "2519", "3818"), I_COMMODITY, "1111")) %>%
  # group_by(time, CTY_CODE, I_COMMODITY) %>%
  # summarise(I_COMMODITY, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  # unique() %>%
  # ungroup() %>%
  # select(-CTY_CODE) %>%
  # pivot_wider(names_from = I_COMMODITY, values_from = GEN_VAL_MO) %>%
  # select(-time) %>%
  # ts(., start = c(2013,1), frequency = 12) %>%
  # seas(x11 = "") %>%
  # final() %>%
  # as.data.frame(value = melt(.)) %>%
  # mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))

US_NAT_MAGNET_GRAPH_CHINA <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(US_NAT_GRAPH_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Natural Graphite"), size = 1.25) + 
  #geom_line(data=filter(US_STEEL_PRODUCTS_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Iron & Steel Products"), size = 1.25) + 
  geom_line(data=filter(US_PERMANENT_MAGNETS_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Permanent Magnets"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = .25),limits = c(0,.75), breaks = c(0,.25,0.5,.75), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Graphite & Magnet Imports From China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA. Note: China Includes HK & MO",subtitle = "Imports of Natural Graphite and Permanent Magnets From China Have Risen Since COVID") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Gross Imports from China\nSeasonally Adjusted at Annual Rates",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Permanent Magnets","Natural Graphite")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*.75), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_MAGNET_GRAPH_CHINA, "US Nat Magnet Graph China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_CRITICAL_MINERALS_CHINA <- ggplot() + #plotting critical mineral imports
  geom_line(data=filter(US_CRITICAL_MINERALS_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "Critical Minerals Excluding Natural Graphite\n(Based on Executive Order 14017 List)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,4), breaks = c(0,1,2,3,4), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Critical Mineral Imports From China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA. Note: China Includes HK & MO",subtitle = "US Imports of Critical Minerals From China Stand at More than $2B") +
  theme_apricitas + theme(legend.position = c(.42,.25), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Gross Imports from China\nSeasonally Adjusted at Annual Rates",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*4), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CRITICAL_MINERALS_CHINA, "US Critical Minerals Graph China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_SOLAR_PRICES_MONTHLY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/US_SOLAR_COST_MONTHLY_DATA.csv") %>%
  mutate(date = as.Date(paste0(date))) %>%
  mutate(quarter = floor_date(date, "quarter")) %>%
  group_by(quarter) %>%
  summarize(cost = mean(cost, na.rm = TRUE)) %>%
  mutate(pct = (cost-lag(cost,4))/lag(cost,4))

US_SOLAR_PRICES_QUARTERLY_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_line(data= filter(US_SOLAR_PRICES_MONTHLY, date >= as.Date("2019-01-01")), aes(x=date,y=pct3,color= "Monthly Solar Prices, Year-on-Year Change"), size = 0.75,) +
  geom_line(data= filter(US_SOLAR_PRICES_MONTHLY, quarter >= as.Date("2019-01-01")), aes(x=quarter,y=pct,color= "US Solar Panel Prices, Year-on-Year Change"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.25,.32), expand = c(0,0)) +
  ylab("Percent Change") +
  ggtitle("US Solar Panel Prices") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "US Prices for Solar Panels (Imported & Domestic) Dropped Significantly in 2023") +
  theme_apricitas + theme(legend.position = c(.35,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*((today()-as.Date("2019-01-01")))), ymin = -.25-(.3*.57), ymax = -.25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_SOLAR_PRICES_QUARTERLY_GRAPH, "US Solar Prices Quarterly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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
  theme_apricitas + theme(legend.position = c(.4,.9)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU Net Imports of Rechargeable Batteries, Rolling 12M Total","EU Net Imports of Rechargeable Batteries, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(30)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_NET_BATTERY_IMPORTS_GRAPH, "EU NET Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

DE_BATTERY_TRADE_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.WORLD.*.*.VALUE_IN_EUROS?c[reporter]=DE&c[product]=8507&c[flow]=1,2&compress=false"))

DE_BATTERY_TRADE <- DE_BATTERY_TRADE_BULK %>%
  transmute(name = flow, value = as.numeric(OBS_VALUE), date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

DE_NET_BATTERY_IMPORTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(DE_BATTERY_TRADE, date >= as.Date("2017-12-01")), aes(x=date,y=-(NET_EXPORTS*12)/1000000000,color= "German Net Imports of Rechargeable Batteries, Monthly Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(DE_BATTERY_TRADE, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollsum`)/1000000000,color= "German Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(0, 30), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("Germany's Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "Germany has a Yawning Deficit in the Battery Trade Despite the EV Surplus") +
  theme_apricitas + theme(legend.position = c(.5,.9)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("German Net Imports of Rechargeable Batteries, Rolling 12M Total","German Net Imports of Rechargeable Batteries, Monthly Annualized"), guide = guide_legend(override.aes = list(linetype = c(1,2), lwd = c(1.25,0.75), alpha = c(1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(30)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DE_NET_BATTERY_IMPORTS_GRAPH, "DE NET Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_DE_NET_BATTERY_IMPORTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_BATTERY_TRADE, date >= as.Date("2017-12-01")), aes(x=date,y=-(rollsum)/1000000000,color= "EU Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(DE_BATTERY_TRADE, date >= as.Date("2017-12-01")), aes(x=date,y=-(`rollsum`)/1000000000,color= "German Net Imports of Rechargeable Batteries, Rolling 12M Total"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(0, 30), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("Germany's Net Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "Germany has a Yawning Deficit in the Battery Trade Despite the EV Surplus") +
  theme_apricitas + theme(legend.position = c(.5,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU Net Imports of Rechargeable Batteries, Rolling 12M Total","German Net Imports of Rechargeable Batteries, Rolling 12M Total")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(30)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_DE_NET_BATTERY_IMPORTS_GRAPH, "EU DE NET Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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

EU_IMPORTS_EXPORTS_CARS_CHINA <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=CN,HK,MO&c[product]=8703&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_CARS_US <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=US&c[product]=8703&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_CARS_US_MOD <- EU_IMPORTS_EXPORTS_CARS_US %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_ICE_CHINA <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=CN,HK,MO&c[product]=870321,870322,870323,870324,870331,870332,870333&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_HYBRID_CHINA <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=CN,HK,MO&c[product]=870340,870350,870360,870370&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_EVS_CHINA <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=CN,HK,MO&c[product]=870380&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_EVS_CHINA_MOD <- EU_IMPORTS_EXPORTS_EVS_CHINA %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_ICE_CHINA_MOD <- EU_IMPORTS_EXPORTS_ICE_CHINA %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_HYBRID_CHINA_MOD <- EU_IMPORTS_EXPORTS_HYBRID_CHINA %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_CARS_CHINA_MOD <- EU_IMPORTS_EXPORTS_CARS_CHINA %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))


EU_EV_EXPORTS_IMPORTS_CHINA_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`1`*12)/1000000000,color= "Monthly Imports, Annualized"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`2`*12)/1000000000,color= "Monthly Exports, Annualized "), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumimports`)/1000000000,color= "EU Imports of Electric Vehicles From China, Rolling 12M Total"), size = 1.25) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumexports`)/1000000000,color= "EU Exports of Electric Vehicles to China, Rolling 12M Total"), size = 1.25) +
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

EU_NET_EXPORTS_CHINA_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "EVs"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_HYBRID_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "Hybrids"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_ICE_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "ICE Vehicles"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CARS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "Motor Vehicles"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(rollsumNET_EXPORTS)/1000000000,color= "EVs"), size = 1.25) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_HYBRID_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(rollsumNET_EXPORTS)/1000000000,color= "Hybrids"), size = 1.25) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_ICE_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(rollsumNET_EXPORTS)/1000000000,color= "ICE Vehicles"), size = 1.25) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CARS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(rollsumNET_EXPORTS)/1000000000,color= "Motor Vehicles"), size = 2) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(-15, 35), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("EU Net Vehicle Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data  Note: China Includes HK and MO, Hybrids includes Plug-in Hybrids",subtitle = "The EU's Strong Vehicles Trade Surplus With China is Shrinking—Thanks to EVs") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  #theme(legend.key.width =  unit(.82, "cm"), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Solid = Rolling 12M Total, Dashed = Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#A7ACD9","#9A348E"), breaks = c("Motor Vehicles","ICE Vehicles","Hybrids","EVs"), guide = guide_legend(ncol = 2, override.aes = list(lwd = c(2, 1.25,1.25,1.25)))) +#, override.aes = list(linetype = c(1,2,1,2,1,2,1,2),  alpha = c(1,0.5,1,0.5,1,0.5,1,0.5)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -15-(.3*(50)), ymax = -15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_NET_EXPORTS_CHINA_GRAPH, "EU NET Exports China Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_IMPORTS_EXPORTS_CARS_US <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=US&c[product]=8703&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_CARS_US_MOD <- EU_IMPORTS_EXPORTS_CARS_US %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))


EU_VEHICLE_NET_EXPORTS_CHINA_US_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CARS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CARS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "China"), size = 1.25) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CARS_US_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "United States"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_CARS_US_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(-2.5, 40), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("EU Net Export of Motor Vehicles") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data  Note: China Includes HK and MO",subtitle = "EU Vehicle Exports are Increasingly Headed to the US, Not China") +
  theme_apricitas + theme(legend.position = c(.4,.89)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United States", "China"), guide = guide_legend(override.aes = list(linetype = c(1,1), lwd = c(1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -2.5-(.3*(42.5)), ymax = -2.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_VEHICLE_NET_EXPORTS_CHINA_US_GRAPH, "EU VEHICLE NET EXPORTS CHINA US Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_IMPORTS_EXPORTS_EVS_US <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=US&c[product]=870380&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_EVS_US_MOD <- EU_IMPORTS_EXPORTS_EVS_US %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_EVS_UK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=GB&c[product]=870380&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_EVS_UK_MOD <- EU_IMPORTS_EXPORTS_EVS_UK %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_EVS_SK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=KR&c[product]=870380&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_EVS_SK_MOD <- EU_IMPORTS_EXPORTS_EVS_SK %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_IMPORTS_EXPORTS_EVS_TOTAL <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=EU27_2020_EXTRA&c[product]=870380&c[flow]=1,2&compress=false"))

EU_IMPORTS_EXPORTS_EVS_TOTAL_MOD <- EU_IMPORTS_EXPORTS_EVS_TOTAL %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

DE_IMPORTS_EXPORTS_EVS_TOTAL <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/DS-045409/1.0/M.*.*.*.*.VALUE_IN_EUROS?c[reporter]=DE&c[partner]=WORLD&c[product]=870380&c[flow]=1,2&compress=false"))

DE_IMPORTS_EXPORTS_EVS_TOTAL_MOD <- DE_IMPORTS_EXPORTS_EVS_TOTAL %>%
  group_by(TIME_PERIOD, flow) %>%
  summarise(value = sum(as.numeric(OBS_VALUE))) %>%
  ungroup() %>%
  transmute(name = flow, value, date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  unique() %>%
  pivot_wider() %>%
  mutate(NET_EXPORTS = `2`-`1`) %>%
  arrange(date) %>%
  mutate(rollsumimports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`1`,12))) %>%
  mutate(rollsumexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`2`,12))) %>%
  mutate(rollsumNET_EXPORTS = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(NET_EXPORTS,12)))

EU_VEHICLE_NET_EXPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_US_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "United States"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_US_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "United States"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "China"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_UK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "United Kingdom"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_UK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "United Kingdom"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_SK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_SK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "South Korea"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_TOTAL_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_TOTAL_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "Total"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(-10, 12.5), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("EU Net Export of Electric Vehicles, 12MMT") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data  Note: China Includes HK and MO",subtitle = "EU Vehicle Exports are Increasingly Headed to the US, Not China") +
  theme_apricitas + theme(legend.position = c(.3,.75)) +
  #theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total","United States","China","United Kingdom","South Korea"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -10-(.3*(22.5)), ymax = -10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_VEHICLE_NET_EXPORTS_BREAKDOWN_GRAPH, "EU VEHICLE NET EXPORTS BREAKDOWN Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_GERMANY_NET_EV_EXPORTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_TOTAL_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "European Union"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_TOTAL_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "European Union"), size = 1.25) +
  geom_line(data= filter(DE_IMPORTS_EXPORTS_EVS_TOTAL_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "Germany"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(DE_IMPORTS_EXPORTS_EVS_TOTAL_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "Germany"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(-7.5, 30), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("EU Net Exports of Electric Vehicles") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "The EU and Germany are Making") +
  theme_apricitas + theme(legend.position = c(.4,.89)) +
  #theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= "Solid = 12M Moving Total, Dashed = Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany"), guide = guide_legend(override.aes = list(linetype = c(1,1),lwd = c(1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -7.5-(.3*(37.5)), ymax = -7.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_GERMANY_NET_EV_EXPORTS_GRAPH, "EU NET EV Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_EV_NET_EXPORTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_US_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "United States"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_US_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "United States"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_CHINA_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "China"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_UK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "United Kingdom"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_UK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "United Kingdom"), size = 1.25) +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_SK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(NET_EXPORTS*12)/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  #geom_line(data= filter(EU_IMPORTS_EXPORTS_EVS_SK_MOD, date >= as.Date("2017-12-01")), aes(x=date,y=(`rollsumNET_EXPORTS`)/1000000000,color= "South Korea"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€",accuracy = 1, suffix = "B"),limits = c(-10, 7.5), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("EU Net Exports of EVs, 12MMT") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data  Note: China Includes HK and MO",subtitle = "EU Vehicle Exports are Increasingly Headed to the US, Not China") +
  theme_apricitas + theme(legend.position = c(.4,.89)) +
  #theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United States","China","United Kingdom","South Korea"), guide = guide_legend(override.aes = list(linetype = c(1,1), lwd = c(1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = -10-(.3*(17.5)), ymax = -10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_EV_NET_EXPORTS_CHINA_US_GRAPH, "EU EV NET Exports China US Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




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

US_SOLAR_SPLIT <- US_SOLAR %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= max(year)-5) %>%
  mutate(year = as.character(year))

US_SOLAR_SPLIT_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= US_SOLAR_SPLIT, aes(x=month,y=value/1000,color= year), size = 1.25) +
  geom_point(data= US_SOLAR_SPLIT, aes(x=month,y=value/1000,color= year), size = 3) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(US_SOLAR_SPLIT$value)/10000)*10), expand = c(0,0)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ylab("TWh, Monthly") +
  ggtitle("US Monthly Solar Generation") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "US Solar Generation is Growing Quickly, and is Up 25% Compared to Last Year") +
  theme_apricitas + theme(legend.position = c(.085,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = sort(unique(TX_SOLAR_SPLIT$year), decreasing = TRUE)[1:6]) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*((today()-as.Date("2002-01-01")))), ymin = 0-(.3*(ceiling(max(US_SOLAR_SPLIT$value)/10000)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_SOLAR_SPLIT_GRAPH, "US Solar Split Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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

TX_SOLAR_SPLIT <- TX_SOLAR %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= max(year)-5) %>%
  mutate(year = as.character(year))

TX_SOLAR_SPLIT_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= TX_SOLAR_SPLIT, aes(x=month,y=value/1000,color= year), size = 1.25) +
  geom_point(data= TX_SOLAR_SPLIT, aes(x=month,y=value/1000,color= year), size = 3) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(TX_SOLAR_SPLIT$value)/2000000)*5), expand = c(0,0)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ylab("TWh, Monthly") +
  ggtitle("Texas Monthly Solar Generation") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Texas Solar Generation is Growing Quickly, and is Up 50% Compared to Last Year") +
  theme_apricitas + theme(legend.position = c(.085,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = sort(unique(TX_SOLAR_SPLIT$year), decreasing = TRUE)[1:6]) +
  annotation_custom(apricitas_logo_rast, xmin = 1-(.1861*11), xmax = 1-(0.049*11), ymin = 0-(.3*(ceiling(max(TX_SOLAR_SPLIT$value)/2000000)*5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_SOLAR_SPLIT_GRAPH, "TX Solar Split Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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

TX_CA_TOTAL_SOLAR_SHARE <- merge(TX_SOLAR,CA_SOLAR, by = "date") %>%
  transmute(date, value = value.x/value.y, rollmean = rollmean.x/rollmean.y)

TX_UTILITY_SOLAR <- eia1_series("ELEC.GEN.SUN.TX.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

CA_UTILITY_SOLAR <- eia1_series("ELEC.GEN.SUN.CA.99.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = generation) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

TX_CA_UTILITY_SOLAR_SHARE <- merge(TX_UTILITY_SOLAR,CA_UTILITY_SOLAR, by = "date") %>%
  transmute(date, value = value.x/value.y, rollmean = rollmean.x/rollmean.y)


TX_CA_SOLAR_SHARE_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(TX_CA_TOTAL_SOLAR_SHARE, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Total Texas Solar as a Share of Total California Solar"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_CA_TOTAL_SOLAR_SHARE, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Total Texas Solar as a Share of Total California Solar"), size = 1.25) +
  geom_line(data= filter(TX_CA_UTILITY_SOLAR_SHARE, date >= as.Date("2015-01-01")), aes(x=date,y=value,color= "Utility-Scale Texas Solar as a Share of Utility-Scale California Solar"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(TX_CA_UTILITY_SOLAR_SHARE, date >= as.Date("2015-01-01")), aes(x=date,y=rollmean,color= "Utility-Scale Texas Solar as a Share of Utility-Scale California Solar"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), expand = c(0,0)) +
  ylab("Percent of California's") +
  ggtitle("Texas Solar as a % of California's") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Texas is Quickly Catching Up to California, America's Solar Leader") +
  theme_apricitas + theme(legend.position = c(.45,.85), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "Net Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#FFE98F","#00A99D"), breaks = c("Utility-Scale Texas Solar as a Share of Utility-Scale California Solar","Total Texas Solar as a Share of Total California Solar")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_CA_SOLAR_SHARE_GRAPH, "TX CA Electricity Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


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

US_TOTAL <- eia1_series("STEO.TOEPGEN_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Total", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

#Just used for calculating percentages
# US_RBIND <- rbind(US_TOTAL,US_GAS_STEO,US_COL_STEO,US_NUC_STEO,US_HYD_STEO,US_WND_STEO,US_SOL_STEO) %>%
#   select(-value) %>%
#   pivot_wider(names_from = category, values_from = rollmean) %>%
#   filter(Solar > 0) %>%
#   mutate(across(where(is.numeric), ~ .x / Total)) %>%
#   mutate(Other = Total - `Nat Gas`-Coal-Nuclear-Hydro-Wind-Solar)

US_ELECTRICITY_PRODUCTION_STEO_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(US_GAS_STEO$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
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
  scale_y_continuous(labels = scales::number_format(suffix = "TWh"),limits = c(0, ceiling(max(US_GAS_STEO$value)/10)*10+10), expand = c(0,0)) +
  ylab("TWh, Monthly") +
  ggtitle("America's Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Natural Gas, Wind, and Solar Now Make Up A Larger Share of America's Grid") +
  theme_apricitas + theme(legend.position = c(.3,.86), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "US Net Utility-Scale Electricity Generation\nDashed = Monthly, Solid = 12M Moving Average",values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"), breaks = c("Coal","Natural Gas","Nuclear","Hydro","Wind","Solar"), guide = guide_legend(ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*((today()-as.Date("2015-01-01")))), ymin = 0-(.3*(ceiling(max(US_GAS_STEO$value)/10)*10+10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ELECTRICITY_PRODUCTION_STEO_GRAPH, "US Electricity Production STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_BATTERY_STEO <- eia1_series("STEO.BAEPCGW_US.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value) %>%
  arrange(date) %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value,12)))

US_BATTERY_STORAGE_STEO_GRAPH <- ggplot() + 
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(US_BATTERY_STEO$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -475), "month"), y = 30, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data= filter(US_BATTERY_STEO, date >= as.Date("2018-01-01")), aes(x=date,y=value,color= "US Grid-Scale Battery Storage Power Capacity, GW"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "GW"),limits = c(0, ceiling(max(US_BATTERY_STEO$value)/10)*10), expand = c(0,0)) +
  ylab("GW") +
  ggtitle("America's Battery Boom") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "US Battery Storage Capacity is Booming and Projected to Nearly-Triple by 2026") +
  theme_apricitas + theme(legend.position = c(.4,.86), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= NULL,values = rev(c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()+700-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()+700-as.Date("2018-01-01")))), ymin = 0-(.3*(ceiling(max(US_BATTERY_STEO$value)/10)*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_BATTERY_STORAGE_STEO_GRAPH, "US Battery Storage STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


TX_ELECTRICITY_PRODUCTION_STEO_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(ERCOT_RBIND$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -575), "month"), y = 0.6, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data= ERCOT_RBIND, aes(x=date,y=Clean_Share,color= "Texas (ERCOT) Clean Electricity (Renewables, Nuclear, Hydro)\nas a Share of Total Utility-Scale Generation\nRolling 12M Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0, 1), expand = c(0,0)) +
  ylab("Percent, Rolling 12M Average") +
  ggtitle("Texas' Changing Grid") +
  labs(caption = "Graph created by @JosephPolitano using EIA Data",subtitle = "Clean Energy Now Makes Up A Larger Share of Texas' Grid") +
  theme_apricitas + theme(legend.position = c(.4,.86), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= NULL,values = rev(c("#EE6055","#A7ACD9","#00A99D","#3083DC","#9A348E","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-01-01")-(.1861*(today()-as.Date("2011-01-01"))), xmax = as.Date("2011-01-01")-(0.049*((today()-as.Date("2011-01-01")))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_ELECTRICITY_PRODUCTION_STEO_GRAPH, "TX Electricity STEO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#GET DATA FROM:
#https://www.eia.gov/electricity/data/eia860m/
p_load(tigris)

GENERATOR_CAPACITY_MAP_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/GENERATOR_CAPACITY_ADDITIONS_MAP_DATA.csv") %>%
  transmute(capacity = as.numeric(gsub(",","",Nameplate.Capacity..MW.)), name = Energy.Source.Code, year = Planned.Operation.Year, state = Plant.State, Latitude, Longitude) %>%
  filter(year == 2024) %>%
  filter(name %in% c("SUN","WND","MWH","NUC")) %>%
  mutate(name = case_when(
    name == "SUN" ~ "Solar",
    name == "WND" ~ "Wind",
    name == "NUC" ~ "Nuclear",
    name == "MWH" ~ "Batteries",
    TRUE ~ as.character(name)  # Keeps other values unchanged
  )) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326) %>%
  shift_geometry(position = "below")

states <- states(cb = TRUE, year = 2021) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS)) %>%
  shift_geometry(position = "below") %>% #THIS PUTS HAWAII AND ALASKA BELOW THE MAP
  filter(STATEFP < 60)

GENERATOR_CAPACITY_MAP <- states %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  geom_point(data = GENERATOR_CAPACITY_MAP_DATA, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = name, size = capacity), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_manual(name = "Type",
                    values = c("#FFE98F","#9A348E","#00A99D","#3083DC"),
                    breaks = c("Solar", "Wind","Nuclear","Batteries"), 
                    labels = c("Solar", "Wind","Nuclear","Batteries"),
                    guide = guide_legend(override.aes = list(color = c("#FFE98F","#9A348E","#00A99D","#3083DC"), size = 5))) +
  scale_size_area(name = "Capacity",
                  max_size = 7,
                  breaks = c(250,500,750,1000),
                  limits = c(0,1114.0),
                  labels = c("250MW","500MW","750MW","1GW"),
                  guide = guide_legend(override.aes = list(fill = c("#FFE98F")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("       New Clean Power Capacity Planned for 2024") +
  labs(caption = "Graph created by @JosephPolitano using EIA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = GENERATOR_CAPACITY_MAP, "New Generator Capacity Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TX <- counties(cb = TRUE, year = 2021) %>%
  filter(STUSPS == "TX") %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS))

TX_GENERATOR_CAPACITY_MAP <- TX %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = filter(states, STUSPS == "TX"), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  geom_point(data = filter(GENERATOR_CAPACITY_MAP_DATA, state == "TX"), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = name, size = capacity), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  coord_sf(xlim = c(-1500000, 500000)) +
  scale_fill_manual(name = "Type",
                    values = c("#FFE98F","#9A348E","#3083DC"),
                    breaks = c("Solar", "Wind","Batteries"), 
                    labels = c("Solar", "Wind","Batteries"),
                    guide = guide_legend(override.aes = list(color = c("#FFE98F","#9A348E","#3083DC"), size = 5))) +
  scale_size_area(name = "Capacity",
                  max_size = 10,
                  breaks = c(250,500,750,1000),
                  limits = c(0,1114.0),
                  labels = c("250MW","500MW","750MW","1GW"),
                  guide = guide_legend(override.aes = list(fill = c("#FFE98F")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Texas New Clean Power Capacity Planned for 2024") +
  labs(caption = "Graph created by @JosephPolitano using EIA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = TX_GENERATOR_CAPACITY_MAP, "TX New Generator Capacity Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CA <- counties(cb = TRUE, year = 2021) %>%
  filter(STUSPS == "CA") %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS))

CA_GENERATOR_CAPACITY_MAP <- CA %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = filter(states, STUSPS == "CA"), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  geom_point(data = filter(GENERATOR_CAPACITY_MAP_DATA, state == "CA"), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = name, size = capacity), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  coord_sf(xlim = c(-3000000, -1300000)) +
  scale_fill_manual(name = "Type",
                    values = c("#FFE98F","#9A348E","#3083DC"),
                    breaks = c("Solar", "Wind","Batteries"), 
                    labels = c("Solar", "Wind","Batteries"),
                    guide = guide_legend(override.aes = list(color = c("#FFE98F","#9A348E","#3083DC"), size = 5))) +
  scale_size_area(name = "Capacity",
                  max_size = 10,
                  breaks = c(250,500,750,1000),
                  limits = c(0,1114.0),
                  labels = c("250MW","500MW","750MW","1GW"),
                  guide = guide_legend(override.aes = list(fill = c("#FFE98F")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("CA New Clean Power Capacity Planned for 2024") +
  labs(caption = "Graph created by @JosephPolitano using EIA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = CA_GENERATOR_CAPACITY_MAP, "CA New Generator Capacity Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAPACITY_BREAKDOWN <- GENERATOR_CAPACITY_MAP_DATA %>%
  st_drop_geometry() %>%
  group_by(state, name) %>%
  summarise(total_capacity = sum(capacity, na.rm = TRUE))

TOTAL_CAPACITY <- GENERATOR_CAPACITY_MAP_DATA %>%
  st_drop_geometry() %>%
  group_by(name) %>%  # Group by the type, e.g., solar
  summarise(total_capacity_by_type = sum(capacity, na.rm = TRUE))

TOTAL_CAPACITY_PCT <- left_join(CAPACITY_BREAKDOWN,TOTAL_CAPACITY, by = "name") %>%
  mutate(percent_of_type_total = round(total_capacity/total_capacity_by_type*100,2))


state_codes <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                 "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

get_state_solar <- function(state_code) {
  series_id <- paste0("ELEC.GEN.TSN.", state_code, ".99.M")
  data <- eia1_series(series_id) %>%
    transmute(date = as.Date(paste0(period, "-01")),
              category = "Solar",
              value = generation,
              state = state_code) %>%
    arrange(date) %>%
    mutate(rollmean = c(rep(0, 11), rollmean(value, 12)))
  return(data)
}

all_states_data <- state_codes %>%
  map_dfr(get_state_solar)

State_solar_data <- all_states_data %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(pct_growth = (value-lag(value,12))/lag(value,12)) %>%
  mutate(change = value - lag(value,12)) %>%
  filter(date == max(date))

states_solar <- states(cb = TRUE, year = 2021) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state = gsub("\\s", "", STUSPS)) %>%
  shift_geometry(position = "below") %>% #THIS PUTS HAWAII AND ALASKA BELOW THE MAP
  filter(STATEFP < 60) %>%
  merge(.,State_solar_data, by = "state")

states_solar_centroids <- states_solar %>%
  st_centroid() %>%
  mutate(pct_growth = ifelse(pct_growth > 0.5, 0.5, 
                               ifelse(pct_growth < 0, 0, pct_growth)))

states_solar_graph <- states_solar %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = states_solar, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = states_solar_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth, size = value), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = "Y-o-Y Growth",
                       limits = c(0,.50),
                       breaks = c(0,.1,.2,.3,.4,.5),
                       labels = c("<0%+","+10%","+20%","+30%","+40%","+50%+"),
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill"
  ) +
  scale_size_area(name = "Generation",
                  max_size = 17,
                  breaks = c(0,1000,2000,3000,4000,5000),
                  labels = c("0TWh","1TWh","2TWh","3TWh","4TWh","5TWh"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("         Solar Generation by State: March 2024") +
  labs(caption = "Graph created by @JosephPolitano using EIA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = states_solar_graph, "States Solar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

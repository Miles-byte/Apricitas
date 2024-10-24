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


EU_BATTERY_MANUFACTURING <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/sts_inpr_m/1.0/M.PRD.C272.SCA.I21.EU27_2020?compress=false")) %>%
  transmute(value = as.numeric(OBS_VALUE), date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[1]*100)

REAL_EU_BATTERY_MANU_graph <- ggplot() + #plotting real battery shipments
  geom_line(data=EU_BATTERY_MANUFACTURING, aes(x=date,y= value,color="EU Industrial Production: Batteries and Accumulators"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,350), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("The EU's Battery Manufacturing Boom") +
  labs(caption = "Graph created by @JosephPolitano using Census and BLS Data",subtitle = "EU Battery Manufacturing Has Grown Significantly Over the Last 5 Years") +
  theme_apricitas + theme(legend.position = c(.3,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*240), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_EU_BATTERY_MANU_graph, "Real EU Battery Manufacturing graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing





p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
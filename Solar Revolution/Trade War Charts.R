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


US_SOLAR_IMPORTS_BULK_TARIFFS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR","CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "854142",
  I_COMMODITY = "854143",
  CTY_CODE = "-", #TOTAL
)

US_SOLAR_CELL_IMPORTS <- US_SOLAR_IMPORTS_BULK_TARIFFS %>%
  filter(I_COMMODITY == 854142) %>%
  select(time,I_COMMODITY, CON_VAL_MO, GEN_VAL_MO, CAL_DUT_MO) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO),CAL_DUT_MO = as.numeric(CAL_DUT_MO), CON_VAL_MO = as.numeric(CON_VAL_MO), tariff = CAL_DUT_MO/CON_VAL_MO)

US_SOLAR_PANEL_IMPORTS <- US_SOLAR_IMPORTS_BULK_TARIFFS %>%
  filter(I_COMMODITY == 854143) %>%
  select(time,I_COMMODITY, CON_VAL_MO, GEN_VAL_MO, CAL_DUT_MO) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO),CAL_DUT_MO = as.numeric(CAL_DUT_MO), CON_VAL_MO = as.numeric(CON_VAL_MO), tariff = CAL_DUT_MO/CON_VAL_MO)

US_SOLAR_PANEL_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-08-01"), y = 21, size = 4,color = "white", lineheight = 0.8, hjust = 1) +
  annotate(geom = "vline",x = as.Date("2024-06-01"), xintercept = as.Date("2024-06-01"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Antidumping\nTariffs\nStart\nIncreasing",x = as.Date("2024-06-15"), y = 21, size = 4,color = "white", lineheight = 0.8, hjust = 0) +
  geom_line(data=US_SOLAR_PANEL_IMPORTS, aes(x=date,y= CON_VAL_MO/1000000000*12,color= "US Solar Panel Imports,\nMonthly Annualized"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Dollars, Monthly Annualized") +
  ggtitle("US Solar Panel Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census International Trade data",subtitle = "US Solar Panels Imports Have Sunk to the Lowest Levels in Two Years Amidst Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*27.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_SOLAR_PANEL_IMPORTS_GRAPH, "US Solar Panel Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_SOLAR_PANEL_TARIFFS_GRAPH <- ggplot() + #plotting integrated circuits exports
  #annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  #annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-08-01"), y = 21, size = 4,color = "white", lineheight = 0.8, hjust = 1) +
  #annotate(geom = "vline",x = as.Date("2024-06-01"), xintercept = as.Date("2024-06-01"), size = 0.75,color = "white", linetype = "dashed") +
  #annotate(geom = "text", label = "Antidumping\nTariffs\nStart\nIncreasing",x = as.Date("2024-06-15"), y = 21, size = 4,color = "white", lineheight = 0.8, hjust = 0) +
  geom_line(data=US_SOLAR_PANEL_IMPORTS, aes(x=date,y= tariff,color= "Effective Tariff Rate\n(Tariffs Paid as a Share of Imports)"), size = 1.25) + 
  #geom_line(data=US_SOLAR_CELL_IMPORTS, aes(x=date,y= tariff,color= "Solar Cells"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,ceiling(max(US_SOLAR_PANEL_IMPORTS$tariff)*100)/100), breaks = c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16), expand = c(0,0)) +
  ylab("Effective Tariff Rate, %") +
  ggtitle("US Solar Panel Effective Tariff Rate") +
  labs(caption = "Graph created by @JosephPolitano using Census International Trade data",subtitle = "US Solar Panels Imports Have Sunk to the Lowest Levels in Two Years Amidst Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*(ceiling(max(US_SOLAR_PANEL_IMPORTS$tariff)*100)/100)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_SOLAR_PANEL_TARIFFS_GRAPH, "US Solar Panel Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE




US_LION_EV_BATTERY_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "8507204000",
  #I_COMMODITY = "8507304000",
  #I_COMMODITY = "8507404000",
  I_COMMODITY = "8507600010",
  #I_COMMODITY = "8507804000",
  #I_COMMODITY = "8507804100",
)

US_LION_EV_BATTERY_IMPORTS <- US_LION_EV_BATTERY_IMPORTS_BULK %>%
  mutate(value = as.numeric(CON_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  group_by(date, name) %>%
  summarise(value = sum(as.numeric(value))) %>%
  ungroup() %>%
  pivot_wider()

US_LION_EV_BATTERY_IMPORTS_BREAKDOWN <- US_LION_EV_BATTERY_IMPORTS %>%
  mutate(Mexico = replace_na(Mexico, 0)) %>%
  mutate(`Hong Kong` = replace_na(`Hong Kong`,0)) %>%
  #mutate(date, `Net Exports`=`Total For All Countries`, `China` = `China` + `Hong Kong`) %>%
  mutate(date, `Net Exports`=`Total For All Countries`) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollEU = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`European Union`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Korea, South`,12))) %>%
  mutate(rollMX = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Mexico`,12))) %>%
  mutate(rollJP = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Japan`,12))) %>%
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China`,12)))


US_LION_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-07-01"), y = 3.5, size = 4,color = "white", lineheight = 0.8,hjust = 1) +
  annotate(geom = "vline",x = as.Date("2024-10-01"), xintercept = as.Date("2024-10-01"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Tariffs\non China\nIncreased",x = as.Date("2024-11-01"), y = 3.5, size = 4,color = "white", lineheight = 0.8, hjust = 0) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union`*12/1000000000,color= "EU"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico`*12/1000000000,color= "Mexico"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South`*12/1000000000,color= "South Korea"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan`*12/1000000000,color= "Japan"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`China`*12/1000000000,color= "China"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollJP/1000000000,color= "Japan"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollSK/1000000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollMX/1000000000,color= "Mexico"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollEU/1000000000,color= "EU"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=rollCN/1000000000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"),limits = c(0,5),breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Lithium-ion EV Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "Gross Imports of EV Batteries Have Surged Over the Last Few Years, Led by China") +
  theme_apricitas + theme(legend.position = c(.33,.70)) +
  scale_color_manual(name= "US Gross Imports of Lithium-ion EV Batteries\nSolid = Rolling 12M Total, Dashed = Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU","South Korea","Mexico","Japan","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_LION_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH, "US Lion EV Battery Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_LION_EV_BATTERY_IMPORTS_BREAKDOWN_MONTHLY_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-07-01"), y = 3.5, size = 4,color = "white", lineheight = 0.8,hjust = 1) +
  annotate(geom = "vline",x = as.Date("2024-10-01"), xintercept = as.Date("2024-10-01"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Tariffs\non China\nIncreased",x = as.Date("2024-11-01"), y = 3.5, size = 4,color = "white", lineheight = 0.8, hjust = 0) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan`*12/1000000000,color= "Japan"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South`*12/1000000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico`*12/1000000000,color= "Mexico"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union`*12/1000000000,color= "EU"), size = 1.25) +
  geom_line(data= filter(US_LION_EV_BATTERY_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`China`*12/1000000000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"),limits = c(0,5),breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Lithium-ion EV Battery Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "Gross Imports of EV Batteries Have Surged Over the Last Few Years, Led by China") +
  theme_apricitas + theme(legend.position = c(.33,.70)) +
  scale_color_manual(name= "US Gross Imports of Lithium-ion EV Batteries\nMonthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU","South Korea","Mexico","Japan","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_LION_EV_BATTERY_IMPORTS_BREAKDOWN_MONTHLY_GRAPH, "US Lion EV Battery Imports Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



US_BATTERY_PARTS_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "8507204000",
  #I_COMMODITY = "8507304000",
  #I_COMMODITY = "8507404000",
  I_COMMODITY = "8507904000",
  #I_COMMODITY = "8507804000",
  #I_COMMODITY = "8507804100",
)

US_BATTERY_PARTS_IMPORTS <- US_BATTERY_PARTS_IMPORTS_BULK %>%
  mutate(value = as.numeric(CON_VAL_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date) %>%
  group_by(date, name) %>%
  summarise(value = sum(as.numeric(value))) %>%
  ungroup() %>%
  pivot_wider()

US_BATTERY_PARTS_IMPORTS_BREAKDOWN <- US_BATTERY_PARTS_IMPORTS %>%
  mutate(Mexico = replace_na(Mexico, 0)) %>%
  mutate(`Hong Kong` = replace_na(`Hong Kong`,0)) %>%
  #mutate(date, `Net Exports`=`Total For All Countries`, `China` = `China` + `Hong Kong`) %>%
  mutate(date, `Net Exports`=`Total For All Countries`) %>%
  mutate(rollnetexports = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Net Exports`,12))) %>%
  mutate(rollEU = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`European Union`,12))) %>%
  mutate(rollSK = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Korea, South`,12))) %>%
  mutate(rollMX = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Mexico`,12))) %>%
  mutate(rollJP = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`Japan`,12))) %>%
  mutate(rollCN = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(`China`,12)))


US_BATTERY_PARTS_TARIFF_BULK <- US_BATTERY_PARTS_IMPORTS_BULK %>%
  mutate(value = as.numeric(CON_VAL_MO)) %>%
  mutate(tariff = as.numeric(CAL_DUT_MO)) %>%
  mutate(name = str_to_title (CTY_NAME)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(value,name,date,tariff) %>%
  group_by(date, name) %>%
  summarise(value = sum(as.numeric(value)), tariff = sum(as.numeric(tariff))) %>%
  mutate(tariff_rate = tariff/value) %>%
  ungroup()

US_BATTERY_PARTS_TARIFF_CHINA <- US_BATTERY_PARTS_TARIFF_BULK %>%
  filter(name == "China")

US_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  #annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-04-01"), y = 3, size = 4,color = "white", lineheight = 0.8) +
  #geom_line(data= filter(US_LION_EV_BATTERY_TARIFF_CHINA, date >= as.Date("2017-12-01")), aes(x=date,y= tariff*12/1000000000,color= "tariff"), size = 1.25) +
  geom_line(data= filter(US_BATTERY_PARTS_TARIFF_CHINA, date >= as.Date("2023-12-01")), aes(x=date,y= tariff_rate,color= "tariff rate"), size = 1.25)
  
  
  
US_BATTERY_PARTS_BREAKDOWN_MONTH_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  geom_line(data= filter(US_BATTERY_PARTS_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Japan`*12/1000000,color= "Japan"), size = 1.25) +
  geom_hline(yintercept = 0, color = "white", size = 0.5) +
  annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-07-01"), y = 125, size = 4,color = "white", lineheight = 0.8,hjust = 1) +
  annotate(geom = "vline",x = as.Date("2024-10-01"), xintercept = as.Date("2024-10-01"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Tariffs\non China\nIncreased",x = as.Date("2024-11-01"), y = 125, size = 4,color = "white", lineheight = 0.8, hjust = 0) +
  geom_line(data= filter(US_BATTERY_PARTS_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Korea, South`*12/1000000,color= "South Korea"), size = 1.25) +
  geom_line(data= filter(US_BATTERY_PARTS_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`Mexico`*12/1000000,color= "Mexico"), size = 1.25) +
  geom_line(data= filter(US_BATTERY_PARTS_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`European Union`*12/1000000,color= "EU"), size = 1.25) +
  geom_line(data= filter(US_BATTERY_PARTS_IMPORTS_BREAKDOWN, date >= as.Date("2017-12-01")), aes(x=date,y=`China`*12/1000000,color= "China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "M"),limits = c(0,150),breaks = c(0,50,100,150), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Lead-Acid Battery Part Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "Gross Imports of EV Batteries Have Surged Over the Last Few Years, Led by China") +
  theme_apricitas + theme(legend.position = c(.33,.80)) +
  scale_color_manual(name= "Imports, Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU","South Korea","Mexico","Japan","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(150)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_BATTERY_PARTS_BREAKDOWN_MONTH_GRAPH, "US Battery Parts Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


  


US_CRITIAL_MINERALS_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "2602",
  I_COMMODITY = "2605",
  I_COMMODITY = "2606",
  I_COMMODITY = "2608",
  I_COMMODITY = "2610",
  I_COMMODITY = "26110060",
  I_COMMODITY = "28259030",
  I_COMMODITY = "284180",
  I_COMMODITY = "284441",
  I_COMMODITY = "284442",
  I_COMMODITY = "284443",
  I_COMMODITY = "284444",
  I_COMMODITY = "28499030", 
  I_COMMODITY = "720260",
  I_COMMODITY = "72029340",
  I_COMMODITY = "72029380", 
  I_COMMODITY = "790111",
  I_COMMODITY = "79011210",
  I_COMMODITY = "79011250",
  I_COMMODITY = "790120", 
  I_COMMODITY = "800110", 
  I_COMMODITY = "800120", 
  I_COMMODITY = "810110", 
  I_COMMODITY = "810320",
  I_COMMODITY = "811221",
  I_COMMODITY = "81129230",
)


US_CRITIAL_MINERALS_IMPORTS_CHINA <- US_CRITIAL_MINERALS_IMPORTS_BULK %>%
  #filter(CTY_NAME %in% c("CHINA","TOTAL FOR ALL COUNTRIES")) %>%
  filter(CTY_NAME == "CHINA") %>%
  group_by(time, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(as.numeric(CON_VAL_MO)),
            GEN_VAL_MO = sum(as.numeric(CON_VAL_MO)),
            CAL_DUT_MO = sum(as.numeric(CAL_DUT_MO))) %>%
  transmute(date = as.Date(paste0(time, "-01")), CON_VAL_MO, GEN_VAL_MO, CAL_DUT_MO, CTY_NAME)


US_EV_BATTERY_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  #annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-04-01"), y = 3, size = 4,color = "white", lineheight = 0.8) +
  #geom_line(data= filter(US_LION_EV_BATTERY_TARIFF_CHINA, date >= as.Date("2017-12-01")), aes(x=date,y= tariff*12/1000000000,color= "tariff"), size = 1.25) +
  geom_line(data= filter(US_CRITIAL_MINERALS_IMPORTS_CHINA, date >= as.Date("2015-12-01")), aes(x=date,y= CON_VAL_MO*12/1000000,color= "Critical Minerals"), size = 1.25)




US_KEY_STEEL_ALUM_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "72",
  #I_COMMODITY = "73",
  I_COMMODITY = "76",
)

US_STEEL_ALUM_BULK_CHINA <- US_KEY_STEEL_ALUM_BULK %>%
  filter(CTY_NAME == "CHINA") %>%
  #filter(I_COMMODITY == "722920") %>%
  group_by(time) %>%
  summarise(CON_VAL_MO = sum(as.numeric(CON_VAL_MO)),
            GEN_VAL_MO = sum(as.numeric(CON_VAL_MO)),
            CAL_DUT_MO = sum(as.numeric(CAL_DUT_MO))) %>%
  transmute(date = as.Date(paste0(time, "-01")), CON_VAL_MO, GEN_VAL_MO, CAL_DUT_MO)

US_KEY_MATERIALS_IMPORTS_BREAKDOWN_GRAPH <- ggplot() + #plotting US Net Imports of EVs
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #annotate(geom = "vline",x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), size = 0.75,color = "white", linetype = "dashed") +
  #annotate(geom = "text", label = "IRA\nPassage",x = as.Date("2022-04-01"), y = 3, size = 4,color = "white", lineheight = 0.8) +
  #geom_line(data= filter(US_STEEL_ALUM_BULK_CHINA, date >= as.Date("2013-12-01")), aes(x=date,y= CAL_DUT_MO/GEN_VAL_MO,color= "tariff"), size = 1.25)
  geom_line(data= filter(US_CRITIAL_MINERALS_IMPORTS_CHINA, date >= as.Date("2017-12-01")), aes(x=date,y= CON_VAL_MO*12/1000000,color= "Critical Minerals"), size = 1.25) +
  geom_line(data= filter(US_STEEL_ALUM_BULK_CHINA, date >= as.Date("2017-12-01")), aes(x=date,y= CON_VAL_MO*12/1000000,color= "Steel & Aluminum Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "M"),limits = c(0,1000),breaks = c(0,250,500,750,1000), expand = c(0,0)) +
  ylab("Millions of Dollars") +
  ggtitle("US Imports of Key Materials From China") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "After Stockpiling, US Imports of Key Materials From China Have Fallen") +
  theme_apricitas + theme(legend.position = c(.33,.80)) +
  scale_color_manual(name= "Imports, Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Critical Minerals","Steel & Aluminum Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*((today()-as.Date("2018-01-01")))), ymin = 0-(.3*(1000)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = US_KEY_MATERIALS_IMPORTS_BREAKDOWN_GRAPH, "US Key Materials Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_KEY_STEEL_ALUM_BULK <- getCensus(
    name = "timeseries/intltrade/imports/hs",
    vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
    time = paste("from 2013 to", format(Sys.Date(), "%Y")),
I_COMMODITY = "720610",
I_COMMODITY = "720690",
I_COMMODITY = "720711",
I_COMMODITY = "720712",
I_COMMODITY = "720719", 
I_COMMODITY = "720720",
I_COMMODITY = "72081015",
I_COMMODITY = "72081030",
I_COMMODITY = "72081060",
I_COMMODITY = "72082530",
I_COMMODITY = "72082560",
I_COMMODITY = "720826",
I_COMMODITY = "720827",
I_COMMODITY = "720836",
I_COMMODITY = "720837",
I_COMMODITY = "720838",
I_COMMODITY = "720839",
I_COMMODITY = "72084030",
I_COMMODITY = "72084060",
I_COMMODITY = "720851",
I_COMMODITY = "720852",
I_COMMODITY = "720853",
I_COMMODITY = "720854",
I_COMMODITY = "720890",
I_COMMODITY = "720915",
I_COMMODITY = "720916",
I_COMMODITY = "720917",
I_COMMODITY = "72091815",
I_COMMODITY = "72091825",
I_COMMODITY = "72091860",
I_COMMODITY = "720925",
I_COMMODITY = "720926",
I_COMMODITY = "720927",
I_COMMODITY = "720928",
I_COMMODITY = "720990",
I_COMMODITY = "721011",
I_COMMODITY = "721012",
I_COMMODITY = "721020",
I_COMMODITY = "721030",
I_COMMODITY = "721041",
I_COMMODITY = "721049",
I_COMMODITY = "721050",
I_COMMODITY = "721061",
I_COMMODITY = "721069",
I_COMMODITY = "72107030",
I_COMMODITY = "72107060",
I_COMMODITY = "72109010",
I_COMMODITY = "72109060",
I_COMMODITY = "72109090",
I_COMMODITY = "721113",
I_COMMODITY = "721114",
I_COMMODITY = "72111915",
I_COMMODITY = "72111920",
I_COMMODITY = "72111930",
I_COMMODITY = "72111945",
I_COMMODITY = "72111960",
I_COMMODITY = "72111975",
I_COMMODITY = "72112315",
I_COMMODITY = "72112320",
I_COMMODITY = "72112330",
I_COMMODITY = "72112345",
I_COMMODITY = "72112360",
I_COMMODITY = "72112920",
I_COMMODITY = "72112945",
I_COMMODITY = "72112960",
I_COMMODITY = "721190",
I_COMMODITY = "721210",
I_COMMODITY = "721220",
I_COMMODITY = "72123010",
I_COMMODITY = "72123030",
I_COMMODITY = "72123050",
I_COMMODITY = "72124010",
I_COMMODITY = "72124050",
I_COMMODITY = "721250",
I_COMMODITY = "721260",
I_COMMODITY = "721310",
I_COMMODITY = "721320",
I_COMMODITY = "72139130",
I_COMMODITY = "72139145",
I_COMMODITY = "72139160",
I_COMMODITY = "721399",
I_COMMODITY = "721420",
I_COMMODITY = "721430",
I_COMMODITY = "721491",
I_COMMODITY = "721499",
I_COMMODITY = "721510",
I_COMMODITY = "721550",
I_COMMODITY = "72159010",
I_COMMODITY = "72159030",
I_COMMODITY = "72159050",
I_COMMODITY = "721610",
I_COMMODITY = "721621",
I_COMMODITY = "721622",
I_COMMODITY = "721631",
I_COMMODITY = "721632",
I_COMMODITY = "721633",
I_COMMODITY = "721640",
I_COMMODITY = "721650",
I_COMMODITY = "721699",
I_COMMODITY = "72171010",
I_COMMODITY = "72171020",
I_COMMODITY = "72171030",
I_COMMODITY = "72171050",
I_COMMODITY = "72171060",
I_COMMODITY = "72171070",
I_COMMODITY = "72171080",
I_COMMODITY = "72171090",
I_COMMODITY = "72172015",
I_COMMODITY = "72172030",
I_COMMODITY = "72172045",
I_COMMODITY = "72172060",
I_COMMODITY = "72172075",
I_COMMODITY = "72173015",
I_COMMODITY = "72173030",
I_COMMODITY = "72173060",
I_COMMODITY = "72173075",
I_COMMODITY = "72179010",
I_COMMODITY = "72179050",
I_COMMODITY = "721810",
I_COMMODITY = "721891",
I_COMMODITY = "721899",
I_COMMODITY = "721911",
I_COMMODITY = "721912",
I_COMMODITY = "721913",
I_COMMODITY = "721914",
I_COMMODITY = "721921",
I_COMMODITY = "721922",
I_COMMODITY = "721923",
I_COMMODITY = "721924",
I_COMMODITY = "721931",
I_COMMODITY = "721932",
I_COMMODITY = "721933",
I_COMMODITY = "721934",
I_COMMODITY = "721935",
I_COMMODITY = "721990",
I_COMMODITY = "72201210",
I_COMMODITY = "72201250",
I_COMMODITY = "72202010",
I_COMMODITY = "72202060",
I_COMMODITY = "72202070",
I_COMMODITY = "72202080",
I_COMMODITY = "722090",
I_COMMODITY = "7221",
I_COMMODITY = "722211",
I_COMMODITY = "722219",
I_COMMODITY = "722220",
I_COMMODITY = "722230",
I_COMMODITY = "72224030",
I_COMMODITY = "72224060",
I_COMMODITY = "72230010",
I_COMMODITY = "72230050",
I_COMMODITY = "72230090",
I_COMMODITY = "722410",
I_COMMODITY = "722490",
I_COMMODITY = "722511",
I_COMMODITY = "722519",
I_COMMODITY = "72253011",
I_COMMODITY = "72253030",
I_COMMODITY = "72253051",
I_COMMODITY = "72253070",
I_COMMODITY = "72254011",
I_COMMODITY = "72254030",
I_COMMODITY = "72254051",
I_COMMODITY = "72254070",
I_COMMODITY = "72255011",
I_COMMODITY = "72255060",
I_COMMODITY = "72255070",
I_COMMODITY = "72255080",
I_COMMODITY = "722591",
I_COMMODITY = "722592",
I_COMMODITY = "722599",
I_COMMODITY = "72261110",
I_COMMODITY = "72261190",
I_COMMODITY = "72261910",
I_COMMODITY = "72261990",
I_COMMODITY = "722620",
I_COMMODITY = "72269105",
I_COMMODITY = "72269115",
I_COMMODITY = "72269125",
I_COMMODITY = "72269150",
I_COMMODITY = "72269170",
I_COMMODITY = "72269180",
I_COMMODITY = "72269210",
I_COMMODITY = "72269230",
I_COMMODITY = "72269250",
I_COMMODITY = "72269270",
I_COMMODITY = "72269280",
I_COMMODITY = "72269901",
I_COMMODITY = "722710",
I_COMMODITY = "722720",
I_COMMODITY = "72279010",
I_COMMODITY = "72279020",
I_COMMODITY = "72279060",
I_COMMODITY = "72282010",
I_COMMODITY = "72282050",
I_COMMODITY = "72283040",
I_COMMODITY = "72283060",
I_COMMODITY = "72283080",
I_COMMODITY = "722840",
I_COMMODITY = "72285010",
I_COMMODITY = "72285050",
I_COMMODITY = "72286010",
I_COMMODITY = "72286060",
I_COMMODITY = "72286080",
I_COMMODITY = "72287030",
I_COMMODITY = "72287060",
I_COMMODITY = "722920",
I_COMMODITY = "72299010",
I_COMMODITY = "72299050",
I_COMMODITY = "72299090",
I_COMMODITY = "730110",
I_COMMODITY = "73021010",
I_COMMODITY = "73021050",
I_COMMODITY = "730240",
I_COMMODITY = "73029010",
I_COMMODITY = "73029090",
I_COMMODITY = "730411",
I_COMMODITY = "73041910",
I_COMMODITY = "73041950",
I_COMMODITY = "730422",
I_COMMODITY = "73042330",
I_COMMODITY = "73042360",
I_COMMODITY = "73042430",
I_COMMODITY = "73042440",
I_COMMODITY = "73042460",
I_COMMODITY = "73042910",
I_COMMODITY = "73042920",
I_COMMODITY = "73042931",
I_COMMODITY = "73042941",
I_COMMODITY = "73042950",
I_COMMODITY = "73042961",
I_COMMODITY = "73043130",
I_COMMODITY = "73043160",
I_COMMODITY = "730439",
I_COMMODITY = "730449",
I_COMMODITY = "73045110",
I_COMMODITY = "73045150",
I_COMMODITY = "73045910",
I_COMMODITY = "73045920",
I_COMMODITY = "73045960",
I_COMMODITY = "73045980",
I_COMMODITY = "73049010",
I_COMMODITY = "73049070",
I_COMMODITY = "73051110",
I_COMMODITY = "73051150",
I_COMMODITY = "73051210",
I_COMMODITY = "73051250",
I_COMMODITY = "73051910",
I_COMMODITY = "73051950",
I_COMMODITY = "73052020",
I_COMMODITY = "73052040",
I_COMMODITY = "73052060",
I_COMMODITY = "73052080",
I_COMMODITY = "73053120",
I_COMMODITY = "73053140",
I_COMMODITY = "73053160",
I_COMMODITY = "73053910",
I_COMMODITY = "73053950",
I_COMMODITY = "73059010",
I_COMMODITY = "73059050",
I_COMMODITY = "730611",
I_COMMODITY = "73061910",
I_COMMODITY = "73061951",
I_COMMODITY = "73062130",
I_COMMODITY = "73062140",
I_COMMODITY = "73062180",
I_COMMODITY = "73062910",
I_COMMODITY = "73062920",
I_COMMODITY = "73062931",
I_COMMODITY = "73062941",
I_COMMODITY = "73062960",
I_COMMODITY = "73062981",
I_COMMODITY = "73063010",
I_COMMODITY = "73063030",
I_COMMODITY = "73063050",
I_COMMODITY = "73064010",
I_COMMODITY = "73064050",
I_COMMODITY = "73065010",
I_COMMODITY = "73065030",
I_COMMODITY = "73065050",
I_COMMODITY = "73066110",
I_COMMODITY = "73066130",
I_COMMODITY = "73066150",
I_COMMODITY = "73066170",
I_COMMODITY = "73066910",
I_COMMODITY = "73066930",
I_COMMODITY = "73066970",
I_COMMODITY = "73069010",
I_COMMODITY = "73069050",
I_COMMODITY = "76011030",
I_COMMODITY = "76011060",
I_COMMODITY = "76012030",
I_COMMODITY = "76012060",
I_COMMODITY = "76012090",
I_COMMODITY = "76041010",
I_COMMODITY = "76041030",
I_COMMODITY = "76041050",
I_COMMODITY = "760421",
I_COMMODITY = "76042910",
I_COMMODITY = "76042930",
I_COMMODITY = "76042950",
I_COMMODITY = "760511",
I_COMMODITY = "760519",
I_COMMODITY = "760521",
I_COMMODITY = "760529",
I_COMMODITY = "76061130",
I_COMMODITY = "76061160",
I_COMMODITY = "76061230",
I_COMMODITY = "76061260",
I_COMMODITY = "76069130",
I_COMMODITY = "76069160",
I_COMMODITY = "76069230",
I_COMMODITY = "76069260",
I_COMMODITY = "76071130",
I_COMMODITY = "76071160",
I_COMMODITY = "76071190",
I_COMMODITY = "76071960",
I_COMMODITY = "76072010",
I_COMMODITY = "760810",
I_COMMODITY = "760820",
I_COMMODITY = "760900",)



US_CHINA_TARIFFED_GOODS1 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2023 to", format(Sys.Date(), "%Y")),
  CTY_CODE = "5700", #FROM CHINA
  I_COMMODITY ="870360",
  I_COMMODITY ="870370",
  I_COMMODITY ="870380",
  I_COMMODITY ="8703900100",
  I_COMMODITY ="6307909842",
  I_COMMODITY ="6307909844",
  I_COMMODITY ="6307909850",
  I_COMMODITY ="6307909870",
  I_COMMODITY ="6307909875",
  I_COMMODITY ="8507600010",
  I_COMMODITY ="8507600020",
  I_COMMODITY ="2602",
  I_COMMODITY ="2605",
  I_COMMODITY ="2606",
  I_COMMODITY ="2608",
  I_COMMODITY ="2610",
  I_COMMODITY ="2611006000",
  I_COMMODITY ="284180",
  I_COMMODITY ="284441",
  I_COMMODITY ="284442",
  I_COMMODITY ="284443",
  I_COMMODITY ="284444",
  I_COMMODITY ="2849903000",
  I_COMMODITY ="720260",
  I_COMMODITY ="790111",
  I_COMMODITY ="790120",
  I_COMMODITY ="800110",
  I_COMMODITY ="800120",
  I_COMMODITY ="810110",
  I_COMMODITY ="810320",
  I_COMMODITY ="811221",
  I_COMMODITY ="850511",
  I_COMMODITY ="854110",
  I_COMMODITY ="854121",
  I_COMMODITY ="854129",
  I_COMMODITY ="854130",
  I_COMMODITY ="854151",
  I_COMMODITY ="854159",
  I_COMMODITY ="854190",
  I_COMMODITY ="854231",
  I_COMMODITY ="854232",
  I_COMMODITY ="854233",
  I_COMMODITY ="854239",
  I_COMMODITY ="854290",
  I_COMMODITY ="854142",
  I_COMMODITY ="854143",
  I_COMMODITY = "720610",
  I_COMMODITY = "720690",
  I_COMMODITY = "720711",
  I_COMMODITY = "720712",
  I_COMMODITY = "720719", 
  I_COMMODITY = "720720",
  I_COMMODITY = "720826",
  I_COMMODITY = "720827",
  I_COMMODITY = "720836",
  I_COMMODITY = "720837",
  I_COMMODITY = "720838",
  I_COMMODITY = "720839",
  I_COMMODITY = "720851",
  I_COMMODITY = "720852",
  I_COMMODITY = "720853",
  I_COMMODITY = "720854",
  I_COMMODITY = "720890",
  I_COMMODITY = "720915",
  I_COMMODITY = "720916",
  I_COMMODITY = "720917",
  I_COMMODITY = "720925",
  I_COMMODITY = "720926",
  I_COMMODITY = "720927",
  I_COMMODITY = "720928",
  I_COMMODITY = "720990",
  I_COMMODITY = "721011",
  I_COMMODITY = "721012",
  I_COMMODITY = "721020",
  I_COMMODITY = "721030",
  I_COMMODITY = "721041",
  I_COMMODITY = "721049",
  I_COMMODITY = "721050",
  I_COMMODITY = "721061",
  I_COMMODITY = "721069",
  I_COMMODITY = "721113",
  I_COMMODITY = "721114",
  I_COMMODITY = "721190",
  I_COMMODITY = "721210",
  I_COMMODITY = "721220",
  I_COMMODITY = "721250",
  I_COMMODITY = "721260",
  I_COMMODITY = "721310",
  I_COMMODITY = "721320",
  I_COMMODITY = "721399",
  I_COMMODITY = "721420",
  I_COMMODITY = "721430",
  I_COMMODITY = "721491",
  I_COMMODITY = "721499",
  I_COMMODITY = "721510",
  I_COMMODITY = "721550",
  I_COMMODITY = "721610",
  I_COMMODITY = "721621",
  I_COMMODITY = "721622",
  I_COMMODITY = "721631",
  I_COMMODITY = "721632",
  I_COMMODITY = "721633",
  I_COMMODITY = "721640",
  I_COMMODITY = "721650",
  I_COMMODITY = "721699",
  I_COMMODITY = "721810",
  I_COMMODITY = "721891",
  I_COMMODITY = "721899",
  I_COMMODITY = "721911",
  I_COMMODITY = "721912",
  I_COMMODITY = "721913",
  I_COMMODITY = "721914",
  I_COMMODITY = "721921",
  I_COMMODITY = "721922",
  I_COMMODITY = "721923",
  I_COMMODITY = "721924",
  I_COMMODITY = "721931",
  I_COMMODITY = "721932",
  I_COMMODITY = "721933",
  I_COMMODITY = "721934",
  I_COMMODITY = "721935",
  I_COMMODITY = "721990",
  I_COMMODITY = "722090",
  I_COMMODITY = "7221",
  I_COMMODITY = "722211",
  I_COMMODITY = "722219",
  I_COMMODITY = "722220",
  I_COMMODITY = "722230",
  I_COMMODITY = "722410",
  I_COMMODITY = "722490",
  I_COMMODITY = "722511",
  I_COMMODITY = "722519",
  I_COMMODITY = "722591",
  I_COMMODITY = "722592",
  I_COMMODITY = "722599",
  I_COMMODITY = "722620",
  I_COMMODITY = "722710",
  I_COMMODITY = "722720",
  I_COMMODITY = "722840",
  I_COMMODITY = "722920",
  I_COMMODITY = "730110",
  I_COMMODITY = "730240",
  I_COMMODITY = "730411",
  I_COMMODITY = "730422",
  I_COMMODITY = "730439",
  I_COMMODITY = "730449",
  I_COMMODITY = "730611",
)

US_CHINA_TARIFFED_GOODS2 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO"), 
  time = paste("from 2023 to", format(Sys.Date(), "%Y")),
  CTY_CODE = "5700", #FROM CHINA
  I_COMMODITY = "7306218010",
  I_COMMODITY = "7306218050",
  I_COMMODITY = "7306291030",
  I_COMMODITY = "7306291090",
  I_COMMODITY = "7306292000",
  I_COMMODITY = "7306293100",
  I_COMMODITY = "7306294100",
  I_COMMODITY = "7306296010",
  I_COMMODITY = "7306296050",
  I_COMMODITY = "7306298110",
  I_COMMODITY = "7306298150",
  I_COMMODITY = "7306301000",
  I_COMMODITY = "7306303000",
  I_COMMODITY = "7306305010",
  I_COMMODITY = "7306305015",
  I_COMMODITY = "7306305020",
  I_COMMODITY = "7306305025",
  I_COMMODITY = "7306305026",
  I_COMMODITY = "7306305027",
  I_COMMODITY = "7306305028",
  I_COMMODITY = "7306305031",
  I_COMMODITY = "7306305032",
  I_COMMODITY = "7306305033",
  I_COMMODITY = "7306305035",
  I_COMMODITY = "7306305040",
  I_COMMODITY = "7306305041",
  I_COMMODITY = "7306305043",
  I_COMMODITY = "7306305055",
  I_COMMODITY = "7306305056",
  I_COMMODITY = "7306305057",
  I_COMMODITY = "7306305085",
  I_COMMODITY = "7306305090",
  I_COMMODITY = "7306401010",
  I_COMMODITY = "7306401015",
  I_COMMODITY = "7306405005",
  I_COMMODITY = "7306405015",
  I_COMMODITY = "7306501000",
  I_COMMODITY = "7306503000",
  I_COMMODITY = "7306505010",
  I_COMMODITY = "7306505030",
  I_COMMODITY = "7306505050",
  I_COMMODITY = "7306505070",
  I_COMMODITY = "7306611000",
  I_COMMODITY = "7306613000",
  I_COMMODITY = "7306615000",
  I_COMMODITY = "7306617030",
  I_COMMODITY = "7306617060",
  I_COMMODITY = "7306691000",
  I_COMMODITY = "7306693000",
  I_COMMODITY = "7306697030",
  I_COMMODITY = "7306697060",
  I_COMMODITY = "7306901000",
  I_COMMODITY = "7306905000",
  I_COMMODITY = "7601103000",
  I_COMMODITY = "7601106030",
  I_COMMODITY = "7601106090",
  I_COMMODITY = "7601203000",
  I_COMMODITY = "7601206000",
  I_COMMODITY = "7601209030",
  I_COMMODITY = "7601209045",
  I_COMMODITY = "7601209060",
  I_COMMODITY = "7601209075",
  I_COMMODITY = "7601209080",
  I_COMMODITY = "7601209085",
  I_COMMODITY = "7601209095",
  I_COMMODITY = "7604101000",
  I_COMMODITY = "7604103000",
  I_COMMODITY = "7604105000",
  I_COMMODITY = "760421",
  I_COMMODITY = "7604291010",
  I_COMMODITY = "7604291090",
  I_COMMODITY = "7604293030",
  I_COMMODITY = "7604293060",
  I_COMMODITY = "7604293090",
  I_COMMODITY = "7604295020",
  I_COMMODITY = "7604295050",
  I_COMMODITY = "7604295090",
  I_COMMODITY = "760511",
  I_COMMODITY = "760519",
  I_COMMODITY = "760521",
  I_COMMODITY = "760529",
  I_COMMODITY = "7606113030",
  I_COMMODITY = "7606113060",
  I_COMMODITY = "7606116000",
  I_COMMODITY = "7606123015",
  I_COMMODITY = "7606123025",
  I_COMMODITY = "7606123035",
  I_COMMODITY = "7606123045",
  I_COMMODITY = "7606123055",
  I_COMMODITY = "7606123091",
  I_COMMODITY = "7606123096",
  I_COMMODITY = "7606126000",
  I_COMMODITY = "7606913055",
  I_COMMODITY = "7606913095",
  I_COMMODITY = "7606916055",
  I_COMMODITY = "7606916095",
  I_COMMODITY = "7606923025",
  I_COMMODITY = "7606923035",
  I_COMMODITY = "7606926055",
  I_COMMODITY = "7606926095",
  I_COMMODITY = "7607113000", 
  I_COMMODITY = "7607116010",
  I_COMMODITY = "7607116090",
  I_COMMODITY = "7607119030",
  I_COMMODITY = "7607119060",
  I_COMMODITY = "7607119090",
  I_COMMODITY = "7607196000",
  I_COMMODITY = "7607201000",
  I_COMMODITY = "760810",
  I_COMMODITY = "760820",
  I_COMMODITY = "7609",
  I_COMMODITY = "901831",
  I_COMMODITY = "901832",
  I_COMMODITY = "810194",
  I_COMMODITY = "8101991000",
  I_COMMODITY = "8101998000",
  I_COMMODITY = "280461",
  I_COMMODITY = "3818",
  I_COMMODITY = "842619",
)

CHINA_IMPORTS_HS10_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO","GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME","CAL_DUT_MO","COMM_LVL"), 
  time = paste("from 2023 to", format(Sys.Date(), "%Y")),
  CTY_CODE = "5700", #FROM CHINA
  COMM_LVL = "HS10", #10 DIGIT HS CODE
)

CHINA_IMPORTS_HS8 <- CHINA_IMPORTS_HS10_BULK %>%
  mutate(I_COMMODITY_HS8 = substr(I_COMMODITY, 1, 8)) %>%
  filter(I_COMMODITY_HS8 %in% c("85414910", "85414970", "85414980", "85414995", "72081015", "72081030", "72081060", "72082530", "72082560", "72084030", "72084060", "72091815", "72091825", "72091860", "72107030", "72107060", "72109010", "72109060", "72109090", "72111915", "72111920", "72111930", "72111945", "72111960", "72111975", "72112315", "72112320", "72112330", "72112345", "72112360", "72112920", "72112945", "72112960", "72123010", "72123030", "72123050", "72124010", "72124050", "72139130", "72139145", "72139160", "72159010", "72159030", "72159050", "72171010", "72171020", "72171030", "72171050", "72171060", "72171070", "72171080", "72171090", "72172015", "72172030", "72172045", "72172060", "72172075", "72173015", "72173030", "72173060", "72173075", "72179010", "72179050", "72201210", "72201250", "72202010", "72202060", "72202070", "72202080", "72224030", "72224060", "72230010", "72230050", "72230090", "72253011", "72253030", "72253051", "72253070", "72254011", "72254030", "72254051", "72254070", "72255011", "72255060", "72255070", "72255080", "72261110", "72261190", "72261910", "72261990", "72269105", "72269115", "72269125", "72269150", "72269170", "72269180", "72269210", "72269230", "72269250", "72269270", "72269280", "72269901", "72279010", "72279020", "72279060", "72282010", "72282050", "72283040", "72283060", "72283080", "72285010", "72285050", "72286010", "72286060", "72286080", "72287030", "72287060", "72299010", "72299050", "72299090", "73021010", "73021050", "73042330", "73042360", "73042430", "73042440", "73042460", "73042910", "73042920", "73042931", "73042941", "73042950", "73042961", "73043130", "73043160", "73041910", "73041950", "73029010", "73029090", "73045110", "73045150", "73045910", "73045920", "73045960", "73045980", "73049010", "73049070", "73051110", "73051150", "73051210", "73051250", "73051910", "73051950", "73052020", "73052040", "73052060", "73052080", "73053120", "73053140", "73053160", "73053910", "73053950", "73059010", "73059050", "73061910", "73061951", "73062130", "73062140", "87024031","87024061","87029031","87029061","40151210","25041010","25041050","25049000","28259030","72029340","72029380","79011210","79011250","81129230")) %>%
  group_by(time,I_COMMODITY_HS8) %>%
  mutate(across(c(CON_VAL_MO, CAL_DUT_MO, GEN_VAL_MO), as.numeric)) %>%
  group_by(time,I_COMMODITY_HS8) %>%
  summarize(across(c(CON_VAL_MO, CAL_DUT_MO, GEN_VAL_MO), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(CTY_NAME = "CHINA", CTY_CODE = "5700") %>%
  mutate(I_COMMODITY = I_COMMODITY_HS8) %>%
  select(-I_COMMODITY_HS8)

US_CHINA_TARIFFED_GOODS <- US_CHINA_TARIFFED_GOODS1 %>%
  transmute(time,I_COMMODITY, CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO), GEN_VAL_MO = as.numeric(GEN_VAL_MO), CTY_NAME, CTY_CODE) %>%
  rbind(US_CHINA_TARIFFED_GOODS2 %>% transmute(time,I_COMMODITY, CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO), GEN_VAL_MO = as.numeric(GEN_VAL_MO), CTY_NAME, CTY_CODE)) %>%
  rbind(CHINA_IMPORTS_HS8)

US_CHINA_TARIFFED_GOODS <- US_CHINA_TARIFFED_GOODS %>%
  mutate(category = case_when(
    grepl("^87", I_COMMODITY) ~ "EVs",
    grepl("^63", I_COMMODITY) ~ "Medical Supplies",
    grepl("^40", I_COMMODITY) ~ "Medical Supplies",
    grepl("^9018", I_COMMODITY) ~ "Medical Supplies",
    grepl("^9018", I_COMMODITY) ~ "Medical Supplies",
    grepl("^85079040", I_COMMODITY) ~ "Non-EV Batteries",
    grepl("^8507600010", I_COMMODITY) ~ "EV Batteries",
    grepl("^8507600020", I_COMMODITY) ~ "Non-EV Batteries",
    grepl("^2504", I_COMMODITY) ~ "Natural Graphite",
    grepl("^8505", I_COMMODITY) ~ "Permanent Magnets",
    grepl("^8541", I_COMMODITY) ~ "Semiconductors",
    grepl("^8542", I_COMMODITY) ~ "Semiconductors",
    grepl("^842619", I_COMMODITY) ~ "Ship-to-Shore Cranes",
    grepl("^72", I_COMMODITY) ~ "Steel & Aluminum Products",
    grepl("^73", I_COMMODITY) ~ "Steel & Aluminum Products",
    grepl("^76", I_COMMODITY) ~ "Steel & Aluminum Products",
    grepl("^3818", I_COMMODITY) ~ "Silicon & Wafers",
    grepl("^280461", I_COMMODITY) ~ "Silicon & Wafers",
    grepl("^26", I_COMMODITY) ~ "Critical Minerals",
    grepl("^28", I_COMMODITY) ~ "Critical Minerals",
    grepl("^79", I_COMMODITY) ~ "Critical Minerals",
    grepl("^80", I_COMMODITY) ~ "Critical Minerals",
    grepl("^81", I_COMMODITY) ~ "Critical Minerals",
    TRUE ~ NA_character_  # Default case
  )) %>%
  mutate(category = case_when(
         grepl("^854142", I_COMMODITY) ~ "Solar Cells",
         grepl("^854143", I_COMMODITY) ~ "Solar Cells",
         grepl("^854143", I_COMMODITY) ~ "Solar Cells",
         grepl("^8101", I_COMMODITY) ~ "Tungsten",
         grepl("^7202", I_COMMODITY) ~ "Critical Minerals",
         TRUE ~ category)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  filter(time >= as.Date("2023-10-01") & time <= as.Date("2024-09-01")) %>%
  group_by(category) %>%
  summarize(across(c(CON_VAL_MO, CAL_DUT_MO, GEN_VAL_MO), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(phase = case_when(
    category == "EVs" ~ "Sep 2024",
    category == "EV Batteries" ~ "Sep 2024",
    category == "Medical Supplies" ~ "Sep 2024",
    category == "Non-EV Batteries" ~ "Jan 2026",
    category == "Natural Graphite" ~ "Jan 2026",
    category == "Critical Minerals" ~ "Sep 2024",
    category == "Permanent Magnets" ~ "Jan 2026",
    category == "Semiconductors" ~ "Jan 2025",
    category == "Ship-to-Shore Cranes" ~ "Sep 2024",
    category == "Solar Cells" ~ "Sep 2024",
    category == "Steel & Aluminum Products" ~ "Sep 2024",
    category == "Tungsten" ~ "Jan 2025",
    category == "Silicon & Wafers" ~ "Jan 2025",
    TRUE ~ category
  )) %>%
  mutate(category = factor(category, levels = rev(c("EV Batteries","Steel & Aluminum Products","Medical Supplies","Critical Minerals","EVs","Ship-to-Shore Cranes","Solar Cells","Semiconductors","Silicon & Wafers","Tungsten","Non-EV Batteries","Permanent Magnets","Natural Graphite"))))


US_CHINA_TARIFFED_GOODS_GRAPH <- ggplot(data = US_CHINA_TARIFFED_GOODS, aes(x = category, y = CON_VAL_MO/1000000000, fill = phase)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_text(aes(label = paste0(" ",category)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("Chinese Imports Hit by New US Tariffs") +
  ylab("Imports 12M Before Oct 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,12.5), expand = c(0,0)) +
  scale_fill_manual(name= "Phase:",values = c("#3083DC","#00A99D","#EE6055"), breaks = c("Sep 2024","Jan 2025","Jan 2026")) +
  labs(caption = "Graph created by @JosephPolitano using Census data") +
  theme_apricitas + theme(legend.position = c(.75,.75), plot.margin= grid::unit(c(0.2, .2, 0.1, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = US_CHINA_TARIFFED_GOODS_GRAPH, "US China Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

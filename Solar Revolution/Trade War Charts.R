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
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Dollars, Monthly Annualized") +
  ggtitle("US Solar Panel Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census International Trade data",subtitle = "US Solar Panels Imports Have Sunk to the Lowest Levels in Two Years Amidst Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*25), ymax = 0) +
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

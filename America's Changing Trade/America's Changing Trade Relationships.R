pacman::p_load(censusapi,comtradr,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Graphing Friendshoring Import Partners
South_Korea <- fredr(series_id = "IMPKR",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Vietnam <- fredr(series_id = "IMP5520",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Taiwan <- fredr(series_id = "IMP5830",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Malaysia <- fredr(series_id = "IMP5570",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Japan <- fredr(series_id = "IMPJP",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
India <- fredr(series_id = "IMP5330",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

US_Imports_Friends <- ggplot() + #plotting bank consumer loan data
  geom_line(data=South_Korea, aes(x=date,y= value/1000,color= "South Korea"), size = 1.25) + 
  geom_line(data=Vietnam, aes(x=date,y= value/1000,color= "Vietnam"), size = 1.25) + 
  geom_line(data=Taiwan, aes(x=date,y= value/1000,color= "Taiwan"), size = 1.25) + 
  #geom_line(data=Malaysia, aes(x=date,y= value/1000,color= "Malaysia"), size = 1.25) + 
  geom_line(data=Japan, aes(x=date,y= value/1000,color= "Japan"), size = 1.25) + 
  geom_line(data=India, aes(x=date,y= value/1000,color= "India"), size = 1.25) + 
  xlab("Date") +
  ylab("Billions of Dollars, NSA") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,3,6,9,12,15,18), limits = c(0,20), expand = c(0,0)) +
  ggtitle("Friend-Shoring?") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "US Imports from Select Key Asian Economies are Growing") +
  theme_apricitas + theme(legend.position = c(.60,.79)) +
  scale_color_manual(name= "US Imports of Goods",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Vietnam","Japan","South Korea","Taiwan","India")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Imports_Friends, "US Imports Friends.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#China Exports to US

China_Exports <- fredr(series_id = "XTEXVA01CNM667N",observation_start = as.Date("1999-01-01"),realtime_start = NULL, realtime_end = NULL)
US_Imports_China <- fredr(series_id = "IMPCH",observation_start = as.Date("1999-01-01"),realtime_start = NULL, realtime_end = NULL)

China_Merge <- merge(China_Exports,US_Imports_China,by = "date") %>%
  select(date,value.x,value.y) %>%
  mutate(value = value.y/(value.x/1000000)) %>%
  select(date, value) %>%
  mutate(value = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value, 12))) %>%
  subset(date > as.Date("2000-01-01"))

China_Exports_US <- ggplot() + #plotting bank consumer loan data
  geom_line(data=China_Merge, aes(x=date,y= value,color= "Approximate Share of Chinese Goods Exports Going to the US, 12 MMA"), size = 1.25) + 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50), limits = c(0,.50), expand = c(0,0)) +
  ggtitle("Decoupling or Diversifying?") +
  labs(caption = "Graph created by @JosephPolitano using Census and OECD data", subtitle = "Chinese Exports are Near Record Highs, But a Smaller Share are Going to the US") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*0.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = China_Exports_US, "China Exports US.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#US Trade with Russia

US_RUSSIA_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR","GEN_VAL_MO","I_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  I_COMMODITY = "-",
  CTY_CODE = "4621", #Russia country code
  ) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO))

US_RUSSIA_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  CTY_CODE = "4621", #Russia country code
  E_COMMODITY = "-",
  ) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO))

US_Trade_Russia <- ggplot() + #plotting bank consumer loan data
  geom_line(data=US_RUSSIA_IMPORTS, aes(x=time,y= GEN_VAL_MO/1000000000,color= "US Imports from Russia"), size = 1.25) + 
  geom_line(data=US_RUSSIA_EXPORTS, aes(x=time,y= ALL_VAL_MO/1000000000,color= "US Exports to Russia"), size = 1.25) + 
  xlab("Date") +
  ylab("Billions of Dollars, NSA") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,1,2,3), limits = c(0,3), expand = c(0,0)) +
  ggtitle("The Russian Trade War") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "US Trade with Russia has Collapsed Amidst Sanctions") +
  theme_apricitas + theme(legend.position = c(.40,.79)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("US Imports from Russia","US Exports to Russia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Trade_Russia, "US Trade Russia.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#US Imports from China vs Chinese Exports to the US
USA_Imports_China <- ct_search(reporters = "USA", 
          partners = c("China"), 
          trade_direction = "imports",
          freq = "monthly") %>%
          mutate(period_desc = as.Date(as.yearmon(period_desc)))

China_Exports_US <- ct_search(reporters = "China", 
                    partners = c("USA"), 
                    trade_direction = "imports",
                    freq = "monthly") %>%
                    mutate(period_desc = as.Date(as.yearmon(period_desc))) %>%
                    select(period_desc,trade_value_usd)

China_Exports_Viet <- ct_search(reporters = "Viet Nam", 
                              partners = c("China"), 
                              trade_direction = "imports",
                              freq = "monthly") %>%
                              mutate(period_desc = as.Date(as.yearmon(period_desc))) %>%
                              select(period_desc,trade_value_usd)

US_China_Imports_Exports_graph <- ggplot() + #plotting bank consumer loan data
  geom_line(data=USA_Imports_China, aes(x=period_desc,y= trade_value_usd/1000000000,color= "US Imports from China"), size = 1.25) + 
  geom_line(data=China_Exports_US, aes(x=period_desc,y= trade_value_usd/1000000000,color= "Chinese Exports to the US"), size = 1.25) + 
  geom_line(data=China_Exports_Viet, aes(x=period_desc,y= trade_value_usd/1000000000,color= "Vietnam Imports from China"), size = 1.25) + 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,3,6,9,12,15,18), limits = c(0,80), expand = c(0,0)) +
  ggtitle("Friend-Shoring?") +
  labs(caption = "Graph created by @JosephPolitano using UN COMTRADE data", subtitle = "US Imports from Select Key Asian Economies are Growing") +
  theme_apricitas + theme(legend.position = c(.60,.79)) +
  scale_color_manual(name= "US Imports of Goods",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#us Crude Refined Space
US_CRUDE_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR","GEN_VAL_MO","I_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  I_COMMODITY = "2709",
  CTY_CODE = "-", #Russia country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  select(time,GEN_VAL_MO)

US_CRUDE_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  E_COMMODITY = "2709",
  CTY_CODE = "-", #Russia country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  select(time,ALL_VAL_MO)

US_REFINED_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR","GEN_VAL_MO","I_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  I_COMMODITY = "2710",
  CTY_CODE = "-", #Russia country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  select(time,GEN_VAL_MO)

US_REFINED_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  E_COMMODITY = "2710",
  CTY_CODE = "-", #Russia country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  select(time,ALL_VAL_MO)

US_CRUDE_NET_IMPORTS <- merge(US_CRUDE_EXPORTS, US_CRUDE_IMPORTS, by = "time")
US_REFINED_NET_EXPORTS <- merge(US_REFINED_EXPORTS, US_REFINED_IMPORTS, by ="time")
  
US_Trade_Crude_Refined <- ggplot() + #plotting crude margins
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=subset(US_CRUDE_NET_IMPORTS, time > as.Date("2016-01-01")), aes(x=time,y= (ALL_VAL_MO-GEN_VAL_MO)/1000000000,color= "US Net Exports of Crude Oil"), size = 1.25) + 
  geom_line(data=subset(US_REFINED_NET_EXPORTS, time > as.Date("2016-01-01")), aes(x=time,y= (ALL_VAL_MO-GEN_VAL_MO)/1000000000,color= "US Net Exports of Gasoline, Diesel, Jet Fuel, and Other Refined Petroleum Oils"), size = 1.25) + 
  xlab("Date") +
  ylab("Billions of Dollars, NSA") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-10,-5,0,5,10), limits = c(-13,10), expand = c(0,0)) +
  ggtitle("Refiner to the World") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "America Imports Crude Oil and Exports Refined Products") +
  theme_apricitas + theme(legend.position = c(.50,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -13-(.3*23), ymax = -13) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Trade_Crude_Refined, "US Trade Crude Refined.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#US energy exports

US_COAL_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "2701",
  CTY_CODE = "-", #World country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  select(time,ALL_VAL_MO)

US_PROPANE_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "271112",
  CTY_CODE = "-", #World country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  select(time,ALL_VAL_MO)

US_BUTENE_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "271113",
  CTY_CODE = "-", #World country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  select(time,ALL_VAL_MO)

US_Energy_Exports <- ggplot() + #plotting Energy Exports
  geom_line(data=US_COAL_EXPORTS, aes(x=time,y= ALL_VAL_MO/1000000000,color= "US Exports of Coal"), size = 1.25) + 
  geom_line(data=US_PROPANE_EXPORTS, aes(x=time,y= ALL_VAL_MO/1000000000,color= "US Exports of Liquefied Propane"), size = 1.25) + 
  geom_line(data=US_BUTENE_EXPORTS, aes(x=time,y= ALL_VAL_MO/1000000000,color= "US Exports of Liquefied Butane"), size = 1.25) + 
  xlab("Date") +
  ylab("Billions of Dollars, NSA") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,1,2,3), limits = c(0,3), expand = c(0,0)) +
  ggtitle("Energy Exports") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "American Exports of a Broad Array of Energy Commodities are Increasing") +
  theme_apricitas + theme(legend.position = c(.50,.63)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("US Exports of Liquefied Propane","US Exports of Coal","US Exports of Liquefied Butane")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Energy_Exports, "US Energy Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#US LNG Exports to Europe

US_NAT_GAS_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "2711110000", #nat gas commodity code
  CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
)

US_NAT_GAS_EXPORTS$time <- as.Date(as.yearmon(US_NAT_GAS_EXPORTS$time))
US_NAT_GAS_EXPORTS$ALL_VAL_MO <- as.numeric(US_NAT_GAS_EXPORTS$ALL_VAL_MO)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("-","All US LNG Exports",US_NAT_GAS_EXPORTS$CTY_CODE)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("4XXX","US LNG Exports to Europe",US_NAT_GAS_EXPORTS$CTY_CODE)

US_NAT_GAS_EXPORTS_Graph <- ggplot() + #plotting nat gas exports
  geom_area(data = US_NAT_GAS_EXPORTS, aes(x = time, y = ALL_VAL_MO/1000000000, fill = CTY_CODE), color = NA, size = 0, position = "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,5), breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Bridging the Gap") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Natural Gas Exports Are Skyrocketing, and the Vast Majority are Headed to Europe") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_GAS_EXPORTS_Graph, "US Nat Gas Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#US Integrated Circuits Exports
US_CIRCUITS_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "DF"), 
  DF = 1, #excluding reexport
  time = "from 2013 to 2022",
  E_COMMODITY = "8542", #integrated circuits commodity code
  CTY_CODE = "4XXX", #europe country code
  CTY_CODE = "5XXX", #asia
  CTY_CODE = "5830", #taiwan
  CTY_CODE = "5570", #malaysia
  CTY_CODE = "5800", #south Korea
  CTY_CODE = "5700", #China
  ) %>%
  select(time,ALL_VAL_MO,CTY_CODE) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  pivot_wider(values_from = "ALL_VAL_MO",names_from = "CTY_CODE")


US_IC_EXPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_CIRCUITS_EXPORTS, aes(x=time,y= `5700`/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_EXPORTS, aes(x=time,y= `5570`/1000000000,color= "Malaysia"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_EXPORTS, aes(x=time,y= `5800`/1000000000,color= "South Korea"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_EXPORTS, aes(x=time,y= `5830`/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_EXPORTS, aes(x=time,y= `4XXX`/1000000000,color= "Europe"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_EXPORTS, aes(x=time,y= (`5XXX`-`5570`-`5800`-`5830`-`5700`)/1000000000,color= "Asia Ex China, Malaysia, South Korea, and Taiwan"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = .25),limits = c(0,1.25), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Dis-Integrated") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
  theme_apricitas + theme(legend.position = c(.50,.72)) +
  scale_color_manual(name= "Export Destination for US-Manufactured Integrated Circuits & Microassemblies",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("China","Asia Ex China, Malaysia, South Korea, and Taiwan","Taiwan","Europe","Malaysia","South Korea")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IC_EXPORTS_Graph, "US Integrated Circuits Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#China US Net Exports of Integrated Circuits
China_IC_NEX <- ct_search(reporters = "China", 
                                partners = c("World"), 
                                trade_direction = c("exports","imports"),
                                freq = "monthly",
                                commod_codes = "8542"
                                ) %>%
  mutate(period_desc = as.Date(as.yearmon(period_desc))) %>%
  select(period_desc,trade_value_usd,trade_flow) %>%
  pivot_wider(values_from = trade_value_usd, names_from = trade_flow) %>%
  transmute(date = period_desc, net_exports = Exports - Imports)

US_IC_NEX <- ct_search(reporters = "USA", 
                          partners = c("World"), 
                          trade_direction = c("exports","imports"),
                          freq = "monthly",
                          commod_codes = "8542"
) %>%
  mutate(period_desc = as.Date(as.yearmon(period_desc))) %>%
  select(period_desc,trade_value_usd,trade_flow) %>%
  pivot_wider(values_from = trade_value_usd, names_from = trade_flow) %>%
  transmute(date = period_desc, net_exports = Exports - Imports)

US_China_IC_NEX <- ggplot() + #plotting crude margins
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=subset(China_IC_NEX, date > as.Date("2016-01-01")), aes(x=date,y= net_exports/1000000000,color= "China Net Exports of Integrated Circuits & Microassemblies"), size = 1.25) + 
  geom_line(data=subset(US_IC_NEX, date > as.Date("2016-01-01")), aes(x=date,y= net_exports/1000000000,color= "US Net Exports of Integrated Circuits & Microassemblies"), size = 1.25) + 
  xlab("Date") +
  ylab("Billions of Dollars, NSA") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-30,-25,-20,-15,-10,-5,0,5), limits = c(-30,5), expand = c(0,0)) +
  ggtitle("Short-Circuited") +
  labs(caption = "Graph created by @JosephPolitano using COMTRADE data", subtitle = "China's Net Imports of Integrated Circuits are Rising, Though US Net Exports Have Declined") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -30-(.3*35), ymax = -30) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_China_IC_NEX, "US China NEX IC.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
#Share of Chinese IC Coming from US

China_Imports_US_Share <- ct_search(reporters = "China", 
                          partners = c("World","USA"), 
                          trade_direction = c("imports"),
                          freq = "annual",
                          commod_codes = "8542"
) %>%
  select(year,trade_value_usd,partner) %>%
  pivot_wider(values_from = trade_value_usd, names_from = partner) %>%
  transmute(date = year, Share = USA/World)

US_China_IC_Share <- ggplot() + #plotting crude margins
  geom_line(data=China_Imports_US_Share, aes(x=date,y= Share,color= "US Share of Chinese Imports of Integrated Circuits and Microassemblies"), size = 1.25) + 
  xlab("Date") +
  ylab("% of Total Imports") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.1,0.15), limits = c(0,.15), expand = c(0,0)) +
  ggtitle("Short-Circuited") +
  labs(caption = "Graph created by @JosephPolitano using COMTRADE data", subtitle = "America Makes Up a Small Share of Chinese Chip Imports") +
  theme_apricitas + theme(legend.position = c(.50,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 1992-(.1861*(2021-1992)), xmax = 1992-(0.049*(2021-1992)), ymin = 0-(.3*0.15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_China_IC_Share, "US China IC Share.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
#US Pound for Pound Comparison

China_Imports_Weight_Dollar <- ct_search(reporters = "China", 
                                    partners = c("World","USA"), 
                                    trade_direction = c("imports"),
                                    freq = "annual",
                                    commod_codes = "8542"
  ) %>%
  select(year,trade_value_usd,partner,netweight_kg) %>%
  transmute(year, partner, dollar_kg = trade_value_usd/netweight_kg) %>%
  pivot_wider(values_from = dollar_kg, names_from = partner)
  
US_China_IC_Pound <- ggplot() + #plotting crude margins
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=subset(China_Imports_Weight_Dollar, year > 2016), aes(x=year,y= USA,color= "$/kg, Chinese Imports of American Integrated Circuits & Microassemblies"), size = 1.25) + 
  geom_line(data=subset(China_Imports_Weight_Dollar, year > 2016), aes(x=year,y= World,color= "$/kg, Chinese Imports of Integrated Circuits & Microassemblies"), size = 1.25) + 
  xlab("Date") +
  ylab("Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), breaks = c(0,10000,20000,30000), limits = c(0,30000), expand = c(0,0)) +
  ggtitle("Pound for Pound") +
  labs(caption = "Graph created by @JosephPolitano using COMTRADE data", subtitle = "American Exports to China Represent Highly Valuable, Complex Chips") +
  theme_apricitas + theme(legend.position = c(.50,.53)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 2017-(.1861*(2021-2017)), xmax = 2017-(0.049*(2021-2017)), ymin = 0-(.3*30000), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_China_IC_Pound, "US China IC Pound.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Share of US Imports from China

US_China_Imports <- fredr(series_id = "IMPCH",observation_start = as.Date("1999-01-01"),realtime_start = NULL, realtime_end = NULL)

US_Total_Imports <- fredr(series_id = "IMP0004",observation_start = as.Date("1999-01-01"),realtime_start = NULL, realtime_end = NULL)

US_China_Imports_Share <- merge(US_China_Imports,US_Total_Imports,by = "date") %>%
  mutate(share = value.x/value.y) %>%
  mutate(share = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(share,12))) %>%
  select(date, share) %>%
  subset(.,date> as.Date("2000-01-01"))

China_Imports_US <- ggplot() + #plotting bank consumer loan data
  geom_line(data=US_China_Imports_Share, aes(x=date,y= share,color= "Share of American Goods Imports Coming from China, 12 MMA"), size = 1.25) + 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,.10,.15,.20,.25), limits = c(0,.25), expand = c(0,0)) +
  ggtitle("Decoupling or Diversifying?") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "The Share of American Imports Coming From China Have Dropped Significantly") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*0.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = China_Imports_US, "China Imports US.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#do 8541 and 8542 next time

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
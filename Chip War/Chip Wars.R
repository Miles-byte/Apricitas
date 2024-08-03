pacman::p_load(rsdmx,bea.R,cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("seokhoonj/ecos", force = TRUE)
library("ecos")

#FOR FUTURE DO SUBSPLIT ON MEMORY 854232 VS PROCESSORS 854231
#FOR FUTURE ALSO DO SUBSPLIT ON 848620 (SEMI EQUIPMENT SPECIFICALLY) AND 848690 (ALL PARTS)

# Trade_search <- estat_getStatsList(
#   appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
#   searchWord = "Production",
#   lang = "E"
# )

# JPN_TRADE_DATA <- estat_getStatsData(
#   appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
#   statsDataId = "0003334002",
#   lang = "E", #english language
#   limit = 5000,
#   cdArea = "50105"
# )
# 
# JPN_TRADE_DATA2 <- estat_getStatsData(
#   appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
#   statsDataId = "0003228235",
#   lang = "E", #english language
#   limit = 5000,
#   cdArea = "50105"
# )

JPN_TRADE_DATA_2016_2020 <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003313967",
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = c("70323000","70131000"),
  cdArea = c("50105","50129","50108")
)

JPN_TRADE_DATA_2021_Plus <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003425295",
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = c("70323000","70131000"),
  cdArea = c("50105","50129","50108")
)

JAPAN_CHIP <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70323000") %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Value", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Value-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Value-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

JAPAN_CHIP_MACHINES <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70131000") %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Value", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Value-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Value-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

JAPAN_CHIP_MACHINES_QTY <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70131000") %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Quantity", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Quantity-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Quantity-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

YEN_EXCHANGE_RATE <- fredr("EXJPUS", observation_start = as.Date("2016-01-01"))

JAPAN_CHIP_DOLLAR <- left_join(JAPAN_CHIP,YEN_EXCHANGE_RATE, by = "date")

JAPAN_CHIP_MACHINES_DOLLAR <- left_join(JAPAN_CHIP_MACHINES,YEN_EXCHANGE_RATE, by = "date")

JAPAN_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP, aes(x=date,y= exports/1000000,color= "Semiconductors"), size = 1.25) + 
  geom_line(data=JAPAN_CHIP_MACHINES, aes(x=date,y= exports/1000000,color= "Machines For Manufacturing Semiconductors"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "¥", accuracy = 1),limits = c(0,225), breaks = c(1,50,100,150,200), expand = c(0,0)) +
  ylab("Billions of Yen, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan data seasonally adjusted usint X-13ARIMA",subtitle = "Japan is a Major Supplier of Chinese Chip Equipment, and Just Announced Chip Sanctions") +
  theme_apricitas + theme(legend.position = c(.39,.89)) +
  scale_color_manual(name= "Japanese Exports to China, HK, and Macau",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors","Machines For Manufacturing Semiconductors")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*225), ymax = 0) +
  coord_cartesian(clip = "off")

JAPAN_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP_DOLLAR, aes(x=date,y= ((exports*12)/value)/1000000,color= "Semiconductors"), size = 1.25) + 
  geom_line(data=JAPAN_CHIP_MACHINES_DOLLAR, aes(x=date,y= ((exports*12)/value)/1000000,color= "Machines For Manufacturing Semiconductors"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,22.5), breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("Japanese Chip Related Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan data seasonally adjusted usint X-13ARIMA",subtitle = "Japanese Exports of Chip Equipment to China Have Risen as Exports of Chips Themselves Fell") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= "Japanese Exports to China, HK, and Macau",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors","Machines For Manufacturing Semiconductors")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*22.5), ymax = 0) +
  coord_cartesian(clip = "off")

JAPAN_CHIP_MACHINES_QTY_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP_MACHINES_QTY, aes(x=date,y= exports/1000000,color= "Machines For Manufacturing Semiconductors"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "kt", accuracy = 1),limits = c(0,10), breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan data seasonally adjusted usint X-13ARIMA",subtitle = "Japan is a Major Supplier of Chinese Chip Equipment, and Just Announced Chip Sanctions") +
  theme_apricitas + theme(legend.position = c(.39,.89)) +
  scale_color_manual(name= "Japanese Exports to China, HK, and Macau",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors","Machines For Manufacturing Semiconductors")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*4), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = JAPAN_IC_EXPORTS_CHINA_Graph, "Japan IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = JAPAN_IC_EXPORTS_CHINA_Dollar_Graph, "Japan IC Exports China Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = JAPAN_CHIP_MACHINES_QTY_Graph, "Japan Chip Machines Qty.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_CIRCUITS_CHINA_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "DF"), 
  DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "8542", #integrated circuits commodity code
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
  #E_COMMODITY = "8541", #Diodes, transistors and similar semiconductor devices
  #E_COMMODITY = "8486", #Machines Principally Used to Manufacture Semiconductor
  #E_COMMODITY = "848071", #Molds For the Manufacture of Semiconductor Devices
  #E_COMMODITY = "903141", #Optical Instruments for Inspecting Semiconductor Wafers
  #E_COMMODITY = "903082", #Oscilloscopes, spectrum analyzers and other instruments and apparatus for For measuring or checking semiconductor wafers or devices (including integrated circuits)
  #E_COMMODITY = "90314970", #For Inspecting Masks (Other than Photomasks) Used in Manufacturing Semiconductor Devices;
  #E_COMMODITY = "90318040", #Electron Beam Microscopes Specifically for Handling and Transport of Semiconductor Wafers
  #E_COMMODITY = "90309084", #Parts and accessories for instruments and apparatus for measuring or checking semiconductor wafers or devices
  #E_COMMODITY = "70171030", #Fuzed Quartz for Semiconductor Wafer Production
  #E_COMMODITY = "8534", #Printed Circuits
  #E_COMMODITY = "84561170", #Semiconductor laser machine tools
  #E_COMMODITY = "84561270", #Semiconductor photon machine tools
  #E_COMMODITY = "84248910", #Projecting, dispersing, or spraying appliances for printed circuits
  #E_COMMODITY = "84669396", #Parts and accessories of machine tool of subheadings of a kind used solely or principally for the manufacture of printed circuits, printed circuit assemblies,
  #E_COMMODITY = "85143110", #Electron Beam Furnaces for manufacture of printed circuits or printed circuit assemblies
  #E_COMMODITY = "85143210", #Plasma and vacuum arc furnaces for manufacture of printed circuits or printed circuit assemblies
  #E_COMMODITY = "85143910", #Other furnaces for manufacture of printed circuits or printed circuit assemblies
  #E_COMMODITY = "85433020", #Signal Generators of a kind used solely or principally for the manufacture of printed circuits
  #E_COMMODITY = "85439015", #Parts of printed circuit assemblies
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820"  #Hong Kong
) %>%
  select(time,ALL_VAL_MO,E_COMMODITY,CTY_CODE) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  pivot_wider(values_from = "ALL_VAL_MO",names_from = "E_COMMODITY") %>%
  group_by(time) %>%
  summarise(`8542` = sum(`8542`, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-time) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)), `8542`=x)

US_CHIPMAKING_CHINA_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "DF"), 
  DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #E_COMMODITY = "8542", #integrated circuits commodity code
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
  #E_COMMODITY = "8541", #Diodes, transistors and similar semiconductor devices
  E_COMMODITY = "8486", #Machines Principally Used to Manufacture Semiconductor
  #E_COMMODITY = "848071", #Molds For the Manufacture of Semiconductor Devices
  #E_COMMODITY = "903141", #Optical Instruments for Inspecting Semiconductor Wafers
  #E_COMMODITY = "903082", #Oscilloscopes, spectrum analyzers and other instruments and apparatus for For measuring or checking semiconductor wafers or devices (including integrated circuits)
  #E_COMMODITY = "90314970", #For Inspecting Masks (Other than Photomasks) Used in Manufacturing Semiconductor Devices;
  #E_COMMODITY = "90318040", #Electron Beam Microscopes Specifically for Handling and Transport of Semiconductor Wafers
  #E_COMMODITY = "90309084", #Parts and accessories for instruments and apparatus for measuring or checking semiconductor wafers or devices
  #E_COMMODITY = "70171030", #Fuzed Quartz for Semiconductor Wafer Production
  #E_COMMODITY = "8534", #Printed Circuits
  #E_COMMODITY = "84561170", #Semiconductor laser machine tools
  #E_COMMODITY = "84561270", #Semiconductor photon machine tools
  #E_COMMODITY = "84248910", #Projecting, dispersing, or spraying appliances for printed circuits
  #E_COMMODITY = "84669396", #Parts and accessories of machine tool of subheadings of a kind used solely or principally for the manufacture of printed circuits, printed circuit assemblies,
  #E_COMMODITY = "85143110", #Electron Beam Furnaces for manufacture of printed circuits or printed circuit assemblies
  #E_COMMODITY = "85143210", #Plasma and vacuum arc furnaces for manufacture of printed circuits or printed circuit assemblies
  #E_COMMODITY = "85143910", #Other furnaces for manufacture of printed circuits or printed circuit assemblies
  #E_COMMODITY = "85433020", #Signal Generators of a kind used solely or principally for the manufacture of printed circuits
  #E_COMMODITY = "85439015", #Parts of printed circuit assemblies
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820"  #Hong Kong
) %>%
  select(time,ALL_VAL_MO,E_COMMODITY,CTY_CODE) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  pivot_wider(values_from = "ALL_VAL_MO",names_from = "E_COMMODITY") %>%
  group_by(time) %>%
  summarise(`8486` = sum(`8486`, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-time) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)), `8486`=x)


US_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(US_CIRCUITS_CHINA_EXPORTS, date>= as.Date("2016-01-01")), aes(x=date,y= (`8542`*12)/1000000000,color= "Semiconductors"), size = 1.25) + 
  geom_line(data=filter(US_CHIPMAKING_CHINA_EXPORTS, date >= as.Date("2016-01-01")), aes(x=date,y= (`8486`*12)/1000000000,color= "Machines For Manufacturing Semiconductors"), size = 1.25) + 
  # geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=date,y= `854231`/1000000000,color= "Logic Chips (test)"), size = 1.25) + 
  # geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=date,y= `854232`/1000000000,color= "Memory Chips (test)"), size = 1.25) + 
  annotate("vline", x = as.Date("2022-10-01"), xintercept = as.Date("2022-10-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "Chip Sanctions", x = as.Date("2021-12-01"), y = 2.5, color = "white", size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,15), breaks = c(0,5,10,15), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA",subtitle = "Sanctions Have Led to a Major Fall in US Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.32,.89)) +
  scale_color_manual(name= "US-Manufactured Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors","Machines For Manufacturing Semiconductors")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IC_EXPORTS_CHINA_Graph, "US IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_CHIP_IMPORTS_CHINA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8542",#, #Machines Principally Used to Manufacture Semiconductors
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

US_IC_IMPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(US_CHIP_IMPORTS_CHINA, date>= as.Date("2016-01-01")), aes(x=date,y= (x*12)/1000000000,color= "US Gross Imports of Semiconductors from China\nSeasonally Adjusted at Annual Rates"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,5.75), breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Semiconductor Imports From China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA. Note: China Includes HK & MO",subtitle = "The US Doesn't Import Many Semiconductors From China, and Imports Have Been Declining") +
  theme_apricitas + theme(legend.position = c(.65,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*5.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IC_IMPORTS_CHINA_Graph, "US IC Imports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_CHIP_MACHINES_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8486",#, #Machines Principally Used to Manufacture Semiconductors
  CTY_CODE = "5700", #China
  CTY_CODE = "5660", #Macao
  CTY_CODE = "5820", #Hong Kong
  CTY_CODE = "5880", #Japan
  CTY_CODE = "0003", #EU
  CTY_CODE = "5570", #Malaysia
  CTY_CODE = "5800", #South Korea
  CTY_CODE = "5830", #Taiwan
  CTY_CODE = "5590", #Singapore
  CTY_CODE = "-", #Total
) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(CTY_CODE = replace(CTY_CODE, CTY_CODE %in% c("5660", "5820"), "5700")) %>%
  mutate(CTY_NAME = replace(CTY_NAME, CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA")) %>%
  group_by(time, CTY_CODE) %>%
  summarise(CTY_NAME, `GEN_VAL_MO` = sum(`GEN_VAL_MO`, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  select(-CTY_CODE) %>%
  pivot_wider(names_from = CTY_NAME, values_from = GEN_VAL_MO) %>%
  mutate(`TOTAL FOR ALL COUNTRIES` = `TOTAL FOR ALL COUNTRIES`-rowSums(select(., -`TOTAL FOR ALL COUNTRIES`,-time))) %>%
  transmute(Other = `TOTAL FOR ALL COUNTRIES`,`EU` = `EUROPEAN UNION`,`Malaysia` = `MALAYSIA`,`China (incl. HK and Macau)` = CHINA, `South Korea` = `KOREA, SOUTH`, `Taiwan` = `TAIWAN`, `Japan` = `JAPAN`, Singapore = SINGAPORE) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.))) %>%
  rename(`China (incl. HK and Macau)` = `ChinainclHKandMacau`,`South Korea` = SouthKorea) %>%
  pivot_longer(cols = `Other`:`Singapore`) %>%
  mutate(name = factor(name, levels = c("Other","China (incl. HK and Macau)","South Korea","Malaysia","Taiwan","Singapore","EU","Japan")))

US_CHIP_MACHINES_IMPORTS_DOLLAR_BAR_Graph <- ggplot(data = filter(US_CHIP_MACHINES_IMPORTS, date >= as.Date("2016-01-01")), aes(x = date, y = (value*12)/1000000000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Annual Rate") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,5,10,15), limits = c(0,15), expand = c(0,0)) +
  ggtitle("US Chipmaking Machine Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data Seasonally Adjusted using X-13ARIMA", subtitle = "American Imports of Chipmaking Equipment have Risen Significantly in the Wake of the CHIPS Act") +
  theme_apricitas + theme(legend.position = c(.2,.8), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CHIP_MACHINES_IMPORTS_DOLLAR_BAR_Graph, "US Chipmaking Machine Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_CHIPS_TOTAL_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "8542", #integrated circuits commodity code
) %>%
  transmute(time = as.Date(paste0(time,"-01")), exports = as.numeric(ALL_VAL_MO))

US_CHIPS_TOTAL_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8542",#, #Machines Principally Used to Manufacture Semiconductors
) %>%
  transmute(time = as.Date(paste0(time,"-01")), imports = as.numeric(GEN_VAL_MO))

US_CHIPS_NET_EXPORTS <- merge(US_CHIPS_TOTAL_EXPORTS,US_CHIPS_TOTAL_IMPORTS, by = "time") %>%
  mutate(netexports = exports-imports) %>%
  select(-time) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)))

US_CHIPS_NET_EXPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(US_CHIPS_NET_EXPORTS, date>= as.Date("2016-01-01")), aes(x=date,y= (netexports*12)/1000000000,color= "US Net Exports of Semiconductors"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-2,27.5), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Net Semiconductor Exports") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA",subtitle = "Sanctions Have Led to a Major Fall in US Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.225,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -2-(.3*29.5), ymax = -2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CHIPS_NET_EXPORTS_Graph, "US CHIPS Net Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


EU_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/EU_EXPORTS.csv")

EU_CHIP_EXPORTS <- EU_EXPORTS %>%
  subset(PRODUCT_LAB == "Electronic integrated circuits; parts thereof") %>%
  group_by(PERIOD_LAB) %>%
  summarise(INDICATOR_VALUE = sum(INDICATOR_VALUE, na.rm = TRUE)) %>%
  transmute(date = as.Date(PERIOD_LAB),INDICATOR_VALUE) %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

EU_CHIP_MACHINES <- EU_EXPORTS %>%
  subset(PRODUCT_LAB == "Machines and apparatus of a kind used solely or principally for the manufacture of semiconductor boules or wafers, semiconductor devices, electronic integrated circuits or flat panel displays; machines and apparatus specified in note 9 C to chapter 84; parts and accessories, n.e.s.") %>%
  group_by(PERIOD_LAB) %>%
  summarise(INDICATOR_VALUE = sum(INDICATOR_VALUE, na.rm = TRUE)) %>%
  transmute(date = as.Date(PERIOD_LAB),INDICATOR_VALUE) %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

DUTCH_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/DUTCH_EXPORTS.csv") %>%
  transmute(date = as.Date(Date),Exports_China,Exports_HK)

EURO_EXCHANGE_RATE <- fredr("EXUSEU", observation_start = as.Date("2015-01-01"))

EU_CHIP_DOLLAR <- left_join(EU_CHIP_EXPORTS,EURO_EXCHANGE_RATE)
EU_CHIP_MACHINES_DOLLAR <- left_join(EU_CHIP_MACHINES,EURO_EXCHANGE_RATE)
DUTCH_EXPORTS_DOLLAR <- left_join(DUTCH_EXPORTS,EURO_EXCHANGE_RATE)

EU_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=EU_CHIP_EXPORTS, aes(x=date,y= exports/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=EU_CHIP_MACHINES, aes(x=date,y= exports/1000000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "€", accuracy = .25),limits = c(-0.03,1.30), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("EU Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Sanctions Have Led to a Major Fall in EU Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.385,.87)) +
  scale_color_manual(name= "EU Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -0.03-(.3*1.33), ymax = -0.03) +
  coord_cartesian(clip = "off")

EU_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=EU_CHIP_DOLLAR, aes(x=date,y= (exports/value)/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=EU_CHIP_MACHINES_DOLLAR, aes(x=date,y= (exports/value)/1000000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(-0.02,1.25), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("EU Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Sanctions Have Led to a Major Fall in EU Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.385,.87)) +
  scale_color_manual(name= "EU Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -0.02-(.3*1.27), ymax = -0.02) +
  coord_cartesian(clip = "off")

DUTCH_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  #geom_line(data=EU_CHIP_DOLLAR, aes(x=date,y= (exports/value)/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=DUTCH_EXPORTS_DOLLAR, aes(x=date,y= ((Exports_China+Exports_HK)/value)/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M",prefix = "$", accuracy = 1),limits = c(0,500), breaks = c(0,250,500), expand = c(0,0)) +
  ylab("Millions of Dollars, Monthly") +
  ggtitle("Dutch Chipmaking Equipment Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Key Dutch Chipmaking Equipment Exports to China are at a Record High") +
  theme_apricitas + theme(legend.position = c(.425,.87), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "Dutch Exports to China and HK",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*500), ymax = 0) +
  coord_cartesian(clip = "off")

DUTCH_CHIPMAKING_EXPORTS <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-045409/1.0/M.*.*.*.2.VALUE_IN_EUROS?c[reporter]=EA19,NL&c[partner]=CN,HK,MO&c[product]=8486&compress=false")) 

DUTCH_CHIPMAKING_EXPORTS <- DUTCH_CHIPMAKING_EXPORTS %>%
  transmute(date = as.Date(paste0(TIME_PERIOD,"-01")),reporter,OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  group_by(date,reporter) %>%
  summarize(OBS_VALUE = sum(OBS_VALUE, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = reporter, values_from = OBS_VALUE) %>%
  left_join(.,EURO_EXCHANGE_RATE) %>%
  mutate(NL = NL/value) %>%
  mutate(RollNL = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollsum(NL,12))) %>%
  drop_na()

DUTCH_CHIPMAKING_EXPORTS_CHINA_DOLLAR_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=DUTCH_CHIPMAKING_EXPORTS, aes(x=date,y= ((RollNL)/value)/1000000000,color= "Dutch Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau"), size = 1.25) + 
  geom_line(data=DUTCH_CHIPMAKING_EXPORTS, aes(x=date,y= ((NL*12)/value)/1000000000,color= "Dutch Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau"), size = 0.75, alpha = 0.5, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = 1),limits = c(0,14), breaks = c(0,3,6,9,12), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("Dutch Chipmaking Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Dutch Chipmaking Equipment Exports to China are Booming") +
  theme_apricitas + theme(legend.position = c(.425,.8)) +
  scale_color_manual(name= "Solid = Rolling 12M Total, Dashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DUTCH_CHIPMAKING_EXPORTS_CHINA_DOLLAR_Graph, "Dutch Chipmaking Equipment Exports China Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


ggsave(dpi = "retina",plot = DUTCH_EXPORTS_CHINA_Dollar_Graph, "Dutch Chip Equipment Exports China Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = EU_IC_EXPORTS_CHINA_Dollar_Graph, "EU IC Exports China Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = EU_IC_EXPORTS_CHINA_Graph, "EU IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Creating NSA Japanese Chipmaking Exports to Roll
JAPAN_CHIPMAKING_EXPORTS <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70131000") %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Value", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Value-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Value-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(JP = sum(value, na.rm = TRUE)) %>%
  left_join(.,YEN_EXCHANGE_RATE) %>%
  mutate(RollJP = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollsum(JP,12))) %>%
  drop_na()
  
DUTCH_JAPAN_CHIPMAKING_EXPORTS_CHINA_DOLLAR_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(DUTCH_CHIPMAKING_EXPORTS, date >= as.Date("2017-01-01")), aes(x=date,y= ((RollNL)/value)/1000000000,color= "Dutch Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau"), size = 1.25) + 
  geom_line(data=filter(DUTCH_CHIPMAKING_EXPORTS, date >= as.Date("2017-01-01")), aes(x=date,y= ((NL*12)/value)/1000000000,color= "Dutch Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau"), size = 0.75, alpha = 0.5, linetype = "dashed") + 
  geom_line(data=JAPAN_CHIPMAKING_EXPORTS, aes(x=date,y= ((RollJP)/value)/1000000,color= "Japanese Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau"), size = 1.25) + 
  geom_line(data=JAPAN_CHIPMAKING_EXPORTS, aes(x=date,y= ((JP*12)/value)/1000000,color= "Japanese Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau"), size = 0.75, alpha = 0.5, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = 1),limits = c(0,20), breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("Key Chipmaking Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat & e-Stat data. NOTE: Definitions Differ Slightly b/c Japan Doesn't Use HS Codes",subtitle = "Dutch & Japanese Chipmaking Equipment Exports to China are Booming") +
  theme_apricitas + theme(legend.position = c(.425,.85)) +
  scale_color_manual(name= "Solid = Rolling 12M Total, Dashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Japanese Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau","Dutch Exports of Semiconductor Manufacturing Equipment\nto China, Hong Kong, and Macau")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*20), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DUTCH_JAPAN_CHIPMAKING_EXPORTS_CHINA_DOLLAR_Graph, "Dutch Japanese Monthly Chipmaking Equipment Exports China Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


KOR_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/KOR_EXPORTS.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  select(-Date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)))

KOR_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=KOR_EXPORTS, aes(x=date,y= X8542/1000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=KOR_EXPORTS, aes(x=date,y= X8486/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Korean Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Korean Exports of Chips & Manufacturing Equipment to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.415,.9)) +
  scale_color_manual(name= "Korean Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = KOR_IC_EXPORTS_CHINA_Dollar_Graph, "KOR IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


TAIWAN_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_EXPORTS.csv") %>%
  mutate(Time = as.Date(as.yearmon(Time, "%Y/%m")))

TAIWAN_CHIP_EXPORTS <- TAIWAN_EXPORTS %>%
  subset(Commodity.Code == "8542") %>%
  group_by(Time) %>%
  summarise(INDICATOR_VALUE = sum(Value.USD..1000., na.rm = TRUE)) %>%
  transmute(date = as.Date(Time),INDICATOR_VALUE) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

TAIWAN_CHIP_MACHINES <- TAIWAN_EXPORTS %>%
  subset(Commodity.Code == "8486") %>%
  group_by(Time) %>%
  summarise(INDICATOR_VALUE = sum(Value.USD..1000., na.rm = TRUE)) %>%
  transmute(date = as.Date(Time),INDICATOR_VALUE) %>%
  ungroup() %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

TAIWAN_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=TAIWAN_CHIP_EXPORTS, aes(x=date,y= exports/1000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=TAIWAN_CHIP_MACHINES, aes(x=date,y= exports/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10.5), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Korean Exports of Chips & Manufacturing Equipment to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.385,.92)) +
  scale_color_manual(name= "Taiwanese Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*10.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_IC_EXPORTS_CHINA_Dollar_Graph, "TWN IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CHINA_IMPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/CHINA_IMPORTS.csv") %>%
  mutate(Date = as.Date(as.yearmon(as.character(Date.of.data), "%Y%m"))) %>%
  select(Date, Commodity.code, Trading.partner, US.dollar) %>%
  mutate(US.dollar = as.numeric(gsub(",","",US.dollar))) %>%
  subset(Trading.partner != "China" & Trading.partner != "Hong Kong,China" & Trading.partner != "Macau,China" ) 

#https://tradeidds.censtatd.gov.hk/Index/358b806b82e94870b017c5c42d897d2e
HK_IMPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/HONG_KONG_IMPORTS.csv") %>%
  mutate(Date = as.Date(as.yearmon(as.character(paste0(Month,Year)), "%b%Y"))) %>%
  select(Date, Value, HKHS.4.digit, Origin) %>%
  mutate(Value = gsub("!","",Value)) %>%
  mutate(US.dollar = as.numeric(gsub(",","",Value))) %>%
  drop_na()

HK_EXCHANGE_RATE <- fredr("EXHKUS", observation_start = as.Date("2016-01-01")) %>%
  transmute(Date = date, exchange = value)

HK_CHIP_IMPORTS <- HK_IMPORTS %>%
  filter(str_detect(`HKHS.4.digit`, fixed("8542", ignore_case = TRUE))) %>%
  select(Date, Origin, US.dollar) %>%
  pivot_wider(names_from = Origin, values_from = US.dollar) %>%
  transmute(Date, Japan = JAPAN, `South Korea` = KOREA, Malaysia = MALAYSIA, Taiwan = TAIWAN, `United States` = USA, `Vietnam` = `VIET NAM`,Other = Total-MACAO-`THE MAINLAND OF CHINA`-`VIET NAM`-USA-TAIWAN-MALAYSIA-KOREA-JAPAN) %>%
  pivot_longer(cols = `Japan`:`Other`) %>%
  left_join(.,HK_EXCHANGE_RATE) %>%
  transmute(Date,name,value = (value*1000)/exchange)

HK_CHIP_MACHINES_IMPORTS <- HK_IMPORTS %>%
  filter(str_detect(`HKHS.4.digit`, fixed("8486", ignore_case = TRUE))) %>%
  select(Date, Origin, US.dollar) %>%
  pivot_wider(names_from = Origin, values_from = US.dollar) %>%
  transmute(Date, Japan = JAPAN, `South Korea` = KOREA, Netherlands = NETHERLANDS, Taiwan = TAIWAN, `United States` = USA, `Singapore` = `SINGAPORE`,Other = Total-`THE MAINLAND OF CHINA`-`SINGAPORE`-USA-TAIWAN-NETHERLANDS-KOREA-JAPAN) %>%
  pivot_longer(cols = `Japan`:`Other`) %>%
  left_join(.,HK_EXCHANGE_RATE) %>%
  transmute(Date,name,value = (value*1000)/exchange)

CHINA_CHIP_IMPORTS <- CHINA_IMPORTS %>%
  mutate(Trading.partner = case_when(
    Trading.partner == "Taiwan,China" ~ "Taiwan",
    Trading.partner == "Republic of Korea" ~ "South Korea",
    Trading.partner == "Malaysia" ~ "Malaysia",
    Trading.partner == "Japan" ~ "Japan",
    Trading.partner == "Viet Nam" ~ "Vietnam",
    Trading.partner == "United States" ~ "United States",
    TRUE ~ "Other"
  )) %>%
  subset(Commodity.code == 8542)  %>%
  group_by(Trading.partner, Date) %>%
  summarise(value = sum(US.dollar, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(Date, name = Trading.partner, value)

CHINA_CHIP_MACHINES_IMPORTS <- CHINA_IMPORTS %>%
  mutate(Trading.partner = case_when(
    Trading.partner == "Taiwan,China" ~ "Taiwan",
    Trading.partner == "Republic of Korea" ~ "South Korea",
    Trading.partner == "Netherlands" ~ "Netherlands",
    Trading.partner == "Japan" ~ "Japan",
    Trading.partner == "Singapore" ~ "Singapore",
    Trading.partner == "United States" ~ "United States",
    TRUE ~ "Other"
  )) %>%
  subset(Commodity.code == 8486)  %>%
  group_by(Trading.partner, Date) %>%
  summarise(value = sum(US.dollar, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(Date, name = Trading.partner, value)

CHINA_HK_CHIP_IMPORTS <- rbind(HK_CHIP_IMPORTS,CHINA_CHIP_IMPORTS) %>%
  group_by(name, Date) %>%
  filter(n() >= 2) %>% #checking to make sure all months are accounted for
  summarise(value = sum(value, na.rm = TRUE))

CHINA_HK_CHIP_MACHINES_IMPORTS <- rbind(HK_CHIP_MACHINES_IMPORTS,CHINA_CHIP_MACHINES_IMPORTS) %>%
  group_by(name, Date) %>%
  filter(n() >= 2) %>% #checking to make sure all months are accounted for
  summarise(value = sum(value, na.rm = TRUE))

CHINA_HK_IC_Imports_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=CHINA_HK_CHIP_IMPORTS, aes(x=Date,y= value/1000000,color= name), size = 1.25) + 
  xlab("Date") +
  #scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10.5), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Korean Exports of Chips & Manufacturing Equipment to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.385,.92)) +
  scale_color_manual(name= "Taiwanese Exports to China, HK, and Macao",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

CHINA_CHIP_MACHINES_Imports_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=CHINA_CHIP_MACHINES_IMPORTS, aes(x=Date,y= US.dollar/1000000,color= Trading.partner), size = 1.25) + 
  xlab("Date") +
  #scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10.5), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Korean Exports of Chips & Manufacturing Equipment to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.385,.92)) +
  #scale_color_manual(name= "Taiwanese Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

CHINA_HK_CHIP_IMPORTS <- CHINA_HK_CHIP_IMPORTS %>%
  ungroup() %>%
  pivot_wider() %>%
  select(-Date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(Date = seq(from = as.Date("2016-01-01"), length = nrow(. ),by = "1 months"),Japan, Malaysia, Other, `South Korea` = SouthKorea, Taiwan, `United States` = UnitedStates, Vietnam) %>%
  pivot_longer(cols = Japan:Vietnam) %>%
  mutate(name = factor(name, levels = c("Other","United States","Vietnam","Japan","Malaysia","South Korea","Taiwan")))

CHINA_HK_IC_Imports_Dollar_Bar_Graph <- ggplot(data = CHINA_HK_CHIP_IMPORTS, aes(x = Date, y = (value*12)/1000000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Annual Rate") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,100,200,300,400,500,600), limits = c(0,650), expand = c(0,0)) +
  ggtitle("Chinese Semiconductor Imports") +
  labs(caption = "Graph created by @JosephPolitano using PRC GACC and HK C&SD data Seasonally Adjusted using X-13ARIMA", subtitle = "Chinese Semiconductor Imports Fell Significantly in 2022, But Have Since Stabilized") +
  theme_apricitas + theme(legend.position = c(.3,.8), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Chip Imports to China and Hong Kong by Country",values = c("#6A4C93","#3083DC","#A7ACD9","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_HK_IC_Imports_Dollar_Bar_Graph, "China HK Chips.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CHINA_HK_CHIP_MACHINES_IMPORTS <- CHINA_HK_CHIP_MACHINES_IMPORTS %>%
  ungroup() %>%
  pivot_wider() %>%
  select(-Date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(Date = seq(from = as.Date("2016-01-01"), length = nrow(. ),by = "1 months"),Japan, Singapore, Other, `South Korea` = SouthKorea, Taiwan, `United States` = UnitedStates, Netherlands) %>%
  pivot_longer(cols = Japan:Netherlands) %>%
  mutate(name = factor(name, levels = c("Other","Taiwan","Netherlands","Singapore","South Korea","United States","Japan")))

CHINA_HK_CHIP_MACHINES_Imports_Dollar_Bar_Graph <- ggplot(data = CHINA_HK_CHIP_MACHINES_IMPORTS, aes(x = Date, y = (value*12)/1000000000, fill = name)) + #plotting permanent and temporary job losers
  #geom_line(data=CHINA_CHIP_MACHINES_IMPORTS, aes(x=Date,y= value/1000000,color= name), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Annual Rate") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,10,20,30,40,50,60), limits = c(0,65), expand = c(0,0)) +
  ggtitle("Chinese Chipmaking Machine Imports") +
  labs(caption = "Graph created by @JosephPolitano using PRC GACC and HK C&SD data Seasonally Adjusted Using X-13ARIMA", subtitle = "Chinese Imports of Machines for Semiconductor Production Have Risen Dramatically") +
  theme_apricitas + theme(legend.position = c(.4,.82), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Chipmaking Machine Imports to China and Hong Kong by Country",values = c("#6A4C93","#3083DC","#A7ACD9","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*65), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_HK_CHIP_MACHINES_Imports_Dollar_Bar_Graph, "China HK Chip Machines.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CHINA_CHIP_MACHINES_IMPORT_DETAILS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/CHINA_ANNUAL_SEMICONDUCTOR_MACHINERY_IMPORTS.csv") %>%
  mutate(date = as.Date(date))
  
CHINA_CHIP_BOULES_WAFERS_IMPORT_DETAILS_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Grinding.machines.for.the.manufacture.of.boules.or.wafers/1000000,color= "Grinding Machines for the Manufacture of Boules or Wafers"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Sawing.machines.for.the.manufacutre.of.boules.or.wafers/1000000,color= "Sawing Machines for the Manufacture of Boules or Wafers"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Chemical.mechanical.polishers.for.the.manufacture.of.boules.ro.wafers/1000000,color= "Chemical Mechanical Polishers for the Manufacture of Boules or Wafers"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Other.machines.for.the.manufacture.of.boules.or.wafers/1000000,color= "Other Machines for the Manufacture of Boules or Wafers"), size = 1.25) + 
  xlab("Date") +
  ylab("Millions of Dollars, Annual") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "M", prefix = "$"), breaks = c(0,100,200,300,400,500,600,700,800), limits = c(0,800), expand = c(0,0)) +
  ggtitle("Chinese Chipmaking Machine Imports") +
  labs(caption = "Graph created by @JosephPolitano using PRC GACC data", subtitle = "Chinese Imports of Machines for Semiconductor Production Have Risen Dramatically") +
  theme_apricitas + theme(legend.position = c(.44,.9)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = rev(c("#6A4C93","#3083DC","#A7ACD9","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Chemical Mechanical Polishers for the Manufacture of Boules or Wafers","Grinding Machines for the Manufacture of Boules or Wafers","Sawing Machines for the Manufacture of Boules or Wafers","Other Machines for the Manufacture of Boules or Wafers")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-365-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-365-as.Date("2016-01-01"))), ymin = 0-(.3*800), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_CHIP_BOULES_WAFERS_IMPORT_DETAILS_Graph, "China Chip Boules Wafers Details.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


CHINA_CHIP_MACHINES_IMPORT_DETAILS_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Ion.implanters/1000000000,color= "Ion Implanters"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Physical.Vapour.Deposition.PVD..equipment.for.the.manufacture.of.semiconductor.or.IC/1000000000,color= "Physical Vapor Deposition Equipment"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Step.and.repeat.alignersw/1000000000,color= "Step and Repeat Aligners"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Oxidation.diffusion.annealing.and.other.heat.treatment.equipment.for.the.manufacture.of.semiconductor.or.IC/1000000000,color= "Oxidation, Diffusion, Annealing and Other Heat Treatment Equipment"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Dry.plasma.etching.for.the.manufacture.of.semiconductor.or.IC/1000000000,color= "Dry Plasma Etching Machines"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Chemical.Vapour.Deposition.CVD..equipment.for.the.manufacture.of.semiconductor.or.IC/1000000000,color= "Chemical Vapor Deposition Equipment"), size = 1.25) + 
  geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Other.apparatus.used.for.projected.circuit.onto.semiconductor.materials.for.the.manufacture.of.semiconductor.or.IC/1000000000,color= "Other Apparatus Used to Project Circuits (Mostly Lithography Machines)"), size = 1.25) + 
  #geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Apparatus.used.for.the.manufacture.or.repair.of.masks.and.reticles/1000000000,color= "Apparatus for the Manufacture or Repair of Masks & Reticles"), size = 1.25) + 
  #geom_line(data=CHINA_CHIP_MACHINES_IMPORT_DETAILS, aes(x=date,y= Automated.material.handing.machines.used.in.IC.factories/1000000000,color= "Automated Material Handing Machines Used in Semiconductor Factories"), size = 1.25) + 
  xlab("Date") +
  ylab("Billions of Dollars, Annual Rate") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,2,4,6,8), limits = c(0,8), expand = c(0,0)) +
  ggtitle("Chinese Chipmaking Machine Imports") +
  labs(caption = "Graph created by @JosephPolitano using PRC GACC data", subtitle = "Chinese Imports of Machines for Semiconductor Production Have Risen Dramatically") +
  theme_apricitas + theme(legend.position = c(.45,.72)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Annual Chipmaking Machine Imports to China by Category",values = rev(c("#6A4C93","#3083DC","#A7ACD9","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Other Apparatus Used to Project Circuits (Mostly Lithography Machines)","Chemical Vapor Deposition Equipment","Dry Plasma Etching Machines","Oxidation, Diffusion, Annealing and Other Heat Treatment Equipment","Step and Repeat Aligners","Physical Vapor Deposition Equipment","Ion Implanters")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-365-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-365-as.Date("2016-01-01"))), ymin = 0-(.3*8), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_CHIP_MACHINES_IMPORT_DETAILS_Graph, "China Chip Machines Details.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



JAPAN_IP_ITEM <- read.xlsx("https://www.meti.go.jp/english/statistics/tyo/iip/xls/b2015_hsm1e.xlsx") %>%
  select(-`Seasonally.adjusted.Index.by.Commodity.:.Industrial.Production.(2015=100.0)`,-X3) %>%
  drop_na() %>%
  data.table::transpose() %>%
  as.data.frame() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  mutate(date = seq.Date(from = as.Date("2013-01-01"), by = "month", length.out = nrow(.))) %>%
  #mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate_if(is.character,as.numeric)

JAPAN_IP_ITEM_2020 <- read.xlsx("https://www.meti.go.jp/english/statistics/tyo/iip/xls/b2020_hsm1e.xlsx", sheet = "Production") %>%
  select(-`Seasonally.adjusted.Index.by.Commodity.:.Industrial.Production.(2020=100.0)`,-X3) %>%
  drop_na() %>%
  data.table::transpose() %>%
  as.data.frame() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = nrow(.))) %>%
  #mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate_if(is.character,as.numeric)

#MUST MANUALLY UPDATE GITHUB DATA WITH METI SPREADSHEETS NOW
JAPAN_IP_ITEM_2020 <- read.xlsx("https://github.com/Miles-byte/Apricitas/raw/main/Chip%20War/b2020_hsm1e.xlsx", sheet = "Production") %>%
  select(-`Seasonally.adjusted.Index.by.Commodity.:.Industrial.Production.(2020=100.0)`,-X3) %>%
  drop_na() %>%
  data.table::transpose() %>%
  as.data.frame() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = nrow(.))) %>%
  #mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate_if(is.character,as.numeric)


JAPAN_IP_Semiconductor_Manufacturing_Equipment <- ggplot() + #plotting Japanese Semiconductor Manufacturing Equipment
  geom_line(data=subset(JAPAN_IP_ITEM, date > as.Date("2012-12-01")), aes(x=date,y= semiconductor_products_machinery/semiconductor_products_machinery[1]*100,color= "Semiconductor Manufacturing Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,460), breaks = c(100,200,300,400), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Japanese Chipmaking Equipment Production") +
  labs(caption = "Graph created by @JosephPolitano using METI Data",subtitle = "Japanese Production of Chipmaking Equipment Has Fallen From 2022 Highs But Remains Strong") +
  theme_apricitas + theme(legend.position = c(.4,.75), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "Industrial Production, Japan",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 90-(.3*370), ymax = 90) +
  coord_cartesian(clip = "off")

JAPAN_IP_Semiconductor_Manufacturing_Equipment_2020 <- ggplot() + #plotting Japanese Semiconductor Manufacturing Equipment
  geom_line(data=subset(JAPAN_IP_ITEM_2020, date > as.Date("2012-12-01")), aes(x=date,y= semiconductor_products_machinery/semiconductor_products_machinery[1]*100,color= "Semiconductor Manufacturing Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,200), breaks = c(100,150,200), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Japanese Chipmaking Equipment Production") +
  labs(caption = "Graph created by @JosephPolitano using METI Data",subtitle = "Japanese Production of Chipmaking Equipment Has Fallen From 2022 Highs But Remains Strong") +
  theme_apricitas + theme(legend.position = c(.3,.80), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "Industrial Production, Japan",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*140), ymax = 60) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = JAPAN_IP_Semiconductor_Manufacturing_Equipment, "Japan Chip Machine Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = JAPAN_IP_Semiconductor_Manufacturing_Equipment_2020, "Japan Chip Machine Manufacturing 2020.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

ecos.setKey("2DNSQWJY32YGLL8EM95R")

EXP_PRICE_SEMICONDUCTOR <- statSearch(lang = "en",stat_code = "402Y014", item_code1 = "3091AA", item_code2 = "C",start_time = "201601", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))
EXP_PRICE_COMPUTER <- statSearch(lang = "en",stat_code = "402Y014", item_code1 = "309AA", item_code2 = "C",start_time = "201601", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

EXP_IMP_PI_COMPUTERS_CONDUCTORS_Graph <- ggplot() + #plotting Korean Electronics Prices
  geom_line(data=EXP_PRICE_SEMICONDUCTOR, aes(x=time,y= data_value/data_value[49]*100, color= "Semiconductors"), size = 1.25) +
  geom_line(data=EXP_PRICE_COMPUTER, aes(x=time,y= data_value/data_value[49]*100, color= "Computers, Electronic & Optical Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,170),breaks = c(40,60,80,100,120,140,160), expand = c(0,0)) +
  ylab("Index: Jan 2020 = 100") +
  ggtitle("The Chip Cycle") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "Prices for Korean Tech Exports Have Fallen Dramatically") +
  theme_apricitas + theme(legend.position = c(.45,.15)) +
  scale_color_manual(name= "Export Price Index, Contractual Currency Basis, South Korea" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 50-(.3*110), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXP_IMP_PI_COMPUTERS_CONDUCTORS_Graph, "NETEXP Chips Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

datasets <- cbs_get_datasets() %>%
  filter(Language == "en") %>% # only English tables
  select(Identifier, ShortTitle) 

#DOWNLOADING NETHERLANDS PRODUCTION DATA
NETHERLANDS_PRODUCTION <- cbs_get_data('83838ENG') %>%
  subset(SectorBranchesSIC2008 == "336500") %>%
  filter(str_detect(`Periods`, fixed("MM", ignore_case = TRUE))) %>%
  mutate(date = as.Date(as.yearmon(Periods, "%YMM%m")))
  
NETHERLANDS_PRODUCTION_CHIP_MACHINES <- ggplot() + #plotting Dutch Semiconductor Manufacturing Equipment Production
  geom_line(data=subset(NETHERLANDS_PRODUCTION, date >= as.Date("2016-01-01")), aes(x=date,y= SeasonallyAdjustedProduction_3/SeasonallyAdjustedProduction_3[1]*100,color= "Machinery and Equipment, n.e.c.\n(Mostly Chip Machinery/Equipment)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(80,260), breaks = c(100,150,200,250), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Dutch Chipmaking Equipment Production") +
  labs(caption = "Graph created by @JosephPolitano using CBS Data",subtitle = "Dutch Production of Chipmaking Equipment Has Also Fallen From 2022 Highs") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= "Industrial Production, Netherlands",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 80-(.3*170), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

NETHERLANDS_FOREIGN_TURNOVER_CHIP_MACHINES <- ggplot() + #plotting Dutch Semiconductor Manufacturing Equipment Production
  geom_line(data=subset(NETHERLANDS_PRODUCTION, date >= as.Date("2016-01-01")), aes(x=date,y= SeasonallyAdjDailyTurnoverForeign_12/SeasonallyAdjDailyTurnoverForeign_12[1]*100,color= "Machinery and Equipment, n.e.c.\n(Mostly Chip Machinery/Equipment)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,300), breaks = c(100,150,200,250,300), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Dutch Chipmaking Equipment Sales") +
  labs(caption = "Graph created by @JosephPolitano using CBS Data",subtitle = "Dutch Production of Chipmaking Equipment Has Also Fallen From 2022 Highs") +
  theme_apricitas + theme(legend.position = c(.5,.7)) +
  scale_color_manual(name= "Foreign Sales, Netherlands",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 70-(.3*230), ymax = 70) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NETHERLANDS_PRODUCTION_CHIP_MACHINES, "Dutch Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = NETHERLANDS_FOREIGN_TURNOVER_CHIP_MACHINES, "Dutch Foreign Sales Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

install_github("pcdi/rstatscn")
library(rstatscn)

#please note: the package is weird in that it will only let me retrieve a certain n of previous results, so I just used 60 here
statscnQueryLastN(100, lang = "en")

IND_PRO_BULK <- statscnQueryData('A02092Q',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_BULK <- statscnQueryLastN(700, lang = "en") 

IND_PRO_CN <- IND_PRO_BULK %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(date >= as.Date("1992-01-01")) %>%
  subset(.,`Output of Integrated Circuits, Current Period` != 0) %>%
  mutate(reference_value = ifelse(year(date) == 2014 & month(date) == 12, `Output of Integrated Circuits, Current Period`, NA)) %>%
  fill(reference_value, .direction = "downup") %>%
  mutate(`Output of Integrated Circuits, Current Period` = 100*(`Output of Integrated Circuits, Current Period` / reference_value)) %>%
  select(-reference_value)

CHINA_IND_PRO_CHIP <- ggplot() + #plotting Chinese Semiconductor Production
  geom_line(data= IND_PRO_CN, aes(x=date,y=`Output of Integrated Circuits, Current Period` ,color= "China, Industrial Production of Integrated Circuits"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,400), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ylab("Index, Dec 2014 = 100") +
  ggtitle("Chinese Chip Production") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "Chinese Chip Production Fell Significantly in 2022 But Has Since Rebounded a Bit") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_CN$date)-(.1861*(max(IND_PRO_CN$date)-min(IND_PRO_CN$date))), xmax = min(IND_PRO_CN$date)-(0.049*(max(IND_PRO_CN$date)-min(IND_PRO_CN$date))), ymin = 0-(.3*400), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_IND_PRO_CHIP, "China Ind Pro Chip Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHINA_IND_PRO_CHIP_2016 <- ggplot() + #plotting Chinese Semiconductor Production
  geom_line(data= filter(IND_PRO_CN, date >= as.Date("2015-12-01")), aes(x=date,y=`Output of Integrated Circuits, Current Period`/1.4855295,color= "China, Industrial Production of Semiconductors\n(Number of Units, Indexed)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,300), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ylab("Index, Dec 2017 = 100") +
  ggtitle("Chinese Chip Production") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "Chinese Chip Production Fell Significantly in 2022 But Has Since Rebounded to Record Highs") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-12-01")-(.1861*(max(IND_PRO_CN$date)-as.Date("2015-12-01"))), xmax = as.Date("2015-12-01")-(0.049*(max(IND_PRO_CN$date)-as.Date("2015-12-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_IND_PRO_CHIP_2016, "China Ind Pro Chip 2016 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAPUTE <- statscnQueryData('A030202',dbcode='hgjd',lang = "en", rowcode = "sj", colcode = "zb") 

CAPUTE <- statscnQueryLastN(100, lang = "en") %>%
  mutate(date = as.Date(as.yearqtr(rownames(.), "%qQ %Y"))) %>%#capute data  
  subset(.,`Industrial Capacity Utilization, Manufacture of Computers,Communication and Other Electronic Equipment, Current Quarter` != 0)
  
CHINA_CAPUTE_ELECTRONICS <- ggplot() + #plotting Chinese Semiconductor Production
  geom_line(data=subset(CAPUTE,`Industrial Capacity Utilization, Manufacture of Computers,Communication and Other Electronic Equipment, Current Quarter` != 0), aes(x=date,y= `Industrial Capacity Utilization, Manufacture of Computers,Communication and Other Electronic Equipment, Current Quarter`/100, color= "Manufacture of Computers, Communication, and Electronic Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.70,.83), breaks = c(.70,.75,.80), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Chinese Electronic Production") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "Chinese Electronic Capacity Utilization Has Fallen to The Lowest Levels Since 2021") +
  theme_apricitas + theme(legend.position = c(.515,.2)) +
  scale_color_manual(name= "China, Industrial Capacity Utilization",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(CAPUTE$date)-(.1861*(max(CAPUTE$date)-min(CAPUTE$date))), xmax = min(CAPUTE$date)-(0.049*(max(CAPUTE$date)-min(CAPUTE$date))), ymin = .70-(.3*.13), ymax = .70) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_CAPUTE_ELECTRONICS, "China Capute Electronics Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FIXED_INVESTMENT <- statscnQueryData('A0403',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") %>%
  mutate(date = as.Date(as.yearmon(rownames(.))))

FIXED_INVESTMENT_CN <- statscnQueryLastN(1000, lang = "en") %>%
  subset(`Investment in Fixed Assets, Information Transmission, _Computer Services and Software, Accumulated Growth _Rate` != 0) %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  filter(month(.[1, 'date']) == month(.$date))

CHINA_FIXED_INVESTMENT_ELECTRONICS <- ggplot() + #plotting Chinese Semiconductor Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(FIXED_INVESTMENT_CN,`Investment in Fixed Assets, Information Transmission, _Computer Services and Software, Accumulated Growth _Rate` != 0), aes(x=date,y= `Investment in Fixed Assets, Information Transmission, _Computer Services and Software, Accumulated Growth _Rate`/100, color= "Information Transmission, Computer Services, and Software"), size = 1.25) +
  geom_line(data=subset(FIXED_INVESTMENT_CN,`Investment in Fixed Assets, Manufacture of _Communication Equipment, Computers and Other _Electronic Equipment, Accumulated Growth Rate` != 0), aes(x=date,y= `Investment in Fixed Assets, Manufacture of _Communication Equipment, Computers and Other _Electronic Equipment, Accumulated Growth Rate`/100, color= "Manufacture of Communication Equipment, Computers, and Other Electronic Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.075,.375), breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("Growth in Fixed Investment, Year-on-Year") +
  ggtitle("Chinese Electronic Investment") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "Chinese Electronic Manufacturing Investment Slowed as Software/Services Investment Rebounds") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= paste0("China, Investment in Fixed Assets, Jan to ", month(FIXED_INVESTMENT$date[1], label = TRUE, abbr = TRUE), ", Growth"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Manufacture of Communication Equipment, Computers, and Other Electronic Equipment","Information Transmission, Computer Services, and Software")) +
  annotation_custom(apricitas_logo_rast, xmin = min(FIXED_INVESTMENT$date)-(.1861*(max(FIXED_INVESTMENT$date)-min(FIXED_INVESTMENT$date))), xmax = min(FIXED_INVESTMENT$date)-(0.049*(max(FIXED_INVESTMENT$date)-min(FIXED_INVESTMENT$date))), ymin = -0.075-(.3*.45), ymax = -0.075) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_FIXED_INVESTMENT_ELECTRONICS, "China Fixed Investment Electronics Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



CHINA_FIXED_INVESTMENT_ELECTRONICS_ONLY_Graph <- ggplot() + #plotting Chinese Semiconductor Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  #geom_line(data=subset(FIXED_INVESTMENT,`Investment in Fixed Assets, Information Transmission, _Computer Services and Software, Accumulated Growth _Rate` != 0), aes(x=date-335,y= `Investment in Fixed Assets, Information Transmission, _Computer Services and Software, Accumulated Growth _Rate`/100, color= "Information Transmission, Computer Services, and Software"), size = 1.25) +
  geom_line(data=subset(FIXED_INVESTMENT_CN,`Investment in Fixed Assets, Manufacture of _Communication Equipment, Computers and Other _Electronic Equipment, Accumulated Growth Rate` != 0), aes(x=date-335,y= `Investment in Fixed Assets, Manufacture of _Communication Equipment, Computers and Other _Electronic Equipment, Accumulated Growth Rate`/100, color= "China, Investment in Fixed Assets, Annual Growth\nManufacture of Communication Equipment, Computers, and Other Electronic Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.30), breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("Growth in Fixed Investment, Year-on-Year") +
  ggtitle("Chinese Electronic Investment") +
  labs(caption = "Graph created by @JosephPolitano using NBSS Data",subtitle = "2023 Saw a Slowdown in Chinese Electronic Manufacturing Investment Growth") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(FIXED_INVESTMENT$date)-335-(.1861*(max(FIXED_INVESTMENT$date)-335-min(FIXED_INVESTMENT$date))), xmax = min(FIXED_INVESTMENT$date)-335-(0.049*(max(FIXED_INVESTMENT$date)-335-min(FIXED_INVESTMENT$date))), ymin = 0-(.3*.30), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_FIXED_INVESTMENT_ELECTRONICS_ONLY_Graph, "China Fixed Investment Electronics Only Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


FIXED_EQUIP_INVEST_SPECS_REAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50506',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST_REAL <- beaGet(FIXED_EQUIP_INVEST_SPECS_REAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_EQUIP_INVEST_SPECS_NOMINAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50505',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2018, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST_NOMINAL <- beaGet(FIXED_EQUIP_INVEST_SPECS_NOMINAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

US_CHIP_FIXED_EQUIP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = (u50506_c272rx_18_special_industry_machinery_n_e_c_chained_dollars_level_6/u50506_c272rx_18_special_industry_machinery_n_e_c_chained_dollars_level_6[1]*FIXED_EQUIP_INVEST_NOMINAL$u50505_c272rc_18_special_industry_machinery_n_e_c_current_dollars_level_6[1])/1000, color = "Real 2018 Dollars"), size = 1.25) + 
  geom_line(data = FIXED_EQUIP_INVEST_NOMINAL, aes(x=date, y = (u50505_c272rc_18_special_industry_machinery_n_e_c_current_dollars_level_6/1000), color = "Nominal"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,90), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("US Chip Equipment Spending Has Surged") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Nominal Business Investment in Special Industry Equipment Has Doubled Since 2018") +
  theme_apricitas + theme(legend.position = c(.51,.23)) +
  scale_color_manual(name= "Private Fixed Investment, Special Industry Machinery N.E.C. (Mostly Semiconductor Equipment)",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*90), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CHIP_FIXED_EQUIP_INVEST_GRAPH, "US Chip Equipment Fixed Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1F02011&conn_path=I2&language=en
KOREA_IND_PRO <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/KOR_PRODUCTION.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(semi_prod_growth = (semi-lag(semi, 12))/lag(semi, 12)) %>%
  mutate(moving_average_3month = rollapply(semi, width = 3, FUN = mean, align = "right", fill = NA)) %>%
  mutate(semi_prod_growth3 = (moving_average_3month / lag(moving_average_3month, 12) - 1)) %>%
  subset(date >= as.Date("2017-01-01"))

TWN_IND_PRO <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_PROD.csv") %>%
  select(-X) %>%
  mutate(date = as.Date(Item)) %>%
  drop_na() %>%
  mutate(semi_prod_growth = (Manufacture.of.Integrated.Circuits-lag(Manufacture.of.Integrated.Circuits, 12))/lag(Manufacture.of.Integrated.Circuits, 12)) %>%
  mutate(moving_average_3month = rollapply(Manufacture.of.Integrated.Circuits, width = 3, FUN = mean, align = "right", fill = NA)) %>%
  mutate(semi_prod_growth3 = (moving_average_3month / lag(moving_average_3month, 12) - 1))

US_IND_PRO <- fredr("IPG3344S", observation_start = as.Date("2016-01-01")) %>%
  mutate(semi_prod_growth = (value-lag(value, 12))/lag(value, 12))

#JAPAN_IND_PRO <- read.xlsx("https://www.meti.go.jp/english/statistics/tyo/iip/xls/b2015_gsm1e.xlsx") %>%
  
JAPAN_IND_PRO <- read.xlsx("https://github.com/Miles-byte/Apricitas/raw/main/Chip%20War/b2020_gsm1e.xlsx") %>%  
  select(-`Seasonally.adjusted.Index.by.Industry.:.Industrial.Production.(2020=100.0)`,-X3) %>%
  drop_na() %>%
  data.table::transpose() %>%
  as.data.frame() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = nrow(.))) %>%
  #mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(semi_prod_growth = (integrated_circuits-lag(integrated_circuits, 12))/lag(integrated_circuits, 12)) %>%
  mutate(moving_average_3month = rollapply(integrated_circuits, width = 3, FUN = mean, align = "right", fill = NA)) %>%
  mutate(semi_prod_growth3 = (moving_average_3month / lag(moving_average_3month, 12) - 1)) %>%
  subset(date >= as.Date("2017-01-01"))

EU_IND_PRO <- get_eurostat("sts_inpr_m")

EU_IND_PRO <- EU_IND_PRO %>%
  subset(s_adj == "SCA") %>%
  subset(TIME_PERIOD >= as.Date("2016-01-01")) %>%
  subset(unit == "I15") %>%
  subset(geo == "EU27_2020") %>%
  subset(nace_r2 == "C2611") %>%
  mutate(semi_prod_growth = (values-lag(values, 12))/lag(values, 12))

SEMI_INDPRO_GROWTH_ASIA <- ggplot() + #plotting Chinese Semiconductor Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=JAPAN_IND_PRO, aes(x=date,y= semi_prod_growth3, color= "Japan"), size = 1.25) +
  #geom_line(data=US_IND_PRO, aes(x=date,y= semi_prod_growth3, color= "United States (Semiconductors and Other Electronic Components)"), size = 1.25) +
  geom_line(data=subset(TWN_IND_PRO, date >= as.Date("2017-01-01")), aes(x=date,y= semi_prod_growth3, color= "Taiwan"), size = 1.25) +
  geom_line(data=KOREA_IND_PRO, aes(x=date,y= semi_prod_growth3, color= "South Korea"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.40,.45), breaks = c(-.40,-.20,0,.20,.40), expand = c(0,0)) +
  ylab("Percent Growth From Year Ago") +
  ggtitle("Key Exporters' Semiconductor Production") +
  labs(caption = "Graph created by @JosephPolitano using MOEA, BOK, and METI Data",subtitle = "Semiconductor Production in Key Asian Markets is Rebounding From a Recent Fall") +
  theme_apricitas + theme(legend.position = c(.43,.87)) +
  scale_color_manual(name= "Industrial Production, Semiconductors, Annual Growth in 3MMA",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Taiwan","Japan","South Korea","United States (Semiconductors and Other Electronic Components)")) +
  annotation_custom(apricitas_logo_rast, xmin = min(KOREA_IND_PRO$date)-(.1861*(max(KOREA_IND_PRO$date)-min(KOREA_IND_PRO$date))), xmax = min(KOREA_IND_PRO$date)-(0.049*(max(KOREA_IND_PRO$date)-min(KOREA_IND_PRO$date))), ymin = -0.40-(.3*.85), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SEMI_INDPRO_ASIA <- ggplot() + #plotting Chinese Semiconductor Production
  geom_line(data=JAPAN_IND_PRO, aes(x=date,y= integrated_circuits/integrated_circuits[1]*100, color= "Japan"), size = 1.25) +
  #geom_line(data=subset(US_IND_PRO, date >= as.Date("2017-01-01")), aes(x=date,y= value/value[1]*100, color= "United States (Includes Other Electronic Components)"), size = 1.25) +
  geom_line(data=subset(TWN_IND_PRO, date >= as.Date("2017-01-01")), aes(x=date,y= Manufacture.of.Integrated.Circuits/Manufacture.of.Integrated.Circuits[13]*100, color= "Taiwan"), size = 1.25) +
  geom_line(data=KOREA_IND_PRO, aes(x=date,y= semi/semi[13]*100, color= "South Korea"), size = 1.25) +
  #geom_line(data=subset(EU_IND_PRO, time >= as.Date("2017-01-01")), aes(x=time,y= values/values[nrow(subset(EU_IND_PRO, time >= as.Date("2017-01-01")))]*100, color= "EU (Includes Other Electronic Components)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,215), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Key Exporters' Semiconductor Production") +
  labs(caption = "Graph created by @JosephPolitano using MOEA, BOK, and METI Data",subtitle = "Output of Chips is Rebounding in in Key Asian Exporters, Beating or Closing in on Previous Records") +
  theme_apricitas + theme(legend.position = c(.25,.8)) +
  scale_color_manual(name= "Industrial Production, Semiconductors",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Taiwan","South Korea","Japan","United States (Includes Other Electronic Components)","EU (Includes Other Electronic Components)")) +
  annotation_custom(apricitas_logo_rast, xmin = min(KOREA_IND_PRO$date)-(.1861*(max(KOREA_IND_PRO$date)-min(KOREA_IND_PRO$date))), xmax = min(KOREA_IND_PRO$date)-(0.049*(max(KOREA_IND_PRO$date)-min(KOREA_IND_PRO$date))), ymin = 0.0-(.3*225), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


SEMI_INDPRO_MAJOR_COUNTRIES <- ggplot() + #plotting Chinese Semiconductor Production
  geom_line(data=JAPAN_IND_PRO, aes(x=date,y= integrated_circuits/integrated_circuits[1]*100, color= "Japan"), size = 1.25) +
  geom_line(data=subset(US_IND_PRO, date >= as.Date("2017-01-01")), aes(x=date,y= value/value[1]*100, color= "United States (Includes Other Electronic Components)"), size = 1.25) +
  geom_line(data=subset(TWN_IND_PRO, date >= as.Date("2017-01-01")), aes(x=date,y= Manufacture.of.Integrated.Circuits/Manufacture.of.Integrated.Circuits[13]*100, color= "Taiwan"), size = 1.25) +
  geom_line(data=KOREA_IND_PRO, aes(x=date,y= semi/semi[13]*100, color= "South Korea"), size = 1.25) +
  #geom_line(data=subset(EU_IND_PRO, time >= as.Date("2017-01-01")), aes(x=time,y= values/values[nrow(subset(EU_IND_PRO, time >= as.Date("2017-01-01")))]*100, color= "EU (Includes Other Electronic Components)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,215), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Global Semiconductor Production") +
  labs(caption = "Graph created by @JosephPolitano using MOEA, BOK, METI, Federal Reserve, and Eurostat Data",subtitle = "Output of Chips Have Fallen in Key Asian Markets, But are Holding Up in the US/EU") +
  theme_apricitas + theme(legend.position = c(.4,.2), legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 12), legend.title = element_text(size = 13)) +
  scale_color_manual(name= "Industrial Production, Semiconductors",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Taiwan","South Korea","Japan","United States (Includes Other Electronic Components)","EU (Includes Other Electronic Components)")) +
  annotation_custom(apricitas_logo_rast, xmin = min(JAPAN_IND_PRO$date)-(.1861*(max(JAPAN_IND_PRO$date)-min(JAPAN_IND_PRO$date))), xmax = min(JAPAN_IND_PRO$date)-(0.049*(max(JAPAN_IND_PRO$date)-min(JAPAN_IND_PRO$date))), ymin = 0.0-(.3*225), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SEMI_INDPRO_GROWTH_ASIA, "Semi Asia Indpro Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SEMI_INDPRO_ASIA, "Semi Indpro Asia Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ggsave(dpi = "retina",plot = SEMI_INDPRO_MAJOR_COUNTRIES, "Semi Indpro Major Countries Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TWN_EXPORT_ORDERS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_EXPORT_ORDERS_CHINA.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(across(where(is.character), ~ as.numeric(gsub(",", "", .))))

TAIWAN_CHIP_EXPORTS_CN_HK <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TW_EXP_EXCLUDING_REEXPT.csv") %>%
  mutate(Date = as.Date(Date))

TAIWAN_EXPORTS_ORDERS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=TAIWAN_CHIP_EXPORTS, aes(x=date,y= exports/1000000,color= "Exports of Semiconductors and Related Items to China, HK, and Macau"), size = 1.25) + 
  geom_line(data=TAIWAN_CHIP_EXPORTS_CN_HK, aes(x=Date,y= Total/1000000000,color= "New Export Orders of Electronic Products from China & HK"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10.5), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Chip Trade with China") +
  labs(caption = "Graph created by @JosephPolitano using MOEA data",subtitle = "Taiwanese Exports of Chips to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.5,.15)) +
  scale_color_manual(name= NULL, values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*10.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_EXPORTS_ORDERS_CHINA_Dollar_Graph, "Taiwan Export & Orders China Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TAIWAN_EXPORT_ORDERS_COUNTRY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_EXPORT_ORDERS_ELECTRONICS_COUNTRY.csv") %>%
  select(-date) %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq.Date(from = as.Date("2016-01-01"), by = "month", length.out = nrow(.)))
  
  
TAIWAN_EXPORT_ORDERS_COUNTRY_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= (Others+Japan)/1000,color= "Others"), size = 1.25) +
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= Europe/1000,color= "Europe (Including Russia)"), size = 1.25) + 
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= ASEAN/1000,color= "ASEAN"), size = 1.25) + 
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= USA/1000,color= "USA"), size = 1.25) + 
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= China_HK/1000,color= "China and HK"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = 1),limits = c(0,8.5), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Electronics Export Orders") +
  labs(caption = "Graph created by @JosephPolitano using MOEA data seasonally adjusted usint X-13ARIMA",subtitle = "Taiwanese Exports orders to China and Russia Have Fallen, as Orders to ASEAN Nations Rise") +
  theme_apricitas + theme(legend.position = c(.3,.775)) +
  scale_color_manual(name= "Electronics Export Orders by Region", values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("China and HK","USA","ASEAN","Europe (Including Russia)","Others")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*8.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_EXPORT_ORDERS_COUNTRY_Graph, "Taiwan Export Orders County Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


SEMI_EQUP_JPN_TWN_NTH_GRAPH <- ggplot() + #plotting Japanese Semiconductor Manufacturing Equipment
  geom_line(data=subset(TWN_IND_PRO, date >= as.Date("2016-01-01")), aes(x=date,y= Manufacture.of.Electronic.and.Semi.conductors.Production.Equipment/Manufacture.of.Electronic.and.Semi.conductors.Production.Equipment[25]*100,color= "Taiwan, Semiconductor and Other Electronic Production Equipment"), size = 1.25) +
  geom_line(data=subset(NETHERLANDS_PRODUCTION, date >= as.Date("2016-01-01")), aes(x=date,y= SeasonallyAdjustedProduction_3/SeasonallyAdjustedProduction_3[25]*100,color= "Netherlands, Machinery and Equipment, n.e.c. (Mostly Chip Machinery/Equipment)"), size = 1.25) +
  geom_line(data=subset(JAPAN_IP_ITEM_2020, date >= as.Date("2016-01-01")), aes(x=date,y= semiconductor_products_machinery/semiconductor_products_machinery[1]*100,color= "Japan, Semiconductor Manufacturing Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,240), breaks = c(100,200,300,400), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Key Global Chipmaking Equipment Production") +
  labs(caption = "Graph created by @JosephPolitano using METI, CBS, and MOEA Data",subtitle = "Global Production of Chipmaking Equipment Has Declined From Its 2022 Peaks") +
  theme_apricitas + theme(legend.position = c(.5,.15), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*240), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SEMI_EQUP_JPN_TWN_NTH_GRAPH, "Key Semiconductor Equipment Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_IND_PRO <- fredr("IPG3344S", observation_start = as.Date("2016-01-01"))
US_IND_CAP <- fredr("CAPUTLG3344S", observation_start = as.Date("2016-01-01")) %>%
  merge(.,US_IND_PRO, by = "date") %>%
  transmute(date, value = value.y/(value.x/ 100))

US_IND_PRO_CAP_GRAPH <- ggplot() + #plotting US Semiconductor Indpro
  geom_line(data=US_IND_PRO, aes(x=date,y= value/value[25]*100,color= "Industrial Production, Semiconductors and Other Electronic Components (NAICS = 3344)"), size = 1.25) +
  geom_line(data=US_IND_CAP, aes(x=date,y= value/US_IND_PRO$value[25]*100,color= "Industrial Capacity, Semiconductors and Other Electronic Components (NAICS = 3344)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,200), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("US Semiconductor Production & Capacity") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "US Semiconductor Production and Capacity are at Record Highs") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IND_PRO_CAP_GRAPH, "IND PRO CAP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_IND_PRO_CAP_ADJUSTED_GRAPH <- ggplot() + #plotting US Semiconductor Indpro
  geom_line(data=US_IND_PRO, aes(x=date,y= value/value[25]*100,color= "Industrial Production, Semiconductors and Other Electronic Components (NAICS = 3344)"), size = 1.25) +
  geom_line(data=US_IND_CAP, aes(x=date,y= value/US_IND_PRO$value[25]*100,color= "Industrial Capacity, Semiconductors and Other Electronic Components (NAICS = 3344)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,175), breaks = c(75,100,125,150,175), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("US Semiconductor Production & Capacity") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "US Semiconductor Production and Capacity are at Record Highs") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 75-(.3*100), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IND_PRO_CAP_ADJUSTED_GRAPH, "IND PRO CAP ADJUSTED Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

p_unload(all)

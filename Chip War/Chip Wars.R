pacman::p_load(cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#70323000 Semiconductors
#70323050 Integrated Circuits

Trade_search <- estat_getStatsList(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  searchWord = "Trade",
  lang = "E"
)

JPN_TRADE_DATA <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003334002", #Labor Force Survey Basic Tabulation
  lang = "E", #english language
  limit = 5000,
  cdArea = "50105"
)

JPN_TRADE_DATA2 <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003228235", #Labor Force Survey Basic Tabulation
  lang = "E", #english language
  limit = 5000,
  cdArea = "50105"
)

JPN_TRADE_DATA_2016_2020 <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003313967", #Labor Force Survey Basic Tabulation
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = c("70323000","70131010"),
  cdArea = c("50105","50129","50108")
)

JPN_TRADE_DATA_2021_Plus <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003425295", #Labor Force Survey Basic Tabulation
  lang = "E", #english language
  #limit = 5000,
  cdCat01 = c("70323000","70131010"),
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
  subset(sum_value != 0) #%>%
  # ungroup() %>%
  # select(-date) %>%
  # ts(., start = c(2016,1), frequency = 12) %>%
  # seas() %>%
  # final() %>%
  # as.data.frame(value = melt(.)) %>%
  # transmute(date = seq(from = as.Date("2016-01-01"), by = "month", length = nrow(.)), exports = x)

JAPAN_CHIP_MACHINES <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70131010") %>%
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
  subset(cat01_code == "70131010") %>%
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
  geom_line(data=JAPAN_CHIP, aes(x=date,y= exports/1000000,color= "Semiconductors & Related Items"), size = 1.25) + 
  geom_line(data=JAPAN_CHIP_MACHINES, aes(x=date,y= exports/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "¥", accuracy = 1),limits = c(0,225), breaks = c(1,50,100,150,200), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan data seasonally adjusted usint X-13ARIMA",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
  theme_apricitas + theme(legend.position = c(.39,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors & Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

JAPAN_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP_DOLLAR, aes(x=date,y= (exports/value)/1000000,color= "Semiconductors & Related Items"), size = 1.25) + 
  geom_line(data=JAPAN_CHIP_MACHINES_DOLLAR, aes(x=date,y= (exports/value)/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 0.5),limits = c(0,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan data seasonally adjusted usint X-13ARIMA",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
  theme_apricitas + theme(legend.position = c(.39,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors & Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

JAPAN_CHIP_MACHINES_QTY_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP_MACHINES_QTY, aes(x=date,y= sum_value/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "kt", accuracy = 1),limits = c(0,4), breaks = c(1,2,3,4), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan data seasonally adjusted usint X-13ARIMA",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
  theme_apricitas + theme(legend.position = c(.39,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors & Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

US_CIRCUITS_CHINA_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "DF"), 
  DF = 1, #excluding reexport
  time = "from 2013 to 2023",
  E_COMMODITY = "8542", #integrated circuits commodity code
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
  summarise(`8542` = sum(`8542`, na.rm = TRUE),`8486` = sum(`8486`, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-time) %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)), `8542`=`8542`, `8486`=`8486`)

US_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=date,y= `8542`/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `8541`/1000000000,color= "Diodes, Transistors, and Similar Semiconductor Devices"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=date,y= `8486`/1000000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `848071`/1000000000,color= "Molds For the Manufacture of Semiconductor Devices"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `903141`/1000000000,color= "Optical Instruments for Inspecting Semiconductor Wafers"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `903082`/1000000000,color= "Oscilloscopes, spectrum analyzers"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `8534`/1000000000,color= "Printed Circuits"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = .25),limits = c(0,1.25), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("US Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA",subtitle = "Sanctions Have Led to a Major Fall in US Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.385,.87)) +
  scale_color_manual(name= "US-Manufactured Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IC_EXPORTS_CHINA_Graph, "US IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

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

EURO_EXCHANGE_RATE <- fredr("EXUSEU", observation_start = as.Date("2016-01-01"))

EU_CHIP_DOLLAR <- left_join(EU_CHIP_EXPORTS,EURO_EXCHANGE_RATE)
EU_CHIP_MACHINES_DOLLAR <- left_join(EU_CHIP_MACHINES,EURO_EXCHANGE_RATE)

EU_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=EU_CHIP_EXPORTS, aes(x=date,y= exports/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=EU_CHIP_MACHINES, aes(x=date,y= exports/1000000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "€", accuracy = .25),limits = c(0,1.30), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("EU Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Sanctions Have Led to a Major Fall in EU Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.385,.87)) +
  scale_color_manual(name= "EU Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

EU_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=EU_CHIP_DOLLAR, aes(x=date,y= (exports/value)/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=EU_CHIP_MACHINES_DOLLAR, aes(x=date,y= (exports/value)/1000000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,1.25), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("EU Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Sanctions Have Led to a Major Fall in EU Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.385,.87)) +
  scale_color_manual(name= "EU Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_IC_EXPORTS_CHINA_Dollar_Graph, "EU IC Exports China Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = EU_IC_EXPORTS_CHINA_Graph, "EU IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


KOR_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/KOR_EXPORTS.csv") %>%
  mutate(Date = as.Date(Date))

KOR_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=KOR_EXPORTS, aes(x=Date,y= X8542/1000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=KOR_EXPORTS, aes(x=Date,y= X8486/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Korean Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Korean Exports of Chips & Manufacturing Equipment to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.385,.92)) +
  scale_color_manual(name= "Korean Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

TAIWAN_EXPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_EXPORTS.csv") %>%
  mutate(Time = as.Date(as.yearmon(Time, "%Y/%m")))

TAIWAN_CHIP_EXPORTS <- TAIWAN_EXPORTS %>%
  subset(Commodity.Code == "8542") %>%
  group_by(Time) %>%
  summarise(INDICATOR_VALUE = sum(Value.USD..1000., na.rm = TRUE)) %>%
  transmute(date = as.Date(Time),INDICATOR_VALUE)

TAIWAN_CHIP_MACHINES <- TAIWAN_EXPORTS %>%
  subset(Commodity.Code == "8486") %>%
  group_by(Time) %>%
  summarise(INDICATOR_VALUE = sum(Value.USD..1000., na.rm = TRUE)) %>%
  transmute(date = as.Date(Time),INDICATOR_VALUE)

TAIWAN_IC_EXPORTS_CHINA_Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=TAIWAN_CHIP_EXPORTS, aes(x=date,y= INDICATOR_VALUE/1000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  geom_line(data=TAIWAN_CHIP_MACHINES, aes(x=date,y= INDICATOR_VALUE/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = .25),limits = c(0,10.5), breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Korean Exports of Chips & Manufacturing Equipment to China have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.385,.92)) +
  scale_color_manual(name= "Taiwanese Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

CHINA_IMPORTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/CHINA_IMPORTS.csv") %>%
  mutate(Date = as.Date(as.yearmon(as.character(Date.of.data), "%Y%m"))) %>%
  select(Date, Commodity.code, Trading.partner, US.dollar) %>%
  mutate(US.dollar = as.numeric(gsub(",","",US.dollar))) %>%
  subset(Trading.partner != "China" & Trading.partner != "Hong Kong,China" & Trading.partner != "Macau,China" ) 

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
  subset(Date != as.Date("2023-05-01")) %>% #NOTE THIS IS A MANUAL DELETION BECAUSE CHINESE DATA COMES OUT BEFORE HONG KONG DATA. MUST BE EDITED
  group_by(name, Date) %>%
  summarise(value = sum(value, na.rm = TRUE))

CHINA_HK_CHIP_MACHINES_IMPORTS <- rbind(HK_CHIP_MACHINES_IMPORTS,CHINA_CHIP_MACHINES_IMPORTS) %>%
  subset(Date != as.Date("2023-05-01")) %>% #NOTE THIS IS A MANUAL DELETION BECAUSE CHINESE DATA COMES OUT BEFORE HONG KONG DATA. MUST BE EDITED
  group_by(name, Date) %>%
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

CHINA_HK_IC_Imports_Dollar_Bar_Graph <- ggplot(data = CHINA_HK_CHIP_IMPORTS, aes(x = Date, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,10,20,30,40,50), limits = c(0,53), expand = c(0,0)) +
  ggtitle("Chinese Semiconductor Imports") +
  labs(caption = "Graph created by @JosephPolitano using GACC data Seasonally Adjusted using X-13ARIMA", subtitle = "Chinese Semiconductor Imports Have Fallen Significantly, Particularly From Taiwan and Korea") +
  theme_apricitas + theme(legend.position = c(.3,.8), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Chip Imports to China and Hong Kong by Country",values = c("#6A4C93","#3083DC","#A7ACD9","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*53), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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

CHINA_HK_CHIP_MACHINES_Imports_Dollar_Bar_Graph <- ggplot(data = CHINA_HK_CHIP_MACHINES_IMPORTS, aes(x = Date, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,1,2,3,4), limits = c(0,4), expand = c(0,0)) +
  ggtitle("Chinese Chipmaking Machine Imports") +
  labs(caption = "Graph created by @JosephPolitano using GACC data Seasonally Adjusted Using X-13ARIMA", subtitle = "Chinese Imports of Machines for Semiconductor Production Have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.38,.82), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Chipmaking Machine Imports to China and Hong Kong by Country",values = c("#6A4C93","#3083DC","#A7ACD9","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*4), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_HK_CHIP_MACHINES_Imports_Dollar_Bar_Graph, "China HK Chip Machines.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

JAPAN_IP <- read.xlsx("https://www.meti.go.jp/english/statistics/tyo/iip/xls/b2015_gsm1e.xlsx") %>%
  select(-`Seasonally.adjusted.Index.by.Industry.:.Industrial.Production.(2015=100.0)`,-X3) %>%
  drop_na() %>%
  data.table::transpose() %>%
  as.data.frame() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  mutate(date = seq.Date(from = as.Date("2013-01-01"), by = "month", length.out = nrow(.))) %>%
  #mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate_if(is.character,as.numeric)

JAPAN_IP_Semiconductor_Manufacturing_Equipment <- ggplot() + #plotting Japanese Semiconductor Manufacturing Equipment
  geom_line(data=subset(JAPAN_IP, date > as.Date("2014-12-01")), aes(x=date,y= semiconductor_and_flat_panel_display_manufacturing_equipment/semiconductor_and_flat_panel_display_manufacturing_equipment[1]*100,color= "Semiconductor & Flat Panel Display Manufacturing Equipment"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,120), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Japanese Chipmaking Equipment Production") +
  labs(caption = "Graph created by @JosephPolitano using METI Data",subtitle = "Japanese Production of Chipmaking Equipment Has Fallen Significantly From 2022 Highs") +
  theme_apricitas + theme(legend.position = c(.4,.75), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "Industrial Production, Japan",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = JAPAN_IP_Semiconductor_Manufacturing_Equipment, "Japan Semiconductor Chip Machine Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EXP_PRICE_SEMICONDUCTOR <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "402Y014", item_code1 = "3091AA", item_code2 = "C",start_time = "201601", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))
EXP_PRICE_COMPUTER <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "402Y014", item_code1 = "309AA", item_code2 = "C",start_time = "201601", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

EXP_IMP_PI_COMPUTERS_CONDUCTORS_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=EXP_PRICE_SEMICONDUCTOR, aes(x=time,y= data_value/data_value[49]*100, color= "Semiconductors"), size = 1.25) +
  geom_line(data=EXP_PRICE_COMPUTER, aes(x=time,y= data_value/data_value[49]*100, color= "Computers, Electronic & Optical Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,170),breaks = c(40,60,80,100,120,140,160), expand = c(0,0)) +
  ylab("Index: Jan 2020 = 100") +
  ggtitle("Korea's Tech Wreck") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "Prices for Korean Tech Exports Have Fallen Dramatically at the End of 2022") +
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
  geom_line(data=subset(NETHERLANDS_PRODUCTION, date >= as.Date("2015-01-01")), aes(x=date,y= SeasonallyAdjustedProduction_3/SeasonallyAdjustedProduction_3[1]*100,color= "Machinery and Equipment, n.e.c. (Mostly Chip Machinery/Equipment)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(80,250), breaks = c(100,150,200,250), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Dutch Chipmaking Equipment Production") +
  labs(caption = "Graph created by @JosephPolitano using CBS Data",subtitle = "Dutch Production of Chipmaking Equipment Has Also Fallen From 2022 Highs") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= "Industrial Production, Netherlands",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 80-(.3*170), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

NETHERLANDS_FOREIGN_TURNOVER_CHIP_MACHINES <- ggplot() + #plotting Dutch Semiconductor Manufacturing Equipment Production
  geom_line(data=subset(NETHERLANDS_PRODUCTION, date >= as.Date("2015-01-01")), aes(x=date,y= SeasonallyAdjDailyTurnoverForeign_12/SeasonallyAdjDailyTurnoverForeign_12[1]*100,color= "Machinery and Equipment, n.e.c. (Mostly Chip Machinery/Equipment)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,275), breaks = c(100,150,200,250), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Dutch Chipmaking Equipment Sales") +
  labs(caption = "Graph created by @JosephPolitano using CBS Data",subtitle = "Dutch Production of Chipmaking Equipment Has Also Fallen From 2022 Highs") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= "Foreign Sales, Netherlands",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 70-(.3*205), ymax = 70) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NETHERLANDS_PRODUCTION_CHIP_MACHINES, "Dutch Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = NETHERLANDS_FOREIGN_TURNOVER_CHIP_MACHINES, "Dutch Foreign Sales Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

pacman::p_load(censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  subset(sum_value != 0)
  
JAPAN_CHIP_MACHINES <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70131010") %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Value", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Value-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Value-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0)
  
JAPAN_CHIP_MACHINES_QTY <- rbind(JPN_TRADE_DATA_2016_2020,JPN_TRADE_DATA_2021_Plus) %>%
  subset(cat01_code == "70131010") %>%
  filter(str_detect(`Quantity-Value by Principal Commodity`, fixed("Quantity", ignore_case = TRUE))) %>%
  subset(`Quantity-Value by Principal Commodity` != "Quantity-Year") %>%
  mutate(`Quantity-Value by Principal Commodity` = gsub("Quantity-","",`Quantity-Value by Principal Commodity`)) %>%
  mutate(date = as.Date(paste0(`Quantity-Value by Principal Commodity`,"-01-",Year),format = "%B-%d-%Y")) %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  subset(sum_value != 0)

YEN_EXCHANGE_RATE <- fredr("EXJPUS", observation_start = as.Date("2016-01-01"))

JAPAN_CHIP_DOLLAR <- left_join(JAPAN_CHIP,YEN_EXCHANGE_RATE)
JAPAN_CHIP_MACHINES_DOLLAR <- left_join(JAPAN_CHIP_MACHINES,YEN_EXCHANGE_RATE)

JAPAN_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP, aes(x=date,y= sum_value/1000000,color= "Semiconductors & Related Items"), size = 1.25) + 
  geom_line(data=JAPAN_CHIP_MACHINES, aes(x=date,y= sum_value/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "¥", accuracy = 1),limits = c(0,225), breaks = c(1,50,100,150,200), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
  theme_apricitas + theme(legend.position = c(.39,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors & Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

JAPAN_IC_EXPORTS_CHINA__Dollar_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=JAPAN_CHIP_DOLLAR, aes(x=date,y= (sum_value/value)/1000000,color= "Semiconductors & Related Items"), size = 1.25) + 
  geom_line(data=JAPAN_CHIP_MACHINES_DOLLAR, aes(x=date,y= (sum_value/value)/1000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 0.5),limits = c(0,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Japanese Chip Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
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
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "China is the Biggest Importer of American-made Chips, and the US Just Sanctioned Them More") +
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
  summarise(`8542` = sum(`8542`, na.rm = TRUE),`8486` = sum(`8486`, na.rm = TRUE))

US_IC_EXPORTS_CHINA_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `8542`/1000000000,color= "Semiconductors and Related Items"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `8541`/1000000000,color= "Diodes, Transistors, and Similar Semiconductor Devices"), size = 1.25) + 
  geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `8486`/1000000000,color= "Machines For Manufacturing Semiconductors & Related Items"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `848071`/1000000000,color= "Molds For the Manufacture of Semiconductor Devices"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `903141`/1000000000,color= "Optical Instruments for Inspecting Semiconductor Wafers"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `903082`/1000000000,color= "Oscilloscopes, spectrum analyzers"), size = 1.25) + 
  #geom_line(data=US_CIRCUITS_CHINA_EXPORTS, aes(x=time,y= `8534`/1000000000,color= "Printed Circuits"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = .25),limits = c(0,1.25), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("US Semiconductor Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Sanctions Have Led to a Major Fall in US Exports of Chips & Manufacturing Equipment to China") +
  theme_apricitas + theme(legend.position = c(.38,.87)) +
  scale_color_manual(name= "US-Manufactured Exports to China, HK, and Macao",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Semiconductors and Related Items","Machines For Manufacturing Semiconductors & Related Items")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_IC_EXPORTS_CHINA_Graph, "US IC Exports China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#do 8541 and 8542 next time

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

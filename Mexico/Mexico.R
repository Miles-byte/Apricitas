pacman::p_load(dplyr,readr,viridis,siebanxicor,inegiR,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

usethis::edit_r_environ()
RESTATIS_KEY
#https://www.inegi.org.mx/servicios/api_indicadores.html
NONRES_CONSTRUCTION <- inegi_series(serie = "496326", token = Sys.getenv("TOKEN_INEGI"))

setToken(Sys.getenv("TOKEN_BANXICO"))

PESO_DOLLAR_FX <- getSerieDataFrame(getSeriesData(series = "SF17908"), "SF17908") %>%
  filter(date >= as.Date("2017-01-01"))
  
NONRES_CONSTRUCTION <- getSerieDataFrame(getSeriesData(series = "SR17480"), "SR17480") %>%
  filter(date >= as.Date("2017-01-01"))
  
NONRES_CONSTRUCTION_Graph <- ggplot() + 
  geom_line(data = NONRES_CONSTRUCTION, aes(x = date, y = value, color = "Real Fixed Investment, Nonresidential Construction, Mexico"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(50,160), breaks = c(50,100,150), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexico's Construction Boom") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Nonresidential Investment in Mexico is Booming Thanks to Public Investments and Nearshoring") +
  theme_apricitas + theme(legend.position = c(.4,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = "Real Fixed Investment, Nonresidential Construction, Mexico") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 50-(.3*110), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NONRES_CONSTRUCTION_Graph, "Nonresidential Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

ALL_CONSTRUCTION <- inegi_series(serie = "720348", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

BUILDINGS_CONSTRUCTION <- inegi_series(serie = "720355", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

WATER_CONSTRUCTION <- inegi_series(serie = "720362", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

ELECTRICITY_CONSTRUCTION <- inegi_series(serie = "720369", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

TRANSPORT_CONTRUCTION <- inegi_series(serie = "720376", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

OIL_CONSTRUCTION <- inegi_series(serie = "720383", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

OTHER_CONSTRUCTION <- inegi_series(serie = "720390", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

CONSTRUCTION_ACTIVITY_BREAKDOWN_Graph <- ggplot() + 
  geom_line(data = OTHER_CONSTRUCTION, aes(x = date, y = values, color = "Other Construction"), size = 1.25) +
  geom_line(data = ELECTRICITY_CONSTRUCTION, aes(x = date, y = values, color = "Electricity and Telecommunications"), size = 1.25) +
  geom_line(data = BUILDINGS_CONSTRUCTION, aes(x = date, y = values, color = "Buildings (Residential, Commercial, and Industrial)"), size = 1.25) +
  geom_line(data = WATER_CONSTRUCTION, aes(x = date, y = values, color = "Water, Irrigation, and Sanitation"), size = 1.25) +
  geom_line(data = TRANSPORT_CONTRUCTION, aes(x = date, y = values, color = "Transportation and Urbanization"), size = 1.25) +
  geom_line(data = OIL_CONSTRUCTION, aes(x = date, y = values, color = "Oil and Petrochemicals"), size = 1.25) +
  geom_line(data = ALL_CONSTRUCTION, aes(x = date, y = values, color = "All Construction"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,240), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexico's Construction Boom") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Construction is Up the Most In the Oil, Transportation, and Water Sectors—With Buildings Lagging") +
  theme_apricitas + theme(legend.position = c(.32,.79), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Real Construction Activity Index, 2018 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("All Construction","Oil and Petrochemicals","Transportation and Urbanization","Water, Irrigation, and Sanitation","Electricity and Telecommunications","Buildings (Residential, Commercial, and Industrial)","Other Construction"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*225), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_ACTIVITY_BREAKDOWN_Graph, "Construction Activity Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


PUBLIC_NOMINAL_CONSTRUCTION <- inegi_series(serie = "720645", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  merge(.,PESO_DOLLAR_FX, by = "date") %>%
  transmute(date, values = values/value)

PRIVATE_NOMINAL_CONSTRUCTION <- inegi_series(serie = "720646", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  merge(.,PESO_DOLLAR_FX, by = "date") %>%
  transmute(date, values = values/value)

PRIVATE_PUBLIC_NOMINAL_CONSTRUCTION <- ggplot() + 
  geom_line(data = PUBLIC_NOMINAL_CONSTRUCTION, aes(x = date, y = (values*12)/1000000, color = "Nominal Public-Sector Construction"), size = 1.25) +
  geom_line(data = PRIVATE_NOMINAL_CONSTRUCTION, aes(x = date, y = (values*12)/1000000, color = "Nominal Private-Sector Construction"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,26), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Dollars, Annual Rate") +
  ggtitle("Mexico's Construction Boom") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Mexican Construction Spending is Rising Rapidly, Especially Thanks to Public Investments") +
  theme_apricitas + theme(legend.position = c(.32,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Nominal Private-Sector Construction","Nominal Public-Sector Construction")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*26), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PRIVATE_PUBLIC_NOMINAL_CONSTRUCTION, "Private Public Nominal Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

NOMINAL_INDUSTRIAL_COMMERCIAL_SPENDING <- inegi_series(serie = "722086", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  merge(.,PESO_DOLLAR_FX, by = "date") %>%
  transmute(date, values = values/value)

NOMINAL_INDUSTRIAL_COMMERCIAL_SPENDING_Graph <- ggplot() + 
  geom_line(data = NOMINAL_INDUSTRIAL_COMMERCIAL_SPENDING, aes(x = date, y = values*12/1000000, color = "Nominal Industrial, Commercial, and Service Building Construction"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,10), breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Dollars, Annual Rate") +
  ggtitle("Mexico's Construction Boom") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Mexican Spending on Industrial, Commercial, and Service-Sector Buildings is Booming") +
  theme_apricitas + theme(legend.position = c(.45,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Nominal Industrial, Commercial, and Service Building Construction","Nominal Public-Sector Construction")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOMINAL_INDUSTRIAL_COMMERCIAL_SPENDING_Graph, "Nominal Industrial Commercial Spending Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


MEXICO_GDP <- getSerieDataFrame(getSeriesData(series = "SR17622"), "SR17622") %>%
  filter(date >= as.Date("2017-01-01")) %>%
  mutate(value = value/value[11]*100)
  
MEXICO_GVA_MANUFACTURING <- getSerieDataFrame(getSeriesData(series = "SR17628"), "SR17628") %>%
  filter(date >= as.Date("2017-01-01")) %>%
  mutate(value = value/value[11]*100)

MEXICO_GVA_CONSTRUCTION <- getSerieDataFrame(getSeriesData(series = "SR17627"), "SR17627") %>%
  filter(date >= as.Date("2017-01-01")) %>%
  mutate(value = value/value[11]*100)

MEXICO_REAL_GDP_Graph <- ggplot() + 
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  geom_line(data = MEXICO_GVA_MANUFACTURING, aes(x = date, y = value, color = "Real Gross Value Added: Manufacturing"), size = 1.25) +
  geom_line(data = MEXICO_GVA_CONSTRUCTION, aes(x = date, y = value, color = "Real Gross Value Added: Construction"), size = 1.25) +
  geom_line(data = MEXICO_GDP, aes(x = date, y = value, color = "Real GDP"), size = 2.25) +
  annotate("text",label = "Pre-COVID Level", x = as.Date("2020-08-01"), y =102, color = "white", size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(65,120), breaks = c(60,70,80,90,100,110,120), expand = c(0,0)) +
  ylab("Index, Q3 2019 = 100") +
  ggtitle("Mexico's Industrial Boom") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Mexican Manufacturing and Construction Output are Booming, Driving Real GDP Growth") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Real GDP","Real Gross Value Added: Manufacturing","Real Gross Value Added: Construction"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 65-(.3*55), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_REAL_GDP_Graph, "Mexico Real GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_TOTAL <- getSerieDataFrame(getSeriesData(series = "SR17471"), "SR17471") %>%
  filter(date >= as.Date("2017-01-01"))
MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_DOMESTIC <- getSerieDataFrame(getSeriesData(series = "SR17472"), "SR17472") %>%
  filter(date >= as.Date("2017-01-01"))
MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_IMPORTED <- getSerieDataFrame(getSeriesData(series = "SR17475"), "SR17475") %>%
  filter(date >= as.Date("2017-01-01"))
  
MEXICO_MACHINERY_EQUIPMENT_Investment_Graph <- ggplot() + 
  geom_line(data = MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_DOMESTIC, aes(x = date, y = value, color = "Real Fixed Investment in Machinery and Equipment: Domestic-Made"), size = 1.25) +
  geom_line(data = MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_IMPORTED, aes(x = date, y = value, color = "Real Fixed Investment in Machinery and Equipment: Imported"), size = 1.25) +
  geom_line(data = MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_TOTAL, aes(x = date, y = value, color = "Total Real Fixed Investment in Machinery and Equipment"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(50,140), breaks = c(50,60,70,80,90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexico's Investment Boom") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Mexican Machinery & Equipment Investments are Up, Particularly From a Rise in Imports") +
  theme_apricitas + theme(legend.position = c(.42,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Total Real Fixed Investment in Machinery and Equipment","Real Fixed Investment in Machinery and Equipment: Domestic-Made","Real Fixed Investment in Machinery and Equipment: Imported"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 50-(.3*80), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_MACHINERY_EQUIPMENT_Investment_Graph, "Mexico Machinery Equipment Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


MEXICO_MACHINERY_EQUIPMENT_CONSTRUCTION_Investment_Graph <- ggplot() + 
  geom_line(data = MEXICO_MACHINERY_EQUIPMENT_INVESTMENT_TOTAL, aes(x = date, y = value, color = "Machinery & Equipment"), size = 1.25) +
  geom_line(data = NONRES_CONSTRUCTION, aes(x = date, y = value, color = "Nonresidential Construction"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(50,150), breaks = c(50,60,70,80,90,100,110,120,130,140,150), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexico's Investment Boom") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Mexican Industrial Investment in Construction, Machinery, & Equipment Have Risen") +
  theme_apricitas + theme(legend.position = c(.42,.85)) +
  scale_color_manual(name= "Index of Real Fixed Investment, Mexico",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Nonresidential Construction","Machinery & Equipment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 50-(.3*100), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_MACHINERY_EQUIPMENT_CONSTRUCTION_Investment_Graph, "Mexico Machinery Equipment Construction Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


REMITTANCE_QUARTERLY_RECEIPTS <- getSerieDataFrame(getSeriesData(series = "SE28528"), "SE28528") %>%
  filter(date >= as.Date("2016-01-01"))

REMITTANCE_QUARTERLY_EXPENDITURES <- getSerieDataFrame(getSeriesData(series = "SE43945"), "SE43945") %>%
  filter(date >= as.Date("2016-01-01"))

NET_REMITTANCE_FLOWS <- merge(REMITTANCE_QUARTERLY_RECEIPTS,REMITTANCE_QUARTERLY_EXPENDITURES, by = "date") %>%
  transmute(date, value=value.x-value.y) %>%
  mutate(rollmean = c(NA,NA,NA,rollmean(value,4))) %>%
  filter(date>=as.Date("2017-01-01"))
  
NET_REMITTANCE_FLOWS_Graph <- ggplot() + 
  geom_line(data = NET_REMITTANCE_FLOWS, aes(x = date, y = (value*4)/1000, color = "Net Remittance Inflows, Mexico"), size = 0.75, linetype = "dashed") +
  geom_line(data = NET_REMITTANCE_FLOWS, aes(x = date, y = (rollmean*4)/1000, color = "Net Remittance Inflows, Mexico"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,75), breaks = c(0,25,50,75), expand = c(0,0)) +
  ylab("Billions of US Dollars") +
  ggtitle("Mexico's Remittance Boom") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Mexican Remittances are Up Amidst Higher Wages, Employment, and Economic Growth in the US") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= "Sold = Rolling 1Yr Total, Dashed = Quarterly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*75), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NET_REMITTANCE_FLOWS_Graph, "Mexico Net Remittance Flows Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

INTERNATIONAL_TRAVELERS_ACCOUNT <- getSerieDataFrame(getSeriesData(series = "SE5828"), "SE5828") %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(rollmean = rollmean(value,12,fill = NA, align = "right")) %>%
  filter(date>=as.Date("2017-01-01"))

INTERNATIONAL_TRAVELERS_ACCOUNT <- ggplot() + 
  geom_line(data = INTERNATIONAL_TRAVELERS_ACCOUNT, aes(x = date, y = (value*12)/1000000, color = "Net Tourism Inflows, Mexico"), size = 0.75, linetype = "dashed") +
  geom_line(data = INTERNATIONAL_TRAVELERS_ACCOUNT, aes(x = date, y = (rollmean*12)/1000000, color = "Net Tourism Inflows, Mexico"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,35), breaks = c(0,10,20,30), expand = c(0,0)) +
  ylab("Billions of US Dollars") +
  ggtitle("Mexico's Tourism Boom") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Mexican Tourist Spending has More Than Recovered From the Effects of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= "Sold = Rolling 1Yr Total, Dashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*35), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INTERNATIONAL_TRAVELERS_ACCOUNT, "International Travelers Account Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

NET_EXP_USA <- getSerieDataFrame(getSeriesData(series = "SE29167"), "SE29167") %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(rollmean = rollmean(value,12,fill = NA, align = "right")) %>%
  filter(date>=as.Date("2017-01-01"))

NET_IMP_CHN <- getSerieDataFrame(getSeriesData(series = "SE29286"), "SE29286") %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(rollmean = rollmean(value,12,fill = NA, align = "right")) %>%
  filter(date>=as.Date("2017-01-01"))

NET_IMP_HKO <- getSerieDataFrame(getSeriesData(series = "SE29291"), "SE29291") %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(rollmean = rollmean(value,12,fill = NA, align = "right")) %>%
  filter(date>=as.Date("2017-01-01"))

NET_IMP_CHN_TOTAL <- merge(NET_IMP_CHN,NET_IMP_HKO, by = "date") %>%
  transmute(date, value = value.x+value.y, rollmean = rollmean.x+rollmean.y)

NET_IMPORTS_EXPORTS_MEXICO <- ggplot() + 
  geom_line(data = NET_EXP_USA, aes(x = date, y = (rollmean*12)/1000000, color = "Mexico, Net Goods Exports to USA"), size = 1.25) +
  geom_line(data = NET_IMP_CHN_TOTAL, aes(x = date, y = (-rollmean*12)/1000000, color = "Mexico, Net Goods Imports from China"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,275), breaks = c(0,50,100,150,200,250), expand = c(0,0)) +
  ylab("Billions of US Dollars") +
  ggtitle("Mexico's Trade Boom is More Than Reexports") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data\nNote: China includes HK. Data uses different origin/destination methods so is not directly equivalent to US Census Bureau data",subtitle = "Growth in Mexico's Net Exports to the US Have Vastly Dwarfed Growth in its Imports from China") +
  theme_apricitas + theme(legend.position = c(.3,.85), plot.title = element_text(size = 25.5)) +
  scale_color_manual(name= "Rolling 1Yr Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*275), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NET_IMPORTS_EXPORTS_MEXICO, "Net Imports Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

REEXP_NOMINAL <- inegi_series(serie = "436909", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  merge(.,PESO_DOLLAR_FX, by = "date") %>%
  transmute(date, values = values/value)

REEXP_NOMINAL_Graph <- ggplot() + 
  geom_line(data = REEXP_NOMINAL, aes(x = date, y = (values*12)/1000000, color = "Exported Products from Imported Inputs\nMexican Export-Oriented Manufacturers (IMMEX Program)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,70), breaks = c(0,20,40,60), expand = c(0,0)) +
  ylab("Dollars, Annual Rate") +
  ggtitle("Mexico's Trade Boom") +
  labs(caption = "Graph created by @JosephPolitano using INEGI and Bank of Mexico data",subtitle = "Mexican Exports of Processed Imports are Rising—Raising Concerns About Tariff Avoidance") +
  theme_apricitas + theme(legend.position = c(.42,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*70), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REEXP_NOMINAL_Graph, "Reexp Nominal Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


VALUE_ADDED_MANUFACTURING <- read.xlsx("https://www.inegi.org.mx/contenidos/programas/pibval/2018/tabulados/ori/VAEMG_18.xlsx")

VALUE_ADDED_MANUFACTURING_EDIT <- VALUE_ADDED_MANUFACTURING %>%
  slice(-1:-5) %>%
  slice(-88:-90) %>%
  t() %>%
  as.data.frame() %>%
  {colnames(.) <- .[1, ]; .[-1, ]} %>%
  mutate(category = rep(c("GVA_DOM", "GVA_GLOB", "SHARE_GVA"), length.out = n())) %>%
  filter(category == "GVA_GLOB") %>%
  mutate(date = seq.Date(from = as.Date("2003-01-01"), by = "year", length.out = n())) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  clean_names() %>%
  mutate(across(where(is.character), as.numeric))

  
VALUE_ADDED_MANUFACTURING_GRAPH <- ggplot() + 
  geom_line(data = VALUE_ADDED_MANUFACTURING_EDIT, aes(x = date, y = x31_33_industrias_manufactureras/100, color = "All Manufactured Exports"), size = 2.25) +
  geom_line(data = VALUE_ADDED_MANUFACTURING_EDIT, aes(x = date, y = x3361_fabricacion_de_automoviles_y_camiones/100, color = "Motor Vehicles"), size = 1.25) +
  geom_line(data = VALUE_ADDED_MANUFACTURING_EDIT, aes(x = date, y = x3363_fabricacion_de_partes_para_vehiculos_automotores/100, color = "Parts for Motor Vehicles"), size = 1.25) +
  geom_line(data = VALUE_ADDED_MANUFACTURING_EDIT, aes(x = date, y = x3352_fabricacion_de_aparatos_electricos_de_uso_domestico/100, color = "Household Appliances"), size = 1.25) +
  geom_line(data = VALUE_ADDED_MANUFACTURING_EDIT, aes(x = date, y = x3341_fabricacion_de_computadoras_y_equipo_periferico/100, color = "Computers and Related Peripheral Equipment"), size = 1.25) +
  geom_line(data = VALUE_ADDED_MANUFACTURING_EDIT, aes(x = date, y = x3344_fabricacion_de_componentes_electronicos/100, color = "Semiconductors and Other Electronic Components"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.65), breaks = c(0,.25,.50,.75), expand = c(0,0)) +
  ylab("Percent of Exports") +
  ggtitle("Mexican Value-Added % in Manufactured Exports") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Domestic Value-Add's Share in Mexico's Manufactured Exports Has Remained Constant Overall") +
  theme_apricitas + theme(legend.position = c(.5,.95), plot.title = element_text(size =24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("All Manufactured Exports","Motor Vehicles","Parts for Motor Vehicles","Household Appliances","Semiconductors and Other Electronic Components","Computers and Related Peripheral Equipment"), guide = guide_legend(ncol = 2, override.aes = list(lwd = c(2.25,1.25, 1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-700-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-700-as.Date("2017-01-01"))), ymin = 0-(.3*.65), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VALUE_ADDED_MANUFACTURING_GRAPH, "Mexico Value-Added Manufacturing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

INDPRO_CARS <- inegi_series(serie = "737102", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

INDPRO_COMP <- inegi_series(serie = "737088", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

INDPRO_CHEM <- inegi_series(serie = "737046", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

INDPRO_METL <- inegi_series(serie = "737067", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

INDPRO_MACH <- inegi_series(serie = "737081", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

INDPRO_ELEC <- inegi_series(serie = "737095", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

INDPRO_TOTL <- inegi_series(serie = "736969", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))
  
INDPRO_SEMI <- inegi_series(serie = "736503", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))
  
MEXICO_IND_PRO_Graph <- ggplot() + 
  geom_line(data = INDPRO_CARS, aes(x = date, y = values, color = "Motor Vehicles and Other Transportation Equipment"), size = 1.25) +
  geom_line(data = INDPRO_COMP, aes(x = date, y = values, color = "Computers and Electronic Products"), size = 1.25) +
  #geom_line(data = INDPRO_CHEM, aes(x = date, y = values, color = "Chemicals"), size = 1.25) +
  #geom_line(data = INDPRO_METL, aes(x = date, y = values, color = "Metal"), size = 1.25) +
  #geom_line(data = INDPRO_MACH, aes(x = date, y = values, color = "Machinery & Equipment"), size = 1.25) +
  geom_line(data = INDPRO_ELEC, aes(x = date, y = values, color = "Electrical Equipment, Appliances, and Components"), size = 1.25) +
  geom_line(data = INDPRO_TOTL, aes(x = date, y = values, color = "Total Manufacturing Industrial Production"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(65,135), breaks = c(50,60,70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexico's Industrial Boom") +
  labs(caption = "Graph created by @JosephPolitano using INGEI data",subtitle = "Mexican Industrial Production is Up, Especially in Computers, Electrical Equipment, and Vehicles") +
  theme_apricitas + theme(legend.position = c(.32,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Total Manufacturing Industrial Production","Motor Vehicles and Other Transportation Equipment","Computers and Electronic Products","Electrical Equipment, Appliances, and Components"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 65-(.3*70), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_IND_PRO_Graph, "Mexico Industrial Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


PRODUCTIVITY_TOTL <- inegi_series(serie = "655630", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

PRODUCTIVITY_CARS <- inegi_series(serie = "655649", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

PRODUCTIVITY_COMP <- inegi_series(serie = "655647", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

PRODUCTIVITY_ELEC <- inegi_series(serie = "655648", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

MEXICO_PRO_DET_Graph <- ggplot() + 
  geom_line(data = PRODUCTIVITY_CARS, aes(x = date, y = values, color = "Motor Vehicles and Other Transportation Equipment"), size = 1.25) +
  geom_line(data = PRODUCTIVITY_COMP, aes(x = date, y = values, color = "Computers and Electronic Products"), size = 1.25) +
  #geom_line(data = INDPRO_CHEM, aes(x = date, y = values, color = "Chemicals"), size = 1.25) +
  #geom_line(data = INDPRO_METL, aes(x = date, y = values, color = "Metal"), size = 1.25) +
  #geom_line(data = INDPRO_MACH, aes(x = date, y = values, color = "Machinery & Equipment"), size = 1.25) +
  geom_line(data = PRODUCTIVITY_ELEC, aes(x = date, y = values, color = "Electrical Equipment, Appliances, and Components"), size = 1.25) +
  geom_line(data = PRODUCTIVITY_TOTL, aes(x = date, y = values, color = "Total Manufacturing Labor Productivity"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(65,165), breaks = c(50,60,70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexico's Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using INGEI data",subtitle = "Mexican Industrial Production is Up, Especially in Computers, Electrical Equipment, and Vehicles") +
  theme_apricitas + theme(legend.position = c(.32,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Total Manufacturing Labor Productivity","Motor Vehicles and Other Transportation Equipment","Computers and Electronic Products","Electrical Equipment, Appliances, and Components"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 65-(.3*70), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_PRO_DET_Graph, "Mexico Productivity Detailed Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


MEXICO_MANUFACTURING_PRODUCTIVITY <- inegi_series(serie = "793182", token = Sys.getenv("TOKEN_INEGI"))

MEXICO_MANUFACTURING_PRODUCTIVITY_Graph <- ggplot() + 
  geom_line(data = MEXICO_MANUFACTURING_PRODUCTIVITY, aes(x = date, y = values, color = "Mexican Manufacturing Labor Productivity\n(Output Per Hour Worked)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(85,125), breaks = c(50,60,70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexican Manufacturing Productivity, 2005-2023") +
  labs(caption = "Graph created by @JosephPolitano using INGEI data",subtitle = "Mexican Manufacturing Productivity Growth Has Been Weak For Decades") +
  theme_apricitas + theme(legend.position = c(.42,.90), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 85-(.3*40), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEXICO_MANUFACTURING_PRODUCTIVITY_Graph, "Mexican Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


ALL_MANUFACTURING_EMPLOYEES_MEX <- inegi_series(serie = "702335", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

IMMEX_MANUFACTURING_EMPLOYEES_MEX <- inegi_series(serie = "454534", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  mutate(year = lubridate::year(date), values = values / mean(values[lubridate::year(date) == 2018]) * 100)

ALL_MANUFACTURING_EMPLOYEES_MEX_Graph <- ggplot() + 
  geom_line(data = IMMEX_MANUFACTURING_EMPLOYEES_MEX, aes(x = date, y = values, color = "Export-Oriented Manufacturing (IMMEX Program)"), size = 1.25) +
  geom_line(data = ALL_MANUFACTURING_EMPLOYEES_MEX, aes(x = date, y = values, color = "All Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(90,115), breaks = c(90,95,100,105,110,115), expand = c(0,0)) +
  ylab("Index, 2018 = 100") +
  ggtitle("Mexican Manufacturing Employment") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Mexican Manufacturing Employment Has Been Much Stronger in Export-Oriented Firms") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Employment Index, 2018 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("All Manufacturing","Export-Oriented Manufacturing (IMMEX Program)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 90-(.3*25), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_MANUFACTURING_EMPLOYEES_MEX_Graph, "All Manufacturing Employees Mex Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

IMMEX_MANUFACTURING_EMPLOYEES_MEX_RAW <- inegi_series(serie = "454534", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  arrange(date)

XIMMEX_MANUFACTURING_EMPLOYEES_MEX_RAW <- inegi_series(series_id = "702335", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01")) %>%
  mutate(values = values*4768216.75/100) %>%
  merge(.,IMMEX_MANUFACTURING_EMPLOYEES_MEX_RAW, by = "date") %>%
  transmute(date, values = values.x-values.y)

ALL_MANUFACTURING_EMPLOYEES_MEX_RAW_Graph <- ggplot() + 
  geom_line(data = XIMMEX_MANUFACTURING_EMPLOYEES_MEX_RAW, aes(x = date, y = values/1000-values[1]/1000, color = "All Other Manufacturing"), size = 1.25) +
  geom_line(data = IMMEX_MANUFACTURING_EMPLOYEES_MEX_RAW, aes(x = date, y = values/1000-values[1]/1000, color = "Export-Oriented Manufacturing (IMMEX Program)"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(-200,500), breaks = c(-200,-100,0,100,200,300,400,500), expand = c(0,0)) +
  ylab("Change Since Jan 2017") +
  ggtitle("Mexican Manufacturing Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Mexican Manufacturing Employment Has Been Much Stronger in Export-Oriented Firms") +
  theme_apricitas + theme(legend.position = c(.33,.88)) +
  scale_color_manual(name= "Formal Manufacturing Job Growth Since Jan 2017",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Export-Oriented Manufacturing (IMMEX Program)","All Other Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -200-(.3*700), ymax = -200) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_MANUFACTURING_EMPLOYEES_MEX_RAW_Graph, "All Manufacturing Employees Mex Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


CONSUMER_SENTIMENT <- inegi_series(serie = "454186", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

HOUSEHOLD_BETTER_LAST_12M <- inegi_series(serie = "454193", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

PURCHASES <- inegi_series(serie = "454221", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))

COUNTRY_BETTER_LAST_12M <- inegi_series(serie = "454207", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2017-01-01"))


CONSUMER_SENTIMENT_Graph <- ggplot() + 
  geom_line(data = CONSUMER_SENTIMENT, aes(x = date, y = values, color = "Consumer Confidence Index"), size = 2.25) +
  geom_line(data = HOUSEHOLD_BETTER_LAST_12M, aes(x = date, y = values, color = "My Household is Better Off Than 12M Ago"), size = 1.25) +
  geom_line(data = COUNTRY_BETTER_LAST_12M, aes(x = date, y = values, color = "The Country is Better Off Than 12M Ago"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(20,60), breaks = c(20,30,40,50,60), expand = c(0,0)) +
  ylab("Balance, Above 50 is Positive") +
  ggtitle("Mexican Consumer Confidence") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "Mexican Households Are Increasingly Optimistic About the Direction of Their Country & Economy") +
  theme_apricitas + theme(legend.position = c(.6,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Consumer Confidence Index","My Household is Better Off Than 12M Ago","The Country is Better Off Than 12M Ago"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-02-01")-(.1861*(today()-as.Date("2017-02-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 20-(.3*40), ymax = 20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSUMER_SENTIMENT_Graph, "Consumer Sentiment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

GVA_AUTOS  <- inegi_series(serie = "733786", token = Sys.getenv("TOKEN_INEGI"))

GVA_TRAILERS <- inegi_series(serie = "733787", token = Sys.getenv("TOKEN_INEGI"))

GVA_AUTO_PARTS <- inegi_series(serie = "733788", token = Sys.getenv("TOKEN_INEGI"))

REAL_AUTO_VALUE_ADD_Graph <- ggplot() + 
  geom_line(data = GVA_AUTO_PARTS, aes(x = date, y = values/1000, color = "Motor Vehicle Parts Manufacturing"), size = 1.25) +
  geom_line(data = GVA_AUTOS, aes(x = date, y = values/1000, color = "Motor Vehicle Manufacturing"), size = 1.25) +
  #geom_line(data = GVA_TRAILERS, aes(x = date, y = values/1000000, color = "My Household is Better Off Than 12M Ago"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix ="B"), limits = c(0,750), breaks = c(0,250,500,750), expand = c(0,0)) +
  ylab("Billions of 2018 Mexican Pesos") +
  ggtitle("Mexican Auto Output is at Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Mexico data",subtitle = "The Mexican Auto Industry has Been Growing Steadily Over the Last Decade") +
  theme_apricitas + theme(legend.position = c(.4,.72)) +
  scale_color_manual(name= "Real Value Added, Mexico (NSA)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Motor Vehicle Manufacturing","Motor Vehicle Parts Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 0-(.3*750), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_AUTO_VALUE_ADD_Graph, "Real Auto Value Add Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

flash_zip_url <- "https://en.www.inegi.org.mx/contenidos/programas/pibo/2018/datosabiertos/conjunto_de_datos_eopibt_trimestral_csv.zip"
flash_zip_file <- "eopibt_trimestral_csv.zip"
flash_extract_dir <- "inegi_data"

# Download and unzip the file
download.file(flash_zip_url, flash_zip_file, mode = "wb")
unzip(flash_zip_file, exdir = flash_extract_dir)

# List files in extracted folder
flash_data_dir <- file.path(flash_extract_dir, "conjunto_de_datos")
flash_files <- list.files(flash_data_dir, full.names = TRUE)

# Read a specific CSV (adjust the filename as needed)
flash_gdp_annual <- read_csv(flash_files[3])  # Change index to match the desired file

flash_gdp_annual <- as.data.frame(flash_gdp_annual) %>%
  t() %>%
  as.data.frame() %>%
  .[-1, , drop = FALSE] %>%
  setNames(c("total","primary","secondary","tertiary","seasonally")) %>%
  transmute(total = as.numeric(total),date = seq.Date(from = as.Date("2015-01-01"), by = "3 months", length = nrow(.)))

FLASH_GDP_ANNUAL_Graph <- ggplot() + 
  geom_line(data = filter(flash_gdp_annual, date >= as.Date("2022-01-01")), aes(x = date, y = total/100, color = "Mexican Real GDP Growth, Year-on-Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05), breaks = c(-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06), expand = c(0,0)) +
  ylab("Year-on-Year Growth") +
  ggtitle("Mexico's Year-on-Year GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using INGEI data",subtitle = "Mexican GDP Growth is Decelerating") +
  theme_apricitas + theme(legend.position = c(.62,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FLASH_GDP_ANNUAL_Graph, "Mexico Flash GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


temp_file <- tempfile(fileext = ".xlsx")
GET("https://www.inegi.org.mx/contenidos/programas/itaee/2018/tabulados/ori/ITAEE_10.xlsx", write_disk(temp_file, overwrite = TRUE))

STATE_CONSTRUCTION_ACTIVITY <- read.xlsx(temp_file)

STATE_CONSTRUCTION_ACTIVITY_EDIT <- STATE_CONSTRUCTION_ACTIVITY %>%
  slice(-1:-6,-39:-109) %>%
  t() %>%
  as.data.frame() %>%
  {colnames(.) <- .[1, ]; .[-1, ]} %>%
  mutate(category = rep(c("Q1", "Q2", "Q3", "Q4", "6M", "9M", "Annual"), length.out = n())) %>%
  #CHANGE THIS TO MAKE DIFFERENT VERSIONS WHEN DATA IS UPDATED
  subset(category == "6M") %>%
  slice(n()) %>%
  select(-category) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "states") %>%
  setNames(c("states","value")) %>%
  mutate(value = (as.numeric(value)-100)/100) %>%
  mutate(region = sprintf("%02d", 1:32))


devtools::install_github("diegovalle/mxmaps")
library("mxmaps")

MX_CONSTRUCTION_MAP <- mxstate_choropleth(STATE_CONSTRUCTION_ACTIVITY_EDIT,
                   title = "Construction Activity Growth, 2018-2024",
                   num_colors = 1) +
  theme_apricitas +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Construction is Booming in the Mexican South Thanks to Major Public Works Projects") +
  scale_fill_viridis_c("Percent Change", limits = c(-0.5,2.60), breaks = c(-0.5,0,.5,1,1.5,2,2.5), labels = c("-50%","0%","50%","100%","150%","200%","250%")) +
  annotate(geom = "text", label = "Tren Maya", x = -88, y = 16, color ="white",size = 5, fontface = "bold") + 
  annotate(geom = "segment", x = -88, xend = -88, y = 16.5, yend = 19, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -88, xend = -89, y = 16.5, yend = 20.5, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -88, xend = -91, y = 16.5, yend = 19, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -88, xend = -92, y = 16.5, yend = 17, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -88, xend = -91.5, y = 16.5, yend = 17.75, color = "white", lwd = 1.25) +
  annotate(geom = "text", label = "Dos Bocas\nRefinery", x = -93.5, y = 20.5, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm")) +
  annotate(geom = "segment", x = -93, xend = -93.5, y = 18.25, yend = 19.5, color = "white", lwd = 1.25) +
  annotate(geom = "text", label = "Tehuantepec Interoceanic Corridor", x = -101, y = 14.5, color ="white",size = 5, fontface = "bold",lineheight = unit(0.85,"cm")) +
  annotate(geom = "segment", x = -99, xend = -96, y = 15, yend = 18.75, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -99, xend = -96, y = 15, yend = 17, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -99, xend = -93, y = 15, yend = 16, color = "white", lwd = 1.25) +
  annotate(geom = "segment", x = -99, xend = -92.5, y = 15, yend = 17.75, color = "white", lwd = 1.25) +
  theme(legend.position = c(.85,.7),axis.title.y = element_blank() ,axis.title.x = element_blank(), panel.grid.major=element_blank(),panel.grid.minor = element_blank(),  axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, -.75), "in"), legend.key = element_blank())

ggsave(dpi = "retina",plot = MX_CONSTRUCTION_MAP, "MX Construction Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


temp_file <- tempfile(fileext = ".xlsx")
GET("https://www.inegi.org.mx/contenidos/programas/itaee/2018/tabulados/ori/ITAEE_11.xlsx", write_disk(temp_file, overwrite = TRUE))

STATE_MANUFACTURING_ACTIVITY <- read.xlsx(temp_file)

STATE_MANUFACTURING_ACTIVITY_EDIT <- STATE_MANUFACTURING_ACTIVITY %>%
  slice(-1:-6,-39:-109) %>%
  t() %>%
  as.data.frame() %>%
  {colnames(.) <- .[1, ]; .[-1, ]} %>%
  mutate(category = rep(c("Q1", "Q2", "Q3", "Q4", "6M", "9M", "Annual"), length.out = n())) %>%
  #CHANGE THIS TO MAKE DIFFERENT VERSIONS WHEN DATA IS UPDATED
  subset(category == "6M") %>%
  slice(n()) %>%
  select(-category) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "states") %>%
  setNames(c("states","value")) %>%
  mutate(value = (as.numeric(value)-100)/100) %>%
  mutate(region = sprintf("%02d", 1:32))

MX_MANUFACTURING_MAP <- mxstate_choropleth(STATE_MANUFACTURING_ACTIVITY_EDIT,
                                          title = "Manufacturing Activity Growth, 2018-2024",
                                          num_colors = 1) +
  theme_apricitas +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Manufacturing Output Has Risen Significantly In Mexico's Major Northern Export States") +
  scale_fill_viridis_c("Percent Change\nNote: Data Thru Q2 2024", limits = c(-0.16,.26), breaks = c(-.10,0,.10,.20), labels = c("-10%","0%","10%","20%")) +
  annotate(geom = "text", 
           label = paste0("Chihuahua: ", 
                          ifelse(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                   filter(region == "08") %>% 
                                   pull(value) %>% 
                                   first() * 100 > 0, "+", ""), 
                          round(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                  filter(region == "08") %>% 
                                  pull(value) %>% 
                                  first() * 100), "%"), 
           x = -103.5, y = 33, color = "white", size = 5, fontface = "bold") +
  #annotate(geom = "text", label = "Chihuahua: +20%", x = -106, y = 33, color ="white",size = 5, fontface = "bold") + 
  annotate(geom = "segment", x = -106, xend = -107.5, y = 30.5, yend = 32.5, color = "white", lwd = 1.25) +
  annotate(geom = "text", 
           label = paste0("Baja California: ", 
                          ifelse(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                   filter(region == "02") %>% 
                                   pull(value) %>% 
                                   first() * 100 > 0, "+", ""), 
                          round(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                  filter(region == "02") %>% 
                                  pull(value) %>% 
                                  first() * 100), "%"), 
           x = -112, y = 35, color = "white", size = 5, fontface = "bold") +
  annotate(geom = "segment", x = -116, xend = -116.5, y = 32, yend = 34.5, color = "white", lwd = 1.25) +
  annotate(geom = "text", 
           label = paste0("Sonora: ", 
                          ifelse(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                   filter(region == "26") %>% 
                                   pull(value) %>% 
                                   first() * 100 > 0, "+", ""), 
                          round(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                  filter(region == "26") %>% 
                                  pull(value) %>% 
                                  first() * 100), "%"), 
           x = -109, y = 34, color = "white", size = 5, fontface = "bold") +
  annotate(geom = "segment", x = -111, xend = -112, y = 30.5, yend = 33.5, color = "white", lwd = 1.25) +
  annotate(geom = "text", 
           label = paste0("Coahuila: ", 
                          ifelse(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                   filter(region == "05") %>% 
                                   pull(value) %>% 
                                   first() * 100 > 0, "+", ""), 
                          round(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                  filter(region == "05") %>% 
                                  pull(value) %>% 
                                  first() * 100), "%"), 
           x = -102, y = 32, color = "white", size = 5, fontface = "bold") +
  #annotate(geom = "text", label = "Coahuila: -0%", x = -102, y = 32, color ="white",size = 5, fontface = "bold") + 
  annotate(geom = "segment", x = -102.5, xend = -104.5, y = 28.5, yend = 31.5, color = "white", lwd = 1.25) +
  annotate(geom = "text", 
           label = paste0("Nuevo León: ", 
                          ifelse(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                   filter(region == "19") %>% 
                                   pull(value) %>% 
                                   first() * 100 > 0, "+", ""), 
                          round(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                  filter(region == "19") %>% 
                                  pull(value) %>% 
                                  first() * 100), "%"), 
           x = -98, y = 31, color = "white", size = 5, fontface = "bold") +
  #annotate(geom = "text", label = "Nuevo León: +10%", x = -99, y = 31, color ="white",size = 5, fontface = "bold") + 
  annotate(geom = "segment", x = -100, xend = -102, y = 26, yend = 30.5, color = "white", lwd = 1.25) +
  annotate(geom = "text", 
           label = paste0("Tamaulipas: ", 
                          ifelse(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                   filter(region == "28") %>% 
                                   pull(value) %>% 
                                   first() * 100 > 0, "+", ""), 
                          round(STATE_MANUFACTURING_ACTIVITY_EDIT %>% 
                                  filter(region == "28") %>% 
                                  pull(value) %>% 
                                  first() * 100), "%"), 
           x = -96, y = 30, color = "white", size = 5, fontface = "bold") +
  #annotate(geom = "text", label = "Nuevo León: +10%", x = -99, y = 31, color ="white",size = 5, fontface = "bold") + 
  annotate(geom = "segment", x = -98, xend = -99, y = 25, yend = 29.5, color = "white", lwd = 1.25) +
  theme(legend.position = c(1,.8),axis.title.y = element_blank() ,axis.title.x = element_blank(), panel.grid.major=element_blank(),panel.grid.minor = element_blank(),  axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, -.75), "in"), legend.key = element_blank())


ggsave(dpi = "retina",plot = MX_MANUFACTURING_MAP, "MX Manufacturing Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

temp_file <- tempfile(fileext = ".xlsx")
GET("https://en.www.inegi.org.mx/contenidos/programas/exporta_ef/tabulados/EAEF_Entidad.xlsx", write_disk(temp_file, overwrite = TRUE))

STATE_EXPORTS <- read.xlsx(temp_file)

STATE_EXPORTS_EDIT <- STATE_EXPORTS %>%
  slice(-1:-4,-37:-109) %>%
  as.data.frame() %>%
  select(1, last_col()) %>%
  setNames(c("states","value")) %>%
  mutate(region = sprintf("%02d", 1:32)) %>%
  mutate(value = as.numeric(value))

temp_file <- tempfile(fileext = ".xlsx")
GET("https://en.www.inegi.org.mx/contenidos/programas/pibent/2018/tabulados/ori/PIBE_2.xlsx", write_disk(temp_file, overwrite = TRUE))

STATE_GDP <- read.xlsx(temp_file)

STATE_GDP_EDIT <- STATE_GDP %>%
  slice(182:213) %>%
  as.data.frame() %>%
  select(1, last_col()) %>%
  setNames(c("states","value")) %>%
  mutate(region = sprintf("%02d", 1:32)) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(value = value/(fredr("DEXMXUS", frequency = "a")$value[nrow(fredr("DEXMXUS", frequency = "a"))-1]))

STATE_EXPORTS_SHARE_GDP <- merge(STATE_EXPORTS_EDIT,STATE_GDP_EDIT, by = "region") %>%
  transmute(region, states = states.x, value = value.x/(value.y*1000))

devtools::install_github("diegovalle/mxmaps")
library("mxmaps")

MX_STATE_EXPORTS_SHARE_GDP_MAP <- mxstate_choropleth(STATE_EXPORTS_SHARE_GDP,
                                                     title = "Gross Exports Relative to GDP, 2023",
                                                     num_colors = 1) +
  theme_apricitas +
  labs(caption = "Graph created by @JosephPolitano using INEGI data",subtitle = "Mexico's Northern Border States are Export Powerhouses Vulnerable to a Trade War") +
  scale_fill_gradientn(colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(0,.25,.5,.75,1), expand = c(0,0)) +
  theme(legend.position = c(.85,.7),axis.title.y = element_blank() ,axis.title.x = element_blank(), panel.grid.major=element_blank(),panel.grid.minor = element_blank(),  axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, -.75), "in"), legend.key = element_blank())

ggsave(dpi = "retina",plot = MX_STATE_EXPORTS_SHARE_GDP_MAP, "MX State Export Share GDP Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
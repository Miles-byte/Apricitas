pacman::p_load(maps,janitor,rsdmx,bea.R,cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

devtools::install_github("jameelalsalam/eia2")
library("eia2")

install_github("keberwein/blscrapeR")
library(blscrapeR)


US_ADP_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "847150", #Digital Processing Units
  #E_COMMODITY = "847180", #Automated Data Processing Units
  E_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
  CTY_CODE = "-", #All Countries
) 

US_ADP_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "847150", #Digital Processing Units
  #I_COMMODITY = "847180", #Automated Data Processing Units
  I_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #I_COMMODITY = "854231", #logic chips
  #I_COMMODITY = "854232", #memory chips
  CTY_CODE = "-", #Total
)

US_ADP_IMPORTS <- US_ADP_IMPORTS %>%
  select(-I_COMMODITY_1) %>%
  pivot_wider(names_from = I_COMMODITY, values_from = GEN_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  select(-MONTH,-YEAR,-CTY_CODE,-CTY_NAME,-CTY_CODE_1) %>%
  mutate(across(where(is.character),as.numeric))
  
US_ADP_EXPORTS <- US_ADP_EXPORTS %>%
  select(-E_COMMODITY_1) %>%
  pivot_wider(names_from = E_COMMODITY, values_from = ALL_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  select(-MONTH,-YEAR,-CTY_CODE,-CTY_CODE_1) %>%
  mutate(across(where(is.character),as.numeric))

US_ADP_NET_IMPORTS <- merge(US_ADP_IMPORTS,US_ADP_EXPORTS, by = "time") %>%
  transmute(time, `DPU`=`847150.x`-`847150.y`,ADP_PARTS = `847330.x`-`847330.y`) %>%
  mutate(roll_DPU = c(rep(NA,11),rollsum(DPU, 12)), roll_ADP_PARTS = c(rep(NA,11),rollsum(ADP_PARTS, 12)))

US_COMPUTER_NET_IMPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(US_ADP_NET_IMPORTS, time>= as.Date("2014-01-01")), aes(x=time,y= DPU*12/1000000000,color= "Large Computers (excl. Laptops)"), size = 0.75, linetype = "dashed", alpha = 0.5) + 
  geom_line(data=filter(US_ADP_NET_IMPORTS, time>= as.Date("2014-01-01")), aes(x=time,y= ADP_PARTS*12/1000000000,color= "Computer Parts & Accessories"), size = 0.75, linetype = "dashed", alpha = 0.5) + 
  geom_line(data=filter(US_ADP_NET_IMPORTS, time>= as.Date("2014-01-01")), aes(x=time,y= roll_DPU/1000000000,color= "Large Computers (excl. Laptops)"), size = 1.25) + 
  geom_line(data=filter(US_ADP_NET_IMPORTS, time>= as.Date("2014-01-01")), aes(x=time,y= roll_ADP_PARTS/1000000000,color= "Computer Parts & Accessories"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-5,ceiling(max(US_ADP_NET_IMPORTS$DPU)*12/5000000000)*5), breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Computer Net Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data. Large Computers are HS Code 847150, Parts & Accessories are HS Code 847330",subtitle = "US Imports of Large Computers & Parts/Accessories Has Been Skyrocketing") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= "Solid = Rolling 12M Total, Dashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Large Computers (excl. Laptops)","Computer Parts & Accessories")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = -5-(.3*(ceiling(max(US_ADP_NET_IMPORTS$DPU)*12/5000000000)*5+5)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_COMPUTER_NET_IMPORTS_Graph, "US Computer Net Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_COMPUTER_NET_IMPORTS_Monthly_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(US_ADP_NET_IMPORTS, time>= as.Date("2014-01-01")), aes(x=time,y= DPU*12/1000000000,color= "Large Computers (excl. Laptops)"), size = 1.25) + 
  geom_line(data=filter(US_ADP_NET_IMPORTS, time>= as.Date("2014-01-01")), aes(x=time,y= ADP_PARTS*12/1000000000,color= "Computer Parts & Accessories"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-5,ceiling(max(US_ADP_NET_IMPORTS$DPU)*12/5000000000)*5), breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Computer Net Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data. Large Computers are HS Code 847150, Parts & Accessories are HS Code 847330",subtitle = "US Imports of Large Computers & Parts/Accessories Has Been Skyrocketing") +
  theme_apricitas + theme(legend.position = c(.25,.89)) +
  scale_color_manual(name= "Imports, Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Large Computers (excl. Laptops)","Computer Parts & Accessories")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = -5-(.3*(ceiling(max(US_ADP_NET_IMPORTS$DPU)*12/5000000000)*5+5)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_COMPUTER_NET_IMPORTS_Monthly_Graph, "US Computer Net Imports Monthly.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


DATA_CENTER_CONSTRUCTION <- read.xlsx("https://www.census.gov/construction/c30/xlsx/privsatime.xlsx") %>%
  drop_na() %>%
  row_to_names(1) %>%
  select(1,`Data center`) %>%
  mutate(`Data center` = as.numeric(`Data center`)) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = seq.Date(from = as.Date("2014-01-01"), by = "month", length.out = nrow(.)))

OFFICE_CONSTRUCTION <- read.xlsx("https://www.census.gov/construction/c30/xlsx/privsatime.xlsx") %>%
  drop_na() %>%
  row_to_names(1) %>%
  select(1,`General`) %>%
  mutate(`General` = as.numeric(`General`)) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = seq.Date(from = as.Date("2014-01-01"), by = "month", length.out = nrow(.)))


DATA_CENTER_CONSTRUCTION_Graph <- ggplot() + #plotting net tightening data
  annotate(geom = "segment", x = as.Date("2022-11-01"), xend = as.Date("2022-11-01"), y = 0, yend = 20, color = "white",linetype = "dashed", size = 1) +
  annotate("text", label = "ChatGPT\nLaunch", x = as.Date("2022-09-01"), y = 17.5, color = "white", size = 4, hjust = 1, lineheight = 0.8) +
  geom_line(data=DATA_CENTER_CONSTRUCTION, aes(x=date,y= `Data center`/1000,color= "US Data Center Construction Spending,\nSeasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  ylab("Spending, Billions") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(10,20,30,40,50,60,70), limits = c(0,ceiling(max(DATA_CENTER_CONSTRUCTION$`Data center`)/5000)*5), expand = c(0,0)) +
  ggtitle("Data Center Construction at Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "Data Center Construction Spending is Skyrocketing Amidst the AI Boom") +
  theme_apricitas + theme(legend.position = c(.52,.92), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(ceiling(max(DATA_CENTER_CONSTRUCTION$`Data center`)/5000)*5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DATA_CENTER_CONSTRUCTION_Graph, "DATA CENTER CONSTRUCTION GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

OFFICE_CONSTRUCTION <- read.xlsx("https://www.census.gov/construction/c30/xlsx/privsatime.xlsx") %>%
  row_to_names(2) %>%
  select(1,`General`,`Financial`) %>%
  mutate(`General` = as.numeric(`General`) + as.numeric(`Financial`)) %>%
  .[order(nrow(.):1),] %>%
  drop_na() %>%
  mutate(date = seq.Date(from = as.Date("1993-01-01"), by = "month", length.out = nrow(.)))

DATA_CENTER_VS_OFFICE_CONSTRUCTION_Graph <- ggplot() + #plotting net tightening data
  annotate(geom = "segment", x = as.Date("2022-11-01"), xend = as.Date("2022-11-01"), y = 0, yend = 20, color = "white",linetype = "dashed", size = 1) +
  annotate("text", label = "ChatGPT\nLaunch", x = as.Date("2022-09-01"), y = 17.5, color = "white", size = 4, hjust = 1, lineheight = 0.8) +
  geom_line(data=DATA_CENTER_CONSTRUCTION, aes(x=date,y= `Data center`/1000,color= "US Data Center Construction Spending"), size = 1.25) + 
  geom_line(data=filter(OFFICE_CONSTRUCTION, date >= as.Date("2014-01-01")), aes(x=date,y= `General`/1000,color= "US Office Construction Spending"), size = 1.25) + 
  xlab("Date") +
  ylab("Spending, Billions") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(20,40,60,80), limits = c(0,ceiling(max(OFFICE_CONSTRUCTION$`General`)/5000)*5), expand = c(0,0)) +
  ggtitle("Data Center vs Office Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census Construction Spending data", subtitle = "Data Center Construction Spending is Skyrocketing Amidst the AI Boom") +
  theme_apricitas + theme(legend.position = c(.50,.50), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(ceiling(max(OFFICE_CONSTRUCTION$`General`)/5000)*5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DATA_CENTER_VS_OFFICE_CONSTRUCTION_Graph, "DATA CENTER VS OFFICE CONSTRUCTION GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

OFFICE_PRICES <- bls_api("PCU236223236223", startyear = 2006, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(paste0(year,"-",gsub("M","",period),"-01"))) %>%
  arrange(date)

REAL_OFFICE_CONSTRUCTION <- merge(OFFICE_CONSTRUCTION,OFFICE_PRICES, by = "date") %>%
  mutate(value = value/value[128]) %>%
  transmute(date, value = General/value)

REAL_OFFICE_CONSTRUCTION_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=REAL_OFFICE_CONSTRUCTION, aes(x=date,y= value/1000,color= "US Real Office Construction,\nSeasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  ylab("Spending, Billions of Jan 2017 Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,20,40,60,80), limits = c(0,80), expand = c(0,0)) +
  ggtitle("US Office Construction Continues Falling") +
  labs(caption = "Graph created by @JosephPolitano using Census Construction data & BLS PPI Price Data", subtitle = "Office Construction is at the Lowest Level in a Decade Amidst Remote Work & High Interest Rates") +
  theme_apricitas + theme(legend.position = c(.35,.92), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*(80)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_OFFICE_CONSTRUCTION_Graph, "REAL OFFICE CONSTRUCTION GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



QFR_Data <- getCensus(
  name = "timeseries/eits/qfr",
  #region = "CA",
  vars = c("cell_value","category_code","data_type_code","seasonally_adj", "time_slot_id"),
  time = paste("from 2000 to", format(Sys.Date(), "%Y")),
  data_type_code = 219, #Net Property, Plant, and Equipment
  #category_code = 511, #Publishing ex Internet
  #category_code = 519 #Other Information
) %>%
  filter(category_code %in% c("INF",513,516,519)) %>%
  transmute(name = category_code, date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(cell_value)) %>%
  pivot_wider() %>%
  transmute(date, Info = INF, Publish_Info = as.numeric(`513`), Other_Info = as.numeric(`519`), Broadcast_Info = as.numeric(`516`)) %>%
  arrange(date) %>%
  mutate(Publish_Info_Growth = (Publish_Info-lag(Publish_Info,4))/lag(Publish_Info,4), Broadcast_Info_Growth = (Broadcast_Info-lag(Broadcast_Info,4))/lag(Broadcast_Info,4), Other_Info_Growth = (Other_Info-lag(Other_Info,4))/lag(Other_Info,4), Info_Growth = (Info-lag(Info,4))/lag(Info,4), ) %>%
  mutate(Publish_Info_Increase = Publish_Info-lag(Publish_Info,4), Other_Info_Increase = Other_Info-lag(Other_Info,4), Info_Increase = Info-lag(Info,4), Broadcast_Info_Increase = Broadcast_Info-lag(Broadcast_Info,4))


QFR_Data_Total_Increase_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(QFR_Data, date >= as.Date("2015-01-01")), aes(x=date,y= (Publish_Info_Increase+Other_Info_Increase+Broadcast_Info_Increase)/1000, color= "Increase in Net Property, Plant, & Equipment, Year-on-Year\nInformation Technology Sector"), size = 1.25) +
  annotate("text",label = "NOTE: Information Technology Sector Includes Software Publishers,\nComputing Infrastructure,Data Processing, Web Hosting,\nWeb Search Portals, Social Media, and Streaming Services", hjust = 0, x = as.Date("2018-09-01"), y =14, color = "white", size = 4, alpha = 0.5, lineheight = 0.8) +
  xlab("Date") +
  ylab("Dollar Growth, Year-on-year") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,20,40,60,80,100,120,140,160,180,200), limits = c(0,200), expand = c(0,0)) +
  ggtitle("Information Tech Sector Investment") +
  labs(caption = "Graph created by @JosephPolitano using Census QFR data", subtitle = "Information Technology Sector Physical Investment Has Boomed Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.40,.85), legend.key.height = unit(1,"cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*180), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QFR_Data_Total_Increase_graph, "QFR Invest Total Increase Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

VA_COM_ELEC <- eia1_series("ELEC.SALES.VA-COM.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = sales) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(value,12))) %>%
  mutate(value = (value*12)/rollsum[228]*100, rollsum = rollsum/rollsum[228]*100)

TX_COM_ELEC <- eia1_series("ELEC.SALES.TX-COM.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = sales) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(value,12))) %>%
  mutate(value = (value*12)/rollsum[228]*100, rollsum = rollsum/rollsum[228]*100)

ND_COM_ELEC <- eia1_series("ELEC.SALES.ND-COM.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = sales) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(value,12))) %>%
  mutate(value = (value*12)/rollsum[228]*100, rollsum = rollsum/rollsum[228]*100)

OR_COM_ELEC <- eia1_series("ELEC.SALES.OR-COM.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = sales) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(value,12))) %>%
  mutate(value = (value*12)/rollsum[228]*100, rollsum = rollsum/rollsum[228]*100)

US_COM_ELEC <- eia1_series("ELEC.SALES.US-COM.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), category = "Solar", value = sales) %>%
  arrange(date) %>%
  mutate(rollsum = c(0,0,0,0,0,0,0,0,0,0,0,rollsum(value,12))) %>%
  mutate(value = (value*12)/rollsum[228]*100, rollsum = rollsum/rollsum[228]*100)

COM_ELEC_Graph <- ggplot() + #plotting permanent and temporary job losers
  #geom_line(data= filter(US_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= value, color= "US"), size = 0.75, alpha = 0.75, linetype = "dashed") +
  #geom_line(data= filter(VA_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= value, color= "Virginia"), size = 0.75, alpha = 0.75, linetype = "dashed") +
  geom_line(data= filter(OR_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= rollsum, color= "Oregon"), size = 1.25) +
  geom_line(data= filter(VA_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= rollsum, color= "Virginia"), size = 1.25) +
  geom_line(data= filter(TX_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= rollsum, color= "Texas"), size = 1.25) +
  geom_line(data= filter(ND_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= rollsum, color= "North Dakota"), size = 1.25) +
  geom_line(data= filter(US_COM_ELEC, date >= as.Date("2015-01-01")), aes(x=date,y= rollsum, color= "US"), size = 2.25) +
  xlab("Date") +
  ylab("Sales to Commerical Power Users, 2019 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130,140,150,160,170,180,190,200), limits = c(80,175), expand = c(0,0)) +
  ggtitle("Electricity Sales to Commercial Users\n(Including Data Centers & Crypto Mining)") +
  labs(caption = "Graph created by @JosephPolitano using EIA Electricity data", subtitle = "Data Center and Miner Power Demand Has Mattered in Key Markets Like ND, OR, VA, & TX") +
  theme_apricitas + theme(legend.position = c(.25,.7)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Electricity Sales to Commercial Users\nRolling 12M Total, Indexed, 2019 = 100",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("US","North Dakota","Oregon","Virginia","Texas"), guide=guide_legend(override.aes=list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 80-(.3*95), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COM_ELEC_Graph, "Com Elec Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_ADP_IMPORTS_COUNTRY <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "847150", #Digital Processing Units
  #I_COMMODITY = "847180", #Automated Data Processing Units
  #I_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #I_COMMODITY = "854231", #logic chips
  #I_COMMODITY = "854232", #memory chips
  #CTY_CODE = "-", #Total
)

US_ADP_IMPORTS_COUNTRY_ROLLED <- US_ADP_IMPORTS_COUNTRY %>%
  select(-I_COMMODITY_1,-CTY_CODE) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = GEN_VAL_MO) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ rollsum(., 12, fill = NA, align = "right"), .names = "{.col}_rollsum"))

US_ADP_IMPORTS_COUNTRY_ROLLED_PIVOT <- US_ADP_IMPORTS_COUNTRY_ROLLED %>%
  filter(time == max(time)) %>%
  select(-MONTH,-YEAR,-I_COMMODITY) %>%
  pivot_longer(-time)
  
US_ADP_EXPORTS_COUNTRY <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "847150", #Digital Processing Units
  #E_COMMODITY = "847180", #Automated Data Processing Units
  #E_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
) 

US_ADP_EXPORTS_COUNTRY_ROLLED <- US_ADP_EXPORTS_COUNTRY %>%
  select(-E_COMMODITY_1,-CTY_CODE) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = ALL_VAL_MO) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ rollsum(., 12, fill = NA, align = "right"), .names = "{.col}_rollsum"))

US_ADP_EXPORTS_COUNTRY_ROLLED_PIVOT <- US_ADP_EXPORTS_COUNTRY_ROLLED %>%
  filter(time == max(time)) %>%
  select(-MONTH,-YEAR,-E_COMMODITY) %>%
  pivot_longer(-time)

US_ADP_NET_EXPORTS_COUNTRY <- merge(US_ADP_IMPORTS_COUNTRY_ROLLED,US_ADP_EXPORTS_COUNTRY_ROLLED, by = "time")

US_LOGIC_CHIP_IMPORTS_COUNTRY <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "854231", #Digital Processing Units
  #I_COMMODITY = "847180", #Automated Data Processing Units
  #I_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #I_COMMODITY = "854231", #logic chips
  #I_COMMODITY = "854232", #memory chips
  #CTY_CODE = "-", #Total
)

US_LOGIC_CHIP_IMPORTS_COUNTRY_ROLLED <- US_LOGIC_CHIP_IMPORTS_COUNTRY %>%
  select(-I_COMMODITY_1,-CTY_CODE) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = GEN_VAL_MO) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ rollsum(., 12, fill = NA, align = "right"), .names = "{.col}_rollsum"))

US_LOGIC_CHIP_IMPORTS_COUNTRY_ROLLED_PIVOT <- US_LOGIC_CHIP_IMPORTS_COUNTRY_ROLLED %>%
  filter(time == max(time)) %>%
  select(-MONTH,-YEAR,-I_COMMODITY) %>%
  pivot_longer(-time)

US_LOGIC_CHIP_EXPORTS_COUNTRY <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "854231", #Digital Processing Units
  #E_COMMODITY = "847180", #Automated Data Processing Units
  #E_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
) 

US_LOGIC_CHIP_EXPORTS_COUNTRY_ROLLED <- US_LOGIC_CHIP_EXPORTS_COUNTRY %>%
  select(-E_COMMODITY_1,-CTY_CODE) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = ALL_VAL_MO) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ rollsum(., 12, fill = NA, align = "right"), .names = "{.col}_rollsum"))

US_LOGIC_CHIP_EXPORTS_COUNTRY_ROLLED_PIVOT <- US_LOGIC_CHIP_EXPORTS_COUNTRY_ROLLED %>%
  filter(time == max(time)) %>%
  select(-MONTH,-YEAR,-E_COMMODITY) %>%
  pivot_longer(-time)

US_LOGIC_CHIP_NET_EXPORTS_COUNTRY <- merge(US_LOGIC_CHIP_IMPORTS_COUNTRY_ROLLED,US_LOGIC_CHIP_EXPORTS_COUNTRY_ROLLED, by = "time")

US_ADP_PARTS_IMPORTS_COUNTRY <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "854231", #Digital Processing Units
  #I_COMMODITY = "847180", #Automated Data Processing Units
  I_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #I_COMMODITY = "854231", #logic chips
  #I_COMMODITY = "854232", #memory chips
  #CTY_CODE = "-", #Total
)

US_ADP_PARTS_IMPORTS_COUNTRY_ROLLED <- US_ADP_PARTS_IMPORTS_COUNTRY %>%
  select(-I_COMMODITY_1,-CTY_CODE) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = GEN_VAL_MO) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ rollsum(., 12, fill = NA, align = "right"), .names = "{.col}_rollsum"))

US_ADP_PARTS_IMPORTS_COUNTRY_ROLLED_PIVOT <- US_ADP_PARTS_IMPORTS_COUNTRY_ROLLED %>%
  filter(time == max(time)) %>%
  select(-MONTH,-YEAR,-I_COMMODITY) %>%
  pivot_longer(-time)

US_ADP_PARTS_EXPORTS_COUNTRY <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #E_COMMODITY = "854231", #Digital Processing Units
  #E_COMMODITY = "847180", #Automated Data Processing Units
  E_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
) 

US_ADP_PARTS_EXPORTS_COUNTRY_ROLLED <- US_ADP_PARTS_EXPORTS_COUNTRY %>%
  select(-E_COMMODITY_1,-CTY_CODE) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = ALL_VAL_MO) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ rollsum(., 12, fill = NA, align = "right"), .names = "{.col}_rollsum"))

US_ADP_PARTS_EXPORTS_COUNTRY_ROLLED_PIVOT <- US_ADP_PARTS_EXPORTS_COUNTRY_ROLLED %>%
  filter(time == max(time)) %>%
  select(-MONTH,-YEAR,-E_COMMODITY) %>%
  pivot_longer(-time)

US_ADP_PARTS_NET_EXPORTS_COUNTRY <- merge(US_ADP_PARTS_IMPORTS_COUNTRY_ROLLED,US_ADP_PARTS_EXPORTS_COUNTRY_ROLLED, by = "time")


US_TAIWAN_ADP_NET_IMPORTS_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(US_ADP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN.x-TAIWAN.y)*12/1000000000, color= "Large Computers"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_ADP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN_rollsum.x-TAIWAN_rollsum.y)/1000000000, color= "Large Computers"), size = 1.25) +
  geom_line(data= filter(US_LOGIC_CHIP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN.x-TAIWAN.y)*12/1000000000, color= "GPUs/CPUs/TPUs"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_LOGIC_CHIP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN_rollsum.x-TAIWAN_rollsum.y)/1000000000, color= "GPUs/CPUs/TPUs"), size = 1.25) +
  geom_line(data= filter(US_ADP_PARTS_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN.x-TAIWAN.y)*12/1000000000, color= "Computer Parts & Accessories"), size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_line(data= filter(US_ADP_PARTS_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN_rollsum.x-TAIWAN_rollsum.y)/1000000000, color= "Computer Parts & Accessories"), size = 1.25) +
  xlab("Date") +
  ylab("Net Imports, Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140), limits = c(-1,ceiling(max(US_ADP_NET_EXPORTS_COUNTRY$TAIWAN.x*12)/5000000000)*5), expand = c(0,0)) +
  ggtitle("US Imports of Taiwanese Computers") +
  labs(caption = "Graph created by @JosephPolitano using Census Trade data", subtitle = "AI & Data Center Demand Has Driven a Massive Boom in US Computer Imports from Taiwan") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "US Net Imports from Taiwan\nSolid = Rolling 12M Total, Dashed = Monthly, Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Computer Parts & Accessories","Large Computers","GPUs/CPUs/TPUs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -1-(.3*(ceiling(max(US_ADP_NET_EXPORTS_COUNTRY$TAIWAN.x*12)/5000000000)*5)+1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_TAIWAN_ADP_NET_IMPORTS_graph, "US Taiwan ADP Net Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_TAIWAN_ADP_NET_IMPORTS_ROLLED_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_line(data= filter(US_ADP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN.x-TAIWAN.y)*12/1000000000, color= "Large Computers"), size = 0.75, alpha = 0.75, linetype = "dashed") +
  geom_line(data= filter(US_ADP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN_rollsum.x-TAIWAN_rollsum.y)/1000000000, color= "Large Computers"), size = 1.25) +
  #geom_line(data= filter(US_LOGIC_CHIP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN.x-TAIWAN.y)*12/1000000000, color= "GPUs/CPUs"), size = 0.75, alpha = 0.75, linetype = "dashed") +
  geom_line(data= filter(US_LOGIC_CHIP_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN_rollsum.x-TAIWAN_rollsum.y)/1000000000, color= "GPUs/CPUs/TPUs"), size = 1.25) +
  #geom_line(data= filter(US_ADP_PARTS_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN.x-TAIWAN.y)*12/1000000000, color= "Computer Parts & Accessories"), size = 0.75, alpha = 0.75, linetype = "dashed") +
  geom_line(data= filter(US_ADP_PARTS_NET_EXPORTS_COUNTRY, time >= as.Date("2015-01-01")), aes(x=time,y= (TAIWAN_rollsum.x-TAIWAN_rollsum.y)/1000000000, color= "Computer Parts & Accessories"), size = 1.25) +
  xlab("Date") +
  ylab("Net Imports, Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50,60,70,80), limits = c(-1,ceiling(max((US_ADP_PARTS_NET_EXPORTS_COUNTRY%>%select(TAIWAN_rollsum.x)%>%drop_na())$TAIWAN_rollsum.x)/5000000000)*5), expand = c(0,0)) +
  ggtitle("US Imports of Taiwanese Computers") +
  labs(caption = "Graph created by @JosephPolitano using Census Trade data", subtitle = "AI & Data Center Demand Has Driven a Massive Boom in US Computer Imports from Taiwan") +
  theme_apricitas + theme(legend.position = c(.30,.78)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "US Net Imports from Taiwan\nRolling 12M Total",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Computer Parts & Accessories","Large Computers","GPUs/CPUs/TPUs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -1-(.3*(ceiling(max((US_ADP_PARTS_NET_EXPORTS_COUNTRY%>%select(TAIWAN_rollsum.x)%>%drop_na())$TAIWAN_rollsum.x)/5000000000)*5+1)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_TAIWAN_ADP_NET_IMPORTS_ROLLED_graph, "US Taiwan ADP Net Imports Rolled Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


COMPUTER_IMPORTS_TOTAL_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8471", #ADP
  I_COMMODITY = "847130", #ADP
)

COMPUTER_X_LAPTOP_IMPORTS <- COMPUTER_IMPORTS_TOTAL_BULK %>%
  filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  group_by(time,I_COMMODITY) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  pivot_wider(names_from = I_COMMODITY, values_from = CON_VAL_MO) %>%
  transmute(date = as.Date(paste0(time,"-01")),
            imports = `8471`-`847130`,
            total = `8471`)

COMPUTER_PARTS_TOTAL_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8473", #Parts
)

COMPUTER_PARTS_IMPORTS <- COMPUTER_PARTS_TOTAL_BULK %>%
  filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  group_by(time,I_COMMODITY) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  pivot_wider(names_from = I_COMMODITY, values_from = CON_VAL_MO) %>%
  transmute(date = as.Date(paste0(time,"-01")),imports = `8473`)

US_CHIP_TOTAL_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "854231", #Digital Processing Units
  #E_COMMODITY = "847180", #Automated Data Processing Units
  #E_COMMODITY = "847330", #Parts and Accessories of ADP Units
  #E_COMMODITY = "854231", #logic chips
  #E_COMMODITY = "854232", #memory chips
) 

US_CHIP_IMPORTS <- US_CHIP_TOTAL_BULK %>%
  filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  group_by(time,I_COMMODITY) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  pivot_wider(names_from = I_COMMODITY, values_from = CON_VAL_MO) %>%
  transmute(date = as.Date(paste0(time,"-01")),imports = `854231`)


US_LARGE_COMPUTER_IMPORTS_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(COMPUTER_X_LAPTOP_IMPORTS, date >= as.Date("2015-01-01")), aes(x=date,y= imports*12/1000000000, color= "Large Computers"), size = 1.25) +
  geom_line(data= filter(COMPUTER_PARTS_IMPORTS, date >= as.Date("2015-01-01")), aes(x=date,y= imports*12/1000000000, color= "Computer Parts & Accessories"), size = 1.25) +
  #geom_line(data= filter(US_CHIP_IMPORTS, date >= as.Date("2015-01-01")), aes(x=date,y= imports*12/1000000000, color= "Semiconductors"), size = 1.25) +
  xlab("Date") +
  ylab("Net Imports, Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200,250,300,350,400), limits = c(0,ceiling(max(COMPUTER_X_LAPTOP_IMPORTS$imports*12)/5000000000)*5), expand = c(0,0)) +
  ggtitle("Computer Imports Surge Amid the AI Boom") +
  labs(caption = "Graph created by @JosephPolitano using Census Trade data. Large Computers = HS8471 ex Laptops, Computer Parts = HS8473", subtitle = "AI & Data Center Demand Has Driven a Massive Boom in US Computer Imports") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "US Imports, Monthly Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Large Computers","Computer Parts & Accessories","GPUs/CPUs/TPUs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -1-(.3*(ceiling(max(COMPUTER_X_LAPTOP_IMPORTS$imports*12)/5000000000)*5)+1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")


ggsave(dpi = "retina",plot = US_LARGE_COMPUTER_IMPORTS_graph, "US Large Computer Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



COMPUTER_IMPORTS_TOTAL_BREAKDOWN <- COMPUTER_IMPORTS_TOTAL_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO),
         CTY_NAME = case_when(
           CTY_NAME %in% c("TAIWAN") ~ "Taiwan",
           CTY_NAME %in% c("MEXICO") ~ "Mexico",
           CTY_NAME %in% c("VIETNAM") ~ "Vietnam",
           CTY_NAME %in% c("CHINA") ~ "China",
           CTY_NAME %in% c("HONG KONG") ~ "China",
           CTY_NAME %in% c("MACAU") ~ "China",
           CTY_NAME %in% c("THAILAND") ~ "Thailand",
           CTY_NAME %in% c("MALAYSIA") ~ "Malaysia",
           CTY_NAME %in% c("KOREA, SOUTH") ~ "South Korea",
           CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES") ~ "Total",
           TRUE ~ "Other"
         )) %>%
  filter(CTY_NAME != "Other") %>%
  group_by(time, I_COMMODITY, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>% pivot_wider(names_from = I_COMMODITY, values_from = CON_VAL_MO) %>% transmute(date = as.Date(paste0(time,"-01")), CTY_NAME, imports = `8471`-`847130`) %>%
  pivot_wider(names_from = CTY_NAME, values_from = imports) %>%
  filter(date >= as.Date("2021-01-01")) %>%
  mutate(Other = Total - Taiwan - Mexico - Vietnam - China - Thailand - Malaysia - `South Korea`) %>%
  select(-Total)

COMPUTER_PARTS_IMPORTS_TOTAL_BREAKDOWN <- COMPUTER_PARTS_TOTAL_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO),
         CTY_NAME = case_when(
           CTY_NAME %in% c("TAIWAN") ~ "Taiwan",
           CTY_NAME %in% c("MEXICO") ~ "Mexico",
           CTY_NAME %in% c("VIETNAM") ~ "Vietnam",
           CTY_NAME %in% c("CHINA") ~ "China",
           CTY_NAME %in% c("HONG KONG") ~ "China",
           CTY_NAME %in% c("MACAU") ~ "China",
           CTY_NAME %in% c("THAILAND") ~ "Thailand",
           CTY_NAME %in% c("MALAYSIA") ~ "Malaysia",
           CTY_NAME %in% c("KOREA, SOUTH") ~ "South Korea",
           CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES") ~ "Total",
           TRUE ~ "Other"
         )) %>%
  filter(CTY_NAME != "Other") %>%
  group_by(time, I_COMMODITY, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>% pivot_wider(names_from = I_COMMODITY, values_from = CON_VAL_MO) %>% transmute(date = as.Date(paste0(time,"-01")), CTY_NAME, imports = `8473`) %>%
  pivot_wider(names_from = CTY_NAME, values_from = imports) %>%
  filter(date >= as.Date("2021-01-01")) %>%
  mutate(Other = Total - Taiwan - Mexico - Vietnam - China - Thailand - Malaysia - `South Korea`) %>%
  select(-Total)

CHIP_IMPORTS_TOTAL_BREAKDOWN <- US_CHIP_TOTAL_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO),
         CTY_NAME = case_when(
           CTY_NAME %in% c("TAIWAN") ~ "Taiwan",
           CTY_NAME %in% c("MEXICO") ~ "Mexico",
           CTY_NAME %in% c("VIETNAM") ~ "Vietnam",
           CTY_NAME %in% c("CHINA") ~ "China",
           CTY_NAME %in% c("HONG KONG") ~ "China",
           CTY_NAME %in% c("MACAU") ~ "China",
           CTY_NAME %in% c("THAILAND") ~ "Thailand",
           CTY_NAME %in% c("MALAYSIA") ~ "Malaysia",
           CTY_NAME %in% c("KOREA, SOUTH") ~ "South Korea",
           CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES") ~ "Total",
           TRUE ~ "Other"
         )) %>%
  filter(CTY_NAME != "Other") %>%
  group_by(time, I_COMMODITY, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>% pivot_wider(names_from = I_COMMODITY, values_from = CON_VAL_MO) %>% transmute(date = as.Date(paste0(time,"-01")), CTY_NAME, imports = `854231`) %>%
  pivot_wider(names_from = CTY_NAME, values_from = imports) %>%
  filter(date >= as.Date("2021-01-01")) %>%
  mutate(Other = Total - Taiwan - Mexico - Vietnam - China - Thailand - Malaysia - `South Korea`) %>%
  select(-Total)

AI_IMPORTS_BREAKDOWN <- merge(COMPUTER_IMPORTS_TOTAL_BREAKDOWN,COMPUTER_PARTS_IMPORTS_TOTAL_BREAKDOWN, by = "date") %>%
  merge(.,CHIP_IMPORTS_TOTAL_BREAKDOWN, by = "date") %>%
  transmute(date,
            Taiwan = Taiwan.x + Taiwan.y + Taiwan,
            Mexico = Mexico.x + Mexico.y + Mexico,
            Vietnam = Vietnam.x + Vietnam.y + Vietnam,
            China = China.x + China.y + China,
            Thailand = Thailand.x + Thailand.y + Thailand,
            Malaysia = Thailand.x + Thailand.y + Thailand,
            `South Korea` = `South Korea.x` + `South Korea.y` + `South Korea`,
            Other = Other.x + Other.y + Other)


AI_RELATED_IMPORTS <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= `Other`*12/1000000000,color= "All Other Countries"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= China*12/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= Mexico*12/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= Taiwan*12/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= Vietnam*12/1000000000,color= "Vietnam"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= Thailand*12/1000000000,color= "Thailand"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= Malaysia*12/1000000000,color= "Malaysia"), size = 1.25) + 
  geom_line(data=AI_IMPORTS_BREAKDOWN, aes(x=date,y= `South Korea`*12/1000000000,color= "South Korea"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,175),breaks = c(0,25,50,75,100,125,150,175), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US AI-Related Computer & Parts Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data. AI = HS 8471 (computers) - 847130 (laptops) + 8473 (parts) + 854231 (GPUs)",subtitle = "America's AI-Related Imports are Skyrocketing, Especially From Taiwan and Mexico") +
  theme_apricitas + theme(legend.position = c(.45,.60)) +
  scale_color_manual(name = "AI-Related Computer & Parts Imports, Billions, Monthly Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Taiwan","Mexico","Malaysia","Vietnam","Thailand","South Korea","China","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(today()-as.Date("2021-01-01"))), xmax = as.Date("2021-01-01")-(0.049*(today()-as.Date("2021-01-01"))), ymin = 0-(.3*(175)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AI_RELATED_IMPORTS, "AI Related Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


TAIWAN_IMPORTS_BREAKDOWN <- merge(COMPUTER_IMPORTS_TOTAL_BREAKDOWN,COMPUTER_PARTS_IMPORTS_TOTAL_BREAKDOWN, by = "date") %>%
  merge(.,CHIP_IMPORTS_TOTAL_BREAKDOWN, by = "date") %>%
  transmute(date,`Large Computers` = Taiwan.x, `Computer Parts` = Taiwan.y, `GPUs` = Taiwan)

AI_RELATED_IMPORTS_TAIWAN <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=TAIWAN_IMPORTS_BREAKDOWN, aes(x=date,y= `Large Computers`*12/1000000000,color= "Large Computers"), size = 1.25) + 
  geom_line(data=TAIWAN_IMPORTS_BREAKDOWN, aes(x=date,y= `Computer Parts`*12/1000000000,color= "Computer Parts"), size = 1.25) + 
  geom_line(data=TAIWAN_IMPORTS_BREAKDOWN, aes(x=date,y= `GPUs`*12/1000000000,color= "GPUs"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,125),breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US AI-Related Imports From Taiwan") +
  labs(caption = "Graph created by @JosephPolitano using Census data. AI = HS 8471 (computers), 847130 (laptops), 8473 (parts), 854231 (GPUs)",subtitle = "America's AI-Related Imports are Skyrocketing, Especially From Taiwan") +
  theme_apricitas + theme(legend.position = c(.45,.60)) +
  scale_color_manual(name = "AI-Related Computer & Parts Imports, Billions, Monthly Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Large Computers","Computer Parts","GPUs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(today()-as.Date("2021-01-01"))), xmax = as.Date("2021-01-01")-(0.049*(today()-as.Date("2021-01-01"))), ymin = 0-(.3*(125)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AI_RELATED_IMPORTS_TAIWAN, "AI Related Imports Taiwan Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


FIXED_EQUIP_INVEST_SPECS_REAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50506',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST_REAL <- beaGet(FIXED_EQUIP_INVEST_SPECS_REAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2007-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na() %>%
  transmute(date, computers_and_peripheral = u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6, computers_and_peripheral_yoy = computers_and_peripheral/lag(computers_and_peripheral,4))

US_COMPUTER_FIXED_EQUIP_INVEST_GRAPH <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral/1000, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  #geom_line(data = FIXED_EQUIP_INVEST_REAL, aes(x=date, y = computers_and_peripheral_yoy-1, color = "Real Private Fixed Investment,\nComputers and Peripheral Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(FIXED_EQUIP_INVEST_REAL$computers_and_peripheral)/5000)*5), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Computer Investment Near Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Computer Investment Has Grown Significantly Since 2020, Breaking Years of Stagnation") +
  theme_apricitas + theme(legend.position = c(.3,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme(plot.title = element_text(size = 26.5)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*(ceiling(max(FIXED_EQUIP_INVEST_REAL$computers_and_peripheral)/5000)*5)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_COMPUTER_FIXED_EQUIP_INVEST_GRAPH, "US Computer Fixed Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

REGIONAL_DATA_CENTER_DATA_2024 <- read.xlsx("https://www.census.gov/construction/c30/xlsx/region.xlsx", sheet = "2024") %>%
  drop_na() %>%
  setNames(c("Category","Total","Northeast","Midwest","South","West")) %>%
  mutate(Category = str_squish(Category)) %>%
  filter(Category == "Data center") %>%
  select(-Category) %>%
  pivot_longer(Total:West, values_to = "value_2024")

REGIONAL_DATA_CENTER_DATA_2019 <- read.xlsx("https://www.census.gov/construction/c30/xlsx/region.xlsx", sheet = "2019")  %>%
  drop_na() %>%
  setNames(c("Category","Total","Northeast","Midwest","South","West")) %>%
  mutate(Category = str_squish(Category)) %>%
  filter(Category == "Data center") %>%
  select(-Category) %>%
  pivot_longer(Total:West, values_to = "value_2019")

REGIONAL_DATA_CENTER_DATA <- merge(REGIONAL_DATA_CENTER_DATA_2024,REGIONAL_DATA_CENTER_DATA_2019, by = "name") %>%
  mutate(across(where(is.character) & !all_of("name"), ~ as.numeric(.)/1000)) %>%
  mutate(value_change = value_2024-value_2019)
  
REGIONS_MAP <- tigris::regions(class = "sf") %>%
  left_join(.,REGIONAL_DATA_CENTER_DATA, by = c("NAME" = "name"))

ggplot() + 
  geom_sf(REGIONS_MAP)

REGIONS_MAP <- tigris::regions(class = "sf") 

sf_use_s2(FALSE)

div_dat <- states(cb = FALSE, resolution = '20m') %>%
  st_drop_geometry() %>%
  select(NAME, REGION) %>%
  mutate(ID = tolower(NAME))

# get state data, convert to sf, join with division data
states <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  left_join(div_dat)

states <- st_make_valid(states)

states <- merge(states,CENSUS_SUBNATIONAL_MFG_SPENDING, by = "DIVISION")



# create division polygons
REGION_MAP <- states %>%
  group_by(REGION) %>% 
  summarize() %>%
  mutate(name = c("Northeast","Midwest","South","West")) %>%
  left_join(REGIONAL_DATA_CENTER_DATA, by = "name")

REGIONAL_DATA_CENTER_SPENDING_GRAPH <- ggplot() + 
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  geom_sf(data = REGION_MAP, 
          aes(fill = value_2024), 
          color = 'grey25') +
  geom_sf(data = states, 
          fill = NA, 
          color = 'grey50') +
  geom_sf(data = REGION_MAP, 
          color = 'black', 
          fill = NA,
          lwd = 1.25) +
  scale_fill_viridis_c(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,5,10,15), limits = c(0,16), expand = c(0,0)) +
  # scale_fill_gradient(low = "#00A99D",
  #                      high = "#FFE98F",
  #                      space = "Lab",
  #                      na.value = "grey50",
  #                      guide = "colourbar",
  #                      aesthetics = "fill",
  #                     breaks = c(0,10,20,30,40,50), 
  #                     labels = c("$0B","$10B","$20B","$30B","$40B","$50B"),
  #                     limits = c(0,50)) +
  coord_sf(crs = 5070) +
  geom_text(
    data = filter(REGION_MAP, REGION == 1), 
    aes(x = 1800000, y = 2600000, label = paste0("$", sprintf("%.1f", round(value_2024, 1)), "B","\n+$",sprintf("%.1f", round(value_change, 1)),"B Since 2019")), 
    size = 6, 
    hjust = 0.5,
    color = "white",
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(REGION_MAP, REGION == 2), 
    aes(x = 300000, y = 2000000, label = paste0("$", sprintf("%.1f", round(value_2024, 1)), "B","\n+$",sprintf("%.1f", round(value_change, 1)),"B Since 2019")), 
    size = 6, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(REGION_MAP, REGION == 3), 
    aes(x = 600000, y = 1200000, label = paste0("$", sprintf("%.1f", round(value_2024, 1)), "B","\n+$",sprintf("%.1f", round(value_change, 1)),"B Since 2019")), 
    size = 6, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(REGION_MAP, REGION == 4), 
    aes(x = -1500000, y = 2200000, label = paste0("$", sprintf("%.1f", round(value_2024, 1)), "B","\n+$",sprintf("%.1f", round(value_change, 1)),"B Since 2019")), 
    size = 6, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  ggtitle("   Data Center Construction by Region, 2024") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using US Census Construction data") +
  labs(fill = NULL) +
  theme_apricitas + theme(plot.title = element_text(size = 29), legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = REGIONAL_DATA_CENTER_SPENDING_GRAPH, "Regional Data Center Spending.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

QSS_Data <- getCensus(
  name = "timeseries/eits/qss",
  vars = c("data_type_code","category_code","cell_value", "seasonally_adj","time_slot_id"),
  time = paste("from 1990 to", format(Sys.Date(), "%Y")),
  seasonally_adj = "yes",
  data_type_code = "QREV"
) %>%
  filter(category_code %in% c("5112T","518T","5415T","519T")) %>%
  transmute(name = category_code, date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(cell_value)) %>%
  pivot_wider(values_from = value) %>%
  setNames(c("date","Software Publishers","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design, Custom Programming, & Related","Web Search Portals, Libraries, & Related")) %>%
  arrange(date) %>%
  mutate(across(where(is.numeric), ~ (.x-lag(.x,4))/lag(.x,4))) %>%
  drop_na() %>%
  filter(date >= as.Date("2016-01-01"))

QSS_Data_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=QSS_Data, aes(x=date,y= `Web Search Portals, Libraries, & Related`,color= "Web Search Portals, Libraries, & Related"), size = 1.25)+ 
  geom_line(data=QSS_Data, aes(x=date,y= `Computer Systems Design, Custom Programming, & Related`,color= "Computer Systems Design, Custom Programming, & Related"), size = 1.25)+ 
  geom_line(data=QSS_Data, aes(x=date,y= `Software Publishers`,color= "Software Publishers"), size = 1.25)+ 
  geom_line(data=QSS_Data, aes(x=date,y= `Computing Infrastructure, Data Processing, Web Hosting, & Related`,color= "Computing Infrastructure, Data Processing, Web Hosting, & Related"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Revenue Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10,0.15,.2,.25,.3,.35,.40,.45,.50), limits = c(0,.50), expand = c(0,0)) +
  ggtitle("Tech Sector Revenue Growth") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "Revenue Growth Has Reaccelerated Across Tech Sectors in Late 2023/Early 2024 After Slowing Down Significantly") +
  theme_apricitas + theme(legend.position = c(.40,.88), legend.text = element_text(size = 13), legend.title = element_text(size = 14), legend.key.height = unit(0.3,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Year-on-Year Revenue Growth",values = c("#FFE98F","#A7ACD9","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design, Custom Programming, & Related","Web Search Portals, Libraries, & Related","Software Publishers")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*.50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QSS_Data_Graph, "QSS Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

beaSets(Sys.getenv("BEA_KEY"))

beaParams(Sys.getenv("BEA_KEY"),setName = 'FixedAssets')

beaParamVals(Sys.getenv("BEA_KEY"),setName = 'FixedAssets', paramName = "TableName")

FIXED_INVEST_EQUIP_QUANTITY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt308E',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_INVEST_EQUIP_QUANTITY <- beaGet(FIXED_INVEST_EQUIP_QUANTITY_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVEST_IP_QUANTITY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt308I',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_INVEST_IP_QUANTITY <- beaGet(FIXED_INVEST_IP_QUANTITY_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVEST_STRUC_QUANTITY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt308S',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_INVEST_STRUC_QUANTITY <- beaGet(FIXED_INVEST_STRUC_QUANTITY_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  drop_na() 

#NOMINAL FIXED INVESTMENT BY INDUSTRY

FIXED_INVEST_EQUIP_NOMINAL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt307E',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_INVEST_EQUIP_NOMINAL <- beaGet(FIXED_INVEST_EQUIP_NOMINAL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVEST_IP_NOMINAL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt307I',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_INVEST_IP_NOMINAL <- beaGet(FIXED_INVEST_IP_NOMINAL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVEST_STRUC_NOMINAL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt307S',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_INVEST_STRUC_NOMINAL <- beaGet(FIXED_INVEST_STRUC_NOMINAL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names() %>%
  drop_na()

#Selecting Industries
#COMPUTER AND ELECTRONICS MANUFACTURING
#PUBLISHING INDUSTRIES
#INFORMATION AND DATA PROCESSING
#COMPUTER SYSTEMS DESIGN AND RELATED

FIXED_INVEST_EQUIP_QUANTITY <- FIXED_INVEST_EQUIP_QUANTITY %>%
  rename_with(~ gsub("^fa.{21}", "Q_", .), starts_with("fa")) %>%
  select(date,Q_21_computer_and_electronic_products_fisher_quantity_index_level_0,Q_58_publishing_industries_includes_software_fisher_quantity_index_level_0,Q_61_information_and_data_processing_services_fisher_quantity_index_level_0,Q_78_computer_systems_design_and_related_services_fisher_quantity_index_level_0)

FIXED_INVEST_EQUIP_NOMINAL <- FIXED_INVEST_EQUIP_NOMINAL %>%
  rename_with(~ gsub("^fa.{21}", "N_", .), starts_with("fa")) %>%
  select(date,N_21_computer_and_electronic_products_historical_cost_level_6,N_58_publishing_industries_includes_software_historical_cost_level_6,N_61_information_and_data_processing_services_historical_cost_level_6,N_78_computer_systems_design_and_related_services_historical_cost_level_6)

FIXED_INVEST_EQUIP_REAL <- merge(FIXED_INVEST_EQUIP_NOMINAL,FIXED_INVEST_EQUIP_QUANTITY, by = "date") %>%
  transmute(date,
            computer_and_electronic_products = Q_21_computer_and_electronic_products_fisher_quantity_index_level_0/100000*N_21_computer_and_electronic_products_historical_cost_level_6[16],
            publishing = Q_58_publishing_industries_includes_software_fisher_quantity_index_level_0/100000*N_58_publishing_industries_includes_software_historical_cost_level_6[16],
            data_processing =Q_61_information_and_data_processing_services_fisher_quantity_index_level_0/100000*N_61_information_and_data_processing_services_historical_cost_level_6[16],
            computer_systems_design = Q_78_computer_systems_design_and_related_services_fisher_quantity_index_level_0/100000*N_78_computer_systems_design_and_related_services_historical_cost_level_6[16])



FIXED_INVEST_IP_QUANTITY <- FIXED_INVEST_IP_QUANTITY %>%
  rename_with(~ gsub("^fa.{21}", "Q_", .), starts_with("fa")) %>%
  select(date,Q_21_computer_and_electronic_products_fisher_quantity_index_level_0,Q_58_publishing_industries_includes_software_fisher_quantity_index_level_0,Q_61_information_and_data_processing_services_fisher_quantity_index_level_0,Q_78_computer_systems_design_and_related_services_fisher_quantity_index_level_0)

FIXED_INVEST_IP_NOMINAL <- FIXED_INVEST_IP_NOMINAL %>%
  rename_with(~ gsub("^fa.{21}", "N_", .), starts_with("fa")) %>%
  select(date,N_21_computer_and_electronic_products_historical_cost_level_6,N_58_publishing_industries_includes_software_historical_cost_level_6,N_61_information_and_data_processing_services_historical_cost_level_6,N_78_computer_systems_design_and_related_services_historical_cost_level_6)

FIXED_INVEST_IP_REAL <- merge(FIXED_INVEST_IP_NOMINAL,FIXED_INVEST_IP_QUANTITY, by = "date") %>%
  transmute(date,
            computer_and_electronic_products = Q_21_computer_and_electronic_products_fisher_quantity_index_level_0/100000*N_21_computer_and_electronic_products_historical_cost_level_6[16],
            publishing = Q_58_publishing_industries_includes_software_fisher_quantity_index_level_0/100000*N_58_publishing_industries_includes_software_historical_cost_level_6[16],
            data_processing =Q_61_information_and_data_processing_services_fisher_quantity_index_level_0/100000*N_61_information_and_data_processing_services_historical_cost_level_6[16],
            computer_systems_design = Q_78_computer_systems_design_and_related_services_fisher_quantity_index_level_0/100000*N_78_computer_systems_design_and_related_services_historical_cost_level_6[16])



FIXED_INVEST_STRUC_QUANTITY <- FIXED_INVEST_STRUC_QUANTITY %>%
  rename_with(~ gsub("^fa.{21}", "Q_", .), starts_with("fa")) %>%
  select(date,Q_21_computer_and_electronic_products_fisher_quantity_index_level_0,Q_58_publishing_industries_includes_software_fisher_quantity_index_level_0,Q_61_information_and_data_processing_services_fisher_quantity_index_level_0,Q_78_computer_systems_design_and_related_services_fisher_quantity_index_level_0)

FIXED_INVEST_STRUC_NOMINAL <- FIXED_INVEST_STRUC_NOMINAL %>%
  rename_with(~ gsub("^fa.{21}", "N_", .), starts_with("fa")) %>%
  select(date,N_21_computer_and_electronic_products_historical_cost_level_6,N_58_publishing_industries_includes_software_historical_cost_level_6,N_61_information_and_data_processing_services_historical_cost_level_6,N_78_computer_systems_design_and_related_services_historical_cost_level_6)

FIXED_INVEST_STRUC_REAL <- merge(FIXED_INVEST_STRUC_NOMINAL,FIXED_INVEST_STRUC_QUANTITY, by = "date") %>%
  transmute(date,
            computer_and_electronic_products = Q_21_computer_and_electronic_products_fisher_quantity_index_level_0/100000*N_21_computer_and_electronic_products_historical_cost_level_6[16],
            publishing = Q_58_publishing_industries_includes_software_fisher_quantity_index_level_0/100000*N_58_publishing_industries_includes_software_historical_cost_level_6[16],
            data_processing =Q_61_information_and_data_processing_services_fisher_quantity_index_level_0/100000*N_61_information_and_data_processing_services_historical_cost_level_6[16],
            computer_systems_design = Q_78_computer_systems_design_and_related_services_fisher_quantity_index_level_0/100000*N_78_computer_systems_design_and_related_services_historical_cost_level_6[16])

FIXED_INVEST_EQUP_REAL_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= data_processing,color= "Information and Data Processing"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing,color= "Publishing (Including Software)"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= computer_systems_design,color= "Computer Systems Design"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= computer_and_electronic_products,color= "Computer and Electronics Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,90), breaks = c(0,10,20,30,40,50,60,70,80,90), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Real Equipment Investments") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Imports of Large Computers & Parts/Accessories Has Been Skyrocketing") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*90), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVEST_EQUP_REAL_Graph, "Fixed Invest Equip Real.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

FIXED_INVEST_STRUC_REAL_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FIXED_INVEST_STRUC_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= data_processing,color= "Information and Data Processing"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_STRUC_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing,color= "Publishing (Including Software)"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_STRUC_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= computer_systems_design,color= "Computer Systems Design"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_STRUC_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= computer_and_electronic_products,color= "Computer and Electronics Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,30), breaks = c(0,10,20,30,40,50,60), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Real Structure Investment") +
  labs(caption = "Graph created by @JosephPolitano using Census data.",subtitle = "US Imports of Large Computers & Parts/Accessories Has Been Skyrocketing") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(30)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVEST_STRUC_REAL_Graph, "Fixed Invest Struc Real.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

FIXED_INVEST_IP_REAL_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= data_processing,color= "Information and Data Processing"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing,color= "Publishing (Including Software)"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= computer_systems_design,color= "Computer Systems Design"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= computer_and_electronic_products,color= "Computer and Electronics Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,175), breaks = c(0,25,50,75,100,125,150,175), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Real IP Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Real IP Investment") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(175)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVEST_IP_REAL_Graph, "Fixed Invest IP Real.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

REAL_PUBLISHER_INVEST_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing,color= "Intellectual Property (Including R&D)"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing,color= "Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,100), breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("Investment by Software & Other Publishers") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Real Investment by Software Publishers Has Skyrocketed in 2022 and 2023") +
  theme_apricitas + theme(legend.position = c(.35,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Real Investment, 2017 Dollars",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Intellectual Property (Including R&D)","Equipment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(100)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PUBLISHER_INVEST_GRAPH, "Real Publisher Invest Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

REAL_PUBLISHER_INVEST_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing/publishing[8]*100,color= "Intellectual Property (Including R&D)"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= publishing/publishing[8]*100,color= "Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,210), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Index, 2021 = 100") +
  ggtitle("Investment by Software & Other Publishers") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Real Investment by Software Publishers Has Skyrocketed in 2022 and 2023") +
  theme_apricitas + theme(legend.position = c(.35,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Real Investment, Indexed, 2021 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Intellectual Property (Including R&D)","Equipment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-365-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-365-as.Date("2014-01-01"))), ymin = 0-(.3*(210)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PUBLISHER_INVEST_GRAPH, "Real Publisher Invest Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


REAL_DATA_INVEST_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FIXED_INVEST_IP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= data_processing,color= "Intellectual Property (Including R&D)"), size = 1.25) + 
  geom_line(data=filter(FIXED_INVEST_EQUIP_REAL, date>= as.Date("2014-01-01")), aes(x=date,y= data_processing,color= "Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,200), breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("Investment by Data Processing, Web Search, & Related") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Real Investment by Data Processers & Related Has Steadily Increased in 2022 and 2023") +
  theme_apricitas + theme(legend.position = c(.35,.89), plot.title = element_text(size = 24)) +
  scale_color_manual(name= "Real Investment, 2017 Dollars",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Intellectual Property (Including R&D)","Equipment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(100)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_DATA_INVEST_GRAPH, "Real Data Invest Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

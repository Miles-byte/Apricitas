pacman::p_load(onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

US <- fredr(series_id = "GDPC1",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
UK <- fredr(series_id = "NGDPRSAXDCGBQ",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
GER <- fredr(series_id = "CLVMNACSCAB1GQDE",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
ITA <- fredr(series_id = "CLVMNACSCAB1GQIT",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
FRA <- fredr(series_id = "CLVMNACSCAB1GQFR",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
JPN <- fredr(series_id = "NAEXKP01JPQ189S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
CAN <- fredr(series_id = "NGDPRSAXDCCAQ",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

RGDP_G7_Graph <- ggplot() + #RGDP Index
  geom_line(data=US, aes(x=date,y= value,color= "United States"), size = 1.25) +
  geom_line(data=UK, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN, aes(x=date,y= value,color= "Canada"), size = 1.25) +
  geom_line(data=GER, aes(x=date,y= value,color= "Germany"), size = 1.25) +
  geom_line(data=ITA, aes(x=date,y= value,color= "Italy"), size = 1.25) +
  geom_line(data=FRA, aes(x=date,y= value,color= "France"), size = 1.25) +
  geom_line(data=JPN, aes(x=date,y= value,color= "Japan"), size = 1.25) +
  annotate("text",label = "Pre-COVID GDP", x = as.Date("2019-01-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,110), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("Back of the Pack") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED",subtitle = "The UK and Japan are the Only G7 Nations Whose Output Hasn't Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.22,.29)) +
  scale_color_manual(name= "Real GDP 2019 Q3 = 100",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 75-(.3*35), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RGDP_G7_Graph, "G7.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#25-54 Employment Rates
US_EPOP <- fredr(series_id = "LNS12300060",observation_start = as.Date("2018-01-01"), frequency = "q") %>%
  mutate(value = value/value[7]*100)
UK_EPOP <- fredr(series_id = "LREM25TTGBQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
GER_EPOP <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
ITA_EPOP <- fredr(series_id = "LREM25TTITQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
FRA_EPOP <- fredr(series_id = "LREM25TTFRQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
JPN_EPOP <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("2018-01-01"), frequency = "q") %>%
  mutate(value = value/value[7]*100)
CAN_EPOP <- fredr(series_id = "LREM25TTCAM156S",observation_start = as.Date("2018-01-01"), frequency = "q") %>%
  mutate(value = value/value[7]*100)

EPOP_G7_Graph <- ggplot() + #EU chemical imports
  geom_line(data=US_EPOP, aes(x=date,y= value,color= "United States"), size = 1.25) +
  geom_line(data=UK_EPOP, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_EPOP, aes(x=date,y= value,color= "Canada"), size = 1.25) +
  geom_line(data=GER_EPOP, aes(x=date,y= value,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_EPOP, aes(x=date,y= value,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_EPOP, aes(x=date,y= value,color= "France"), size = 1.25) +
  geom_line(data=JPN_EPOP, aes(x=date,y= value,color= "Japan"), size = 1.25) +
  annotate("text",label = "Pre-COVID Prime-Age Employment %", x = as.Date("2018-10-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(85,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("25-54 Employment Rate, Index 2019 Q3 = 100") +
  ggtitle("Back of the Pack") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "The UK is the Only G7 Nations Whose Prime-Age Employment Rate Hasn't Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.23,.29)) +
  scale_color_manual(name= "25-54 Employment %, Index 2019 Q3",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 85-(.3*20), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_G7_Graph, "G7epop.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_EPOP_PA <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"), frequency = "q")
UK_EPOP_PA <- fredr(series_id = "LREM25TTGBQ156S",observation_start = as.Date("1990-01-01"))
GER_EPOP_PA <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("1990-01-01"))
ITA_EPOP_PA <- fredr(series_id = "LREM25TTITQ156S",observation_start = as.Date("1990-01-01"))
FRA_EPOP_PA <- fredr(series_id = "LREM25TTFRQ156S",observation_start = as.Date("1990-01-01"))
JPN_EPOP_PA <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("1990-01-01"), frequency = "q")
CAN_EPOP_PA <- fredr(series_id = "LREM25TTCAM156S",observation_start = as.Date("199-01-01"), frequency = "q") 

EPOP_G7_PA_Graph <- ggplot() + #EU chemical imports
  geom_line(data=US_EPOP_PA, aes(x=date,y= value/100,color= "United States"), size = 1.25) +
  geom_line(data=UK_EPOP_PA, aes(x=date,y= value/100,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_EPOP_PA, aes(x=date,y= value/100,color= "Canada"), size = 1.25) +
  geom_line(data=GER_EPOP_PA, aes(x=date,y= value/100,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_EPOP_PA, aes(x=date,y= value/100,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_EPOP_PA, aes(x=date,y= value/100,color= "France"), size = 1.25) +
  geom_line(data=JPN_EPOP_PA, aes(x=date,y= value/100,color= "Japan"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.67,.87), breaks = c(.70,.75,.80,.85), expand = c(0,0)) +
  ylab("25-54 Employment Rate") +
  ggtitle("The Global Employment Recovery") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Besides the UK, All G7 Nations Have Seen Full Employment Recoveries") +
  theme_apricitas + theme(legend.position = c(.15,.28), legend.title = element_text(size = 13), legend.text = element_text(size = 13)) +
  scale_color_manual(name= "25-54 Employment Rate",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 67-(.3*20), ymax = 67) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_G7_PA_Graph, "G7epoppa.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#GDP data at a high level
GDP <- ons_get("gdp-to-four-decimal-places") %>%
  select(Time,v4_0,`sic-unofficial`) %>%
  mutate(`sic-unofficial` = gsub("G--T","Services", `sic-unofficial`)) %>%
  mutate(`sic-unofficial` = gsub("F","Construction", `sic-unofficial`)) %>%
  mutate(`sic-unofficial` = gsub("B--E","Production Industries", `sic-unofficial`)) %>%
  mutate(`sic-unofficial` = gsub("A--T","Total GDP", `sic-unofficial`)) %>%
  subset(., `sic-unofficial` != "A") %>%
  transmute(date = as.Date(as.yearmon(Time, "%b-%y")), value = v4_0, Category = `sic-unofficial`) %>%
  subset(., date > as.Date("2017-12-31"))
  
GDP_Components_Graph <- ggplot() +
  geom_line(data=subset(GDP, date > as.Date("2017-12-31")), aes(x=date,y= value,color= Category), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(55,115), breaks = c(60,70,80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Average = 100") +
  ggtitle("Under-Heating") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Output Has Been Stagnant This Year—With Declines in Industrial Production") +
  theme_apricitas + theme(legend.position = c(.30,.40)) +
  scale_color_manual(name= "Real Index, UK, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Total GDP","Services","Production Industries","Construction")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 55-(.3*60), ymax = 55) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_Components_Graph, "GDP Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#unnesting function
unnest_dataframes <- function(x) {
  
  y <- do.call(data.frame, x)
  
  if("data.frame" %in% sapply(y, class)) unnest_dataframes(y)
  
  y
  
}

Energy_Imports <- ons_get_obs("trade",direction = "IM", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "3", time = "*") %>%
  as.data.frame() 

Energy_Imports <- unnest_dataframes(unnest_dataframes(Energy_Imports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))
  
Energy_Exports <- ons_get_obs("trade",direction = "EX", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "3", time = "*") %>%
  as.data.frame() 

Energy_Exports <- unnest_dataframes(unnest_dataframes(Energy_Exports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

Energy <- merge(Energy_Exports,Energy_Imports, by = "date")

Food_Imports <- ons_get_obs("trade",direction = "IM", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "0", time = "*") %>%
  as.data.frame() 

Food_Imports <- unnest_dataframes(unnest_dataframes(Food_Imports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

Food_Exports <- ons_get_obs("trade",direction = "EX", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "0", time = "*") %>%
  as.data.frame() 

Food_Exports <- unnest_dataframes(unnest_dataframes(Food_Exports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

Food <- merge(Food_Exports,Food_Imports, by = "date")

Energy_Food_Imports_Graph <- ggplot() +
  geom_line(data=subset(Energy, date > as.Date("1999-12-31")), aes(x=date,y= (value.y-value.x)/1000,color= "Energy, Mineral Fuels, Lubricants, and Related Materials)"), size = 1.25) +
  geom_line(data=subset(Food, date > as.Date("1999-12-31")), aes(x=date,y= (value.y-value.x)/1000,color= "Food and Live Animals"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B", prefix = "£"),limits = c(-1,6), breaks = c(-1,0,1,2,3,4,5,6,7,8,9,10), expand = c(0,0)) +
  ylab("Billions of Pounds, NSA") +
  ggtitle("Under-Heating") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The UK's Energy and Food Import Bill Has Skyrocketed Amidst Shortages") +
  theme_apricitas + theme(legend.position = c(.40,.70)) +
  scale_color_manual(name= "UK Net Imports, NSA",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-90-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-90-as.Date("2000-01-01"))), ymin = -1-(.3*7), ymax = -1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Energy_Food_Imports_Graph, "Energy Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


?ons_get_obs
#CPI data
CPI <- ons_get("cpih01")

#gva by industry
test <- ons_get("gva-by-industry-by-local-authority")
#test
construction <- ons_get("output-in-the-construction-industry")

retail_sales <- ons_get("retail-sales-index")

trade <- ons_get("trade")

cards <- ons_get("uk-spending-on-cards")

regional_gdp_quarter <- ons_get("regional-gdp-by-quarter")

test <- ons_ids()

ons_browse()

ons_codelists()

datasets <- ons_datasets()

#High-Energy-Intensity-Manufacturing

IOP_CHEMICAL <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k232/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1) %>%
  mutate_if(is.character,as.numeric)

IOP_PAPER <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k22v/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_FAB <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k23k/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_BASIC_METALS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k23h/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_MANU <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/kun2/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_Graph <- ggplot() + #Energy Intensive Manufacturing
  geom_line(data=subset(IOP_MANU, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "All Manufacuring ex Coke and Refined Petroleum Products"), size = 1.25) +
  geom_line(data=subset(IOP_FAB, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "Fabricated Metal Products ex Machinery and Equipment"), size = 1.25) +
  geom_line(data=subset(IOP_PAPER, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "Paper and Paper Products"), size = 1.25) +
  geom_line(data=subset(IOP_CHEMICAL, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "Chemicals and Chemical Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("25-54 Employment Rate, Index 2019 Q3 = 100") +
  ggtitle("Under-Heating") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "After a Strong Recovery, British Production Has Dwindled, Especially in Energy-Intensive Industries") +
  theme_apricitas + theme(legend.position = c(.36,.80)) +
  scale_color_manual(name= "UK Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("All Manufacuring ex Coke and Refined Petroleum Products","Fabricated Metal Products ex Machinery and Equipment","Paper and Paper Products","Chemicals and Chemical Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-90-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-90-as.Date("2019-01-01"))), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IOP_Graph, "IOP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_SERVICES <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7f5/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)
  
SERVICES_DECEMBER <- CPI_SERVICES %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)
  
CPI_SERVICES_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/icvi/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)
  
CPI_SERVICES_CONTRIB_test <- merge(CPI_SERVICES,CPI_SERVICES_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_SERVICES_CONTRIB_test2 <- merge(CPI_SERVICES_CONTRIB_test,SERVICES_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Services")

#Energy
CPI_ENERGY <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/dk9t/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)

ENERGY_DECEMBER <- CPI_ENERGY %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)

CPI_ENERGY_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/a9f3/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)

CPI_ENERGY_CONTRIB_test <- merge(CPI_ENERGY,CPI_ENERGY_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_ENERGY_CONTRIB_test2 <- merge(CPI_ENERGY_CONTRIB_test,ENERGY_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Energy")

#Food
CPI_FOOD <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bu/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)

FOOD_DECEMBER <- CPI_FOOD %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)

CPI_FOOD_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/chzr/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)

CPI_FOOD_CONTRIB_test <- merge(CPI_FOOD,CPI_FOOD_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_FOOD_CONTRIB_test2 <- merge(CPI_FOOD_CONTRIB_test,FOOD_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Food")

#Goods
CPI_GOODS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7f4/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)

GOODS_DECEMBER <- CPI_GOODS %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)

CPI_GOODS_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/icvh/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)

CPI_GOODS_CONTRIB_test <- merge(CPI_GOODS,CPI_GOODS_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_GOODS_CONTRIB_test2 <- merge(CPI_GOODS_CONTRIB_test,GOODS_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Goods")


#NEEDS TO BE ADJUSTED FOR RELATIVE CONTRIBUTION

#https://www.oecd.org/sdd/prices-ppp/OECD-calculation-contributions-annual-inflation.pdf

CPI_CONTRIB <- rbind(CPI_SERVICES_CONTRIB_test2, CPI_GOODS_CONTRIB_test2, CPI_ENERGY_CONTRIB_test2, CPI_FOOD_CONTRIB_test2) %>%
  pivot_wider(values_from = contribution,names_from = Category) %>%
  mutate(Goods = Goods - Energy - Food) %>%
  pivot_longer(cols = Services:Food) %>%
  subset(date > as.Date("2003-12-01"))

CPI_CONTRIBUTION_ANNUAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = CPI_CONTRIB, aes(x = date, y = value, fill = name), color = NA, width = 31, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.015,.11), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Inflation is Broad-Based, but Most Come From Volatile Factors Like Food, Energy, and Goods") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services","Goods","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = -0.015-(.3*.125), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_ANNUAL_GRAPH, "CPI Annual.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#UK NGDP 
NGDP_UK <- fredr(series_id = "UKNGDP",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[8]*100)

NGDP_Trend <- data.frame(date = c(seq(as.Date("2019-10-01"), tail(NGDP_UK$date, n=1), "3 months")), trend = 100*1.009853^(0:(length(seq(from = as.Date("2019-10-01"), to = tail(NGDP_UK$date, n=1), by = '3 month')) - 1)))

NGDP_TREND_graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=NGDP_UK, aes(x=date, y=value, color="UK NGDP"), size = 1.25) +
  geom_line(data=NGDP_Trend, aes(x=date,y= trend,color= "UK NGDP 4% Pre-Covid Trend"), size = .75,linetype = "dashed") + #taking 2010-2020 r and n gdp grown as trend
  xlab("Date") +
  scale_y_continuous(limits = c(80,120), breaks = c(80,90,100,110,120), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Bucking the Trend") +
  labs(caption = "Graph created by @JosephPolitano using IHS Markit data",subtitle = "British Nominal Gross Domestic Product Remains Slightly Below Trend") +
  theme_apricitas + theme(legend.position = c(.35,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +#, labels = c("PCE Price Index","Core PCE Price Index", "Trimmed Mean PCE Price Index"))+
  theme(legend.key.width =  unit(.82, "cm")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 80-(.3*40), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_TREND_graph, "NGDP Trend.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Total_Fixed_Capital <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/npqt/ukea") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na()
  
FIXED_graph <- ggplot() + #Plotting GDP Growth Rates
  annotate("vline", x = as.Date("2016-06-23"), xintercept = as.Date("2016-06-23"), color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "Brexit Vote", x = as.Date("2017-04-23"), y = 110, color = "white", linetype = "dashed", size = 5) +
  geom_line(data=subset(Total_Fixed_Capital, date > as.Date("2011-12-01")), aes(x=date, y=value/value[1]*100, color="UK Real Gross Fixed Capital Formation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,130), breaks = c(80,90,100,110,120,130,140,150), expand = c(0,0)) +
  ylab("Index, Q1 2012 = 100") +
  ggtitle("Underinvesting") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Fixed Investment Still Has Slowed and Stagnated Since the Brexit Vote") +
  theme_apricitas + theme(legend.position = c(.70,.175)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 90-(.3*40), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_graph, "Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_IMPORT_INTENSITY <- read.csv("C:/Users/josep/Documents/UK Economy Bad/CPI_IMPORT_INTENSITY.csv") %>%
  `colnames<-`(c("Date","0-10%","10-25%","25-40%","40% or More","Energy")) %>%
  pivot_longer(cols = `0-10%`:`Energy`) %>%
  mutate(Date = as.Date(Date))
  
CPI_CONTRIBUTION_IMPORT_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = CPI_IMPORT_INTENSITY, aes(x = Date, y = value/100, fill = name), color = NA, width = 31, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.015,.115), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Import-Intensive Products Are Driving a Large Amount of British Inflation") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation by Import Intensity",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("0-10%","10-25%","25-40%","40% or More","Energy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = -0.015-(.3*.13), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_IMPORT_GRAPH, "CPI Contribution Import.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#retail trade indexes

retail_sales_current <- ons_get_obs("retail-sales-index",
                geography = "K03000001", 
                prices = "current-prices-percentage-change-3-months-on-same-period-a-year-earlier",
                seasonaladjustment = "seasonal-adjustment",
                time = "*",
                unofficialstandardindustrialclassification = "all-retailing-excluding-automotive-fuel")

retail_sales_current <- unnest_dataframes(unnest_dataframes(retail_sales_current)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

retail_sales_real <- ons_get_obs("retail-sales-index",
                geography = "K03000001", 
                prices = "chained-volume-percentage-change-3-months-on-same-period-a-year-earlier",
                seasonaladjustment = "seasonal-adjustment",
                time = "*",
                unofficialstandardindustrialclassification = "all-retailing-excluding-automotive-fuel")

retail_sales_real <- unnest_dataframes(unnest_dataframes(retail_sales_real)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

RETAIL_SALES_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = subset(retail_sales_current, date > as.Date("2014-12-01")), aes(x = date, y = value/100, color = "Nominal"), size = 1.25) +
  geom_line(data = subset(retail_sales_real,date > as.Date("2014-12-01")), aes(x = date, y = value/100, color = "Real"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.10,.20), breaks = c(-.10,-0.05,0,0.05,.10,.15,.20), expand = c(0,0)) +
  ylab("Annual Growth Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Nominal Retail Sales Growth is at Pre-Pandemic Levels, But Real Growth Has Plummetted") +
  theme_apricitas + theme(legend.position = c(.40,.75)) +
  scale_color_manual(name= "UK 3 Month Rolling Retail Sales ex-Auto Fuel, Annual Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.10-(.3*.30), ymax = -0.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_SALES_graph, "Retail Sales.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BICS <- read.csv("C:/Users/josep/Documents/UK Economy Bad/BICS.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))
  
BICS_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = BICS, aes(x = date, y = Energy.prices, color = "Energy Prices"), size = 1.25) +
  #geom_line(data = BICS, aes(x = date, y = Finance.costs, color = "Finance Costs"), size = 1.25) +
  geom_line(data = BICS, aes(x = date, y = Labour.costs, color = "Labor Costs"), size = 1.25) +
  geom_line(data = BICS, aes(x = date, y = Raw.material.prices, color = "Raw Material Prices"), size = 1.25) +
  #geom_line(data = BICS, aes(x = date, y = Other, color = "Other"), size = 1.25) +
  #geom_line(data = BICS, aes(x = date, y = Not.sure, color = "Not Sure"), size = 1.25) +
  geom_line(data = BICS, aes(x = date, y = Business.is.not.considering.raising.prices, color = "Not Considering Raising Prices"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(.10,.20,.30,.40,.50), expand = c(0,0)) +
  scale_x_date(date_labels = "%Y-%b") +
  ylab("Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Most Businesses Cite Energy Prices as the Biggest Reason They're Considering Raising Prices") +
  theme_apricitas + theme(legend.position = c(.5,.2)) +
  scale_color_manual(name= "UK Businesses: Which Factors Are Causing Your Business to Consider Raising Prices?",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Energy Prices","Raw Material Prices","Labor Costs","Not Considering Raising Prices")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-04-01")-(.1861*(today()-as.Date("2022-04-01"))), xmax = as.Date("2022-04-01")-(0.049*(today()-as.Date("2022-04-01"))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BICS_graph, "BICS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

AVERAGE_PRIVATE_WAGES <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/kaj3/lms") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %B")), value) %>%
  mutate_if(is.character,as.numeric)

AVERAGE_PRIVATE_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = AVERAGE_PRIVATE_WAGES, aes(x = date, y = value/100, color = "Growth in Avg. Weekly Earnings, Private Sector Regular Pay Ex Arrears"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.015,.09), breaks = c(0,.03,.06,.09), expand = c(0,0)) +
  ylab("Annual Growth Percent") +
  ggtitle("The British Labour Shortage") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Wage Growth Has Hit Multi-Decade Highs") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = -0.015-(.3*.105), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AVERAGE_PRIVATE_graph, "Average Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
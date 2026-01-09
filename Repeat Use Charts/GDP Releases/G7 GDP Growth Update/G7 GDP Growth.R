pacman::p_load(purrr,readabs,jsonlite,eurostat,statcanR,cansim,rsdmx,keyring,wiesbaden,insee,ggpubr,sf,onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user=Sys.getenv("DE647235M8"), password=Sys.getenv("PleaseWork23")))
save_credentials(db='de', user="DE647235M8", password="PleaseWork23")
test_login(genesis=c(db='de'))

FRANCE_GDP_INSEE_list_selected =
  get_idbank_list("CNT-2020-PIB-EQB-RF") %>% # Gross domestic product balance
  filter(OPERATION_label_en == "GDP - Gross domestic product") %>%
  filter(FREQ == "T") %>% #quarter
  add_insee_title() %>% #add titles
  filter(cleFlow == "T.CNT-EQUILIBRE_PIB.SO.PIB.SO.VALEUR_ABSOLUE.FE.L.EUROS.CVS-CJO.FALSE")#GDP

US <- fredr(series_id = "GDPC1",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100) %>%
  select(date,value)

#MUST BE CONVERTED TO ENDING IN "PN2" FOR FIRST QUARTERLY ESTIMATE
#MUST BE CONVERTED TO ENDING IN "UKEA" FOR REVISED QUARTERLY ESTIMATE

UK <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/abmi/ukea") %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

# UK <- data.frame(date = seq.Date(from = as.Date("2018-01-01"), by = "3 months", length.out = 22), 
#   value = c(547003,549491,552545,553966,557458,558071,561480,561339,546515,431794,503509,509621,504255,537175,546487,554821,557524,557810,557286,558005,558812,559956)) %>%
#   mutate(value = value/value[7]*100)

# UK <- fredr(series_id = "NGDPRSAXDCGBQ",observation_start = as.Date("2018-01-01")) %>%
#   mutate(value = value/value[7]*100)

GER <- read.csv("https://api.statistiken.bundesbank.de/rest/download/BBKRT/Q.DE.Y.A.AG1.CA010.A.I?format=csv&lang=en") %>%
  select(ncol(.)) %>%
  mutate_at(vars(ncol(.)), as.numeric) %>% 
  drop_na() %>%
  slice(-(1:2)) %>%
  setNames("value") %>%
  mutate(date = seq.Date(from = as.Date("1991-01-01"), by = "3 months", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)


ITA <- fredr("CLVMNACSCAB1GQIT", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

ITA_BULK <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_156_DF_DCCN_SQCQ_1,1.0/Q...../ALL/?detail=full&startPeriod=2018-01-01&dimensionAtObservation=TIME_PERIOD"))

ITA <- ITA_BULK %>%
  subset(VALUATION == "L_2020") %>%
  subset(DATA_TYPE_AGGR == "B1GQ_B_W2_S1") %>%
  subset(ADJUSTMENT == "Y") %>%
  mutate(PRELIMINARY = grepl("_1$", EDITION),
         EDITION_CLEAN = gsub("_1$", "", EDITION),
         EDITION_DATE = ymd(paste0(sub("M", "-", EDITION_CLEAN), "-01")),
         EDITION_NUM = as.integer(gsub("M", "", gsub("M", "0", EDITION_CLEAN)))) %>%
  #arrange(desc(EDITION_NUM), PRELIMINARY) %>%
  #filter(EDITION_NUM == max(EDITION_NUM)) %>%
  arrange(desc(EDITION_DATE), PRELIMINARY) %>%
  filter(EDITION_DATE == max(EDITION_DATE)) %>%
  group_by(EDITION_NUM) %>%
  mutate(n = n()) %>%
  filter(EDITION_NUM == max(EDITION_NUM)) %>%
  ungroup() %>%
  #filter(ifelse(n > 1, PRELIMINARY, TRUE)) %>%
  select(-n) %>%
  transmute(date = as.Date(as.yearqtr(obsTime, "%Y-Q%q")),value = obsValue/obsValue[7]*100)

# ITA <- fredr(series_id = "CLVMNACSCAB1GQIT",observation_start = as.Date("2018-01-01")) %>%
#    mutate(value = value/value[7]*100)
FRA <- FRANCE_GDP_INSEE_list_selected %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  arrange(date) %>%
  mutate(value = value/value[7]*100)
#Update links from here: https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2024/qe243/gdemenuea.html
JPN <- read.csv("https://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2025/qe253_2/tables/gaku-jk2532.csv",fileEncoding="latin1") %>%
  slice(-1:-6) %>%
  select(X) %>%
  transmute(date = seq.Date(from = as.Date("1994-01-01"), by = "quarter", length.out = nrow(.)), value = as.numeric(gsub(",","",X))) %>%
  drop_na() %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

# JPN <- fredr(series_id = "JPNRGDPEXP",observation_start = as.Date("2018-01-01")) %>%
#   mutate(value = value/value[7]*100) %>%
#   select(date, value)

#https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2023/qe232/gdemenuea.html

CAN <- statcan_data("36-10-0104-01", "eng") %>%
  filter(GEO == "Canada", Prices == "Chained (2017) dollars", Estimates == "Gross domestic product at market prices", `Seasonal adjustment` == "Seasonally adjusted at annual rates", REF_DATE  >= as.Date("2018-01-01")) %>%
  transmute(date = REF_DATE, value = VALUE/VALUE[7]*100)

# CAN <- get_cansim_vector("v62305752") %>%
#   subset(REF_DATE >= as.Date("2018-01-01")) %>%
#   transmute(date = Date, value = VALUE/VALUE[7]*100)

# CAN <- fredr(series_id = "NGDPRSAXDCCAQ",observation_start = as.Date("2018-01-01")) %>%
#   mutate(value = value/value[7]*100)


AUS_GDP <- read_abs(series_id = "A2304402X") %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(date = date - 60) %>%
  mutate(value = value/value[7]*100)

RGDP_G7_Graph <- ggplot() + #RGDP Index
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
  #geom_line(data=SPA_GDP, aes(x=date,y= value,color= "Spain"), size = 1.25) +
  geom_line(data=UK, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN, aes(x=date,y= value,color= "Canada"), size = 1.25) +
  geom_line(data=GER, aes(x=date,y= value,color= "Germany"), size = 1.25) +
  geom_line(data=ITA, aes(x=date,y= value,color= "Italy"), size = 1.25) +
  geom_line(data=FRA, aes(x=date,y= value,color= "France"), size = 1.25) +
  geom_line(data=JPN, aes(x=date,y= value,color= "Japan"), size = 1.25) +
  geom_line(data=US, aes(x=date,y= value,color= "United States"), size = 1.25) +
  annotate("text",label = "Pre-COVID GDP", x = as.Date("2019-01-01"), y =101.5, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(72.5,117.5), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("Real GDP Growth in the G7") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED",subtitle = "The US is Leading the Recovery, and All Countries but Germany are Now Above pre-COVID GDP") +
  theme_apricitas + theme(legend.position = c(.17,.26), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Real GDP\n2019 Q3 = 100",values = c("#B30089","#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("Spain","United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 72.5-(.3*45), ymax = 72.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RGDP_G7_Graph, "G7 Total.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

combined_df <- reduce(list(ITA, GER, FRA,JPN,US,UK,CAN), full_join, by = "date") %>% rename_with(~c("date", "ITA", "GER", "FRA", "JPN","US","UK","CAN"))


#PER CAPITA

US_PER_CAPITA <- fredr(series_id = "A939RX0Q048SBEA",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100) %>%
  select(date,value)

#change to pn2 for first estimates
UK_PER_CAPITA <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ihxw/ukea") %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

GER_PER_CAPITA <- retrieve_data(tablename = "81000BV007", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPKM") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, STR006_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = STR006_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

EU_GDP_BULK <- get_eurostat("namq_10_gdp",legacy_bulk_download = FALSE)

EU_POP_BULK <- get_eurostat("namq_10_pe",legacy_bulk_download = FALSE)

SPA_GDP <- EU_GDP_BULK %>%
  filter(s_adj == "SCA", geo == "ES", TIME_PERIOD >= as.Date("2018-01-01"), na_item == "B1GQ", unit == "CLV20_MEUR") %>%
  transmute(date = TIME_PERIOD, value = values) %>%
  mutate(value = value/value[7]*100)

SPA_POP <- EU_POP_BULK %>%
  filter(s_adj == "SCA", geo == "ES", TIME_PERIOD >= as.Date("2018-01-01"), na_item == "POP_NC", unit == "THS_PER") %>%
  transmute(date = TIME_PERIOD, value = values)

SPA_PER_CAPITA <- merge(SPA_POP,SPA_GDP, by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[7]*100)

ITA_POP <- EU_POP_BULK %>%
  filter(s_adj == "SCA", geo == "IT", TIME_PERIOD >= as.Date("2018-01-01"), na_item == "POP_NC", unit == "THS_PER") %>%
  transmute(date = TIME_PERIOD, value = values)

FRA_POP <- EU_POP_BULK %>%
  filter(s_adj == "SA", geo == "FR", TIME_PERIOD >= as.Date("2018-01-01"), na_item == "POP_NC", unit == "THS_PER") %>%
  transmute(date = TIME_PERIOD, value = values)

GER_POP <- EU_POP_BULK %>%
  filter(s_adj == "SCA", geo == "DE", TIME_PERIOD >= as.Date("2018-01-01"), na_item == "POP_NC", unit == "THS_PER") %>%
  transmute(date = TIME_PERIOD, value = values)

GER_PER_CAPITA <- merge(GER_POP,GER, by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[7]*100)


ITA_PER_CAPITA <- merge(ITA_POP,ITA, by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[7]*100)

FRA_PER_CAPITA <- merge(FRA_POP,FRA,by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[7]*100)

CAN_POP <- statcan_data("17-10-0009-01", "eng") %>%
  filter(GEO=="Canada", REF_DATE >= as.Date("2018-01-01")) %>%
  transmute(date = REF_DATE, value = VALUE)

CAN_PER_CAPITA <- merge(CAN_POP,CAN, by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[7]*100)

JAPAN_POP <- fromJSON("https://dashboard.e-stat.go.jp/api/1.0/Json/getData?Lang=EN&IndicatorCode=0201010000000010000")$GET_STATS[3]$STATISTICAL_DATA$DATA_INF$DATA_OBJ$VALUE %>%  
  filter(`@indicator`=="0201010000000010000", `@cycle` == "1") %>%
  mutate(`@time` = ifelse(substr(`@time`, 7, 8) == "00", paste0(substr(`@time`, 1, 6), "01"), `@time`)) %>% 
  mutate(`@time` = as.Date(`@time`, format="%Y%m%d")) %>%
  transmute(date = `@time`, value = as.numeric(`$`)) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(quarter = quarter(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year, quarter) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01")))
  
JPN_PER_CAPITA <- merge(JAPAN_POP,JPN, by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[7]*100)

RGDP_G7_Per_Capita_Graph <- ggplot() + #RGDP Index
  #geom_line(data=SPA_PER_CAPITA, aes(x=date,y= value,color= "Spain"), size = 1.25) +
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
  geom_line(data=UK_PER_CAPITA, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_PER_CAPITA, aes(x=date,y= value,color= "Canada"), size = 1.25) +
  geom_line(data=GER_PER_CAPITA, aes(x=date,y= value,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_PER_CAPITA, aes(x=date,y= value,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_PER_CAPITA, aes(x=date,y= value,color= "France"), size = 1.25) +
  geom_line(data=JPN_PER_CAPITA, aes(x=date,y= value,color= "Japan"), size = 1.25) +
  geom_line(data=US_PER_CAPITA, aes(x=date,y= value,color= "United States"), size = 1.25) +
  annotate("text",label = "Pre-COVID GDP Per Capita", x = as.Date("2018-10-01"), y =101.5, color = "white", size = 4) +
  #annotate(geom = "text", label = "USE FIGURES WITH CAUTION:\n Ukrainian Refugees Boosted Pop Growth Significantly, Especially in Germany (~1.2%),\n But Also in Canada (~0.5%), Italy (~0.3%), the UK (~0.2%), and France (~0.2%)", x = as.Date("2020-03-15"), y = 107.5, color ="white", size = 4, alpha = 0.75,lineheight = 0.9) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(72.5,112.5), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("Real GDP Per Capita Growth in the G7") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED & National Databases",subtitle = "The US is Leading the Recovery, and Germany Has Had the Largest GDP Per Capita Decline") +
  theme_apricitas + theme(legend.position = c(.16,.31), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Real GDP Per Capita\n2019 Q3 = 100",values = c("#B30089","#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("Spain","United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 72.5-(.3*40), ymax = 72.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RGDP_G7_Per_Capita_Graph, "G7 Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
  
combined_df <- reduce(list(ITA_PER_CAPITA, GER_PER_CAPITA, FRA_PER_CAPITA,JPN_PER_CAPITA,US_PER_CAPITA,UK_PER_CAPITA,CAN_PER_CAPITA), full_join, by = "date") %>% rename_with(~c("date", "ITA", "GER", "FRA", "JPN","US","UK","CAN"))

US_PER_CAPITA <- fredr(series_id = "A939RX0Q048SBEA",observation_start = as.Date("1995-01-01")) %>%
  mutate(value = value/value[99]*100) %>%
  select(date,value)

EU_GDP <- EU_GDP_BULK %>%
  filter(s_adj == "SCA", geo == "EU27_2020", TIME_PERIOD >= as.Date("1995-01-01"), na_item == "B1GQ", unit == "CLV20_MEUR") %>%
  transmute(date = TIME_PERIOD, value = values) %>%
  mutate(value = value/value[99]*100)

EU_POP <- EU_POP_BULK %>%
  filter(geo == "EU27_2020", TIME_PERIOD >= as.Date("1995-01-01"), na_item == "POP_NC", unit == "THS_PER") %>%
  transmute(date = TIME_PERIOD, value = values)

EU_PER_CAPITA <- merge(EU_POP,EU_GDP, by = "date") %>%
  transmute(date, value = value.y/value.x) %>%
  mutate(value = value/value[99]*100)

EUvsUSGDP_PER_CAPITA <- ggplot() + #RGDP Index
  geom_line(data=EU_PER_CAPITA, aes(x=date,y= value,color= "European Union"), size = 1.25) +
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
  geom_line(data=US_PER_CAPITA, aes(x=date,y= value,color= "United States"), size = 1.25) +
  annotate("text",label = "Pre-COVID GDP Per Capita", x = as.Date("2010-10-01"), y =101.5, color = "white", size = 4) +
  #annotate(geom = "text", label = "USE FIGURES WITH CAUTION:\n Ukrainian Refugees Boosted Pop Growth Significantly, Especially in Germany (~1.2%),\n But Also in Canada (~0.5%), Italy (~0.3%), the UK (~0.2%), and France (~0.2%)", x = as.Date("2020-03-15"), y = 107.5, color ="white", size = 4, alpha = 0.75,lineheight = 0.9) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,112.5), breaks = c(60,70,80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("Real GDP Per Capita Growth US vs EU") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED & National Databases",subtitle = "The US has Seen Much Faster Per-Capita Growth Than The EU") +
  theme_apricitas + theme(legend.position = c(.16,.61), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Real GDP Per Capita\n2019 Q3 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","European Union")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-90-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-90-as.Date("1995-01-01"))), ymin = 60-(.3*52.5), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EUvsUSGDP_PER_CAPITA, "EU vs US Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


write.csv(combined_df,"GDP_PER_CAPITA_G7.csv")
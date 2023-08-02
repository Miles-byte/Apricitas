pacman::p_load(statcanR,cansim,rsdmx,keyring,wiesbaden,insee,ggpubr,sf,onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user="DEM56460DY", password="XSYhTyP4JV4!Q4b"))
save_credentials(db= 'de',user="DEM56460DY", password="XSYhTyP4JV4!Q4b")

FRANCE_GDP_INSEE_list_selected =
  get_idbank_list("CNT-2014-PIB-EQB-RF") %>% # Gross domestic product balance
  filter(OPERATION_label_en == "GDP - Gross domestic product") %>%
  filter(FREQ == "T") %>% #quarter
  add_insee_title() %>% #add titles
  filter(cleFlow == "T.CNT-EQUILIBRE_PIB.SO.PIB.SO.VALEUR_ABSOLUE.FE.L.EUROS.CVS-CJO")#GDP

US <- fredr(series_id = "GDPC1",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

UK <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/abmi/ukea") %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

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


ITA <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_156_DF_DCCN_SQCQ_3,1.0/Q...../ALL/?detail=full&startPeriod=2018-01-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(NOTE_VALUATION == "VAL__L_2015_N2") %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  transmute(value = obsValue/obsValue[7]*100, date = as.Date(as.yearqtr(obsTime, "%Y-Q%q")))


# ITA <- fredr(series_id = "CLVMNACSCAB1GQIT",observation_start = as.Date("2018-01-01")) %>%
#   mutate(value = value/value[7]*100)
FRA <- FRANCE_GDP_INSEE_list_selected %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  arrange(date) %>%
  mutate(value = value/value[7]*100)

JPN <- fredr(series_id = "JPNRGDPEXP",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

CAN <- get_cansim_vector("v62305752") %>%
  subset(REF_DATE >= as.Date("2018-01-01")) %>%
  transmute(date = Date, value = VALUE/VALUE[7]*100)

# CAN <- fredr(series_id = "NGDPRSAXDCCAQ",observation_start = as.Date("2018-01-01")) %>%
#   mutate(value = value/value[7]*100)


AUS_GDP <- read_abs(series_id = "A2304402X") %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(date = date - 60) %>%
  mutate(value = value/value[7]*100)

RGDP_G7_Graph <- ggplot() + #RGDP Index
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
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
  ggtitle("GDP Growth in the G7") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED",subtitle = "The US is Leading the Recovery, with Japan and the UK Still Below pre-COVID GDP") +
  theme_apricitas + theme(legend.position = c(.22,.30)) +
  scale_color_manual(name= "Real GDP 2019 Q3 = 100",values = c("#B30089","#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("Australia","United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 75-(.3*35), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RGDP_G7_Graph, "G7 Renamed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

test <- read.csv("https://esploradati.istat.it/SDMXWS/rest/dataflow/IT1/163_156_DF_DCCN_SQCQ_3/1.0/?detail=Full&references=Descendants")

  

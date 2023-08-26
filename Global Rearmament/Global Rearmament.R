pacman::p_load(bea.R,insee,seasonal,eurostat,rsdmx,wiesbaden,keyring,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD")))
save_credentials(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD"))

test <- get_idbank_list("ENQ-CONJ-ACT-IND")

?get_idb

INSEE_DATA_SELECTED <- get_dataset_list() 

ENQ-CONJ-ACT-IND OUTLOOK SURVEY IN GOODS PRODUCING INDUSTRIES

ENQ-CONJ-INV-IND INDUSTRIAL INVESTMENT

IPI-2015 INDUSTRIAL PRODUCTION

EUROSTAT_QUARTERLY_INDPRO_BULK <- get_eurostat("sts_inpr_q")

EU_WEAPON_INDPRO <- EUROSTAT_QUARTERLY_INDPRO_BULK %>%
  filter(geo == "EU27_2020" & unit == "I15" & s_adj == "SCA" & nace_r2 == "C254" & time >= as.Date("2018-01-01")) %>%
  transmute(date = time, value = values) %>%
  arrange(date)

FRANCE_INDPRO_WEAPONS_INSEE <- get_idbank_list("IPI-2015") %>%
  filter(cleFlow == "M.BDM_EUR.IPI.SO.25-4.INDICE.FM.SO.CVS-CJO.2015") %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  filter(date >= as.Date("2018-01-01"))

ITALY_INDPRO_WEAPONS_ISTAT <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,115_333_DF_DCSC_INDXPRODIND_1_1,1.0/M..IND_PROD2.Y.254/ALL/?detail=full&dimensionAtObservation=TIME_PERIOD")) %>%
  transmute(value = obsValue, date = as.Date(as.yearmon(obsTime, "%Y-%m"))) %>%
  filter(date >= as.Date("2018-01-01"))
  
UK_INDPRO_WEAPONS_ONS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k23m/diop") %>%
  setNames(c("date", "value")) %>%
  filter(nchar(date) == 8) %>%
  transmute(date = as.Date(paste0(date, " 01"), format = "%Y %b %d"), value = as.numeric(value)) %>%
  filter(date >= as.Date("2018-01-01"))

GERMANY_INDPRO_WEAPONS_DESTATIS <- retrieve_data(tablename = "42153BM003", genesis=c(db='de')) %>%
  filter(WZ08V3 == "WZ08-254") %>% #taking manufacturing and energy intensive manufacturing data 
  filter(WERT03 %in% c("X13JDKSB","BV4KSB","BV4TB")) %>%#calendar and seasonally adjusted
  mutate(MONAT = gsub("MONAT","",MONAT)) %>%
  mutate(date = as.Date(paste0(JAHR,"-", MONAT,"-01"))) %>%
  transmute(date, value = PRO101_val, category = WZ08V3, seasonal = WERT03) %>%
  pivot_wider(names_from = seasonal) %>%
  arrange(date) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  transmute(date, value = BV4KSB)

EU_WEAPON_MANUFACTURING_graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=EU_WEAPON_INDPRO, aes(x=date,y= value/value[16]*100,color="EU Industrial Production of Weapons and Ammunition"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,120), expand = c(0,0)) +
  ylab("Index, Q4 2021 = 100") +
  ggtitle("Europe's Rearmament") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "Weapon and Ammunition Output is Rising in the EU in the Wake of Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*45), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_WEAPON_MANUFACTURING_graph, "EU Weapon Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_WEAPON_BREAKDOWN_MANUFACTURING_graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=ITALY_INDPRO_WEAPONS_ISTAT, aes(x=date,y= value/value[46]*100,color="Italy"), size = 1.25) +
  geom_line(data=UK_INDPRO_WEAPONS_ONS, aes(x=date,y= value/value[46]*100,color="United Kingdom"), size = 1.25) +
  geom_line(data=FRANCE_INDPRO_WEAPONS_INSEE, aes(x=date,y= value/value[46]*100,color="France"), size = 1.25) +
  geom_line(data=GERMANY_INDPRO_WEAPONS_DESTATIS, aes(x=date,y= value/value[46]*100,color="Germany"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,200), expand = c(0,0)) +
  ylab("Index, Oct 2022 = 100") +
  ggtitle("Europe's Rearmament") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis, ONS, IStat, and Insee Data",subtitle = "Weapon and Ammunition Output is Rising in Germany in the Wake of Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Industrial Production, Weapons and Ammunition",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Germany","France","United Kingdom","Italy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_WEAPON_BREAKDOWN_MANUFACTURING_graph, "Europe Manufacturing Breakdown.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

REAL_DEFENSE_US_EXP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T31106',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_DEFENSE_US_EXP <- beaGet(REAL_DEFENSE_US_EXP_SPECS, iTableStyle = FALSE) %>%
  rename_with(~gsub("^T31106", "", .x)) %>%
  mutate(date = (seq(as.Date("2002-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

MISSILES_AMMUNITION_US_GRAPH <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=REAL_DEFENSE_US_EXP, aes(x=date,y= b875rx_33_missiles_chained_dollars_level_6/1000,color="Missiles"), size = 1.25) +
  geom_line(data=REAL_DEFENSE_US_EXP, aes(x=date,y= b781rx_19_ammunition_chained_dollars_level_6/1000,color="Ammunition"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,9), expand = c(0,0)) +
  ylab("Billions of 2012 Dollars") +
  ggtitle("America's Arms Spending") +
    labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "American Arms Consumption Is Slightly Below Late-2020 Levels Despite Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Real Defense Spending on New Military Goods",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Ammunition","Missiles")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*9), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MISSILES_AMMUNITION_US_GRAPH, "Missiles Ammunition US Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


NOMINAL_DEFENSE_US_EXP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T31105',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1972, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

NOMINAL_DEFENSE_US_EXP <- beaGet(NOMINAL_DEFENSE_US_EXP_SPECS, iTableStyle = FALSE) %>%
  rename_with(~gsub("^T31105", "", .x)) %>%
  mutate(date = (seq(as.Date("1972-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  select(date,b781rc_19_ammunition_current_dollars_level_6,b874rc_32_aircraft_current_dollars_level_6,b875rc_33_missiles_current_dollars_level_6,b876rc_34_ships_current_dollars_level_6,b877rc_35_vehicles_current_dollars_level_6,y051rc_36_electronics_current_dollars_level_6) %>%
  setNames(c("date","Ammunition","Aircraft","Missiles","Ships","Vehicles","Electronics")) %>%
  filter(., date >= as.Date("2018-01-01")) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = c("Ammunition","Vehicles","Missiles","Electronics","Ships","Aircraft")))

NOMINAL_DEFENSE_US_EXP_2018 <- ggplot(NOMINAL_DEFENSE_US_EXP, aes(fill=name, x=date, y=value/1000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,75), breaks = c(0,25,50,75),expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Nominal Defense Spending\nSelect New Military Goods") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = "right") +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = rev(c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*75), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOMINAL_DEFENSE_US_EXP_2018, "Nominal Defense Spending US Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GDP <- fredr(series_id = "GDP", observation_start = as.Date("1972-01-01")) %>%
  transmute(GDP = value*1000, date)

NOMINAL_DEFENSE_US_PCT_1972 <- beaGet(NOMINAL_DEFENSE_US_EXP_SPECS, iTableStyle = FALSE) %>%
  rename_with(~gsub("^T31105", "", .x)) %>%
  mutate(date = (seq(as.Date("1972-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  select(date,b781rc_19_ammunition_current_dollars_level_6,b874rc_32_aircraft_current_dollars_level_6,b875rc_33_missiles_current_dollars_level_6,b876rc_34_ships_current_dollars_level_6,b877rc_35_vehicles_current_dollars_level_6,y051rc_36_electronics_current_dollars_level_6,) %>%
  setNames(c("date","Ammunition","Aircraft","Missiles","Ships","Vehicles","Electronics")) %>%
  merge(.,GDP, by = "date") %>%
  mutate(across(where(is.numeric) & !names(. %in% "GDP"), ~ ./GDP)) %>%
  select(-GDP) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = c("Ammunition","Vehicles","Missiles","Electronics","Ships","Aircraft")))

NOMINAL_DEFENSE_US_PCT_1972_Graph <- ggplot(NOMINAL_DEFENSE_US_PCT_1972, aes(fill=name, x=date, y=value)) + 
  geom_bar(position="stack", stat="identity", width = 93, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25),limits = c(0,0.011), breaks = c(0,0.0025,0.005,0.0075,0.01),expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("US Nominal Defense Spending\nSelect New Military Goods, % of GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "") +
  theme_apricitas + theme(legend.position = "right") +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = rev(c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1972-01-01")-(.1861*(today()-as.Date("1972-01-01"))), xmax = as.Date("1972-01-01")-(0.049*(today()-as.Date("1972-01-01"))), ymin = 0-(.3*.011), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOMINAL_DEFENSE_US_PCT_1972_Graph, "Nominal Defense Spending US PCT GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UNFILLED_ORDERS_US_MANU <- fredr(series_id = "UDEFUO", observation_start = as.Date("2018-01-01"))

UNFILLED_ORDERS_US_MANU_Graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=UNFILLED_ORDERS_US_MANU, aes(x=date,y= value/1000,color="US Manufacturers' Unfilled Orders, Defense Capital Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(150,(ceiling(max(UNFILLED_ORDERS_US_MANU$value/1000)/10)*10)), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Arms Spending") +
  labs(caption = "Graph created by @JosephPolitano using Census Data",subtitle = "US Weapon Backlogs Have Increased Marginally in the Wake of Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.5,.125)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 150-(.3*((ceiling(max(UNFILLED_ORDERS_US_MANU$value/1000)/10)*10)-150)), ymax = 150) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNFILLED_ORDERS_US_MANU_Graph, "Unfilled Orders US Manufacturing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

VALUE_SHIPMENTS_US_MANU <- fredr(series_id = "ADEFVS", observation_start = as.Date("2018-01-01"))

VALUE_SHIPMENTS_US_MANU_Graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=VALUE_SHIPMENTS_US_MANU, aes(x=date,y= value/1000,color="US Manufacturers' Value of Shipments, Defense Capital Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(10,(ceiling(max(VALUE_SHIPMENTS_US_MANU$value/1000)))), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Arms Spending") +
  labs(caption = "Graph created by @JosephPolitano using Census Data",subtitle = "US Weapon Shipments Have Increased 12% in the Wake of Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.4,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 10-(.3*((ceiling(max(VALUE_SHIPMENTS_US_MANU$value/1000)))-10)), ymax = 10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VALUE_SHIPMENTS_US_MANU_Graph, "value Shipments US Manufacturers' Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Fed <- beaSearch("International Presentations", beaKey = Sys.getenv("BEA_KEY"))

NATDEFENSE <- fredr(series_id = "FDEFX") 
GDP <- fredr(series_id = "GDP")

DEFENSE_SHARE_GDP <- merge(NATDEFENSE,GDP, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  drop_na()

NOMINAL_DEFENSE_US_PCT_1947_Graph <- ggplot() + 
  geom_line(data=DEFENSE_SHARE_GDP, aes(x=date,y= value,color="US National Defense Spending, % of GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.175), breaks = c(0,.05,.10,.15,.20),expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("US Defense Spending as a Share of GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Defense Spending as a Share of GDP is at Late-1990s Peace-Dividend Lows") +
  theme_apricitas + theme(legend.position = c(0.4,0.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*(today()-as.Date("1947-01-01"))), xmax = as.Date("1947-01-01")-(0.049*(today()-as.Date("1947-01-01"))), ymin = 0-(.3*.175), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOMINAL_DEFENSE_US_PCT_1947_Graph, "Defense Share of GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GFS_data <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/c16198276ec7c5a20223555907dda6e18036f97c/Global%20Rearmament/FEDERAL_GOV_FINANCE_STATS.csv") %>%
  mutate(date = as.Date(date))

GFS_data_grants_Graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=GFS_data, aes(x=date,y= grants_foreign/1000,color="US Federal Grants to Foreign Governments"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,125), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("America's International Goverment Aid") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "US Bilateral Grants to Foreign Governments Have Increased in the Wake of Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.4,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GFS_data_grants_Graph, "GFS Data Grants Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GFS_data_weapon_inventories_Graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=GFS_data, aes(x=date,y= weapon_systems/1000,color="Federal Government, Net Acquisition of Weapon Equipment"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_line(data=GFS_data, aes(x=date,y= inventories/1000,color="Inventories"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(-10,25), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("America's Net Military Equipment Sales") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "The US is Selling or Transferring More Weapon Equipment Than it's Acquiring") +
  theme_apricitas + theme(legend.position = c(.4,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -10-(.3*35), ymax = -10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GFS_data_weapon_inventories_Graph, "GFS Data weapon inventories Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
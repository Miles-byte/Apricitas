pacman::p_load(jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

devtools::install_github("oddworldng/INEbaseR")
library(INEbaseR)
#Spain INE Database API

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD")))
save_credentials(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD"))

UK_GDP <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/abmi/ukea") %>%
  `colnames<-`(c("time","values")) %>%
  transmute(time = as.Date(as.yearqtr(time, "%Y Q%q")), values) %>%
  subset(., values > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  mutate(geo = "GB") %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

DE_GDP <- read.csv("https://api.statistiken.bundesbank.de/rest/download/BBKRT/Q.DE.Y.A.AG1.CA010.A.I?format=csv&lang=en") %>%
  select(ncol(.)) %>%
  mutate_at(vars(ncol(.)), as.numeric) %>% 
  drop_na() %>%
  slice(-(1:2)) %>%
  setNames("values") %>%
  mutate(time = seq.Date(from = as.Date("1991-01-01"), by = "3 months", length.out = nrow(.))) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  mutate(geo = "DE") %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

EU_GDP <- get_eurostat("namq_10_gdp")

IT_GDP <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_156_DF_DCCN_SQCQ_3,1.0/Q...../ALL/?detail=full&startPeriod=2019-07-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(NOTE_VALUATION == "VAL__L_2015_N2") %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  transmute(values = obsValue, time = as.Date(as.yearqtr(obsTime, "%Y-Q%q"))) %>%
  mutate(geo = "IT") %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

AL_GDP <- read.csv("http://databaza.instat.gov.al/sq/feca8039-955f-45fd-afc7-9fd78a2a1690") %>%
  .[-2,] %>%
  transpose() %>%
  transmute(time = seq.Date(from = as.Date("2007-04-01"), by = "3 months", length = nrow(.)), values = as.numeric(V1), geo = "AL") %>%
  drop_na() %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

IS_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & s_adj == "SA" & na_item == "B1GQ" & geo %in% c("IS")) %>%
  select(geo, time, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

ME_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & na_item == "B1GQ" & geo %in% c("ME")) %>%
  select(geo, time, values) %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time) %>%
  select(-time) %>%
  ts(., start = c(2006,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(time = seq(from = as.Date("2006-01-01"), by = "3 month", length = nrow(.)), values = x, geo = "ME") %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

BA_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & na_item == "B1GQ" & geo %in% c("BA")) %>%
  select(geo, time, values) %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time) %>%
  select(-time) %>%
  ts(., start = c(2006,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(time = seq(from = as.Date("2006-01-01"), by = "3 month", length = nrow(.)), values = x, geo = "BA") %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

XK_GDP <- read.csv("https://askdata.rks-gov.net/sq/8980eb97-7d70-456d-be5c-30a9a576e31f") %>%
  transpose() %>%
  mutate(V1 = as.numeric(V1)) %>%
  drop_na() %>%
  arrange(desc(row_number())) %>%
  ts(., start = c(2011,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(time = seq.Date(from = as.Date("2011-01-01"), by = "3 months", length = nrow(.)), values = as.numeric(x), geo = "XK") %>%
  drop_na() %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

EU_GDP_CAGR <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & s_adj == "SCA" & na_item == "B1GQ") %>%
  select(geo, time, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time)) %>%
  subset(geo != c("UK","DE","IT")) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  rbind(.,IS_GDP,UK_GDP,AL_GDP,ME_GDP,BA_GDP,XK_GDP,DE_GDP,IT_GDP)

EU_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2)

# EU_SHP <- get_eurostat_geospatial(resolution = 20, 
#                                  nuts_level = 0, 
#                                  year = 2021) %>%
#   st_transform(., crs = 3035) %>%
#   st_as_sf() 

EU_GDP_CAGR_SHP <- inner_join(EU_GDP_CAGR, EU_SHP, by = "geo") %>%
  select(geometry, CAGR) %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  st_as_sf()

EU_GDP_CAGR_SHP_GRAPH <- ggplot() +
  geom_sf(data = EU_GDP_CAGR_SHP, color = NA, aes(fill = CAGR_bucket)) +
  geom_sf(data = EU_GDP_CAGR_SHP, color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("  Annualized Real GDP
 Growth Since Q3 2019") +
  scale_x_continuous(limits = c(2800000, 7150000)) +
  scale_y_continuous(limits = c(1380000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = c(.75,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank())

ggsave(dpi = "retina",plot = EU_GDP_CAGR_SHP_GRAPH, "EU GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


INSEE_dataset_list = get_dataset_list()

FRANCE_GDP_INSEE_list_selected <-
  get_idbank_list("CNT-2014-PIB-EQB-RF") %>% # Gross domestic product balance
  filter(OPERATION_label_en == "GDP - Gross domestic product") %>%
  filter(FREQ == "T") %>% #quarter
  add_insee_title() %>% #add titles
  filter(cleFlow == "T.CNT-EQUILIBRE_PIB.SO.PIB.SO.VALEUR_ABSOLUE.FE.L.EUROS.CVS-CJO")#GDP

FRANCE_GDP_INSEE <- FRANCE_GDP_INSEE_list_selected %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  arrange(date)

GERMANY_GDP_BUNDESBANK <- read.csv("https://api.statistiken.bundesbank.de/rest/download/BBKRT/Q.DE.Y.A.AG1.CA010.A.I?format=csv&lang=en") %>%
  select(ncol(.)) %>%
  mutate_at(vars(ncol(.)), as.numeric) %>% 
  drop_na() %>%
  slice(-(1:2)) %>%
  setNames("value") %>%
  mutate(date = seq.Date(from = as.Date("1991-01-01"), by = "3 months", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-01-01"))

ITALY_GDP_ISTAT <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_156_DF_DCCN_SQCQ_3,1.0/Q...../ALL/?detail=full&startPeriod=2018-01-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(NOTE_VALUATION == "VAL__L_2015_N2") %>%
  subset(EDITION == EDITION[nrow(.)])
  
EU_GDP_EUROSTAT <- EU_GDP %>%
  filter(unit == "CLV10_MEUR" & s_adj == "SCA" & na_item == "B1GQ" & geo == "EU27_2020" & time >= as.Date("2018-01-01")) %>%
  transmute(value = values, date = as.Date(as.yearqtr(time, "%Y-Q%q"))) %>%
  arrange(date)
  
SPAIN_DIR <- fromJSON("https://servicios.ine.es/wstempus/js/EN/OPERACIONES_DISPONIBLES")
SPAIN_PUB_LIST <- get_publications(lang = "en", det = 2)
SPAIN_TEST_GDP_TABLES <- get_tables(code = "237",lang = "en")
SPAIN_GDP_TABLE <- get_series(30679, resource = "table", lang = "en")
SPAIN_GDP_INE <- get_series("CNTR4851", resource = "data", nlast = 500, lang = "en") %>%
  .$Data %>%
  transmute(date = seq.Date(from = as.Date("1995-01-01"), by = "3 months", length.out = nrow(.)), value = Valor) %>%
  subset(date >= as.Date("2018-01-01"))
  

EU_MAIN_GDP <- ggplot() +
  geom_line(data = SPAIN_GDP_INE, aes(x=date, y = value/value[7]*100, color = "Spain"), size = 1.25) + 
  geom_line(data = ITALY_GDP_ISTAT, aes(x=date, y = value/value[7]*100, color = "Italy"), size = 1.25) + 
  geom_line(data = FRANCE_GDP_INSEE, aes(x=date, y = value/value[7]*100, color = "France"), size = 1.25) + 
  geom_line(data = GERMANY_GDP_BUNDESBANK, aes(x=date, y = value/value[7]*100, color = "Germany"), size = 1.25) + 
  geom_line(data = EU_GDP_EUROSTAT, aes(x=date, y = value/value[7]*100, color = "European Union"), size = 2.25) + 
  annotate("text",label = "Pre-COVID GDP", x = as.Date("2019-01-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,105), expand = c(0,0)) +
  ylab("Index, Q3 2019 = 100") +
  ggtitle("Eurozone GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using INSEE, ISTAT, DeStatis, INE, and Eurostat Data",subtitle = "Major Eurozone Economies are Slightly Larger than Pre-Pandemic but Trailing Eurozone Average Growth") +
  theme_apricitas + theme(legend.position = c(.62,.24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany","France","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*30), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MAIN_GDP, "EU Main GDP Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FRANCE_FOOD_CONSUMPTION <- get_idbank_list("CONSO-MENAGES-2014") %>% # France Food ex-Tobacco Consumption 
  filter(PRODUIT_CONSO_MENAGES_label_en == "Food excepting tobacco") %>%
  add_insee_title() %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  arrange(date) %>%
  mutate(value = value/value[23]*100)

ITALY_FOOD_CONSUMPTION <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_24_DF_DCCN_QNA_4,1.0/Q........./ALL/?detail=full&startPeriod=2018-01-01&endPeriod=2023-03-31&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  subset(EXPEND_PURPOSE_COICOPCOFOG == "CP01" & VALUATION == "L_2015" & ADJUSTMENT == "Y") %>%
  transmute(value = obsValue/obsValue[8]*100, date = as.Date(as.yearqtr(obsTime, "%Y-Q%q")))

#Downloading Netherlands Consumption data
NETHERLANDS_FOOD_CONSUMPTION <- cbs_get_data('82608ENG') %>%
  subset(ConsumptionByHouseholds == "A047875") %>%
  filter(str_detect(`Periods`, fixed("MM", ignore_case = TRUE))) %>%
  transmute(value = Indices_1) %>%
  ts(., start = c(2000,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq.Date(from = as.Date("2000-01-01"), by = "month", length.out = nrow(.)), value = x/x[239]*100) %>%
  subset(date >= as.Date("2018-01-01"))
  
GERMANY_FOOD_CONSUMPTION <- retrieve_data(tablename = "81000BV016", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  subset(CC93Z1 == "CC01-01") %>%
  select(JAHR, QUARTG, VGR102_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = VGR102_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[8]*100)

EU_FOOD_CONSUMPTION_GRAPH <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=ITALY_FOOD_CONSUMPTION, aes(x=date+45,y= value,color="Italy, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco"), size = 1.25) +
  geom_line(data=FRANCE_FOOD_CONSUMPTION, aes(x=date+15,y= value,color="France, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco"), size = 1.25) +
  #geom_line(data=NETHERLANDS_FOOD_CONSUMPTION, aes(x=date,y= value,color="Netherlands, Real Consumption of Food, Drink, and Tobacco"), size = 1.25) +
  geom_line(data=GERMANY_FOOD_CONSUMPTION, aes(x=date+45,y= value,color="Germany, Real Consumption of Food, Drink, and Tobacco"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,110), expand = c(0,0)) +
  ylab("Index, Nov/Q4 2019 = 100") +
  ggtitle("The European Food Crisis") +
  labs(caption = "Graph created by @JosephPolitano using INSEE, ISTAT, and DeStatis Data",subtitle = "Rising Food Prices Have Forced European Households to Cut Back on Food Consumption") +
  theme_apricitas + theme(legend.position = c(.42,.14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Germany, Real Consumption of Food, Drink, and Tobacco","France, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco","Italy, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*20), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_FOOD_CONSUMPTION_GRAPH, "EU Food Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EMP_EXP_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/EI_BSEE_M_R2/1.0?compress=false"))

EMP_EXP_NATIONAL <- EMP_EXP_BULK %>%
  subset(unit == "INX") %>%
  transmute(geo, time = as.Date(as.yearmon(TIME_PERIOD, format = "%Y-%m")), value = as.numeric(OBS_VALUE)) %>%
  subset(time >= as.Date("2018-01-01")) %>%
  pivot_wider(names_from = geo, values_from = value)

EMP_EXP_NATIONAL_graph <- ggplot() + #plotting regular vs non-regular employment
  annotate(geom = "hline",y = 100,yintercept = 100, size = 0.5,color = "white") +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `ES`,color="Spain"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `IT`,color="Italy"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `FR`,color="France"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `DE`,color="Germany"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `EU27_2020`,color="European Union"), size = 2.25) +
  annotate("text", label = "above line = net expansion\nbelow line = net contraction", x = as.Date("2019-01-01"), y = 95, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(40,120), expand = c(0,0), breaks = c(40,60,80,100,120)) +
  ylab("Index, Net Increase") +
  ggtitle("Europe's Labor Market Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "European Employment Expectations Are Weak—Especially in Germany") +
  theme_apricitas + theme(legend.position = c(.80,.40)) +
  scale_color_manual(name= "Employment Expectations, Next 3M",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany","France","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 40-(.3*80), ymax = 40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_NATIONAL_graph, "EMP EXP NATIONAL GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EMP_EXP_INDUSTRY <- EMP_EXP_BULK %>%
  subset(unit == "BAL" & indic == "BS-IEME-BAL") %>%
  transmute(geo, time = as.Date(as.yearmon(TIME_PERIOD, format = "%Y-%m")), value = as.numeric(OBS_VALUE)) %>%
  subset(time >= as.Date("2000-01-01")) %>%
  pivot_wider(names_from = geo, values_from = value)

EMP_EXP_INDUSTRY_graph <- ggplot() + #plotting regular vs non-regular employment
  annotate(geom = "hline",y = 0,yintercept = 00, size = 0.5,color = "white") +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `ES`,color="Spain"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `IT`,color="Italy"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `FR`,color="France"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `DE`,color="Germany"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `EU27_2020`,color="European Union"), size = 2.25) +
  annotate("text", label = "above line = net expansion\nbelow line = net contraction", x = as.Date("2015-01-01"), y = -27.5, color = "white", size = 4.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-40,25), expand = c(0,0), breaks = c(-40,-20,0,20)) +
  ylab("Balance, Net Increase") +
  ggtitle("Europe's Industrial Labor Market Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "European Industry's Employment Expectations Are Weak—Especially in Germany") +
  theme_apricitas + theme(legend.position = c(.40,.85), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Industry, Employment Expectations, Next 3M",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany","France","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -40-(.3*65), ymax = -40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_INDUSTRY_graph, "EMP EXP INDUSTRY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GERMAN_GFCF_PRIVATE_EQUIPMENT <- retrieve_data(tablename = "81000BV015", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, VGR105_val,BAU020_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = VGR105_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2000-01-01"))

GERMAN_GFCF_PRIVATE_EQUIPMENT_graph <- ggplot() + #plotting GDP For US vs Germany
  geom_line(data=GERMAN_GFCF_PRIVATE_EQUIPMENT, aes(x=date,y= value,color="Real Private Fixed Investment in Equipment, Germany"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = "€"),limits = c(30,60), expand = c(0,0)) +
  ylab("Chained Billions of Dollars") +
  ggtitle("Germany's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis and BEA Data",subtitle = "Since 2018, German Economic Growth Has Been Especially Weak") +
  theme_apricitas + theme(legend.position = c(.6,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 95-(.3*40), ymax = 95) +
  coord_cartesian(clip = "off")

GERMAN_GFCF_EQUIPMENT_CATEGORIES <- retrieve_data(tablename = "81000BV009", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, VGR008_val, INV006_val, INV012_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), Equipment = VGR008_val, Machines = INV006_val, Vehicles = INV012_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2016-01-01")) %>%
  mutate(across(where(is.numeric), ~if_else(.x == 0, NA_real_, .x))) %>%
  mutate(across(where(is.numeric), ~ .x / .x[9]*100))

GERMAN_GFCF_EQUIPMENT_CATEGORIES_graph <- ggplot() + #plotting Fixed Investment
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Machines,color="Equipment: Machinery and Other Devices"), size = 1.25) +
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Vehicles,color="Equipment: Vehicles"), size = 1.25) +
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Equipment,color="Equipment"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,120), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Germany's Slow Investment Rebound") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "German Investment In Fixed Manufacturing Assets Has Not Recovered to Pre-Pandemic Lvels") +
  theme_apricitas + theme(legend.position = c(.3,.27)) +
  scale_color_manual(name= "Germany: Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 60-(.3*60), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GERMAN_GFCF_EQUIPMENT_CATEGORIES_graph, "GERMAN GFCF EQUIPMENT CATEGORIES GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CONSTRUCTION_PROD_EU <- get_eurostat("sts_copr_m") %>%
  subset(geo %in% c("IT","EU27_2020","DE","FR","ES")) %>%
  subset(time>= as.Date("2018-01-01")) %>%
  subset(nace_r2 == "F") %>%
  subset(unit == "I15") %>%
  subset(s_adj == "SCA") %>%
  transmute(geo,date = time,value = values) %>%
  pivot_wider(names_from = geo) %>%
  arrange(date) %>%
  mutate(across(where(is.numeric), ~ .x / .x[25]*100))

CONSTRUCTION_PROD_EU_GRAPH <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= DE,color="Germany"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= ES,color="Spain"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= FR,color="France"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= IT,color="Italy"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= EU27_2020,color="European Union"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(20,130), breaks = c(20,40,60,80,100,120,140), expand = c(0,0)) +
  ylab("Index, Nov/Q4 2019 = 100") +
  ggtitle("European Construction Output") +
  labs(caption = "Graph created by @JosephPolitano using INSEE, ISTAT, and CBS Data",subtitle = "Headline EU Construction Output is Tepid—But Boosted by Superbonus Schemes in Italy") +
  theme_apricitas + theme(legend.position = c(.2,.24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","France","Germany","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 20-(.3*110), ymax = 20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_PROD_EU_GRAPH, "CONSTRUCTION PROD EU GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ITALY_EPOP_ISTAT <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,150_872_DF_DCCV_TAXOCCUMENS1_1,1.0/M....../ALL/?detail=full&startPeriod=2004-01-01&dimensionAtObservation=TIME_PERIOD")) %>%
  filter(AGE == "Y15-64" & ADJUSTMENT == "Y" & REF_AREA == "IT" & SEX == "9" & EDITION == EDITION[nrow(.)]) %>%
  transmute(value = obsValue/100, date = as.Date(as.yearmon(obsTime, "%Y-%m")))

ITALY_EPOP_ISTAT_graph <- ggplot() + #plotting car manufacturing
  geom_line(data=ITALY_EPOP_ISTAT, aes(x=date,y= value,color="Italy, 15-64 Employment Rate, %"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Italy's Job Boom") +
  labs(caption = "Graph created by @JosephPolitano using ISTAT Data",subtitle = "Italian Employment Rates are at Record Highs") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = min(ITALY_EPOP_ISTAT$value)-(.3*(max(ITALY_EPOP_ISTAT$value)-min(ITALY_EPOP_ISTAT$value))), ymax = min(ITALY_EPOP_ISTAT$value)) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ITALY_EPOP_ISTAT_graph, "Italy Epop Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EA_MANU_SURVEY_DEMAND_Materials_graph <- ggplot() + #plotting BIE
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= (`BS-FLP1-PC`+`BS-FLP2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP6-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP5-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP4-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.60), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Eurozone Supply Chain Crisis is Easing") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Manufacturers Say Materials Constraints are Easing Significantly") +
  theme_apricitas + theme(legend.position = c(.45,.45), plot.title = element_text(size = 26)) +
  scale_color_manual(name= "Factors Limiting Production in EA-20 Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials and Equipment","Other (Including COVID)","Financial Constraints")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 0-(.3*.60), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_MANU_SURVEY_DEMAND_Materials_graph, "EA_MANU_SURVEY_DEMAND_MATERIALS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#
EA_SERV_SURVEY <- get_eurostat("ei_bsse_q_r2") %>%
  subset(s_adj == "SA" & geo == "EA20" & time >= as.Date("2004-01-01") &
           indic %in% c("BS-FLB1-PC","BS-FLB2-PC","BS-FLB3-PC","BS-FLB4-PC","BS-FLB5-PC","BS-FLB6-PC")) %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EA_SERV_SURVEY_graph <- ggplot() + #plotting BIE
  #geom_line(data=EA_SERV_SURVEY, aes(x=time,y= (`BS-FLB1-PC`+`BS-FLB2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB4-PC`/100,color= "Shortage of Materials, Equipment, or Space"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB5-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB6-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.6), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Service Sector Firms Say Labor Constraints are Tight as COVID Constraints Ease") +
  theme_apricitas + theme(legend.position = c(.50,.67)) +
  scale_color_manual(name= "Factors Limiting Production in EA-20 Service Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials, Equipment, or Space","Other (Including COVID)","Financial Constraints")) + #, breaks = c("None or Insufficient Demand","Shortage of Materials, Equipment, or Space","Shortage of Labor","Financial Constraints","Other (Including COVID)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.6), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_SERV_SURVEY_graph, "EA_Serv Survey.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EMP_EXP <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/EI_BSEE_M_R2/1.0?compress=false"))

EMP_EXP_IT <- EMP_EXP %>%
  subset(geo == "IT" & unit == "BAL") %>%
  transmute(indic, time = as.Date(as.yearmon(TIME_PERIOD, format = "%Y-%m")), value = as.numeric(OBS_VALUE)) %>%
  subset(time >= as.Date("2018-01-01")) %>%
  pivot_wider(names_from = indic, values_from = value)

EMP_EXP_IT_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=EMP_EXP_IT, aes(x=time,y= `BS-IEME-BAL`,color="Industry"), size = 1.25) +
  geom_line(data=EMP_EXP_IT, aes(x=time,y= `BS-SEEM-BAL`,color="Services"), size = 1.25) +
  geom_line(data=EMP_EXP_IT, aes(x=time,y= `BS-REM-BAL`,color="Retail Trade"), size = 1.25) +
  geom_line(data=EMP_EXP_IT, aes(x=time,y= `BS-CEME-BAL`,color="Construction"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .2),limits = c(-20,17.5), expand = c(0,0), breaks = c(-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25)) +
  ylab("Balance, Increase minus Decrease") +
  ggtitle("Italy's Job Boom") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "Italian Construction and Retail Trade Firms Have Strong Hiring Expectations") +
  theme_apricitas + theme(legend.position = c(.80,.20)) +
  scale_color_manual(name= "Employment Expectations, Next 3M",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -20-(.3*37.5), ymax = -20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_IT_graph, "Emp Exp IT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EU_HOUSING_CONSTRUCTION_BULK <- get_eurostat("sts_cobp_m")

EU_SF_MF_CONSTRUCTION <- EU_HOUSING_CONSTRUCTION_BULK %>%
  filter(indic_bt == "PSQM" & s_adj == "SCA" & geo == "EU27_2020" & unit == "I15" & cpa2_1 %in% c("CPA_F410011","CPA_F410012_410013")) %>%
  transmute(category = cpa2_1, date = time,value = values) %>%
  mutate(category = gsub("CPA_F410011","Single-Family Homes",category)) %>%
  mutate(category = gsub("CPA_F410012_410013","Multi-Family Homes",category)) %>%
  pivot_wider(names_from = category) %>%
  mutate(`Single-Family Homes` = `Single-Family Homes`*.87) %>%
  mutate(`Multi-Family Homes` = `Multi-Family Homes`*.709) %>%
  pivot_longer(cols = `Single-Family Homes`:`Multi-Family Homes`)

EU_SF_MF_CONSTRUCTION_graph <- ggplot(data = EU_SF_MF_CONSTRUCTION, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  ylab("Millions of Square Meters, Annual Rate") +
  ggtitle("EU Housing Permits are Falling") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M m2"), breaks = c(0,100,200,300,400,500,600), limits = c(0,650), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "EU Housing Starts are Down Significantly and Approaching Record Lows") +
  theme_apricitas + theme(legend.position = c(.7,.5)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "EU-27 Building Permits, Millions of Square Meters",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_SF_MF_CONSTRUCTION_graph, "EU SF MF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EMPLOYMENT_NACE_2_BULK <- get_eurostat("namq_10_a10_e")

EMPLOYMENT_INDEXED_NACE_2_ITALY <- EMPLOYMENT_NACE_2_BULK %>%
  filter(s_adj == "SCA" & geo == "IT" & time >= as.Date("2019-10-01") & unit == "THS_PER" & na_item == "EMP_DC") %>%
  arrange(time) %>%
  select(time, values, nace_r2) %>%
  pivot_wider(names_from = nace_r2, values_from = values) %>%
  setNames(c("date","Agriculture, Forestry, and Fishing", "Industry ex Construction","Manufacturing","Construction", "Wholesale & Retail Trade, Transport, Food Service, and Accomodations","Information and Communication","Finance and Insurance","Real Estate","Professional, Scientific, Technical, Administrative, and Support Services","Public Administration, Defense, Education, Health, and Social Work", "Arts, Entertainment, Recreation, and Other Service Activities","Total")) %>%
  mutate(across(where(is.numeric), ~ .x-.x[1])) %>%
  select(-Manufacturing,-Total) %>%
  pivot_longer(cols = `Agriculture, Forestry, and Fishing`:`Arts, Entertainment, Recreation, and Other Service Activities`)

EMPLOY_GROWTH_IND_ITA_graph <- ggplot(data = EMPLOYMENT_INDEXED_NACE_2_ITALY, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), expand = c(0,0)) +
  ggtitle("The Shape of Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "There are Now More Jobs Than Pre-Pandemic—and Most Sectors Have Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.725,.325)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  #scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -22-(.3*27), ymax = -22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_GROWTH_IND_ITA_graph, "Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



SPREADS FIND SPREADS

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
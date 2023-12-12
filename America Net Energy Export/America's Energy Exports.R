pacman::p_load(bea.R,censusapi,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

devtools::install_github("jameelalsalam/eia2")
library("eia2")
# 
# test <- beaSearch()
# test <- beaParams(beaKey = Sys.getenv("BEA_KEY"),"ITA")
# test <- beaParamVals(beaKey = Sys.getenv("BEA_KEY"),"ITA",paramName = "Indicator")
# test <- as.data.frame(test$ParamValue)
# test <- beaSets(Sys.getenv("BEA_KEY"))
# 
# test <- beaSets("ITA",beaKey =  Sys.getenv("BEA_KEY"))

NAT_GAS_EXP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ExpGdsNaturalGas',
  'AreaOrCountry' = 'AllCountries',
  'Frequency' = 'QSA',
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

NAT_GAS_EXP <- beaGet(NAT_GAS_EXP_SPECS, iTableStyle = TRUE, asWide = FALSE) %>%
  transmute(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")), value = DataValue)


NAT_GAS_IMP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsNaturalGas',
  'AreaOrCountry' = 'AllCountries',
  'Frequency' = 'QSA',
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

NAT_GAS_IMP <- beaGet(NAT_GAS_IMP_SPECS, iTableStyle = TRUE, asWide = FALSE) %>%
  transmute(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")), value = DataValue)

PETROLEUM_EXP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ExpGdsPetAndProds',
  'AreaOrCountry' = 'AllCountries',
  'Frequency' = 'QSA',
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PETROLEUM_EXP <- beaGet(PETROLEUM_EXP_SPECS, iTableStyle = TRUE, asWide = FALSE) %>%
  transmute(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")), value = DataValue)

PETROLEUM_IMP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsPetAndProds',
  'AreaOrCountry' = 'AllCountries',
  'Frequency' = 'QSA',
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PETROLEUM_IMP <- beaGet(PETROLEUM_IMP_SPECS, iTableStyle = TRUE, asWide = FALSE) %>%
  transmute(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")), value = DataValue)


COAL_EXP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ExpGdsCoalAndRelProds',
  'AreaOrCountry' = 'AllCountries',
  'Frequency' = 'QSA',
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

COAL_EXP <- beaGet(COAL_EXP_SPECS, iTableStyle = TRUE, asWide = FALSE) %>%
  transmute(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")), value = DataValue)


COAL_IMP_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsCoalAndRelProds',
  'AreaOrCountry' = 'AllCountries',
  'Frequency' = 'QSA',
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

COAL_IMP <- beaGet(COAL_IMP_SPECS, iTableStyle = TRUE, asWide = FALSE) %>%
  transmute(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")), value = DataValue)

COAL_NET_EXP <- merge(COAL_EXP,COAL_IMP, by = "date") %>%
  transmute(date, value = value.x-value.y)
PET_PRODUCTS_NET_EXPORTS <- merge(PETROLEUM_EXP,PETROLEUM_IMP,by = "date") %>%
  transmute(date, value = value.x-value.y)
NAT_GAS_NET_EXPORTS <- merge(NAT_GAS_EXP,NAT_GAS_IMP,by = "date") %>%
  transmute(date, value = value.x-value.y)

US_ENERGY_NET_EXP_BREAKDOWN_Graph <- ggplot() + #plotting nat gas exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = COAL_NET_EXP, aes(x = date, y = value*4/1000, color = "Coal and Related Products"), size = 1.25) +
  geom_line(data = PET_PRODUCTS_NET_EXPORTS, aes(x = date, y =  value*4/1000, color = "Petroleum and Products"), size = 1.25) +
  geom_line(data = NAT_GAS_NET_EXPORTS, aes(x = date, y =  value*4/1000, color = "Natural Gas"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-500,100), breaks = c(-500,-400,-300,-200,-100,0,100), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("The US is Now an Energy Exporter") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "America's Energy Exports Have Risen Significantly Over the Last Decade") +
  theme_apricitas + theme(legend.position = c(.80,.25)) +
  scale_color_manual(name= "Net Exports, Annualized Rate",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED"), breaks = c("Petroleum and Products","Natural Gas","Coal and Related Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -500-(.3*600), ymax = -500) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ENERGY_NET_EXP_BREAKDOWN_Graph, "US Energy Net Exports Breakdown.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



US_ENERGY_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "27", #energy commodity code
  #CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(value = as.numeric(ALL_VAL_MO)) %>%
  select(time, value)


US_ENERGY_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "27", #energy commodity code
  #CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  select(time, value)

US_ENERGY_NET_EXP_MERGE <- merge(US_ENERGY_IMPORTS,US_ENERGY_EXPORTS, by = "time") %>%
  mutate(Net = value.y-value.x) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollsum(Net,12)))

US_ENERGY_NET_EXP_Graph <- ggplot() + #plotting nat gas exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = US_ENERGY_NET_EXP_MERGE, aes(x = time, y = Net*12/1000000000, color = "US Net Energy Exports"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data = US_ENERGY_NET_EXP_MERGE, aes(x = time, y = rollmean/1000000000, color = "US Net Energy Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-300,150), breaks = c(-300,-250,-200,-150,-100,-50,0,50,100,150), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("The US is Now an Energy Exporter") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "America's Energy Exports Have Risen Significantly Over the Last Decade") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= "Dashed = Monthly Annualized, Solid = 12M Moving Total",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -300-(.3*450), ymax = -300) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ENERGY_NET_EXP_Graph, "US Energy Net Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


US_NAT_GAS_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "VES_WGT_MO", "E_COMMODITY", "CTY_CODE"), 
  time = paste("from 2016 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "2711110000", #nat gas commodity code
  CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
)

US_NAT_GAS_EXPORTS$time <- as.Date(as.yearmon(US_NAT_GAS_EXPORTS$time))
US_NAT_GAS_EXPORTS$VES_WGT_MO <- as.numeric(US_NAT_GAS_EXPORTS$VES_WGT_MO)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("-","All US LNG Exports",US_NAT_GAS_EXPORTS$CTY_CODE)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("4XXX","US LNG Exports to Europe",US_NAT_GAS_EXPORTS$CTY_CODE)

US_NAT_GAS_EXPORTS_Graph <- ggplot() + #plotting nat gas exports
  geom_area(data = US_NAT_GAS_EXPORTS, aes(x = time, y = VES_WGT_MO/1000000000, fill = CTY_CODE), color = NA, size = 0, position = "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "B", accuracy = 1),limits = c(0,8), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  ylab("Billions of kg") +
  ggtitle("Bridging the Gap") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Natural Gas Exports Are Helping Ease A European Energy Shortage") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*8), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_GAS_EXPORTS_Graph, "US Nat Gas Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

US_NAT_GAS_EXPORTS_DOL <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = paste("from 2016 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "2711110000", #nat gas commodity code
  #CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
)

US_NAT_GAS_EXPORTS_DOL$time <- as.Date(as.yearmon(US_NAT_GAS_EXPORTS_DOL$time))
US_NAT_GAS_EXPORTS_DOL$ALL_VAL_MO <- as.numeric(US_NAT_GAS_EXPORTS_DOL$ALL_VAL_MO)
US_NAT_GAS_EXPORTS_DOL$CTY_CODE <- gsub("-","US LNG Exports",US_NAT_GAS_EXPORTS_DOL$CTY_CODE)

US_NAT_GAS_EXPORTS_DOL_Graph <- ggplot() + #plotting nat gas exports
  geom_line(data = US_NAT_GAS_EXPORTS_DOL, aes(x = time, y = ALL_VAL_MO/1000000000, color = "US Monthly LNG Exports, Dollars"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,5), breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("America's LNG Exports") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Falling Prices Have Dropped US LNG Exports to $2.5B a Month") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_GAS_EXPORTS_DOL_Graph, "US Nat Gas Exports Dollar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#US PRIMARY ENERGY IMPORTS
US_TOTAL_EXPORTS <- eia1_series("TOTAL.TENIBUS.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = value*-1/1000) %>%
  arrange(date)

US_NATGAS_EXPORTS <- eia1_series("TOTAL.NGNIBUS.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = value*-1) %>%
  arrange(date)

US_PETROL_EXPORTS <- eia1_series("TOTAL.PMNIBUS.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = value*-1) %>%
  arrange(date)

US_PRIMARY_ENERGY_NET_EXPORTS_Graph <- ggplot() + #plotting nat gas exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = US_PETROL_EXPORTS, aes(x = date, y = value, color = "Crude Oil and Petroleum Products"), size = 1.25) +
  geom_line(data = US_NATGAS_EXPORTS, aes(x = date, y = value, color = "Natural Gas"), size = 1.25) +
  geom_line(data = US_TOTAL_EXPORTS, aes(x = date, y = value, color = "Total Primary Energy"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "QBTU", accuracy = 1),limits = c(-32,ceiling(max(US_TOTAL_EXPORTS$value))), breaks = c(-30,-20,-10,0,10), expand = c(0,0)) +
  ylab("Quadrillions of BTUs") +
  ggtitle("America's Energy Export Boom") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "America's Has Become a Large Net Energy Exporter Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.25,.20)) +
  scale_color_manual(name= "Primary Energy Net Exports\n(Quadrillion BTUs, Annual)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED"), breaks = c("Total Primary Energy","Crude Oil and Petroleum Products","Natural Gas"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1949-01-01")-(.1861*(today()-as.Date("1949-01-01"))), xmax = as.Date("1949-01-01")-(0.049*(today()-as.Date("1949-01-01"))), ymin = -32-(.3*38), ymax = -32) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_PRIMARY_ENERGY_NET_EXPORTS_Graph, "US Primary Energy Net Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

NAT_GAS_PRODUCTION <- eia1_series("STEO.NGPRPUS.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

NAT_GAS_CONSUMPTION <- eia1_series("STEO.NGTCPUS.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

US_NAT_GAS_PRODUCTION_CONSUMPTION_Graph <- ggplot() + #plotting nat gas exports
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(NAT_GAS_PRODUCTION$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -1000), "month"), y = 20, color = "#EE6055", size = 5, alpha = 0.6) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_line(data = filter(NAT_GAS_PRODUCTION, date >= as.Date("2000-01-01")), aes(x = date, y = value, color = "US Dry Natural Gas Production"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  #geom_line(data = filter(NAT_GAS_CONSUMPTION, date >= as.Date("2000-01-01")), aes(x = date, y = value, color = "US Natural Gas Consumption"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data = filter(NAT_GAS_CONSUMPTION, date >= as.Date("2000-01-01")), aes(x = date, y = rollmean, color = "US Natural Gas Consumption"), size = 1.25) +
  geom_line(data = filter(NAT_GAS_PRODUCTION, date >= as.Date("2000-01-01")), aes(x = date, y = rollmean, color = "US Dry Natural Gas Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "Bcf/d", accuracy = 1),limits = c(0,125), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Billions of Cubic Feet per Day") +
  ggtitle("America's Natural Gas Export Boom") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Natural Gas Production is Growing Faster than Domestic Consumption") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= "12M Moving Average",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_GAS_PRODUCTION_CONSUMPTION_Graph, "US Nat Gas Production Consumption Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

NAT_GAS_PIPELINE_EXPORTS <- eia1_series("STEO.NGEXPUS_PIPE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

NAT_GAS_PIPELINE_IMPORTS <- eia1_series("STEO.NGIMPUS_PIPE.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

NAT_GAS_LNG_EXPORTS <- eia1_series("STEO.NGEXPUS_LNG.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

NAT_GAS_LNG_IMPORTS <- eia1_series("STEO.NGIMPUS_LNG.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

NAT_GAS_NET_PIPELINE_EXPORTS <- merge(NAT_GAS_PIPELINE_EXPORTS,NAT_GAS_PIPELINE_IMPORTS, by = "date") %>%
  transmute(date, value = value.x-value.y, rollmean = rollmean.x-rollmean.y)

NAT_GAS_NET_LNG_EXPORTS <- merge(NAT_GAS_LNG_EXPORTS,NAT_GAS_LNG_IMPORTS, by = "date") %>%
  transmute(date, value = value.x-value.y, rollmean = rollmean.x-rollmean.y)


US_NAT_GAS_LNG_PIPELINE_EXPORTS_Graph <- ggplot() + #plotting nat gas exports
  annotate("rect", xmin = floor_date(as.Date(today() -74), "month"), xmax = max(NAT_GAS_PRODUCTION$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -1000), "month"), y = -9, color = "#EE6055", size = 5, alpha = 0.6) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = filter(NAT_GAS_NET_PIPELINE_EXPORTS, date >= as.Date("2000-01-01")), aes(x = date, y = value, color = "US Net Natural Gas Exports via Pipeline"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data = filter(NAT_GAS_NET_LNG_EXPORTS, date >= as.Date("2000-01-01")), aes(x = date, y = value, color = "US Net Natural Gas Exports via LNG"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data = filter(NAT_GAS_NET_PIPELINE_EXPORTS, date >= as.Date("2000-01-01")), aes(x = date, y = rollmean, color = "US Net Natural Gas Exports via Pipeline"), size = 1.25) +
  geom_line(data = filter(NAT_GAS_NET_LNG_EXPORTS, date >= as.Date("2000-01-01")), aes(x = date, y = rollmean, color = "US Net Natural Gas Exports via LNG"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "Bcf/d", accuracy = 1),limits = c(-12.5,15), breaks = c(-10,-5,0,5,10,15), expand = c(0,0)) +
  ylab("Billions of Cubic Feet per Day") +
  ggtitle("America's Natural Gas Export Boom") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "America Has Become a Major Net Exporter of Natural Gas Especially via LNG") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= "Dashed = Monthly, Solid = 12M Moving Average",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -12.5-(.3*27.5), ymax = -12.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_GAS_LNG_PIPELINE_EXPORTS_Graph, "US Nat Gas LNG Pipeline Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

STEO_Crude_ProductionMonthly <- eia1_series("STEO.COPRPUS.M") %>%
  transmute(date = as.Date(paste0(period,"-01")),value) %>%
  filter(date >= as.Date("2000-01-01"))

STEO_Crude_Production_Graph <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -40), "month"), xmax = max(NAT_GAS_PRODUCTION$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -1000), "month"), y = 5.5, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=STEO_Crude_ProductionMonthly, aes(x=date,y= value, color= "US Crude Oil Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(0,14),breaks = c(0,2,4,6,8,10,12,14), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("America's Oil Recovery") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Oil Production Is Now at Record High Levels") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*14), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STEO_Crude_Production_Graph, "STEO Crude Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FINISHED_MOTOR_GASOLINE_SUPPLIED <- eia1_series("PET.MGFUPUS2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

KEROSENE_JET_FUEL_SUPPLIED <- eia1_series("PET.MKJUPUS2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))

DISTILLATE_FUEL_OIL_SUPPLIED <- eia1_series("PET.MDIUPUS2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value) %>%
  arrange(date) %>%
  mutate(rollmean = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value,12)))


PETROLEUM_PRODUCT_SUPPLIED_Graph <- ggplot() + #plotting US Crude Production
  #geom_line(data=filter(FINISHED_MOTOR_GASOLINE_SUPPLIED, date >= as.Date("1945-01-01")), aes(x=date,y= value/1000, color= "US Finished Motor Gasoline Supplied"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data=filter(FINISHED_MOTOR_GASOLINE_SUPPLIED, date >= as.Date("1945-01-01")), aes(x=date,y= rollmean/1000, color= "US Finished Motor Gasoline Supplied 12MMA"), size = 1.25) +
  geom_line(data=filter(KEROSENE_JET_FUEL_SUPPLIED, date >= as.Date("1945-01-01")), aes(x=date,y= rollmean/1000, color= "US Kerosene Type Jet Fuel Supplied 12MMA"), size = 1.25) +
  geom_line(data=filter(DISTILLATE_FUEL_OIL_SUPPLIED, date >= as.Date("1945-01-01")), aes(x=date,y= rollmean/1000, color= "US Distillate Fuel Oil Supplied 12MMA"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl/d", accuracy = 1), limits = c(0,10),breaks = c(0,1,2,3,4,5,6,7,8,9,10), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("US Petroleum Product Consumption") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Petroleum Product Consumption May Have Peaked With the Pandemic") +
  theme_apricitas + theme(legend.position = c(.32,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Finished Motor Gasoline Supplied 12MMA","US Distillate Fuel Oil Supplied 12MMA","US Kerosene Type Jet Fuel Supplied 12MMA")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1945-01-01")-(.1861*(today()-as.Date("1945-01-01"))), xmax = as.Date("1945-01-01")-(0.049*(today()-as.Date("1945-01-01"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PETROLEUM_PRODUCT_SUPPLIED_Graph, "Petroleum Product Supplied Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FINISHED_MOTOR_GASOLINE_SUPPLIED_2018_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=filter(FINISHED_MOTOR_GASOLINE_SUPPLIED, date >= as.Date("2018-01-01")), aes(x=date,y= value/1000, color= "US Finished Motor Gasoline Supplied"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data=filter(FINISHED_MOTOR_GASOLINE_SUPPLIED, date >= as.Date("2018-01-01")), aes(x=date,y= rollmean/1000, color= "US Finished Motor Gasoline Supplied"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl/d", accuracy = 1), limits = c(5.75,10),breaks = c(1,2,3,4,5,6,7,8,9,10), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("US Gasoline Consumption") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Gasoline Consumption May Have Peaked With the Pandemic") +
  theme_apricitas + theme(legend.position = c(.75,.2)) +
  scale_color_manual(name= "Dashed = Monthly, Solid = 12M Moving Average" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 5.75-(.3*4.25), ymax = 5.75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FINISHED_MOTOR_GASOLINE_SUPPLIED_2018_Graph, "Finished Motor Gasoline Supplied 2018 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


ALL_EMPLOYEES_OIL_GAS_2000 <- bls_api("CES1021100001", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
  
ALL_EMPLOYEES_OIL_GAS_2020 <- bls_api("CES1021100001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)
  
ALL_EMPLOYEES_SUPPORT_OIL_GAS_2000 <- bls_api("CES1021311201", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

ALL_EMPLOYEES_SUPPORT_OIL_GAS_2020 <- bls_api("CES1021311201", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

ALL_EMPLOYEES_OIL_GAS <- rbind(ALL_EMPLOYEES_OIL_GAS_2000,ALL_EMPLOYEES_OIL_GAS_2020) %>%
  transmute(date, value, name = "Oil and Gas Extraction")

ALL_EMPLOYEES_SUPPORT_OIL_GAS <- rbind(ALL_EMPLOYEES_SUPPORT_OIL_GAS_2000,ALL_EMPLOYEES_SUPPORT_OIL_GAS_2020) %>%
  transmute(date, value, name = "Support Activities for Oil and Gas Operations")

ALL_EMPLOYEES_OIL_GAS_RBIND <- rbind(ALL_EMPLOYEES_OIL_GAS,ALL_EMPLOYEES_SUPPORT_OIL_GAS) %>%
  group_by(date) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("Support Activities for Oil and Gas Operations","Oil and Gas Extraction")))

ALL_EMPLOYEES_OIL_GAS_Graph <- ggplot(ALL_EMPLOYEES_OIL_GAS_RBIND, aes(fill=name, x=date, y=value)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 32) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"),limits = c(0,625), breaks = c(0,100,200,300,400,500,600), expand = c(0,0)) +
  ylab("All Employees") +
  ggtitle("The Slow Recovery of Oil & Gas Jobs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment in the US Oil and Gas Industry Still Hasn't Recovered to Pre-COVID Levels") +
  theme_apricitas + theme(legend.position = c(.28,.89)) +
  scale_fill_manual(name = "All Employees, Thousands", values = c("#FFE98F","#00A99D"), breaks = c("Oil and Gas Extraction","Support Activities for Oil and Gas Operations")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*600), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_EMPLOYEES_OIL_GAS_Graph, "All Employees Oil Gas Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

LNG_EXPORT_DATES <- data.frame(date = seq(as.Date("2016-01-01"), as.Date("2029-12-01"), by="month"))
LNG_EXPORT_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America%20Net%20Energy%20Export/LNG_EXPORT_CAPACITY.csv") %>%
  mutate(PEAK.CAPACITY = as.numeric(PEAK.CAPACITY)) %>%
  mutate(date = as.Date(date)) %>%
  pivot_wider(names_from = category, values_from = PEAK.CAPACITY) %>%
  full_join(LNG_EXPORT_DATES,., by = "date") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(across(where(is.numeric), cumsum)) %>%
  group_by(date) %>%
  filter(row_number() == max(row_number())) %>%
  ungroup() %>%
  select(-name) %>%
  pivot_longer(-date) %>%
  mutate(name = gsub("Proposed, Partially/Fully Approved, Not Yet Under Construction","Permitting in Progress, No Start Date",name)) %>%
  mutate(name = gsub("Under Construction","Currently Under Construction",name)) %>%
  mutate(name = factor(name, levels = rev(c("LNG Export Capacity","Currently Under Construction","Permitting in Progress, No Start Date"))))
 
#UPDATE WITH LIQUEFACTION CAPACITY FILE

LNG_EXPORT_CAPACITY_Graph <- ggplot() +
  geom_bar(data = filter(LNG_EXPORT_DATA, name != "Permitting in Progress, No Start Date" & date < as.Date("2029-01-01")), aes(fill = name, x= date, y = value), position = "stack", stat = "identity", size = 0, color = NA, width = 32) +
  geom_rect(data = filter(LNG_EXPORT_DATA, name == "Permitting in Progress, No Start Date"), 
            aes(xmin = as.Date("2030-01-01"), xmax = as.Date("2031-01-01") , ymin = 22.62, ymax = 22.62 + value, fill = name), size = 0, color = NA, width = 32) +
  xlab("Date") +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype = "dashed", color = "white", size = 1.25) +
  annotate("text", x = as.Date("2022-12-01"), y = Inf, label = "History", vjust = 2, color = "white", size = 5) +
  annotate("text", x = as.Date("2024-10-01"), y = Inf, label = "Forecast", vjust = 2, color = "white", size = 5) + 
  scale_y_continuous(labels = scales::number_format(suffix = "Bcf/d", accuracy = 1),limits = c(0,45), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Billions of Cubic Feet per Day") +
  ggtitle("America's Growing LNG Export Capacity") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Liquefied Natural Gas Export Capacity is Growing—With Many Facilities in the Pipeline") +
  theme_apricitas + theme(legend.position = c(.25,.89)) +
  scale_fill_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055"), breaks = c("LNG Export Capacity","Currently Under Construction","Permitting in Progress, No Start Date")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(as.Date("2031-01-01")-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(as.Date("2031-01-01")-as.Date("2016-01-01"))), ymin = 0-(.3*45), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LNG_EXPORT_CAPACITY_Graph, "LNG Export Capacity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CRUDE_OIL_FOOTAGE_DRILLED <- eia1_series("TOTAL.OGPFPUS.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value = as.numeric(value)) %>%
  arrange(date)

CRUDE_OIL_FOOTAGE_DRILLED_Graph <- ggplot() +
  geom_line(data=filter(CRUDE_OIL_FOOTAGE_DRILLED, date >= as.Date("2005-01-01")), aes(x=date,y= value/1000, color= "US Crude Oil Wells, Total Footage Drilled"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "MMft", accuracy = 1),limits = c(0,27.5), breaks = c(5,10,15,20,25), expand = c(0,0)) +
  ylab("Millions of Feet") +
  ggtitle("America's Oil Drilling Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Drilling Levels are Near Modern Highs, but Are Now Declining") +
  theme_apricitas + theme(legend.position = c(.7,.1)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0-(.3*27.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CRUDE_OIL_FOOTAGE_DRILLED_Graph, "Crude Oil Footage Drilled Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ETHANE_NET_EXPORTS <- eia1_series("PET.MENNT_NUS-Z00_2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value = as.numeric(value)) %>%
  arrange(date)

PROPANE_NET_EXPORTS <- eia1_series("PET.MPANT_NUS-Z00_2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value = as.numeric(value)) %>%
  arrange(date)

NORMAL_BUTANE_NET_EXPORTS <- eia1_series("PET.MBUNT_NUS-Z00_2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value = as.numeric(value)) %>%
  arrange(date)

ISOBUTANE_NET_EXPORTS <- eia1_series("PET.MIINT_NUS-Z00_2.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value = as.numeric(value)) %>%
  arrange(date)

NATURAL_GASOLINE_NET_EXPORTS <- eia1_series("PET.M_EPLLNG_IMN_NUS-Z00_MBBLD.M") %>%
  transmute(date = as.Date(paste0(period,"-01")), value = as.numeric(value)) %>%
  arrange(date)

HYDROCARBON_GAS_LIQUID_Graphs <- ggplot() + #plotting US Crude Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=filter(ETHANE_NET_EXPORTS, date >= as.Date("2010-01-01")), aes(x=date,y= -value/1000, color= "Ethane"), size = 1.25) +
  geom_line(data=filter(PROPANE_NET_EXPORTS, date >= as.Date("2010-01-01")), aes(x=date,y= -value/1000, color= "Propane"), size = 1.25) +
  geom_line(data=filter(NORMAL_BUTANE_NET_EXPORTS, date >= as.Date("2010-01-01")), aes(x=date,y= -value/1000, color= "Butane"), size = 1.25) +
  #geom_line(data=filter(ISOBUTANE_NET_EXPORTS, date >= as.Date("2010-01-01")), aes(x=date,y= -value, color= "Isobutane"), size = 1.25) +
  geom_line(data=filter(NATURAL_GASOLINE_NET_EXPORTS, date >= as.Date("2010-01-01")), aes(x=date,y= -value/1000, color= "Natural Gasoline"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl/d", accuracy = 0.5), limits = c(-.15,1.65),breaks = c(0,0.5,1,1.5), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("US Net Hydrocarbon Gas Liquids Exports") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Net US HGL Exports Continue Booming, With Propane Exports Leading the Way") +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  theme(plot.title = element_text(size = 26)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Propane","Butane","Ethane","Natural Gasoline")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = -.15-(.3*1.8), ymax = -.15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HYDROCARBON_GAS_LIQUID_Graphs, "Hydrocarbon Gas Liquid Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#NAT GAS PRODUCTION BY PLAY

for (i in 0:11) {
  date_to_try <- floor_date(Sys.Date(), "month") - months(i)
  url_to_try <- paste0("https://www.eia.gov/naturalgas/weekly/img/shale_gas_", format(date_to_try, "%Y%m"), ".xlsx")
  
  # Attempt to download the file
  temp_file <- tempfile(fileext = ".xlsx")
  response <- GET(url_to_try, write_disk(temp_file, overwrite = TRUE))
  
  # Check if download was successful and file is not empty
  if (status_code(response) == 200 && file.info(temp_file)$size > 0) {
    NAT_GAS_PRODUCTION_BY_PLAY <- tryCatch({
      read.xlsx(temp_file)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(NAT_GAS_PRODUCTION_BY_PLAY)) {
      break
    }
  }
}

NAT_GAS_PRODUCTION_BY_PLAY <- NAT_GAS_PRODUCTION_BY_PLAY %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  select(-X14) %>%
  pivot_longer(-Date) %>%
  mutate(name = gsub("\\."," ", name)) %>%
  mutate(name = gsub("U S  s","US S", name)) %>%
  mutate(name = factor(name, levels = rev(c("Marcellus (PA, WV, OH, and NY)",
                                        "Permian (TX and NM)",
                                        "Haynesville (LA and TX)",
                                        "Utica (OH, PA, and WV)",
                                        "Eagle Ford (TX)",
                                        "Woodford (OK)",
                                        "Niobrara-Codell (CO and WY)",
                                        "Bakken (ND and MT)",
                                        "Mississippian (OK)",
                                        "Barnett (TX)",
                                        "Fayetteville (AR)",
                                        "Rest of US Shale"))))

NAT_GAS_PRODUCTION_BY_PLAY_Graph <- ggplot(NAT_GAS_PRODUCTION_BY_PLAY, aes(fill=name, x=Date, y=value)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 32) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "Bcf/d"),limits = c(0,100), breaks = c(25,50,75,100), expand = c(0,0)) +
  ylab("Billions of Cubic Feed Per Day") +
  ggtitle("Dry Shale Gas Production by Play") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Booming Production in the Permian and Haynesville Drive US Shale Natural Gas Production") +
  theme_apricitas + theme(legend.position = c(.25,.59)) +
  scale_fill_manual(name = NULL, values = rev(c("#5D3A00","#EFBDEB","#074F57","#550C18","#C6D4FF","#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("Marcellus (PA, WV, OH, and NY)",
                                                                                                                                                                                                                 "Permian (TX and NM)",
                                                                                                                                                                                                                 "Haynesville (LA and TX)",
                                                                                                                                                                                                                 "Utica (OH, PA, and WV)",
                                                                                                                                                                                                                 "Eagle Ford (TX)",
                                                                                                                                                                                                                 "Woodford (OK)",
                                                                                                                                                                                                                 "Niobrara-Codell (CO and WY)",
                                                                                                                                                                                                                 "Bakken (ND and MT)",
                                                                                                                                                                                                                 "Mississippian (OK)",
                                                                                                                                                                                                                 "Barnett (TX)",
                                                                                                                                                                                                                 "Fayetteville (AR)",
                                                                                                                                                                                                                 "Rest of US Shale")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*100), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NAT_GAS_PRODUCTION_BY_PLAY_Graph, "Nat Gas Production by Play Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


TIGHT_OIL_PRODUCTION_BY_PLAY <- read.xlsx("https://www.eia.gov/energyexplained/oil-and-petroleum-products/data/US-tight-oil-production.xlsx") %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  select(-X12) %>%
  pivot_longer(-Date) %>%
  mutate(name = gsub("\\."," ", name)) %>%
  mutate(name = gsub("U S  tight oil","US Tight Oil", name)) %>%
  mutate(name = factor(name, levels = rev(c("Wolfcamp (TX and NM Permian)",
                                            "Bakken (ND and MT)",
                                            "Eagle Ford (TX)",
                                            "Spraberry (TX Permian)",
                                            "Bonespring (TX and NM Permian)",
                                            "Niobrara-Codell (CO and WY)",
                                            "Mississippian (OK)",
                                            "Austin Chalk (LA and TX)",
                                            "Woodford (OK)",
                                            "Rest of US Tight Oil"))))
  
TIGHT_OIL_PRODUCTION_BY_PLAY_Graph <- ggplot(TIGHT_OIL_PRODUCTION_BY_PLAY, aes(fill=name, x=Date, y=value)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 32) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "Mbbl/d"),limits = c(0,9), breaks = c(3,6,9), expand = c(0,0)) +
  ylab("Millions of Barrels Per Day") +
  ggtitle("US Tight Oil Production by Play") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Booming Production in the Permian and Haynesville Drive US Shale Natural Gas Production") +
  theme_apricitas + theme(legend.position = c(.25,.59)) +
  scale_fill_manual(name = NULL, values = rev(c("#EFBDEB","#C6D4FF","#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("Wolfcamp (TX and NM Permian)",
                                                                                                                                                                                                    "Bakken (ND and MT)",
                                                                                                                                                                                                    "Eagle Ford (TX)",
                                                                                                                                                                                                    "Spraberry (TX Permian)",
                                                                                                                                                                                                    "Bonespring (TX and NM Permian)",
                                                                                                                                                                                                    "Niobrara-Codell (CO and WY)",
                                                                                                                                                                                                    "Mississippian (OK)",
                                                                                                                                                                                                    "Austin Chalk (LA and TX)",
                                                                                                                                                                                                    "Woodford (OK)",
                                                                                                                                                                                                    "Rest of US Tight Oil")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*9), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TIGHT_OIL_PRODUCTION_BY_PLAY_Graph, "Tight Oil Production by Play Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

US_CONSUMPTION <- eia1_series("STEO.PATC_US.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = as.numeric(value)) %>%
  arrange(date)
CHINA_CONSUMPTION <- eia1_series("STEO.PATC_CH.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = as.numeric(value)) %>%
  arrange(date)
EUROPE_CONSUMPTION <- eia1_series("STEO.PATC_R03.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = as.numeric(value)) %>%
  arrange(date)
MIDEAST_CONSUMPTION <- eia1_series("STEO.PATC_R05.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = as.numeric(value)) %>%
  arrange(date)
INDIA_CONSUMPTION <- eia1_series("STEO.PATC_IN.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = as.numeric(value)) %>%
  arrange(date)
AFRICA_CONSUMPTION <- eia1_series("STEO.PATC_R06.A") %>%
  transmute(date = as.Date(paste0(period,"-01-01")), value = as.numeric(value)) %>%
  arrange(date)

GLOBAL_CONSUMPTION_GRAPH <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -300), "year"), xmax = max(US_CONSUMPTION$date), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "EIA Forecast", x = floor_date(as.Date(today() -1550), "month"), y = 2, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=US_CONSUMPTION, aes(x=date,y= value, color= "United States"), size = 1.25) +
  geom_line(data=CHINA_CONSUMPTION, aes(x=date,y= value, color= "China"), size = 1.25) +
  geom_line(data=EUROPE_CONSUMPTION, aes(x=date,y= value, color= "Europe"), size = 1.25) +
  geom_line(data=MIDEAST_CONSUMPTION, aes(x=date,y= value, color= "Middle East"), size = 1.25) +
  geom_line(data=INDIA_CONSUMPTION, aes(x=date,y= value, color= "India"), size = 1.25) +
  geom_line(data=AFRICA_CONSUMPTION, aes(x=date,y= value, color= "Africa"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl/d", accuracy = 1), limits = c(0,22.5),breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("Global Petroleum & Other Liquids Consumption") +
  labs(caption = "Graph created by @JosephPolitano using EIA data. Note: Europe Excludes Russia",subtitle = "Petroleum Demand Growth Continues to Come from Outside the US, Especially in China") +
  theme_apricitas + theme(legend.position = c(.15,.41)) +
  theme(plot.title = element_text(size = 24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("United States","China","Europe","Middle East","India","Africa")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*22.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLOBAL_CONSUMPTION_GRAPH, "Global Consumption Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#ADD DRILLING PRODUCTIVITY REPORT DATA

ANADARKO_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Anadarko Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Anadarko (OK/TX)")

APPALACHIA_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Appalachia Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Appalachia (PA/WV/OH/NY)")

BAKKEN_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Bakken Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Bakken (ND/MT)")

EAGLE_FORD_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Eagle Ford Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Eagle Ford (TX)")

HAYNESVILLE_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Haynesville Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Haynesville (LA/TX)")

NIOBRARA_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Niobrara Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Niobrara (CO/WY/NE/KS)")

PERMIAN_DPR <- read.xlsx("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx", sheet = "Permian Region") %>%
  setNames(c("date","Rigs","Prod_per_rig_oil","legacy_production_chg_oil","total_production_oil","production_per_rig_gas","legacy_production_chg_gas","total_production_gas")) %>% 
  slice(-1) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  mutate(region = "Permian (TX/NM)")


DPR_DATA <- rbind(ANADARKO_DPR,APPALACHIA_DPR,BAKKEN_DPR,EAGLE_FORD_DPR,HAYNESVILLE_DPR,NIOBRARA_DPR,PERMIAN_DPR)
  
DPR_DATA_NAT_GAS_Graph <- ggplot(DPR_DATA, aes(fill=region, x=date, y=total_production_gas/1000000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 32) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "Bcf/d"),limits = c(0,110), breaks = c(25,50,75,100), expand = c(0,0)) +
  ylab("Billions of Cubic Feet Per Day") +
  ggtitle("Nat Gas Production in Key Shale Regions") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Booming Shale Production in the Permian and Haynesville Drive US Nat Gas Production Growth") +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  scale_fill_manual(name = NULL, values = c("#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*110), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

DPR_DATA_Oil_Graph <- ggplot(DPR_DATA, aes(fill=region, x=date, y=total_production_oil/1000000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 32) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "MMbbl/d"),limits = c(0,10), breaks = c(3,6,9), expand = c(0,0)) +
  ylab("Millions of Barrels Per Day") +
  ggtitle("Oil Production in Key Shale Regions") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Booming Tight Oil Production in the Permian Has Driven US Oil Production Growth") +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  scale_fill_manual(name = NULL, values = c("#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = DPR_DATA_Oil_Graph, "DPR OIL Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = DPR_DATA_NAT_GAS_Graph, "DPR Nat Gas Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
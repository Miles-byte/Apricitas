pacman::p_load(eurostat,jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

devtools::install_github("oddworldng/INEbaseR")
library(INEbaseR)
#Spain INE Database API

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EMP_EXP_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/EI_BSEE_M_R2/1.0?compress=false"))

EMP_EXP_IT <- EMP_EXP_BULK %>%
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

EMPLOYMENT_NACE_2_BULK <- get_eurostat("namq_10_a10_e",legacy_bulk_download = FALSE)

EMPLOYMENT_INDEXED_NACE_2_ITALY <- EMPLOYMENT_NACE_2_BULK %>%
  filter(s_adj == "SCA" & geo == "IT" & TIME_PERIOD >= as.Date("2019-10-01") & unit == "THS_PER" & na_item == "EMP_DC") %>%
  arrange(TIME_PERIOD) %>%
  select(TIME_PERIOD, values, nace_r2) %>%
  pivot_wider(names_from = nace_r2, values_from = values) %>%
  setNames(c("date","Agriculture, Forestry, and Fishing", "Industry ex Construction","Manufacturing","Construction", "Wholesale & Retail Trade, Transport, Food Service, and Accomodations","Information and Communication","Finance and Insurance","Real Estate","Professional, Scientific, Technical, Administrative, and Support Services","Public Administration, Defense, Education, Health, and Social Work", "Arts, Entertainment, Recreation, and Other Service Activities","Total")) %>%
  mutate(across(where(is.numeric), ~ .x-.x[1])) %>%
  mutate(`Wholesale/Retail Trade, Transport, Food Service, Accomodations, Arts, Recreation, etc.` = `Wholesale & Retail Trade, Transport, Food Service, and Accomodations` + `Arts, Entertainment, Recreation, and Other Service Activities`) %>%
  mutate(`Professional & Business Services, Information, Communication, Finance, & Real Estate` = `Information and Communication` + `Finance and Insurance` + `Real Estate` + `Professional, Scientific, Technical, Administrative, and Support Services`) %>%
  select(-Manufacturing,-Total,-`Information and Communication`, -`Finance and Insurance`,-`Real Estate`,-`Professional, Scientific, Technical, Administrative, and Support Services`,-`Wholesale & Retail Trade, Transport, Food Service, and Accomodations`,-`Arts, Entertainment, Recreation, and Other Service Activities`) %>%
  pivot_longer(cols = `Agriculture, Forestry, and Fishing`:`Professional & Business Services, Information, Communication, Finance, & Real Estate`) %>%
  mutate(name = factor(name, c("Professional & Business Services, Information, Communication, Finance, & Real Estate","Construction","Industry ex Construction","Public Administration, Defense, Education, Health, and Social Work","Agriculture, Forestry, and Fishing","Wholesale/Retail Trade, Transport, Food Service, Accomodations, Arts, Recreation, etc.",NA)))

EMPLOYMENT_INDEXED_NACE_2_ITALY_LINE <- EMPLOYMENT_INDEXED_NACE_2_ITALY %>%
  group_by(date) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(name = NA)

EMPLOYMENT_INDEXED_NACE_2_ITALY_LINE_MAX <- EMPLOYMENT_INDEXED_NACE_2_ITALY %>%
  filter(value > 0) %>%
  group_by(date) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(date >= as.Date("2020-01-01")) %>%
  mutate(name = NA)

EMPLOY_GROWTH_IND_ITA_graph <- ggplot(data = EMPLOYMENT_INDEXED_NACE_2_ITALY, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_point(data = EMPLOYMENT_INDEXED_NACE_2_ITALY_LINE, aes(x=date, y = value/1000), size = 3, fill ="#970C10", color = "#970C10", shape = 23) +
  geom_line(data = EMPLOYMENT_INDEXED_NACE_2_ITALY_LINE, aes(x=date, y = value/1000, color = "null"), size = 1.25, show.legend = FALSE) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"), expand = c(0,0), limits = c(-1.500,round(max(EMPLOYMENT_INDEXED_NACE_2_ITALY_LINE_MAX$value / 1000), 1))) +
  ggtitle("The Shape of Italian Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "White Collar Industries, Construction, and Manufacturing are Driving Italian Job Growth") +
  theme_apricitas + theme(legend.position = c(.485,.145), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Professional & Business Services, Information, Communication, Finance, & Real Estate","Construction","Industry ex Construction","Public Administration, Defense, Education, Health, and Social Work","Agriculture, Forestry, and Fishing","Wholesale/Retail Trade, Transport, Food Service, Accomodations, Arts, Recreation, etc.")) +
  scale_color_manual(name= NULL,values = c("#970C10")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-10-01")-(.1861*(today()-as.Date("2019-10-01"))), xmax = as.Date("2019-10-01")-(0.049*(today()-as.Date("2019-10-01"))), ymin = -1.500-(.3*(round(max(EMPLOYMENT_INDEXED_NACE_2_ITALY_LINE_MAX$value / 1000), 1)+1.5)), ymax = -1.500) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_GROWTH_IND_ITA_graph, "Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ITALY_FOOD_CONSUMPTION <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_24_DF_DCCN_QNA_4,1.0/Q........./ALL/?detail=full&startPeriod=1995-01-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  subset(EXPEND_PURPOSE_COICOPCOFOG == "CP01" & VALUATION == "L_2015" & ADJUSTMENT == "Y") %>%
  transmute(value = obsValue/obsValue[1]*100, date = as.Date(as.yearqtr(obsTime, "%Y-Q%q")))

ITALY_FOOD_CONSUMPTION_GRAPH <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=ITALY_FOOD_CONSUMPTION, aes(x=date+45,y= value,color="Italy, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,115), expand = c(0,0)) +
  ylab("Index, Q1 1996 = 100") +
  ggtitle("The Italian Food Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISTAT Data",subtitle = "Rising Food Prices Have Forced Italian Food Consumption to Nearly 30 Year Lows") +
  theme_apricitas + theme(legend.position = c(.42,.14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Germany, Real Consumption of Food, Drink, and Tobacco","France, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco","Italy, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1996-01-01")-(.1861*(today()-as.Date("1996-01-01"))), xmax = as.Date("1996-01-01")-(0.049*(today()-as.Date("1996-01-01"))), ymin = 90-(.3*25), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ITALY_FOOD_CONSUMPTION_GRAPH, "Italy Food Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
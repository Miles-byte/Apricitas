pacman::p_load(censusapi,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EU_CORE_CPI <- fredr(series_id = "EA19CPALTT01GYM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU inflation
US_CORE_CPI <- fredr(series_id = "CPHPTT01EZM659N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US inflation

EU_CORE_CPI <- fredr(series_id = "EA19CPGRLE01GYM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU core inflation
US_CORE_CPI <- fredr(series_id = "CPGRLE01USM659N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US core inflation

EU_EMP <- fredr(series_id = "LREM25TTEZQ156S",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU prime age epop
US_EMP <- fredr(series_id = "LREM25TTUSQ156S",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

EU_Nat_Gas <- fredr(series_id = "PNGASEUUSDM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
US_Nat_Gas <- fredr(series_id = "PNGASUSUSDM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

ICE_HY_SPREAD_EURO <- fredr(series_id = "BAMLHE00EHYIOAS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

EURO_NGDP <- fredr(series_id = "EUNNGDP",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US NGDP
US_NGDP <- fredr(series_id = "GDP",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #EU NGDP

GDPTrend <- data.frame(date = c(seq(as.Date("2019-10-01"), as.Date("2022-01-01"), "months")), trend = 21694.46*1.003274^(0:27))

GRETENYR <- fredr(series_id = "IRLTLT01GRM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
ITATENYR <- fredr(series_id = "IRLTLT01ITM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
PORTENYR <- fredr(series_id = "IRLTLT01PTM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop
GERTENYR <- fredr(series_id = "IRLTLT01DEM156N",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #US prime age epop

SpreadsRbind <- pivot_wider(rbind(GRETENYR,ITATENYR,PORTENYR,GERTENYR), names_from = series_id)

EU_Spreads_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  geom_line(data=SpreadsRbind, aes(x=date,y= (IRLTLT01GRM156N-IRLTLT01DEM156N)/100, color= "Greece"), size = 1.25) +
  geom_line(data=SpreadsRbind, aes(x=date,y= (IRLTLT01ITM156N-IRLTLT01DEM156N)/100, color= "Italy"), size = 1.25) +
  geom_line(data=SpreadsRbind, aes(x=date,y= (IRLTLT01PTM156N-IRLTLT01DEM156N)/100, color= "Portugal"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Spreads with German Bonds, %") +
  ggtitle("European Financial Tightening") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Financial Tightening is Increasing Eurozone Bond Spreads Again") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "Spreads Between 10 Year German Government Bonds",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

US_EU_NGDP_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = EURO_NGDP, aes(x=date, y = value/30252.62, color = "US NGDP"), size = 1.25) + 
  geom_line(data = US_NGDP, aes(x=date, y = value/216.9446 , color = "Euro Area NGDP"), size = 1.25) + 
  geom_line(data = GDPTrend, aes(x=date, y = trend/216.9446, color = "4% NGDP Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,115), breaks = c(85,90,95,100,105,110,115), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("The EU's Different Inflation Problem") +
  labs(caption = "Graph created by @JosephPolitano using BEA and EuroStat data",subtitle = "Aggregate Spending is Above Trend in the US-But Not in the EU") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("US NGDP","Euro Area NGDP","4% NGDP Growth Trend"),guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 85-(.3*30), ymax = 85) +
  coord_cartesian(clip = "off")

US_EU_NAT_GAS_Graph <- ggplot() + #plotting US/EU Nat Gas Prices
  geom_line(data=US_Nat_Gas, aes(x=date,y= value, color= "US Natural Gas"), size = 1.25) +
  geom_line(data=EU_Nat_Gas, aes(x=date,y= value, color= "EU Natural Gas"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,35), breaks = c(0,10,20,30), expand = c(0,0)) +
  ylab("US Dollars per MMBtu") +
  ggtitle("The EU's Different Inflation Problem") +
  labs(caption = "Graph created by @JosephPolitano using IMF data",subtitle = "Energy Prices Are Spiking in the EU, Pulling Up Inflation") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*35), ymax = 0) +
  coord_cartesian(clip = "off")

#using getcensus to import data on us exports of nat gas by weight
US_NAT_GAS_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "VES_WGT_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "2711110000", #nat gas commodity code
  CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
)

US_NAT_GAS_EXPORTS$time <- as.Date(as.yearmon(US_NAT_GAS_EXPORTS$time))
US_NAT_GAS_EXPORTS$VES_WGT_MO <- as.numeric(US_NAT_GAS_EXPORTS$VES_WGT_MO)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("-","All US LNG Exports",US_NAT_GAS_EXPORTS$CTY_CODE)
US_NAT_GAS_EXPORTS$CTY_CODE <- gsub("4XXX","US LNG Exports to Europe",US_NAT_GAS_EXPORTS$CTY_CODE)

US_NAT_GAS_EXPORTS_Graph <- ggplot() + #plotting components of excess savings
  geom_area(data = US_NAT_GAS_EXPORTS, aes(x = time, y = VES_WGT_MO/1000000000, fill = CTY_CODE), color = NA, size = 0, position = "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "B", accuracy = 1),limits = c(0,8), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  ylab("Billions of kg") +
  ggtitle("Arsenal of Democracy") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Natural Gas Exports Are Helping Ease A European Energy Shortage") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*2250), xmax = as.Date("2016-01-01")-(0.049*2250), ymin = 0-(.3*8), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EU_NAT_GAS_Graph, "US EU NAT GAS PRICES.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_NAT_GAS_EXPORTS_Graph, "US NAT GAS EXPORTS.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_EU_NGDP_Graph, "US EU NGDP.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EU_Spreads_Graph, "EU Spreads.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
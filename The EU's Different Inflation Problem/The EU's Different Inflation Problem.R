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

US_NAT_GAS_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "VES_WGT_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2016 to 2022",
  E_COMMODITY = "2711110000",
  CTY_CODE = "4XXX",
  CTY_CODE = "-"
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


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
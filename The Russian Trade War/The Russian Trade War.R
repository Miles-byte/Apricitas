pacman::p_load(censusapi,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

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

US_RUSSIA_CRUDE_REFINED_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR","VES_WGT_MO","GEN_VAL_MO","I_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
  I_COMMODITY = "2709", #crude oil commodity code
  I_COMMODITY = "2710", #refined products commodity code
  CTY_CODE = "4621", #Russia country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(VES_WGT_MO = as.numeric(VES_WGT_MO)) %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(I_COMMODITY = gsub("2709","Crude Oil", I_COMMODITY)) %>%
  mutate(I_COMMODITY = gsub("2710","Refined Products", I_COMMODITY))
  
US_RUSSIA_CRUDE_REFINED_Graph <- ggplot() + #plotting crude and refined
  geom_line(data = US_RUSSIA_CRUDE_REFINED_IMPORTS, aes(x = time, y = GEN_VAL_MO/1000000000, color = I_COMMODITY), size = 1.25, position = "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 0.5),limits = c(0,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Sanction Strike") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Imports of Crude Oil and Refined Products from Russia Hit 0 in May") +
  theme_apricitas + theme(legend.position = c(.35,.83)) +
  scale_color_manual(name= "US Imports From Russia",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*2), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

test <- listCensusMetadata(
  name = "timeseries/intltrade/imports/hs", 
  type = "variables")

ggsave(dpi = "retina",plot = US_NAT_GAS_EXPORTS_Graph, "US Nat Gas Exports.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = US_RUSSIA_CRUDE_REFINED_Graph, "US Russia Crude Imports.png", type = "cairo-png") #cairo gets rid of anti aliasing

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
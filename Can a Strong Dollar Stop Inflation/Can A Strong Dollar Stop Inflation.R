pacman::p_load(censusapi,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

US_ENERGY_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = "from 2013 to 2022",
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
  time = "from 2013 to 2022",
  I_COMMODITY = "27", #energy commodity code
  #CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  select(time, value)

US_ENERGY_NET_EXP_MERGE <- merge(US_ENERGY_IMPORTS,US_ENERGY_EXPORTS, by = "time") %>%
  mutate(Net = value.y-value.x)

US_ENERGY_NET_EXP <- ggplot() + #plotting nat gas exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = US_ENERGY_NET_EXP_MERGE, aes(x = time, y = Net/1000000000, color = "US Net Energy Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-25,7), breaks = c(-25,-20,-15,-10,-5,0,5), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("The Energy Shock") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Net Energy Exports are at a Modern Record High") +
  theme_apricitas + theme(legend.position = c(.20,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -25-(.3*32), ymax = -25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ENERGY_NET_EXP, "US Net Exports.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
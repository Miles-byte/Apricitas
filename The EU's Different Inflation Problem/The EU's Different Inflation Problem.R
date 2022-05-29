pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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


ggsave(dpi = "retina",plot = Port_Throughput_Graph, "Port Throughput.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
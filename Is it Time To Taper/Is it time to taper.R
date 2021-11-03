pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

ECISERV <- fredr(series_id = "CIS201S000000000I",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #downloading ECI services
ECIGOOD <- fredr(series_id = "CIU201G000000000I",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #downloading ECI goods
PCESERV <- fredr(series_id = "DSERRG3M086SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #downloading ECI goods
PCEGOOD <- fredr(series_id = "DGDSRG3M086SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #downloading ECI goods

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Wage_Price_Graph <- ggplot() +
  geom_line(data = ECISERV, aes(x=date, y = value/100, color = "Services Compensation (ECI)"),size = 1.25, alpha = 1) + 
  geom_line(data = ECIGOOD, aes(x=date, y = value/100, color = "Goods Compensation (ECI)"),size = 1.25, alpha = 1) + 
  geom_line(data = PCESERV, aes(x=date, y = value/100, color = "Services Prices (PCE)"),size = 1.25, alpha = 0.5) + 
  geom_line(data = PCEGOOD, aes(x=date, y = value/100, color = "Goods Prices (PCE)"),size = 1.25, alpha = 0.5) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.025,0.065), breaks = c(-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Spiral? Not so Fast!") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "Price Growth is in Goods, but Wage Growth is in Services") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"), guide=guide_legend(override.aes=list(alpha=c(1,.5,1,.5))))

ggsave(dpi = "retina",plot = Wage_Price_Graph, "Spiral.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

AUINSA <- fredr(series_id = "AUINSA",observation_start = as.Date("2018-01-01")) #downloading auto inventories
Durable <- fredr(series_id = "N241RX1Q020SBEA",observation_start = as.Date("2018-01-01")) #downloading durable Goods Inventories
Invent  <- fredr(series_id = "A371RX1Q020SBEA",observation_start = as.Date("2018-01-01")) #downloading total inventories
MotorVehicle <- fredr(series_id = "N864RX1Q020SBEA",observation_start = as.Date("2018-01-01")) #downloading motor vehicle and parts dealers inventories
 

Motor_Vehicle_Graph <- ggplot() + #plotting components of excess savings
  geom_line(data = MotorVehicle, aes(x = date + 90, y = value/2.33, color = "Real private inventories: Retail trade: Motor Vehicle and Parts Dealers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(75,120), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Q4 Saw Motor Vehicle Inventories Grow For the First Time in 2021") +
  theme_apricitas + theme(legend.position = c(.50,.96)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-04-01")-(.1861*1440), xmax = as.Date("2018-04-01")-(0.049*1440), ymin = 75-(.3*45), ymax = 75) +
  coord_cartesian(clip = "off")

Durable_Graph <- ggplot() + #plotting components of excess savings
  geom_line(data = Durable, aes(x = date + 90, y = value/12.11, color = "Real private inventories: Durable Goods Industries"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(95,112), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Q4 Saw Durable Goods Industries' Inventories Grow For the First Time in 2021") +
  theme_apricitas + theme(legend.position = c(.50,.96)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-04-01")-(.1861*1440), xmax = as.Date("2018-04-01")-(0.049*1440), ymin = 75-(.3*45), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Motor_Vehicle_Graph, "Motor Vehicle.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
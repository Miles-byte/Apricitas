pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

CAPOP <- fredr(series_id = "CAPOP", units = "pc1") #downloading California population growth
RENTSF <- fredr(series_id = "CUURA422SEHA", observation_start = as.Date("1990-01-01")) #nominal rents for SF
STARTSSF <- fredr(series_id = "SANF806BPPRIV", observation_start = as.Date("1989-01-01")) #housing starts for SF

STARTSSF$value <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(STARTSSF$value, 12))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CAPOP_Graph <- ggplot() + 
  geom_line(data = CAPOP, aes(x = date, y = value/100, color = "California Annual Population Growth"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.02,.1), breaks = c(-.02,0,.02,.04,.06,.08,.1), expand = c(0,0)) +
  ylab("Population Growth, %") +
  ggtitle("The Shrinking State") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "California's Population Has Begun to Shrink") +
  theme_apricitas + theme(legend.position = c(.70,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1900-01-01")-(.1861*40100), xmax = as.Date("1900-01-01")-(0.049*40100), ymin = -.02-(.3*.1), ymax = -.02) +
  coord_cartesian(clip = "off")

RENTSF_Graph <- ggplot() + 
  geom_line(data = RENTSF, aes(x = date, y = value/1.48, color = "CPI: Rent of Primary Residences, SF-Oakland-Hayward CBSA"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,320), breaks = c(100,200,300), expand = c(0,0)) +
  ylab("Index, Jan 1990 = 100") +
  ggtitle("Shut Out") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Nominal Rents have Tripled in SF since 1990, Increasing by 50% Since 2010 Alone") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*12000), xmax = as.Date("1990-01-01")-(0.049*12000), ymin = 90-(.3*230), ymax = 90) +
  coord_cartesian(clip = "off")

STARTSSF_Graph <- ggplot() + 
  geom_line(data = STARTSSF, aes(x = date, y = value/1.48, color = "New Private Housing Structures Authorized by Building Permits, SF-Oakland-Berkeley"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,1200), breaks = c(0,300,600,900,1200), expand = c(0,0)) +
  ylab("Units, Monthly, 12 Month Moving Average") +
  ggtitle("Shut Out") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "The San Francisco Region Allows Virtually No New Units to be Built") +
  theme_apricitas + theme(legend.position = c(.52,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*12000), xmax = as.Date("1990-01-01")-(0.049*12000), ymin = 0-(.3*1200), ymax = 0) +
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = CAPOP_Graph, "California Population Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RENTSF_Graph, "San Francisco Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTSSF_Graph, "San Francisco Housing Starts.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
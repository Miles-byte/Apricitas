pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EPOPUSA <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #USA Epop

EPOPUSA_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=EPOPUSA, aes(x=date,y= value/100,color= "Prime Age (25-54) Employment-Population Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Still Below Full Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.68,.88)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("2000-06-01"), y = 0.825, color ="#FFE98F", size = 5) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0.69-(.3*0.14), ymax = 0.69) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOPUSA_Graph, "EPOPUSA.png", type = "cairo-png") 

EPOPCAN <- fredr(series_id = "LREM25TTCAM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #Canada Epop
EPOPAUS <- fredr(series_id = "LREM25TTAUM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #Aus EPop
EPOPJPN <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPGER <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPUK <- fredr(series_id = "LREM25TTGBQ156S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

EPOP_PEER_Graph <- ggplot() + #Plotting Anglophone Employment Levels
  geom_line(data=subset(EPOPAUS, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Australia"), size = 1.25)+ 
  geom_line(data=subset(EPOPUK, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "United Kingdom"), size = 1.25)+ 
  geom_line(data=subset(EPOPCAN, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Canada"), size = 1.25)+ 
  geom_line(data=subset(EPOPGER, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Germany"), size = 1.25)+ 
  geom_line(data=subset(EPOPJPN, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Japan"), size = 1.25)+ 
  geom_line(data=subset(EPOPUSA, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "USA"), size = 2)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.695,.885), breaks = c(.7,.75,.8,.85), expand = c(0,0)) +
  ylab("Prime Age 25-54 Employment-Population Ratio, %") +
  ggtitle("America's Great Employment Failure") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "US Employment Rates have Lagged Significantly Behind its Closest Peer Nations") +
  theme_apricitas + theme(legend.position = c(.35,.83), legend.spacing.y = unit(0, 'cm'),legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +
  scale_color_manual(name= "Prime Age (25-54) Employment-Population Ratio",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("USA","Japan","Germany","Canada","United Kingdom","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = .695-(.3*.19), ymax = .695) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_PEER_Graph, "EPOPPEER.png", type = "cairo-png") 

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
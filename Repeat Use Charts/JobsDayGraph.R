pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

EPop <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data
LAH <- fredr(series_id = "USLAH",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
U1RATE <- fredr(series_id = "U1RATE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data

#MANUAL EDITOR - DO NOT USE UNLESS FRED DOES NOT UPDATE
EPop[nrow(EPop) + 1,] = list(as.Date("2021-09-01"),"X", 25, as.Date("2021-10-07"),as.Date("2021-10-07"))
LAH[nrow(LAH) + 1,] = list(as.Date("2021-09-01"),"X", 25, as.Date("2021-10-07"),as.Date("2021-10-07"))
U1RATE[nrow(U1RATE) + 1,] = list(as.Date("2021-09-01"),"X", 25, as.Date("2021-10-07"),as.Date("2021-10-07"))

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


EPop_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=EPop, aes(x=date,y= value/100,color= "Prime Age (25-54) Employment-Population Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-10-01"))) +
  ggtitle("Well Below Full Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.75,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11535), xmax = as.Date("1990-01-01")-(0.049*11535), ymin = 0.69-(.3*0.14), ymax = 0.69) +
  coord_cartesian(clip = "off")

LAH_Graph <- ggplot() + #plotting leisure and hospitality employment
  geom_line(data=LAH, aes(x=date,y= value/1000,color= "All Employees, Leisure and Hospitality"), size = 1.25)+ 
  xlab("Date") +
  ylab("All Employees, Leisure and Hospitality") +
  scale_y_continuous(labels = paste(c(5, 10, 15), "M"), breaks = c(5,10,15), limits = c(5,18), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-10-01"))) +
  ggtitle("Lagging Growth in Leisure & Hospitality Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.75,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 5-(.3*13), ymax = 5) +
  coord_cartesian(clip = "off")

U1RATE_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=U1RATE, aes(x=date,y= value/100,color= "Unemployed 15 Weeks and Over, % of Labor Force"), size = 1.25)+ 
  xlab("Date") +
  ylab("Civilian Labor Force Unemployed 15 Weeks and Over, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.01,0.055), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-10-01"))) +
  ggtitle("Long Term Unemployment Remains Elevated") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.27,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0.01-(.3*0.045), ymax = 0.01) +
  coord_cartesian(clip = "off")

?scale_y_continuous

ggsave(dpi = "retina",plot = EPop_Graph, "EPopUSA.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LAH_Graph, "LAH.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = U1RATE_Graph, "U1RATE.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

pacman::p_load(usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

EPop <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)

ECI <- fredr(series_id = c("ECIALLCIV"),units = "pc1") #downloading ECI data yoy% change

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


ECI_Graph <- ggplot() + #plotting PCEPI growth
  geom_line(data=ECI, aes(x=date,y= value/100,color= "Employment Cost Index"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Percentage Change, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.01,0.04), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2001-01-01"),as.Date("2021-04-01"))) +
  ggtitle("Nominal Compensation Growth Remains Weak") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.75,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*7295), xmax = as.Date("2001-01-01")-(0.049*7295), ymin = 0.01-(.3*0.03), ymax = 0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECI_Graph, "ECIUSA.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

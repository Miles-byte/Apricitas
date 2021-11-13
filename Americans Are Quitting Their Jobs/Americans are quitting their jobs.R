pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

Total_Quits <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2019-01-01")) #downloading quits data

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Total_Quits_Graph <- ggplot() + #plotting leisure and hospitality employment
  geom_line(data=Total_Quits, aes(x=date,y= value/1000,color= "Quits, Total Nonfarm"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(2,3,4), limits = c(2,4.5), expand = c(0,0)) +
  ggtitle("You Can't Fire Me, I Quit!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "A Record Number of Americans are Quitting Their Jobs") +
  theme_apricitas + theme(legend.position = c(.75,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*960), xmax = as.Date("2019-01-01")-(0.049*960), ymin = 2-(.3*2.5), ymax = 2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Total_Quits_Graph, "Total Quits Chart.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

WAGE GROWTH BY INCOME QUINTILE
QUITS BY INDUSTRY (LAH, MANUFACTURING, BUSINESS/PROFESSIONAL, Information)

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()


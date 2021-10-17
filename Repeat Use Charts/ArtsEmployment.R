pacman::p_load(plm,transformr,stringi,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

ARTSEMPLOYMENT <- fredr(series_id = c("CES7071000001")) #downloading arts entertainment, and recreation employment data from FRED
TOTEMPLOYMENT <- fredr(series_id = c("PAYEMS")) #downloading total employment data from FRED
ARTSEMPLOYMENT_MERGE <- merge(ARTSEMPLOYMENT,TOTEMPLOYMENT, by = "date") #merge arts and total employment data

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Arts_Employment_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=ARTSEMPLOYMENT_MERGE, aes(x=date, y= value.x/value.y , color="All Employees, Arts, Entertainment, and Recreation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(0,.02), breaks = c(0,.005,.01,.015,0.02), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-07-01"))) +
  ylab("Number of Employees, % of Total Nonfarm Employees") +
  ggtitle("The Show Must Go On") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment in the Arts Remains Low, but is Recovering") +
  theme_apricitas + theme(legend.position = c(.65,.60)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11566), xmax = as.Date("1990-01-01")-(0.049*11566), ymin = 0-(.3*0.02), ymax = 0) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = Arts_Employment_Graph, "Arts Employment.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

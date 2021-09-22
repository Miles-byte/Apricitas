#The Inherent Flaws in Inflation Metrics

pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI <- fredr(series_id = c("CPIAUCSL")) #downloading PCEPI data
PCEPI <- fredr(series_id = c("PCEPI")) #downloading PCEPI data
IPD <- fredr(series_id = c("USAGDPDEFQISMEI")) #downloading IPD data
CPIURS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/r-cpi-u-rs-allitems.csv")
CPIURSCORE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/r-cpi-u-rs-alllessfe.csv")

CPIURS <- pivot_longer(CPIURS, names_to = "month", values_to = "value", cols = !c(YEAR,AVG,X)) #reorganizing CPIURS data to a real date format in only two columns
CPIURS <- mutate(CPIURS, date = lubridate::dmy(paste("01",month, YEAR, sep = "-")))
CPIURS <- select(CPIURS, -YEAR, -month, - AVG, -X)

CPIURSCORE <- pivot_longer(CPIURSCORE, names_to = "month", values_to = "value", cols = !c(YEAR,AVG)) #reorganizing CPIURS data to a real date format in only two columns
CPIURSCORE <- mutate(CPIURSCORE, date = lubridate::dmy(paste("01",month, YEAR, sep = "-")))
CPIURSCORE <- select(CPIURSCORE, -YEAR, -month, - AVG)


INF_INDEX_GRAPH <- ggplot() + #plotting all inflation indexes
  geom_line(data=PCEPI, aes(x=date,y=value/16.314*100,color= "PCE Price Index"), size = 1.25)+
  geom_line(data=CPI, aes(x=date,y=value/29.37*100,color= "Consumer Price Index"), size = 1.25) +
  geom_line(data=IPD, aes(x=date,y=value/15.81326*100,color= "Implicit Price Deflator"), size = 1.25)+
  geom_line(data=CPIURS, aes(x=date,y=value/(100/2.134)*100,color= "Consumer Price Index - Research Series"), size = 1.25)+
  ylab("Index, January 1960 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,1000), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1950-01-01"),as.Date("2021-09-01"))) +
  ggtitle("The Importance of Indexes") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Federal Reserve Data", subtitle = "CPI, Even the updated 'Research Series', Tends to Overstate Inflation") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26176), xmax = as.Date("1950-01-01")-(0.049*26176), ymin = 0-(.3*1000), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INF_INDEX_GRAPH, "The Importance of Indexes.png", type = "cairo-png") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

RentalVacancy <- fredr(series_id = c("RRVRUSQ156N"), observation_start = as.Date("2000-01-01")) #downloading Rental Vacancy Rates
Housing_Starts <- fredr(series_id = c("HOUST"),observation_start = as.Date("2000-01-01")) #downloading housing starts
Civilian_Population <- fredr(series_id = c("CNP16OV"),observation_start = as.Date("2000-01-01")) #downloading adult noninstitutional civilian population data
Shelter_CPI <- fredr(series_id = c("CUSR0000SAH1"),observation_start = as.Date("2000-01-01")) #downloading adult noninstitutional civilian population data
All_Items_Less_Shelter_CPI <- fredr(series_id = c("CUSR0000SA0L2"),observation_start = as.Date("2000-01-01")) #downloading adult noninstitutional civilian population data

Shelter_CPI_Merge <- merge(All_Items_Less_Shelter_CPI,Shelter_CPI, by = "date")
Pop_Housing_Merge <- merge(Civilian_Population,Housing_Starts, by = "date")

Shelter_Shortage_Graph <- ggplot() + #plotting all inflation indexes
  geom_line(data=Shelter_CPI_Merge, aes(x=date,y=(value.y/value.x)/(191/163)*100,color= "Relative Shelter Price Index (CPI)"), size = 1.25)+
  geom_line(data=Pop_Housing_Merge, aes(x=date,y=(value.y/value.x)/(1636/211410)*100,color= "New Private Housing Starts Per Adult"), size = 1.25) +
  geom_line(data=RentalVacancy, aes(x=date,y=value/7.9*100,color= "Rental Vacancy Rate"), size = 1.25)+
  ylab("Index, January 2000 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(25,150), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2000-01-01"),as.Date("2021-09-01"))) +
  ggtitle("Housing: The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Census Data", subtitle = "A Shortage of Housing Construction Has Driven Prices Up and Vacancies Down") +
  theme_apricitas + theme(legend.position = c(.77,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7914), xmax = as.Date("2000-01-01")-(0.049*7914), ymin = -25-(.3*125), ymax = -25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Shelter_Shortage_Graph, "Shelter Shortage.png", type = "cairo-png") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

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

Shelter_CPI_Merge <- merge(All_Items_Less_Shelter_CPI,Shelter_CPI, by = "date")#mergind data for shelter and all items less shelter
Pop_Housing_Merge <- merge(Civilian_Population,Housing_Starts, by = "date")#merging civ pop and housing starts data

Estonia_Unemployment <- data.frame(x = c(1989,1990,1991,1995,2000,2005,2006,2007,2008,2009,2010), y = c(0.3,0.3,0.8,4.7,6.2,3.9,3,2.4,2.9,7.1,8.6)) #manually adding estonia unemployment data
colnames(Estonia_Unemployment) <- c("date","unemployment")

Hungary_Manager_Survey <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Capitalism%20and%20the%20Surplus%20Economy/Hungary_Manager_Survey.csv")
Hungary_Manager_Survey$date <- as.Date(Hungary_Manager_Survey$date)

Shelter_Shortage_Graph <- ggplot() + #plotting shelter shortage
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
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7914), xmax = as.Date("2000-01-01")-(0.049*7914), ymin = 25-(.3*125), ymax = 25) + #plotting the apricitas logo. The last variable in each of the multipication tables is the total size of the axis and keeps the logo in a consistent place across graphs
  coord_cartesian(clip = "off")

Hungary_Manager_Survey_Graph <- ggplot() + #plotting responses to hungaried managers survey
  geom_line(data=Hungary_Manager_Survey, aes(x=date,y=demand/100,color= "Insufficient Demand"), size = 1.25)+
  geom_line(data=Hungary_Manager_Survey, aes(x=date,y=materials/100,color= "Insufficient Supply of Raw Materials/Parts"), size = 1.25)+
  geom_line(data=Hungary_Manager_Survey, aes(x=date,y=genlabor/100,color= "Shortage of General Labor"), size = 1.25)+
  ylab("Percentage indicating item as an impediment to production, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1987-04-01"),as.Date("2012-04-01"))) +
  ggtitle("Impediments to Production in the Hungarian Transition") +
  labs(caption = "Graph created by @JosephPolitano using data from János Kornai (2013)", subtitle = "Hungarian Industrial Managers Shifted From Worrying About Inputs to Worrying About Demand") +
  theme_apricitas + theme(legend.position = c(.65,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotate("vline", x = as.Date("1988-11-24"), xintercept = as.Date("1988-11-24"), linetype = "dashed", color = "white", size = 1.25) +
  annotate("text", label = "Miklós Németh becomes", x = as.Date("1992-2-24"), y = .84, color = "white") +
  annotate("text", label = "Prime Minister", x = as.Date("1992-2-24"), y = .79, color = "white") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-04-01")-(.1861*9132), xmax = as.Date("1987-04-01")-(0.049*9132), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

Estonia_Unemployment_Graph <- ggplot() + #plotting responses to hungarian managers survey
  geom_line(data=Estonia_Unemployment, aes(x=date,y=unemployment/100,color= "Unemployment Rate, Estonia"), size = 1.25)+
  ylab("Unemployment Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.1), expand = c(0,0)) +
  scale_x_continuous(limits = c(1989,2010)) +
  ggtitle("The Surplus Economy and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using data from János Kornai (2013)", subtitle = "Unemployment Permanently Spiked in Estonia After the Planned Economy Ended") +
  theme_apricitas + theme(legend.position = c(.65,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotate("vline", x = 1991.66, xintercept = 1991.66, linetype = "dashed", color = "white", size = 1.25) +
  annotate("text", label = "Estonia Declares", x = 1993.7, y = .084, color = "white") +
  annotate("text", label = "Independence", x = 1993.7, y = .079, color = "white") +
  annotation_custom(apricitas_logo_rast, xmin = 1989-(.1861*21), xmax = 1989-(0.049*21), ymin = 0-(.3*.1), ymax = 0) +
  coord_cartesian(clip = "off")

#recreate capacity utilization in the US auto sector graph
#recreate input/output inventories graph

ggsave(dpi = "retina",plot = Shelter_Shortage_Graph, "Shelter Shortage.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Hungary_Manager_Survey_Graph, "Hungary Manager's Survey.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Estonia_Unemployment_Graph, "Unemployment Rate Estonia.png", type = "cairo-png") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

pacman::p_load(estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Using an updated version of the Chinese national stats bureau rstatscn package that fixes a json error in the old database
install_github("pcdi/rstatscn")
library(rstatscn)

#statscnQueryZb(dbcode='hgyd', lang = "en") #lists all datasets with monthly national data
#statscnQueryZb(dbcode='hgjd', lang = "en") #lists all datasets with quarterly national data
#statscnQueryZb('A08',dbcode='hgyd', lang = "en")
#statscnQueryZb('A01',dbcode='hgjd', lang = "en")
#CPI <- statscnQueryData('A010301',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") #headline inflation data (A01)
#please note: the package is weird in that it will only let me retrieve a certain n of previous results, so I just used 60 here
#statscnQueryLastN(60, lang = "en")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

IND_PRO_PV <- statscnQueryData('A02092E',dbcode='hgyd',lang = "en", rowcode = "sj", colcode = "zb") 

IND_PRO_PV <- statscnQueryLastN(700, lang = "en") %>%
  mutate(date = as.Date(as.yearmon(rownames(.)))) %>%
  subset(.,`Output of Photovoltaic Cells, Current Period` != 0) %>%
  .[order(nrow(.):1),] %>%
  mutate(rollmean = c(0,0,0,0,0,0,0,0,0,0,rollmean(`Output of Photovoltaic Cells, Current Period`,11))) %>%
  subset(date >= as.Date("2016-01-01"))

IND_PRO_PV_GRAPH <- ggplot() + #plotting Chinese PV Production
  geom_line(data= IND_PRO_PV, aes(x=date,y=rollmean/100,color= "Rolling 1-year Average"), size = 1.25) +
  geom_line(data= IND_PRO_PV, aes(x=date,y=`Output of Photovoltaic Cells, Current Period`/100 ,color= "Chinese Industrial Production of Photovoltaic Cells, Monthly"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"),limits = c(0, round(max(IND_PRO_PV$`Output of Photovoltaic Cells, Current Period`/1000))*10), expand = c(0,0)) +
  ylab("GW of Capacity, Monthly") +
  ggtitle("Chinese Solar Panel Production") +
  labs(caption = "Graph created by @JosephPolitano using National Bureau of Statistics of China Data",subtitle = "Chinese Solar Production is Growing Exponentially and Has Surged Post-Pandemic") +
  theme_apricitas + theme(legend.position = c(.415,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(IND_PRO_PV$date)-(.1861*(max(IND_PRO_PV$date)-min(IND_PRO_PV$date))), xmax = min(IND_PRO_PV$date)-(0.049*(max(IND_PRO_PV$date)-min(IND_PRO_PV$date))), ymin = 0-(.3*(round(max(IND_PRO_PV$`Output of Photovoltaic Cells, Current Period`/1000))*10)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_PV_GRAPH, "China Ind Pro PV Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Add Lithium Ion Batteries
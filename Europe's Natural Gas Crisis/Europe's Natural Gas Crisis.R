pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Creating a theme for charts
theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), plot.title = element_text(size = 28, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"
apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Importing Nordstream import data 
NordStream_Pipeline_Import <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ru-tso-0002itp-00120exit,de-tso-0018itp-00297entry,de-tso-0016itp-00251entry,de-tso-0005itp-00491entry,de-tso-0001itp-00247entry,de-tso-0015itp-00250entry,de-tso-0001itp-00251entry,de-tso-0020itp-00454entry,de-tso-0017itp-00247entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1")
NordStream_Pipeline_Import <- subset(NordStream_Pipeline_Import, operatorLabel == c("OPAL Gastransport","NEL Gastransport")) #grabbing the two correct tsoItemIdentifiers for NordStream Flow
NordStream_Pipeline_Import <- select(NordStream_Pipeline_Import, "value","operatorLabel","periodFrom")
NordStream_Pipeline_Import$periodFrom <- as.Date(NordStream_Pipeline_Import$periodFrom)


NordStream_Pipeline_Import <- pivot_wider(NordStream_Pipeline_Import, names_from = operatorLabel)

NordStream_Pipeline_Import_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=subset(NordStream_Pipeline_Import,periodFrom > as.Date("2021-09-01")) , aes(x=periodFrom,y= (`NEL Gastransport` + `OPAL Gastransport`)/1000000000, color = "Nord Stream Pipeline Imports"), size = 1.25)+ 
  xlab("Date") +
  ylab("Daily Import Volumes, TWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh", accuracy = 0.5), limits = c(0,2), breaks = c(0,.5,1,1.5,2), expand = c(0,0)) +
  ggtitle("Europe's Natural Gas Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Entsog data with assistance from Bruegel", subtitle = "Imports Through the Nord Stream Pipeline have Decreased Nearly 60%") +
  theme_apricitas + theme(legend.position = c(0.25,0.55)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-09-01")-(.1861*(300)), xmax = as.Date("2021-09-01")-(0.049*(300)), ymin = 0-(.3*2), ymax = 0) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = NordStream_Pipeline_Import_Graph, "Nord Stream Pipeline Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
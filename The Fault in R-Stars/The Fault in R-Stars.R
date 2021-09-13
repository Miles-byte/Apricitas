#The Fault in R-Stars
pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
FOMC_Projections_UNRATE <- fredr(series_id = c("UNRATECTMLR")) #downloading midpoint of FOMC estimates for longer run unemployment
UNRATE <- fredr(series_id = c("UNRATE")) #downloading unemployment rate
NROU_CBO <- fredr(series_id = "NROU") #downloading CBO's natural rate of unemployment estimates
PCEPI <- fredr(series_id = "PCEPI",units = "pc1") #downloading PCEPI estimates
Trimmed_PCEPI <- fredr(series_id = "PCETRIM12M159SFRBDAL") #downloading trimmed mean PCEPI estimates

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apriticas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apriticitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FOMC_UNRATE_GRAPH <- ggplot() + #plotting FOMC projections alongside Unemployment Rate
  geom_line(data=FOMC_Projections_UNRATE, aes(x=date,y=value/100,color= "Midpoint of FOMC Projections for Longer Run Unemployment Rate"), size = 1.25)+
  geom_line(data=UNRATE, aes(x=date,y=value/100,color= "Unemployment Rate"), size = 1.25)+
  ylab("Unemployment Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.03,.15), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2008-01-01"),as.Date("2021-09-01"))) +
  ggtitle("The FOMC Has Underestimated How Low Unemployment Can Go") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Federal Reserve Data", subtitle = "As a Result, FOMC Projections of Long Term Unemployment Have Been Decreasing") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(g, xmin = as.Date("2005-06-01"), xmax = as.Date("2007-05-01"), ymin = -0.005, ymax = .03) +
  coord_cartesian(clip = "off")

#Percentages for Logo Organization
#xmin = x- (944/4992) (18.91%)   xmax = x - (245/4992) (4.9%) ymin = y - (0.035/0.12) (0.3%) ymax = y

CBO_NROU_GRAPH <- ggplot() + #plotting long run unemployment rate against CBO NROU estimate
  geom_line(data=NROU_CBO, aes(x=date,y=value/100,color= "CBO NAIRU Estimate"), size = 1.25) +
  geom_line(data=UNRATE, aes(x=date,y=value/100,color= "Unemployment Rate"), size = 1.25) +
  geom_line(data=PCEPI, aes(x=date,y=value/100,color= "PCE Inflation"), size = 1.25) +
  geom_line(data=Trimmed_PCEPI, aes(x=date,y=value/100,color= "Trimmed Mean PCE Inflation"), size = 1.25) +
  ylab("Unemployment/Inflation Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.02,.10), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2020-01-01"))) +
  ggtitle("Un-Natural History") +
  labs(caption = "Graph created by @JosephPolitano using BLS, CBO, BEA and Federal Reserve Data", subtitle = "CBO's 'Natural' Unemployment Rate Estimate Does Not Predict Inflation") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,breaks = c("Unemployment Rate", "CBO NAIRU Estimate", "PCE Inflation", "Trimmed Mean PCE Inflation"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(g, xmin = as.Date("1990-01-01")-2038, xmax = as.Date("1990-01-01")-537, ymin = -0.02-0.036, ymax = -0.02) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = FOMC_UNRATE_GRAPH, "FOMC.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = CBO_NROU_GRAPH, "CBO NROU Graph.png", type = "cairo-png") 


warnings()

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

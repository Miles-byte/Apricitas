#The Fault in R-Stars
pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
FOMC_Projections_UNRATE <- fredr(series_id = c("UNRATECTMLR")) #downloading midpoint of FOMC estimates for longer run unemployment
UNRATE <- fredr(series_id = c("UNRATE")) #downloading unemployment rate
NROU_CBO <- fredr(series_id = "NROU") #downloading CBO's natural rate of unemployment estimates
PCEPI <- fredr(series_id = "PCEPI",units = "pc1") #downloading PCEPI estimates
Trimmed_PCEPI <- fredr(series_id = "PCETRIM12M159SFRBDAL") #downloading trimmed mean PCEPI estimates
NAIRU_Vintage_CBO <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Fault%20in%20R-Stars/CBO_Vintages.csv")
Tealbook_Estimates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Fault%20in%20R-Stars/Tealbook_Estimates.csv")

Tealbook_Estimates$Date <- as.Date(Tealbook_Estimates$Date, "%m/%d/%Y")

NAIRU_Vintage_CBO$Date <- as.Date(NAIRU_Vintage_CBO$Date, "%m/%d/%Y")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FOMC_UNRATE_GRAPH <- ggplot() + #plotting FOMC projections alongside Unemployment Rate
  geom_line(data=FOMC_Projections_UNRATE, aes(x=date,y=value/100,color= "Midpoint of FOMC Projections for Longer Run Unemployment Rate"), size = 1.25)+
  geom_line(data=Tealbook_Estimates, aes(x=Date,y=LR_Unemployment_Rate/100,color= "Federal Reserve 'Tealbook' NAIRU Estimate"), size = 1.25) +
  geom_line(data=UNRATE, aes(x=date,y=value/100,color= "Unemployment Rate"), size = 1.25)+
  ylab("Unemployment Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.03,.15), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2008-01-01"),as.Date("2021-09-01"))) +
  ggtitle("The FOMC Has Underestimated How Low Unemployment Can Go") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Federal Reserve Data", subtitle = "As a Result, FOMC Projections of Long Term Unemployment Have Been Decreasing") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-06-01"), xmax = as.Date("2007-05-01"), ymin = -0.005, ymax = .03) +
  coord_cartesian(clip = "off")

#Percentages for Logo Organization
#xmin = x- (944/4992) (18.91%)   xmax = x - (245/4992) (4.9%) ymin = y - (0.035/0.12) (0.3%) ymax = y

CBO_NROU_GRAPH <- ggplot() + #plotting long run unemployment rate against CBO NROU estimate
  geom_line(data=NROU_CBO, aes(x=date,y=value/100,color= "CBO Noncyclical Unemployment Rate"), size = 1.25) +
  geom_line(data=UNRATE, aes(x=date,y=value/100,color= "Unemployment Rate"), size = 1.25) +
  geom_line(data=PCEPI, aes(x=date,y=value/100,color= "PCE Inflation"), size = 1.25) +
  geom_line(data=Trimmed_PCEPI, aes(x=date,y=value/100,color= "Trimmed Mean PCE Inflation"), size = 1.25) +
  ylab("Unemployment/Inflation Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.02,.10), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2020-01-01"))) +
  ggtitle("Un-Natural History") +
  labs(caption = "Graph created by @JosephPolitano using BLS, CBO, BEA, and Federal Reserve Data", subtitle = "CBO's Noncyclical Unemployment Rate Estimate Does Not Predict Inflation") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,breaks = c("Unemployment Rate", "CBO Noncyclical Unemployment Rate", "PCE Inflation", "Trimmed Mean PCE Inflation"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-2038, xmax = as.Date("1990-01-01")-537, ymin = -0.02-0.036, ymax = -0.02) +
  coord_cartesian(clip = "off")

CBO_Vintages_Graph <- ggplot() + #mention from potential GDP estimates
  geom_line(data=NAIRU_Vintage_CBO, aes(x=Date,y=X2002_Vintage_NAIRU/100,color= "2002 Vintage NAIRU"), size = 1.25) +
  geom_line(data=NAIRU_Vintage_CBO, aes(x=Date,y=X2007_Vintage_NAIRU/100,color= "2007 Vintage NAIRU"), size = 1.25) +
  geom_line(data=NAIRU_Vintage_CBO, aes(x=Date,y=X2012_Vintage_NAIRU/100,color= "2012 Vintage NAIRU"), size = 1.25) +
  geom_line(data=NAIRU_Vintage_CBO, aes(x=Date,y=X2017_Vintage_NAIRU/100,color= "2017 Vintage NAIRU"), size = 1.25) +
  geom_line(data=NAIRU_Vintage_CBO, aes(x=Date,y=X2021_NAIRU/100,color= "2021 NAIRU"), size = 1.25) +
  ylab("Unemployment/Inflation Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(.04,.06), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2000-01-01"),as.Date("2032-01-01"))) +
  ggtitle("Un-Natural Fluctuations") +
  labs(caption = "Graph created by @JosephPolitano using BLS, CBO, BEA, and Federal Reserve Data", subtitle = "CBO's NAIRU Estimates Rose After 2008 But Have Been Declining Since 2012") +
  theme_apricitas + theme(legend.position = c(.85,.62)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*11688), xmax = as.Date("2000-01-01")-(0.049*11688), ymin = 0.04-(.3*0.02), ymax = 0.04) +
  coord_cartesian(clip = "off")

Tealbook_Rstar_Graph_Inflation <- ggplot() + #mention from potential GDP estimates
  geom_line(data=Tealbook_Estimates, aes(x=Date,y=R._Estimate.FRB.US./100,color= "R* Estimate"), size = 1.25) +
  geom_line(data=Tealbook_Estimates, aes(x=Date,y=Actual_RFFR/100,color= "Real Federal Funds Rate"), size = 1.25) +
  geom_line(data=PCEPI, aes(x=date,y=value/100,color= "PCE Inflation"), size = 1.25) +
  geom_line(data=Trimmed_PCEPI, aes(x=date,y=value/100,color= "Trimmed Mean PCE Inflation"), size = 1.25) +
  ylab("Inflation/Interest Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.1,.05),breaks = c(-.1,-.05,0,0.05), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2016-01-01"))) +
  ggtitle("The Fault in R-Stars") +
  labs(caption = "Graph created by @JosephPolitano using BEA and Federal Reserve Data", subtitle = "Inflation is Not Highly Correlated With Gaps Between R* Estimates and Real Interest Rates") +
  theme_apricitas + theme(legend.position = c(.85,.42)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*4017), xmax = as.Date("2005-01-01")-(0.049*4017), ymin = -0.1-(.3*0.15), ymax = -0.1) +
  coord_cartesian(clip = "off")

Tealbook_Rstar_Graph_Unemployment <- ggplot() + #mention from potential GDP estimates
  geom_line(data=Tealbook_Estimates, aes(x=Date,y=R._Estimate.FRB.US./100,color= "R* Estimate"), size = 1.25) +
  geom_line(data=Tealbook_Estimates, aes(x=Date,y=Actual_RFFR/100,color= "Real Federal Funds Rate"), size = 1.25) +
  geom_line(data=UNRATE, aes(x=date,y=value/100,color= "Unemployment Rate"), size = 1.25) +
  ylab("Unemployment/Interest Rate, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.1,.1),breaks = c(-.1,-.05,0,0.05,.1), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2016-01-01"))) +
  ggtitle("The Fault in R-Stars") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Federal Reserve Data", subtitle = "R* Estimates Lean Hawkish and Give Too Much Credence to `Natural` Unemployment Rates") +
  theme_apricitas + theme(legend.position = c(.85,.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"), breaks = c("Unemployment Rate","R* Estimate","Real Federal Funds Rate"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*4017), xmax = as.Date("2005-01-01")-(0.049*4017), ymin = -0.1-(.3*0.2), ymax = -0.1) +
  coord_cartesian(clip = "off")

Tealbook_Potential_GDP <- ggplot() + #mention from potential GDP estimates
  geom_line(data=Tealbook_Estimates, aes(x=Date,y=LR_RGDP/100,color= "Long Run Potential RGDP Growth Estimate"), size = 1.25) +
  ylab("Long Run Potential GDP Growth, %") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.035),breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2016-01-01"))) +
  ggtitle("Unit Rooting To Death") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data", subtitle = "The Federal Reserve Consistently Revises Down Real Potential GDP Estimates") +
  theme_apricitas + theme(legend.position = c(.75,.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*4017), xmax = as.Date("2005-01-01")-(0.049*4017), ymin = 0-(.3*0.035), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FOMC_UNRATE_GRAPH, "FOMC.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = CBO_NROU_GRAPH, "CBO NROU Graph.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = CBO_Vintages_Graph, "CBO Vintages.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Tealbook_Rstar_Graph_Inflation, "Tealbook R-Star Inflation.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Tealbook_Rstar_Graph_Unemployment, "Tealbook R-Star Unemployment.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Tealbook_Potential_GDP, "Tealbook Potential GDP.png", type = "cairo-png") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

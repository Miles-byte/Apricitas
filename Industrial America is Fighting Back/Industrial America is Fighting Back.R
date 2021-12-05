pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), plot.title = element_text(size = 28, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


RPNRFI <- fredr(series_id = "PNFIC1",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading real private fixed investment data
Auto_Industrial_Capacity <- fredr(series_id = "CAPG33611SQ",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading auto industrial capacity

Rail_Freight_Carloads <- fredr(series_id = "RAILFRTCARLOADSD11",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading auto industrial capacity
Rail_Freight_Intermodal <- fredr(series_id = "RAILFRTINTERMODALD11",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading auto industrial capacity

Semiconductor <- fredr(series_id = "IPB53122S",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading semiconductor industrial production

Nat_Gas <- fredr(series_id = "IPN211111GS",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading natural gas industrial production
Crude_Oil <- fredr(series_id = "IPG211111CS",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading crude oil industrial capacity

Industrial_Machinery <- fredr(series_id = "IPG3332S",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading industrial machinery production
NEWORDER <- fredr(series_id = "NEWORDER",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #downloading industrial machinery production


Auto_Industrial_Capacity_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=Auto_Industrial_Capacity, aes(x=date,y= value, color = "Industrial Capacity: Automobile and Light Duty Motor Vehicles"), size = 1.25)+ 
  xlab("Date") +
  ylab("Industrial Capacity, 2017 = 100") +
  scale_y_continuous(limits = c(60,140), breaks = c(60,80,100,120,140), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Capacity Failure") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "American Automobile Industrial Capacity Still Shows the Scars of 2008") +
  theme_apricitas + theme(legend.position = c(0.40,0.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(11596)), xmax = as.Date("1990-01-01")-(0.049*(11596)), ymin = 60-(.3*80), ymax = 60) +
  coord_cartesian(clip = "off")

RPNFI_Graph <- ggplot() + #plotting real private non residential fixed investment
  geom_line(data=RPNRFI, aes(x=date,y= value/1000, color = "Real Private Non Residential Fixed Investment"), size = 1.25)+ 
  xlab("Date") +
  ylab("Trillions of 2012 Dollars") +
  scale_y_continuous(labels = scales::number_format(suffix = "T", accuracy = 0.5),limits = c(1.4,3), breaks = c(1.5,2,2.5,3), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Reinvesting") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Private Investment Recovered Rapidly-in Contrast to 2008") +
  theme_apricitas + theme(legend.position = c(0.40,0.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(5660)), xmax = as.Date("2006-01-01")-(0.049*(5660)), ymin = 1.4-(.3*1.6), ymax = 1.4) +
  coord_cartesian(clip = "off")

Train_Graph <- ggplot() + #plotting train freight carloads
  geom_line(data=Rail_Freight_Carloads, aes(x=date,y= value/10890.51, color = "Rail Freight Carloads"), size = 1.25)+ 
  geom_line(data=Rail_Freight_Intermodal, aes(x=date,y= value/11735.26, color = "Rail Freight Intermodal Traffic"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2018 = 100") +
  scale_y_continuous(limits = c(70,115), breaks = c(70,80,90,100,110), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("The Real Job is Laying the Track") +
  labs(caption = "Graph created by @JosephPolitano using BTS data", subtitle = "Rail Freight Traffic Quickly Recovered and Even Beat Pre-Recession Highs-in Contrast to 2008") +
  theme_apricitas + theme(legend.position = c(0.40,0.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(1369)), xmax = as.Date("2018-01-01")-(0.049*(1369)), ymin = 70-(.3*40), ymax = 70) +
  coord_cartesian(clip = "off")

Semiconductor_Graph <- ggplot() + #plotting semiconductor industrial production
  geom_line(data=Semiconductor, aes(x=date,y= value, color = "Industrial Production: Semiconductors, Printed Circuit Boards, and Other Electronic Components"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2017 = 100") +
  scale_y_continuous(limits = c(10,140), breaks = c(20,60,100,140), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Semiconductor Production Grows") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Semiconductor Production Kept Apace Despite Supply Chain Issues") +
  theme_apricitas + theme(legend.position = c(0.50,0.98),legend.text = element_text(size = 12, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(5752)), xmax = as.Date("2006-01-01")-(0.049*(5752)), ymin = 10-(.3*130), ymax = 10) +
  coord_cartesian(clip = "off")

Oil_Gas_Production_Graph <- ggplot() + #plotting oil and natural gas industrial production
  geom_line(data=Nat_Gas, aes(x=date,y= value/.756, color = "Industrial Production: Natural Gas"), size = 1.25)+ 
  geom_line(data=Crude_Oil, aes(x=date,y= value/.775, color = "Industrial Production: Crude Oil"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2006 = 100") +
  scale_y_continuous(limits = c(70,260), breaks = c(100,150,200,250), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("The Fuel Transition") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "American Fuel Production Has Boomed Since 2008, But Has Yet to Recover From COVID-19") +
  theme_apricitas + theme(legend.position = c(0.50,0.82)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(5752)), xmax = as.Date("2006-01-01")-(0.049*(5752)), ymin = 70-(.3*190), ymax = 70) +
  coord_cartesian(clip = "off")

Industrial_Machinery_Graph <- ggplot() + #plotting industrial machinery
  geom_line(data=Industrial_Machinery, aes(x=date,y= value, color = "Industrial Production: Industrial Machinery"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2006 = 100") +
  scale_y_continuous(limits = c(90,135), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("The Music of Machinery") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "American Production of Industrial Machinery is up Nearly 30% Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(0.50,0.82)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(1642)), xmax = as.Date("2017-01-01")-(0.049*(1642)), ymin = 90-(.3*45), ymax = 90) +
  coord_cartesian(clip = "off")

Capital_Goods_Graph <- ggplot() + #plotting industrial machinery
  geom_line(data=NEWORDER, aes(x=date,y= value/1000, color = "Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2006 = 100") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(59,80), breaks = c(60,70,80), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("The Music of Machinery") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "American Orders for Capital Goods Rocketed Up During the Pandemic") +
  theme_apricitas + theme(legend.position = c(0.48,0.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(1642)), xmax = as.Date("2017-01-01")-(0.049*(1642)), ymin = 59-(.3*21), ymax = 59) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Auto_Industrial_Capacity_Graph, "Auto Industrial Capacity.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = RPNFI_Graph, "Real Private Non Residential Fixed Investment.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Train_Graph, "Trains.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Semiconductor_Graph, "Semiconductor.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Oil_Gas_Production_Graph, "Oil and Gas.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Industrial_Machinery_Graph, "Industrial Machinery.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Capital_Goods_Graph, "Capital Goods.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
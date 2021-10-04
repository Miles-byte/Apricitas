pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

NFB_Productivity <- fredr(series_id = c("OPHNFB"), observation_start = as.Date("2000-01-01")) #downloading nonfarm business productivity
NFB_Productivity_Trend <- data.frame(x = c("2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01","2022-01-01"),
                                    y = c(91.642,93.9880352,96.3941289, 98.8618186, 101.3926812, 103.9883338, 106.6504351, 109.3806863, 112.1808318, 115.0526611, 117.9980093, 121.0187583, 124.1168385, 127.2942296,130.5529619)) #manually graphing trends
NFB_Productivity_Trend$x <- as.Date(NFB_Productivity_Trend$x)

NR_and_D <- fredr(series_id = c("Y694RC1Q027SBEA")) #downloading R&D spending in nominal dollars
N_Priv_R_and_D <- fredr(series_id = c("Y006RC1Q027SBEA")) #downloading gross private R&D spending in nominal dollars
GDP <- fredr(series_id = c("GDP")) #downloading GDP spending in nominal dollars

N_Priv_R_and_D <- merge(N_Priv_R_and_D, GDP, by = "date") #merging private and total R&D measurements with GDP 
NR_and_D <- merge(NR_and_D, GDP, by = "date") 

Internet_Users <- fredr(series_id = c("ITNETUSERP2USA")) #downloading internet users for the US
Cell_Phone_Subscriptions <- fredr(series_id = c("ITCELSETSP2USA")) #downloading mobile phone subscriptions for the US

Private_Investment_Information <- fredr(series_id = c("A679RC1Q027SBEA")) #downloading information fixed investment
Private_Investment_Computers <- fredr(series_id = c("B935RC1Q027SBEA")) #downloading software fixed investment

Private_Investment_Information <- merge(Private_Investment_Information, GDP, by = "date") #merging information and computer fixed investment
Private_Investment_Computers <- merge(Private_Investment_Computers, GDP, by = "date") 

NFB_Productivity_Graph <- ggplot() + #plotting nonfarm business productivity
  geom_line(data=NFB_Productivity, aes(x=date,y=value, color= "Nonfarm Business Sector Labor Productivity"), size = 1.25)+
  geom_line(data=NFB_Productivity_Trend, aes(x=x,y=y, color= "2000-2008 2.56% Trend Growth Rate"), size = 1.25, linetype = "dashed")+
  ylab("Index, 2012=100") +
  xlab("Date") +
  scale_y_continuous(limits = c(70,130), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2000-01-01"),as.Date("2021-09-02"))) +
  ggtitle("The Great Productivity Undershoot") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Productivity Has Undershot the Pre-Recession Trend Since 2010") +
  theme_apricitas + theme(legend.position = c(.45,.84)) +
  scale_color_manual(name= NULL,breaks = c("Nonfarm Business Sector Labor Productivity","2000-2008 2.56% Trend Growth Rate"),values = c("#FFE98F","#FFE98F"), guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,1))))+
  theme(legend.key.width =  unit(1.15, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7430), xmax = as.Date("2000-01-01")-(0.049*7430), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

R_and_D_graph <- ggplot() + #plotting r&d spending
  geom_line(data=NR_and_D, aes(x=date,y=value.x/value.y, color= "R&D Spending as a Share of GDP"), size = 1.25)+
  geom_line(data=N_Priv_R_and_D, aes(x=date,y=value.x/value.y, color= "Private Fixed Investment: R&D Spending as a Share of GDP"), size = 1.25)+
  ylab("% of GDP") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.04), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-09-02"))) +
  ggtitle("Developing Research") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "R&D Spending as a % of GDP Has Been Relatively Stable Despite Private Sector Spending Growth") +
  theme_apricitas + theme(legend.position = c(.45,.84)) +
  scale_color_manual(name= NULL,breaks = c("R&D Spending as a Share of GDP","Private Fixed Investment: R&D Spending as a Share of GDP"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))+
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11566), xmax = as.Date("1990-01-01")-(0.049*11566), ymin = 0-(.3*0.04), ymax = 0) +
  coord_cartesian(clip = "off")

Phone_Internet_Graph <- ggplot() + #plotting cellphone and internet penetration in the United States
  geom_line(data=Cell_Phone_Subscriptions, aes(x=date,y=value, color= "Cell Phone Subscriptions Per 100 People"), size = 1.25)+
  geom_line(data=Internet_Users, aes(x=date,y=value, color= "Internet Users Per 100 People"), size = 1.25)+
  ylab("Number Per 100 People") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,150), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2019-01-01"))) +
  ggtitle("The Information Economy") +
  labs(caption = "Graph created by @JosephPolitano using World Bank data", subtitle = "Innovations in Electronics Have Radically Transformed America in the Last 30 Years") +
  theme_apricitas + theme(legend.position = c(.45,.84)) +
  scale_color_manual(name= NULL, values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))+
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*10593), xmax = as.Date("1990-01-01")-(0.049*10593), ymin = 0-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off")

Information_Computers_Graph <- ggplot() + #plotting computers and investment information spending
  geom_line(data=Private_Investment_Information, aes(x=date,y=value.x/value.y, color= "Private Fixed Investment: Information Processing Equipment and Software"), size = 1.25)+
  geom_line(data=Private_Investment_Computers, aes(x=date,y=value.x/value.y, color= "Private Fixed Investment: Computers and Peripherals"), size = 1.25)+
  ylab("% of GDP") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.05), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-09-02"))) +
  ggtitle("Information, Innovation, and Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "COVID Has Pushed Investments in Information Technology") +
  theme_apricitas + theme(legend.position = c(.45,.44)) +
  scale_color_manual(name= NULL, breaks = c("Private Fixed Investment: Information Processing Equipment and Software","Private Fixed Investment: Computers and Peripherals"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))+
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11566), xmax = as.Date("1990-01-01")-(0.049*11566), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = NFB_Productivity_Graph, "Non Farm Business Productivity.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = R_and_D_graph, "R&D as a Share of GDP.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Phone_Internet_Graph, "Phone and Internet Penetration.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Information_Computers_Graph, "Information Processing and Computers Investment.png", type = "cairo-png") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

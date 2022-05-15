pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

RGDP <- fredr(series_id = "GDPC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP

RFSALEDOMPRIV <- fredr(series_id = "LB0000031Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

RealPrivateInventories <- fredr(series_id = "A371RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

RGDPQuarterly <- fredr(series_id = "A191RL1Q225SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
PCEContribQuarterly <- fredr(series_id = "DPCERY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
IVSTContribQuarterly <- fredr(series_id = "A006RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
NEXContribQuarterly <- fredr(series_id = "A019RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
GOVContribQuarterly <- fredr(series_id = "A822RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 

ContribQuarterlyBind <- rbind(PCEContribQuarterly,IVSTContribQuarterly,NEXContribQuarterly,GOVContribQuarterly)

ContribQuarterlyBind$series_id <- gsub("DPCERY2Q224SBEA", "Consumption", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A006RY2Q224SBEA", "Investment", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A019RY2Q224SBEA", "Net Exports", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A822RY2Q224SBEA", "Government", ContribQuarterlyBind$series_id)

LaborProductivity <- fredr(series_id = "OPHNFB",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pca") #labor productivity


#Graphing GDP
RGDP_Graph <- ggplot() +
  geom_line(data = RGDP, aes(x=date, y = value/1000, color = "Real GDP"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(17,20), breaks = c(17,18,19,20), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real GDP Shank 1.4% in Q1 2022 (at a Seasonally Adjusted Annual Rate)") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 17-(.3*3), ymax = 17) +
  coord_cartesian(clip = "off")

#Real Final Sales to Domestic Final Purchasers
RFSALEDOMPRIV_Graph <- ggplot() + 
  geom_line(data = RFSALEDOMPRIV, aes(x=date, y = value/1000, color = "Real Final Sales to Private Domestic Purchasers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(14.5,18), breaks = c(15,16,17,18), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Growth In Real Final Private Domestic Consumption and Investment was Robust") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 14.5-(.3*3.5), ymax = 14.5) +
  coord_cartesian(clip = "off")

#Real Private Inventories
RealPrivateInventories_Graph <- ggplot() + 
  geom_line(data = RealPrivateInventories, aes(x=date, y = value/1000, color = "Real Private Inventories"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = .05),limits = c(2.75,2.95), breaks = c(2.75,2.8,2.85,2.9,2.95), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Taking Inventory") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Inventory Growth Was High-But Lower Than Last Quarter") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 2.75-(.3*.2), ymax = 2.75) +
  coord_cartesian(clip = "off")

GDPMonthlyContrib_Graph <- ggplot(ContribQuarterlyBind, aes(fill=series_id, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.09), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("What's Got You Down?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "A Drop In Investment and Net Exports Made GDP Negative in Q1") +
  theme_apricitas + theme(legend.position = c(.92,.85)) +
  scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-08-15")-(.1861*500), xmax = as.Date("2020-08-15")-(0.049*500), ymin = -0.04-(.3*.13), ymax = -0.04) +
  coord_cartesian(clip = "off")

LaborProductivity_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = LaborProductivity, aes(x=date, y = value/100, color = "Nonfarm Business Sector: Labor Productivity for All Employed Persons"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.09,0.11), breaks = c(-0.05,0,0.05,.1), expand = c(0,0)) +
  ylab("Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Do You Buy It?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Drop in Output and Increase in Employment Caused Productivity to `Fall` Dramatically") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7000), xmax = as.Date("2000-01-01")-(0.049*7000), ymin = -0.09-(.3*.20), ymax = -0.09) +
  coord_cartesian(clip = "off")

CHINA IMPORTS NSA AND SA
INITIAL CLAIMS and CONTINUING CLAIMS


ggsave(dpi = "retina",plot = GDPMonthlyContrib_Graph, "Monthly GDP.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RFSALEDOMPRIV_Graph, "Real Final Private Sales.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RealPrivateInventories_Graph, "Real Private Inventories.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RGDP_Graph, "RGDP.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LaborProductivity_Graph, "Labor Productivity Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
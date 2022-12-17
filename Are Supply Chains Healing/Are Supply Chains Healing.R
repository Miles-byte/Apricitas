pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("quantmod")
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

tq_get("XOM")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#adding NY Fed global supply chains index data
GSCPI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/gscpi_data.csv") %>%
  mutate(date = ?..date) %>%
  mutate(date = as.Date(date))

#adding OTI data
OTI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/oti.csv") %>%
  mutate(FEWB_date = as.Date(FEWB_date)) %>%
  mutate(TPEB_date = as.Date(TPEB_date))

#adding Atlanta Fed BIE data
BIE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/bie.csv") %>%
  mutate(date = as.Date(?..dates))

#adding GSPC data
QSPC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/Compiled_QSPC.csv") %>%
  mutate(date = paste(Year,"Q",Quarter)) %>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q %q"))) %>%
  mutate(Value = as.numeric(Value))

#downloading aggregate PCE and PCE durable/nondurable goods for spending comparisons graph
PCE2 <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01")) #downloading PCE
PCEDG <- fredr(series_id = "PCEDG",observation_start = as.Date("2019-01-01")) #downloading PCE durable goods
PCEND <- fredr(series_id = "PCEND",observation_start = as.Date("2019-01-01")) #downloading PCE nondurable goods
PCEDGmerge <- merge(PCE2, PCEDG, by = "date")
PCENDmerge <- merge(PCE2, PCEND, by = "date")

#Downloading Inventory Sales Ratios
MANU_INV_SALES <- fredr(series_id = "MNFCTRIRSA",observation_start = as.Date("2019-01-01")) #downloading PCE
WHOLESALE_INV_SALES <- fredr(series_id = "WHLSLRIRSA",observation_start = as.Date("2019-01-01")) #downloading PCE durable goods
RETAIL_INV_SALES <- fredr(series_id = "RETAILIRSA",observation_start = as.Date("2019-01-01")) #downloading PCE nondurable goods

GSCPI_Graph <- ggplot() + #plotting GSCI
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=GSCPI, aes(x=date,y= GSPI,color= "New York Fed Global Supply Chain Pressures Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-2,5), breaks = c(-2,-1,0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Are Supply Chains Healing?") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data",subtitle = "FRBNY's Supply Chain Index Shows Pressures Easing This Year") +
  theme_apricitas + theme(legend.position = c(.45,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1998-03-01")-(.1861*(today()-as.Date("1998-03-01"))), xmax = as.Date("1998-03-01")-(0.049*(today()-as.Date("1998-03-01"))), ymin = -2-(.3*7), ymax = -2) +
  coord_cartesian(clip = "off")

PCE_Goods_Graph <- ggplot() + #plotting nondurable and durable share of PCE
  geom_line(data=PCEDGmerge, aes(x=date,y= (value.y/value.x) ,color= "Durable Goods (lhs)"), size = 1.25) +
  geom_line(data=PCENDmerge, aes(x=date,y= (value.y/value.x)-.1 ,color= "Nondurable Goods (rhs)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.095,.145), breaks = c(.09,.10,.11,.12,.13,.14), expand = c(0,0), sec.axis = sec_axis(~.+.1, name="Share of Total PCE", labels = scales::percent_format(accuracy = 1))) +
  ylab("Share of Total PCE") +
  ggtitle("Good to Go?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Goods Shot Up after the Pandemic Hit, and has Remained High") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Share of Total Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(.1861*(today()-as.Date("2019-01-01")))), ymin = .095-(.3*.050), ymax = 0.095) +
  coord_cartesian(clip = "off")

Inv_Sales_Graph <- ggplot() + #plotting Inventory Sales Ratios
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=MANU_INV_SALES, aes(x=date,y= value,color= "Manufacturers"), size = 1.25) +
  geom_line(data=WHOLESALE_INV_SALES, aes(x=date,y= value,color= "Merchant Wholesalers"), size = 1.25) +
  geom_line(data=RETAIL_INV_SALES, aes(x=date,y= value,color= "Retailers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.25),limits = c(1,2), breaks = c(1,1.25,1.5,1.75,2), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Taking Inventory") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Retailers' and Wholesalers' Inventories/Sales Ratios are Still Below Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.75,.75)) +
  scale_color_manual(name= "Inventory to Sales Ratio",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(.1861*(today()-as.Date("2019-01-01")))), ymin = 1-(.3*1), ymax = 1) +
  coord_cartesian(clip = "off")

OTI_Graph <- ggplot() + #plotting OTI
  geom_line(data=OTI, aes(x=FEWB_date,y= FEWB,color= "Far East Westbound (China to Europe)"), size = 1.25) +
  geom_line(data=OTI, aes(x=TPEB_date,y= TPEB,color= "Transpacific Eastbound (China to US)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,130), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Days From Cargo Ready to Destination Departure") +
  ggtitle("Crossing an Ocean") +
  labs(caption = "Graph created by @JosephPolitano using Flexport data",subtitle = "The Time it Takes for Cargo To Arrive from China is Decreasing") +
  theme_apricitas + theme(legend.position = c(.30,.82)) +
  scale_color_manual(name= "Flexport Ocean Timeliness Indicator",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-04-01")-(.1861*(today()-as.Date("2019-04-01"))), xmax = as.Date("2019-04-01")-(0.049*(.1861*(today()-as.Date("2019-04-01")))), ymin = 0-(.3*130), ymax = 0) +
  coord_cartesian(clip = "off")

BIE_Graph <- ggplot() + #plotting BIE
  geom_line(data=BIE, aes(x=date,y= Diffusion_Index,color= "Business' Forecasted Effects of Nonlabor Unit Costs on Prices in the Next Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,80), breaks = c(0,20,40,60,80), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("What Did You Expect?") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data",subtitle = "Business Owners Still Expect Nonlabor Costs to Contribute More to Rising Prices Over the Next Year") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-11-18")-(.1861*(today()-as.Date("2011-11-18"))), xmax = as.Date("2011-11-18")-(0.049*(.1861*(today()-as.Date("2011-11-18")))), ymin = 0-(.3*80), ymax = 0) +
  coord_cartesian(clip = "off")

QSPC_Supply_Graph <- ggplot() + #plotting BIE
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Insufficient Supply of Materials"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Logistics/transportation constraints"), aes(x=date,y= Value/100,color= "Logistics/Transportation Constraints"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Still Stressed Out") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Manufacturers Still Cite Materials Shortage and Logistics Constraints for Underutilization") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= "Reasons for Plant Capacity Under-utilization",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-04-01")))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

QSPC_Supply_Selected_Graph <- ggplot() + #plotting BIE
  geom_line(data=subset(QSPC, Sector == "325" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Chemicals Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "334" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Computers/Electronics Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "335" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Electrical Equipment/Appliances Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "336" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Transportation Equipment Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.70), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Still Stressed Out") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Key Industries Still Cite Materials Shortage and Logistics Constraints for Underutilization") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= "% Citing Materials Shortages for Plant Capacity Under-utilization",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(.1861*(today()-as.Date("2014-04-01")))), ymin = 0-(.3*.70), ymax = 0) +
  coord_cartesian(clip = "off")

QSPC_Demand_Graph <- ggplot() + #plotting BIE
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient orders"), aes(x=date,y= Value/100,color= "Insufficient Orders"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Insufficient Supply of Materials"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.90), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Supply and Demand Chains") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Firms Aren't Complaining About a Shortage of Orders As Much as Before the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.5)) +
  scale_color_manual(name= "Reasons for Plant Capacity Under-utilization",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-04-01")))), ymin = 0-(.3*.90), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GSCPI_Graph, "GSCPI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PCE_Goods_Graph, "PCE Goods Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Inv_Sales_Graph, "Inventory Sales Ratios.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = OTI_Graph, "OTI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BIE_Graph, "BIE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ggsave(dpi = "retina",plot = QSPC_Supply_Graph, "QSPC Supply Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = QSPC_Supply_Selected_Graph, "QSPC Selected Supply Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ggsave(dpi = "retina",plot = QSPC_Demand_Graph, "QSPC Demand Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
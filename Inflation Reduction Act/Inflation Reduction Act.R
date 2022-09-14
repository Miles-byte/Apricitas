pacman::p_load(eia,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

SPR_LEVEL <- eia_series("PET.WCSSTUS1.W", start = "2019")
SPR_LEVEL <- as.data.frame(SPR_LEVEL$data)

CRUDE_CURVE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Inflation%20Reduction%20Act/crude-oil-wti-prices-intraday-08-19-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

#collecting electricity generation stats
POWER_NAT_GAS <- eia_series("ELEC.GEN.NG-US-99.M", start = "2001")
POWER_NAT_GAS <- as.data.frame(POWER_NAT_GAS$data) %>%
  mutate(power = "Natural Gas")

POWER_COAL <- eia_series("ELEC.GEN.COW-US-99.M", start = "2001")
POWER_COAL <- as.data.frame(POWER_COAL$data) %>%
  mutate(power = "Coal")

POWER_NUCLEAR <- eia_series("ELEC.GEN.NUC-US-99.M", start = "2001")
POWER_NUCLEAR <- as.data.frame(POWER_NUCLEAR$data) %>%
  mutate(power = "Nuclear")

POWER_WIND <- eia_series("ELEC.GEN.WND-US-99.M", start = "2001")
POWER_WIND <- as.data.frame(POWER_WIND$data) %>%
  mutate(power = "Wind")

POWER_HYDRO <- eia_series("ELEC.GEN.HYC-US-99.M", start = "2001")
POWER_HYDRO <- as.data.frame(POWER_HYDRO$data) %>%
  mutate(power = "Hydro")

POWER_SOLAR <- eia_series("ELEC.GEN.TSN-US-99.M", start = "2001")
POWER_SOLAR <- as.data.frame(POWER_SOLAR$data) %>%
  mutate(power = "Solar")

POWER_RBIND <- rbind(POWER_NAT_GAS,POWER_COAL,POWER_NUCLEAR,POWER_WIND,POWER_HYDRO,POWER_SOLAR)

CPI_ELECTRICITY <- fredr(series_id = "CUSR0000SEHF01",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)
CPI_UTILITY_GAS <- fredr(series_id = "CUSR0000SEHF02",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)
CPI_GASOLINE <- fredr(series_id = "CUSR0000SETB01",observation_start = as.Date("2015-01-01"),realtime_start = NULL, realtime_end = NULL)

TOTAL_POWER_Graph <- ggplot(data = POWER_RBIND, aes(x = date, y = value/1000, fill = power)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA, width = 31) +
  xlab("Date") +
  ylab("Monthly Power Generation, GWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "GWh", accuracy = 1), breaks = c(0,200,400,600), limits = c(0,650), expand = c(0,0)) +
  ggtitle("Power Play") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The US is Slowly Transitioning to Better Power Sources-But Total Output is Stagnant") +
  theme_apricitas + theme(legend.position = c(.15,.79)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = 0-(.3*650), ymax = 0) +
  coord_cartesian(clip = "off")

CPI_ENERGY_Graph <- ggplot() + #plotting CPI for Different Energy Goods
  geom_line(data=CPI_ELECTRICITY, aes(x=date,y= (value/2.14475) ,color= "Electricity"), size = 1.25) +
  geom_line(data=CPI_UTILITY_GAS, aes(x=date,y= (value/1.70689) ,color= "Utility (Piped) Gas Service"), size = 1.25) +
  geom_line(data=CPI_GASOLINE, aes(x=date,y= (value/2.36591) ,color= "Gasoline"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,180), breaks = c(0,50,100,150), expand = c(0,0)) +
  ylab("CPI: January 2020 = 100") +
  ggtitle("The Energy Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for a Variety of Energy Commodities are Spiking") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "Consumer Price Index:",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*160), ymax = 0) +
  coord_cartesian(clip = "off")

TOTAL_POWER_Graph <- ggplot(data = POWER_RBIND, aes(x = date, y = value/1000, fill = power)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA, width = 31) +
  xlab("Date") +
  ylab("Monthly Power Generation, GWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "GWh", accuracy = 1), breaks = c(0,200,400,600), limits = c(0,650), expand = c(0,0)) +
  ggtitle("Power Play") +
  labs(caption = "Graph created by @JosephPolitano using EIA data", subtitle = "The US is Slowly Transitioning to Betterr Power Sources-But Total Output is Stagnant") +
  theme_apricitas + theme(legend.position = c(.15,.79)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = 0-(.3*650), ymax = 0) +
  coord_cartesian(clip = "off")

SPR_LEVEL_Graph <- ggplot() + #plotting US SPR Crude Oil Stocks
  geom_line(data=SPR_LEVEL, aes(x=date,y= value/1000, color= "Stocks of Crude Oil in the Strategic Petroleum Reserve"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(0,675),breaks = c(0,150,300,450,600), expand = c(0,0)) +
  ylab("Mbbl") +
  ggtitle("Breaking the Emergency Glass") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "The Drawdown in the Strategic Petroleum Reserve is Historicly Large") +
  theme_apricitas + theme(legend.position = c(.45,.78)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*675), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CRUDE_CURVE_Graph <- ggplot() + #plotting Crude Curve
  geom_line(data=CRUDE_CURVE, aes(x=Contract,y= Last,color= "Crude Oil (WTI) Futures Curve"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),limits = c(0,100), breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Back to Backwardation") +
  labs(caption = "Graph created by @JosephPolitano using CME data",subtitle = "Crude Oil Markets Remain Highly Backwardated") +
  theme_apricitas + theme(legend.position = c(.45,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-08-01")-(.1861*3600), xmax = as.Date("2022-08-01")-(0.049*3600), ymin = 0-(.3*100), ymax = 0) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = SPR_LEVEL_Graph, "SPR Level.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CRUDE_CURVE_Graph, "Crude Curve Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TOTAL_POWER_Graph, "Total Power Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_ENERGY_Graph, "CPI Energy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
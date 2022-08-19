pacman::p_load(graphics,seasonal,eia,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CRUDE_IMPORTS <- eia_series("PET.MCRNTUS2.M", start = 1965)
CRUDE_IMPORTS <- as.data.frame(CRUDE_IMPORTS$data)

#adding seasonal adjustments
CRUDE_IMPORTS  <- select(CRUDE_IMPORTS , -year,-month)

CRUDE_IMPORTS_TS <- ts(CRUDE_IMPORTS $value, frequency = 12, start = c(1965))

CRUDE_IMPORTS_TS <- seas(CRUDE_IMPORTS_TS)

CRUDE_IMPORTS <- final(CRUDE_IMPORTS_TS)
CRUDE_IMPORTS <- data.frame(date=rev(as.Date(CRUDE_IMPORTS)), value = melt(CRUDE_IMPORTS)$value)


PRODUCT_IMPORTS <- eia_series("PET.MTPNTUS2.M")
PRODUCT_IMPORTS <- as.data.frame(PRODUCT_IMPORTS$data)

#adding seasonal adjustments
PRODUCT_IMPORTS  <- select(PRODUCT_IMPORTS, -year,-month)

PRODUCT_IMPORTS_TS <- ts(PRODUCT_IMPORTS$value, frequency = 12, start = c(1993))

PRODUCT_IMPORTS_TS <- seas(PRODUCT_IMPORTS_TS)

PRODUCT_IMPORTS <- final(PRODUCT_IMPORTS_TS)
PRODUCT_IMPORTS <- data.frame(date=rev(as.Date(PRODUCT_IMPORTS)), value = melt(PRODUCT_IMPORTS)$value)


TOTAL_SUPPLIED <- eia_series("PET.MTTUPUS2.M", start = 1965)
TOTAL_SUPPLIED <- as.data.frame(TOTAL_SUPPLIED$data)
#adding seasonal adjustments
TOTAL_SUPPLIED <- select(TOTAL_SUPPLIED, -year,-month)

TOTAL_SUPPLIED_TS <- ts(TOTAL_SUPPLIED$value, frequency = 12, start = c(1965))

TOTAL_SUPPLIED_TS <- seas(TOTAL_SUPPLIED_TS)

TOTAL_SUPPLIED <- final(TOTAL_SUPPLIED_TS)
TOTAL_SUPPLIED <- data.frame(date=rev(as.Date(TOTAL_SUPPLIED)), value = melt(TOTAL_SUPPLIED)$value)

GAS_SUPPLIED <- eia_series("PET.MGFUPUS2.M", start = 1965)
GAS_SUPPLIED <- as.data.frame(GAS_SUPPLIED$data)
#adding seasonal adjustments
GAS_SUPPLIED <- select(GAS_SUPPLIED, -year,-month)

GAS_SUPPLIED_TS <- ts(GAS_SUPPLIED$value, frequency = 12, start = c(1965))

GAS_SUPPLIED_TS <- seas(GAS_SUPPLIED_TS)

GAS_SUPPLIED <- final(GAS_SUPPLIED_TS)
GAS_SUPPLIED <- data.frame(date=rev(as.Date(GAS_SUPPLIED)), value = melt(GAS_SUPPLIED)$value)

WORKINGAGEPOP <- fredr(series_id = "LNU00000060",realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "avg")

MANEMP <- fredr(series_id = "MANEMP",observation_start = as.Date("1965-01-01"), realtime_end = NULL)
PAYEMS <- fredr(series_id = "PAYEMS",observation_start = as.Date("1965-01-01"), realtime_end = NULL)

MAN_SHARE_EMP <- merge(MANEMP,PAYEMS, by = "date")

NGDP_PER_CAPITA <- fredr(series_id = "A939RC0Q052SBEA",observation_start = as.Date("1965-01-01"), realtime_end = NULL, units = "pc1")
RGDP_PER_CAPITA <- fredr(series_id = "A939RX0Q048SBEA",observation_start = as.Date("1965-01-01"), realtime_end = NULL, units = "pc1")

UNRATE <- fredr(series_id = "UNRATE",observation_start = as.Date("1965-01-01"), realtime_end = NULL)
ELEV <- fredr(series_id = "CE16OV",observation_start = as.Date("1965-01-01"), realtime_end = NULL, units = "pc1")

UMICH <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stagflation%2C%20Again/UMICH.csv") %>%
  mutate(date = as.Date(ï..datemy, "%m/%d/%Y"))

ENERGY_IMPORTS_GRAPH <- ggplot() + #plotting US Crude Production
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CRUDE_IMPORTS, aes(x=date,y= -value/1000, color= "US Crude Oil Net Exports"), size = 1.25) +
  geom_line(data=PRODUCT_IMPORTS, aes(x=date,y= -value/1000, color= "US Petroleum Products Net Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(-12.5,7.5),breaks = c(-10,-5,0,5), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Mbbl Per Day") +
  ggtitle("Energy Independence?") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Though Still Affected by Global Markets, America Produces More of its Energy Consumption") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1965-01-01")-(.1861*(today()-as.Date("1965-01-01"))), xmax = as.Date("1965-01-01")-(0.049*(today()-as.Date("1965-01-01"))), ymin = -12.5-(.3*20), ymax = -12.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

TOTAL_GAS_SUPPLY_GRAPH <- ggplot() + #plotting US Crude Production
  geom_line(data=TOTAL_SUPPLIED, aes(x=date,y= value/1000, color= "U.S. Product Supplied of Crude Oil and Petroleum Products"), size = 1.25) +
  geom_line(data=GAS_SUPPLIED, aes(x=date,y= value/1000, color= "U.S. Product Supplied of Finished Motor Gasoline"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(0,25),breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Mbbl Per Day") +
  ggtitle("Energy Crunch") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "The 1973 and especially 1979-1980 Oil Shocks Were Larger Than Today's") +
  theme_apricitas + theme(legend.position = c(.55,.95)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1965-01-01")-(.1861*(today()-as.Date("1965-01-01"))), xmax = as.Date("1965-01-01")-(0.049*(today()-as.Date("1965-01-01"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Workingagepop_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=WORKINGAGEPOP, aes(x=date,y=value/1000,color= "US Working Age (25-54) Population"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"), limits = c(0,150), breaks = c(0,50,100,150), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("1950-01-01","1960-01-01","1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Millions") +
  ggtitle("Baby Boom and Bust") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Working Age Population Growth Was High in the 70s-and is Extremely Low Now") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1948-01-01")-(.1861*26664), xmax = as.Date("1948-01-01")-(0.049*26664), ymin = 00-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off")

MAN_SHARE_EMP_GRAPH <- ggplot() + #plotting corporate bond issuance
  geom_line(data=MAN_SHARE_EMP, aes(x=date,y=value.x/value.y,color= "Manufacturing Share of US Employment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.3), breaks = c(0,0.05,.1,.15,.2,.25,.30), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Percent") +
  ggtitle("Post-Industrial Economics") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Manufacuring Share of US Employment is 8%-Compared to 25% in the 70s") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1965-01-01")-(.1861*(today()-as.Date("1965-01-01"))), xmax = as.Date("1965-01-01")-(0.049*(today()-as.Date("1965-01-01"))), ymin = 0-(.3*.30), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

GDP_PER_CAPITA_GRAPH <- ggplot() + #plotting corporate bond issuance
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=NGDP_PER_CAPITA, aes(x=date,y=value/100,color= "NGDP Per Capita"), size = 1.25) +
  geom_line(data=RGDP_PER_CAPITA, aes(x=date,y=value/100,color= "RGDP Per Capita"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.10,.175), breaks = c(-.10,-0.05,0,.05,.1,.15), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Percent") +
  ggtitle("Off-Target") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Per-Capita NGDP (and To a Lesser Extent RGDP) Growth Was Persistently High in the 1970s") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1965-01-01")-(.1861*(today()-as.Date("1965-01-01"))), xmax = as.Date("1965-01-01")-(0.049*(today()-as.Date("1965-01-01"))), ymin = -.10-(.3*.275), ymax = -.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

UNRATE_ELEV_Graph <- ggplot() + #plotting corporate bond issuance
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("text", y = -0.025, x = as.Date("2015-06-01"), label = "Note: Initial 2020", color = "white") +
  annotate("text", y = -0.035, x = as.Date("2015-06-01"), label = "Data Cut Off", color = "white") +
  geom_line(data=UNRATE, aes(x=date,y=value/100,color= "Unemployment Rate"), size = 1.25) +
  geom_line(data=ELEV, aes(x=date,y=value/100,color= "Employment Level, Annual Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.05,.15), breaks = c(-0.15,-.10,-0.05,0,.05,.1,.15), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Percent") +
  ggtitle("(Un)Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Unemployment Was High in the 1970s-But So Was Employment Growth") +
  theme_apricitas + theme(legend.position = c(.55,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1965-01-01")-(.1861*(today()-as.Date("1965-01-01"))), xmax = as.Date("1965-01-01")-(0.049*(today()-as.Date("1965-01-01"))), ymin = -.05-(.3*.20), ymax = -.05) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

UMICH_Graph <- ggplot() + #plotting inflation Expectations
  geom_line(data=UMICH[!(is.na(UMICH$DUR_BIAP)), ], aes(x=date,y=DUR_BIAP/100*.25,color= "Durable Good Buying Rationales: Prices Are Going Up Or Won't Come Down (rhs)"), size = 1.25) + #note- subsetting data that is not NA to make the graph line work
  geom_line(data=UMICH[!(is.na(UMICH$PX5_MD)), ], aes(x=date,y=PX5_MD/100,color= "5-10 Year Inflation Expectations (lhs)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.15), breaks = c(0,.05,.1,.15), expand = c(0,0), sec.axis = sec_axis(~.*1/.25, name="Percent",labels = scales::percent_format(accuracy = 1))) +
  scale_x_date(limits = c(as.Date("1965-01-01"),today()),breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ylab("Percent") +
  ggtitle("The Psychology of Inflation") +
  labs(caption = "Graph created by @JosephPolitano using University of Michigan data",subtitle = "Inflation Expectations Were High in the 1970s-and People Acted on Them") +
  theme_apricitas + theme(legend.position = c(.5,.92), legend.text = element_text(size = 12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1965-01-01")-(.1861*(today()-as.Date("1965-01-01"))), xmax = as.Date("1965-01-01")-(0.049*(today()-as.Date("1965-01-01"))), ymin = .0-(.3*.15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = ENERGY_IMPORTS_GRAPH, "Energy Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = TOTAL_GAS_SUPPLY_GRAPH, "Total Gas Supply.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Workingagepop_Graph, "Working Age Pop.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = MAN_SHARE_EMP_GRAPH, "Manufacturing Share of Employment.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = GDP_PER_CAPITA_GRAPH, "GDP Per Capita.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = UNRATE_ELEV_Graph, "UNRATE ELEV.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = UMICH_Graph, "UMICH.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
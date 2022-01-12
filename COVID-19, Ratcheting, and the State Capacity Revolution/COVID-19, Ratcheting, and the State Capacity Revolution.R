pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

BIZ_APP <- fredr(series_id = "BABATOTALSAUS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
BIZ_APP_HP <- fredr(series_id = "BAHBATOTALSAUS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
TRANSFERS <- fredr(series_id = "NA000300Q",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
QUITS <- fredr(series_id = "JTSQUR",realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
SWAPS <- fredr(series_id = "SWPT",observation_start = as.Date("2007-06-01"), realtime_start = NULL, realtime_end = NULL)
RAND <- fredr(series_id = "Y694RX1Q020SBEA",observation_start = as.Date("2000-01-01"), realtime_start = NULL, realtime_end = NULL)

CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("1970-01-01"), realtime_start = NULL, realtime_end = NULL)
WAGE <- fredr(series_id = "CES0500000030",observation_start = as.Date("1970-01-01"), realtime_start = NULL, realtime_end = NULL)
PCE <- fredr(series_id = "PCEPI",observation_start = as.Date("1970-01-01"), realtime_start = NULL, realtime_end = NULL)
COMP <- fredr(series_id = "COMPNFB",observation_start = as.Date("1970-01-01"), realtime_start = NULL, realtime_end = NULL)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

BIZ_APP_GRAPH <- ggplot() + #plotting business applications
  geom_line(data=BIZ_APP, aes(x=date,y= value/1000 ,color= "Business Applications"), size = 1.25) +
  geom_line(data=BIZ_APP_HP, aes(x=date,y= value/1000 ,color= "High-Propensity Business Applications"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"),limits = c(0,700), breaks = c(0,200,400,600), expand = c(0,0)) +
  ylab("Applications, Thousands") +
  ggtitle("Back in Business") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Business Applications, Especially for the Self-Employed, Surged During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.60,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1102), xmax = as.Date("2019-01-01")-(0.049*1102), ymin = 0-(.3*700), ymax = 0) +
  coord_cartesian(clip = "off")

TRANSFERS_GRAPH <- ggplot() + #plotting tranfers
  geom_line(data=TRANSFERS, aes(x=date,y= value/1000000 ,color= "Government Transfers: Social Benefits to Persons"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = (0.5)),limits = c(0,1.5), breaks = c(.5,1,1.5), expand = c(0,0)) +
  ylab("Trillions of Dollars, Quarterly") +
  ggtitle("Transfer Window") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Transfers Rose Due to Stimulus Checks, Enhanced Unemployment Benefits, and the CTC") +
  theme_apricitas + theme(legend.position = c(.60,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1467), xmax = as.Date("2018-01-01")-(0.049*1467), ymin = 0-(.3*1.5), ymax = 0) +
  coord_cartesian(clip = "off")

SWAPS_GRAPH <- ggplot() + #plotting Liquidity Swaps
  geom_line(data=SWAPS, aes(x=date,y= value/1000 ,color= "Federal Reserve Assets: Central Bank Liquidity Swaps"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = (1)),limits = c(0,750), breaks = c(0,250,500,750), expand = c(0,0)) +
  ylab("Billions of Dollars, Level") +
  ggtitle("Swapping Over") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Central Bank Liquidity Swaps Spiked During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.60,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-06-01")-(.1861*5334), xmax = as.Date("2007-06-01")-(0.049*5334), ymin = 0-(.3*750), ymax = 0) +
  coord_cartesian(clip = "off")

QUITS_GRAPH <- ggplot() + #plotting quits
  geom_line(data=QUITS, aes(x=date,y= value/100 ,color= "Quits Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.032), breaks = c(0,.01,.02,.03), expand = c(0,0)) +
  ylab("Rate, Seasonally Adjusted") +
  ggtitle("Labor Re-Allocation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Quits Are at Record Levels as Workers Find More Productive, Higher-Paying Jobs") +
  theme_apricitas + theme(legend.position = c(.60,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-09-01")-(.1861*7640), xmax = as.Date("2000-09-01")-(0.049*640), ymin = 0-(.3*.032), ymax = 0) +
  coord_cartesian(clip = "off")

RAND_GRAPH <- ggplot() + #plotting Real R and D Investment
  geom_line(data=RAND, aes(x=date,y= value ,color= "Real Gross Domestic Product: Research and Development"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = (1)),limits = c(300,700), breaks = c(0,100,200,300,400,500,600,700), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Research and Development") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spurred By Strong Demand, R&D Output Has Increased Dramatically") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7975), xmax = as.Date("2000-01-01")-(0.049*7975), ymin = 300-(.3*400), ymax = 300) +
  coord_cartesian(clip = "off")

CPImerge <- merge(CPI, WAGE, by = "date")
CPImerge2 <- merge(PCE, WAGE, by = "date")
PCEmerge <- merge(PCE, COMP, by = "date")

BERNIE_GRAPH <- ggplot() + #plotting Real R and D Investment
  geom_line(data=CPImerge, aes(x=date,y= (value.y/value.x)/3.245*100 ,color= "Average Weekly Earnings of Production and Nonsupervisory Employees/CPI"), size = 1.25) +
  geom_line(data=CPImerge2, aes(x=date,y= (value.y/value.x)/6*100 ,color= "Average Weekly Earnings of Production and Nonsupervisory Employees/PCE"), size = 1.25) +
  geom_line(data=PCEmerge, aes(x=date,y= (value.y/value.x)/0.580*100 ,color= "Hourly Compensation for All Employed Persons/PCEPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(75,225), breaks = c(100,150,200), expand = c(0,0)) +
  ylab("Index, 1970=100") +
  ggtitle("Work and Wages") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "No, Real Wages Haven't Been Decreasing for 50 Years") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1970-01-01")-(.1861*19000), xmax = as.Date("1970-01-01")-(0.049*19000), ymin = 75-(.3*150), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIZ_APP_GRAPH, "Biz App Graph.png", type = "cairo-png")
ggsave(dpi = "retina",plot = TRANSFERS_GRAPH, "Transfers Graph.png", type = "cairo-png")
ggsave(dpi = "retina",plot = SWAPS_GRAPH, "SWAPS Graph.png", type = "cairo-png")
ggsave(dpi = "retina",plot = QUITS_GRAPH, "Quits Graph.png", type = "cairo-png")
ggsave(dpi = "retina",plot = RAND_GRAPH, "Rand Graph.png", type = "cairo-png")
ggsave(dpi = "retina",plot = BERNIE_GRAPH, "Bernie Graph 2.png", type = "cairo-png")


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

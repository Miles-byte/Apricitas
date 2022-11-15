pacman::p_load(stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
p_load(gtrendsR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

T10Y2Y <- fredr(series_id = "T10Y2Y") %>%
  drop_na()

#Diffusion Indices
NY_SERVICES_DIFF <- fredr(series_id = "EMFDINA066MNFRBNY",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
NY_MANUFACT_DIFF <- fredr(series_id = "NEFDISA066MSFRBNY",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
TX_SERVICES_DIFF <- fredr(series_id = "TSSOSFEMPSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
TX_MANUFACT_DIFF <- fredr(series_id = "FNEMPSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
PHI_MANUFACT_DIFF <- fredr(series_id = "NEFDFSA066MSFRBPHI",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)


#taking google search trends data on "recession" in the US
Recession_Gtrends <- as.data.frame(gtrends(
  keyword = "Recession",
  geo = "US",
  time = "all",
  onlyInterest = TRUE
)) %>%
  mutate(interest_over_time.date = as.Date(interest_over_time.date))

RGDP <- fredr(series_id = "GDPC1",observation_start = as.Date("1998-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
RGDI <- fredr(series_id = "A261RX1Q020SBEA",observation_start = as.Date("1998-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")

#Continued and Initial Claims
CONTINUED_CLAIMS <- fredr(series_id = "CCSA",observation_start = as.Date("2022-01-01"),realtime_start = NULL, realtime_end = NULL)
INITIAL_CLAIMS <- fredr(series_id = "ICSA",observation_start = as.Date("2022-01-01"),realtime_start = NULL, realtime_end = NULL)

ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)

TOTAL_HOUSING_STARTS <- fredr(series_id = "HOUST",observation_start = as.Date("1995-01-01")) #downloading total housing starts

EPOP <- fredr(series_id = "LNS12300060",observation_start = as.Date("1998-01-01"))#downloading "Employment Population Ratio - 25-54 Yrs" data from Fred to calculate Gross Labor Income using a second method

TRUCK_SALES <- fredr(series_id = "HTRUCKSSAAR",observation_start = as.Date("1998-01-01"))#downloading heavey truck sales
 

RGDP_RGDI_Graph <- ggplot() + #yield curve
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = RGDP, aes(x = date, y = value/100, color = "Real Gross Domestic Product"), size = 1.25) +
  geom_line(data = RGDI, aes(x = date, y = value/100, color = "Real Gross Domestic Income"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.10,0.14), breaks = c(-.1,-0.05,0,0.05,0.1,0.15), expand = c(0,0)) +
  ylab("Percent Change from Year Ago, %") +
  ggtitle("Production Problems") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "GDP is an Imperfect Indicator, but Can Indicate a Pre-Recession Slowdown") +
  theme_apricitas + theme(legend.position = c(.40,.77)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED"), breaks = c("Real Gross Domestic Product","Real Gross Domestic Income")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1998-01-01")-(.1861*9000), xmax = as.Date("1998-01-01")-(0.049*9000), ymin = -0.10-(.3*0.24), ymax = -0.10) +
  coord_cartesian(clip = "off")

MANUFACT_DIFF_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = NY_MANUFACT_DIFF, aes(x = date, y = value, color = "NY Manufacturing"), size = 1.25) +
  geom_line(data = TX_MANUFACT_DIFF, aes(x = date, y = value, color = "TX Manufacturing"), size = 1.25) +
  geom_line(data = PHI_MANUFACT_DIFF, aes(x = date, y = value, color = "PHI Region Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-40,60), breaks = c(-40,-20,0,20,40,60), expand = c(0,0)) +
  ylab("Diffusion Index, Positive Number Indicates Growth") +
  ggtitle("Hiring Plans") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Manufacturing Employment Growth Forecasts are Decreasing as the Economy Weakens") +
  theme_apricitas + theme(legend.position = c(.22,.20)) +
  scale_color_manual(name= "Future Employment Diffusion Index",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED"), breaks = c("NY Manufacturing","TX Manufacturing","PHI Region Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1700), xmax = as.Date("2018-01-01")-(0.049*1700), ymin = -40-(.3*100), ymax = -40) +
  coord_cartesian(clip = "off")

SERVICES_DIFF_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = NY_SERVICES_DIFF, aes(x = date, y = value, color = "NY Region Services"), size = 1.25) +
  geom_line(data = TX_SERVICES_DIFF, aes(x = date, y = value, color = "TX Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-40,60), breaks = c(-40,-20,0,20,40,60), expand = c(0,0)) +
  ylab("Diffusion Index, Positive Number Indicates Growth") +
  ggtitle("Hiring Plans") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Services Employment Growth Forecasts are Holding Up Better, Likely Due to Sectoral Rotations") +
  theme_apricitas + theme(legend.position = c(.22,.20)) +
  scale_color_manual(name= "Future Employment Diffusion Index",values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1700), xmax = as.Date("2018-01-01")-(0.049*1700), ymin = -40-(.3*100), ymax = -40) +
  coord_cartesian(clip = "off")

Recession_Graph <- ggplot() + #plotting google search trends for recession
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=Recession_Gtrends, aes(x=interest_over_time.date,y= interest_over_time.hits ,color= "US Google Searches for `Recession`"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,100), breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Stressed Out") +
  labs(caption = "Graph created by @JosephPolitano using Google Trends data",subtitle = "People Are Extremely Worried About the Possibility of a Recession") +
  theme_apricitas + theme(legend.position = c(.35,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*6700), xmax = as.Date("2004-01-01")-(0.049*6700), ymin = 0-(.3*100), ymax = 0) +
  coord_cartesian(clip = "off")

CLAIMS_GRAPH <- ggplot() + #plotting initial and continued claims
  geom_line(data=INITIAL_CLAIMS, aes(x=date,y= value/2240.00 ,color= "Initial Unemployment Claims"), size = 1.25) +
  geom_line(data=CONTINUED_CLAIMS, aes(x=date,y= value/17780.00 ,color= "Continued Unemployment Claims"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,120), breaks = c(70,80,90,100,110,120), expand = c(0,0)) +
  ylab("Index, Jan 2022 = 100") +
  ggtitle("Live From the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using ETA data",subtitle = "Initial Claims Have Ticked Up, and Continued Claims are Starting to Follow") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("Initial Unemployment Claims","Continued Unemployment Claims")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 70-(.3*50), ymax = 70) +
  coord_cartesian(clip = "off")

T10Y2Y_Graph <- ggplot() + #yield curve
  annotate("rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1980-07-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1981-07-01"), xmax = as.Date("1982-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1990-07-01"), xmax = as.Date("1991-03-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = T10Y2Y, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.02,0.035), breaks = c(-0.02,-.01,0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("An Early Warning?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Yield Curve Inversions Have Preceeded All of America's Recent Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1976-01-01")-(.1861*16889), xmax = as.Date("1976-01-01")-(0.049*16889), ymin = -0.02-(.3*0.055), ymax = -0.02) +
  coord_cartesian(clip = "off")
  
ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  annotate("hline", y = 0.089, yintercept = 0.089, color = "#EE6055", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2016/2012 Cycle Peak", x = as.Date("2019-01-01"), y = 0.095, color ="#EE6055", size = 5) +
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Financial Conditions are Rapidly Tightening as the Federal Reserve Raises Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

STARTS_Graph <- ggplot() + #plotting new housing starts
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=TOTAL_HOUSING_STARTS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Is Housing The Business Cycle?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Housing Starts Led in 2008, But Are Not a Consistent Indicator") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Privately-Owned Housing Units Started","New Privately-Owned Housing Units Completed")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

TRUCK_SALES_Graph <- ggplot() + #plotting new housing starts
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=TRUCK_SALES, aes(x=date,y= value*1000, color= "Motor Vehicle Retail Sales: Heavy Weight Trucks"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,600), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Are Trucks The Business Cycle?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Heavy Truck Sales Can Be A Leading Indicator, But Might Be Muddled This Cycle") +
  theme_apricitas + theme(legend.position = c(.65,.25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1998-01-01")-(.1861*(today()-as.Date("1998-01-01"))), xmax = as.Date("1998-01-01")-(0.049*(today()-as.Date("1998-01-01"))), ymin = 0-(.3*600), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

EPOP_Graph <- ggplot() +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data = EPOP, aes(x=date + 90, y = value/100, color = "Prime Age (25-54) Employment-Population Ratio"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(.69,0.825), breaks = c(.70,.725,.75,.775,.80,.825), expand = c(0,0)) +
  ylab("%") +
  ggtitle("A Worrying Trend") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Prime Age Epop Declined Before the 2001 and 2008 Recessions and is Stalling Now") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#A7ACD9","#9A348E","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1998-01-01")-(.1861*(today()-as.Date("1998-01-01"))), xmax = as.Date("1998-01-01")-(0.049*(today()-as.Date("1998-01-01"))), ymin = .69-(.3*.135), ymax = .69) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CEI_LEI_data <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/111122/LEI_CEI_dataset.csv") %>%
  mutate(LEI_date = as.Date(LEI_date)) %>%
  mutate(CEI_date = as.Date(CEI_date)) %>%
  subset(., LEI_date> as.Date("2012-01-01"))

CEI_LEI_Graph <- ggplot() + #plotting CEI/LEI
  geom_line(data=CEI_LEI_data, aes(x=CEI_date,y= (CEI),color= "Coincident Economic Index"), size = 1.25) +
  geom_line(data=CEI_LEI_data, aes(x=LEI_date,y= (LEI),color= "Leading Economic Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(85,120), breaks = c(80,90,100,110,120,130,140,150.160), expand = c(0,0)) +
  ylab("Index, January 2016 = 100") +
  ggtitle("Loss Leaders") +
  labs(caption = "Graph created by @JosephPolitano using Conference Board data",subtitle = "Leading Economic Indicators are Still Weak") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= "Conference Board Economic Indices",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 85-(.3*40), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Recession_Graph, "Recession.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RGDP_RGDI_Graph, "RGDP RGDI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T10Y2Y_Graph, "T10Y2Y.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = MANUFACT_DIFF_Graph, "Manufact Diff.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SERVICES_DIFF_Graph, "Services Diff.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "ICECCORPORATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_Graph, "Starts Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EPOP_Graph, "EPOP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TRUCK_SALES_Graph, "Truck Sales.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CLAIMS_GRAPH, "Claims.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CEI_LEI_Graph, "CEI_LEI_Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

Yield_Curve <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/YIELD_CURVE.csv")

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

T10Y2Y2018 <- fredr(series_id = "T10Y2Y",observation_start = as.Date("2018-01-01")) #downloading 2s10s
T10Y2Y <- fredr(series_id = "T10Y2Y") #downloading 10s2s
T10Y2Y2018 <- drop_na(T10Y2Y2018)
T10Y2Y <- drop_na(T10Y2Y)
T10Y3M <- fredr(series_id = "T10Y3M",observation_start = as.Date("2018-01-01"))
T10Y3M <- drop_na(T10Y3M)

ICECORPORATE <- fredr(series_id = "BAMLC0A0CM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECORPORATE <- drop_na(ICECORPORATE)

MOVE <- tq_get("^MOVE", from = "2019-01-01")#importing stock market data

FIVEYEARBREAKEVEN <- fredr(series_id = "T5YIE",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN <- fredr(series_id = "T5YIFR",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

FIVEYEARBREAKEVEN <- drop_na(FIVEYEARBREAKEVEN)
FIVEYEARFWDBREAKEVEN <- drop_na(FIVEYEARFWDBREAKEVEN)

TWOYR <- fredr(series_id = "DGS2",observation_start = as.Date("2019-11-04")) #downloading 2yr yields
TWOYR <- drop_na(TWOYR)

TWOYR_Graph <- ggplot() + 
  geom_line(data = TWOYR, aes(x = date, y = value/100, color = "Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(0,.025), breaks = c(0,.005,.01,.01,.015,.02,0.025), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("The Fed's Pivot") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Yield on 2YR Treasury Notes Has Rapidly Returned to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-11-04")-(.1861*780), xmax = as.Date("2019-11-04")-(0.049*780), ymin = 0-(.3*.025), ymax = 0) +
  coord_cartesian(clip = "off")

Yield_Curve_Graph <- ggplot() + #plotting national financial conditions indexes
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Dec/100,color= "December 1st 2021"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Dec/100,color= "December 1st 2021"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Jan/100,color= "January 3rd 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Jan/100,color= "January 3rd 2022"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Feb/100,color= "February 1st 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Feb/100,color= "February 1st 2022"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Mar/100,color= "March 1st 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Mar/100,color= "March 1st 2022"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Marend/100,color= "March 29th 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Marend/100,color= "March 29th 2022"), size = 2) +
  xlab("Years") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.03), breaks = c(0,0.005,.01,.015,.02,.025,0.03), expand = c(0,0)) +
  scale_x_continuous(labels = c("1yr","2yr","3yr","5yr","7yr","10yr","20yr","30yr"), breaks = c(1,2,3,5,7,10,20,30)) +
  ylab("Yield, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "The Yield Curve is Flattening") +
  theme_apricitas + theme(legend.position = c(.60,.25), panel.grid.minor = element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*30), xmax = 0-(0.049*30), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

T10Y2Y2018_Graph <- ggplot() + #yield curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = T10Y2Y2018, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.0025,0.0175), breaks = c(0,0.005,0.01,0.015), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("A Flattening Yield Curve") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Yield Curve is Flattening as the Fed Tightens Policy") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = -0.0025-(.3*0.02), ymax = -0.0025) +
  coord_cartesian(clip = "off")

T10Y3M2018_Graph <- ggplot() + #yield curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = T10Y3M, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.006,0.022), breaks = c(-0.005,0,0.005,0.01,0.015,0.02), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("A Flattening Yield Curve?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The 10Y3M Yield Spread is Actually Increasing") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = -0.006-(.3*0.028), ymax = -0.006) +
  coord_cartesian(clip = "off")

T10Y2Y_Graph <- ggplot() + #yield curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = T10Y2Y, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.02,0.035), breaks = c(-0.02,-.01,0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("An Early Warning?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Yield Curve Inversions Have Preceeded All of America's Recent Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1976-01-01")-(.1861*16889), xmax = as.Date("1976-01-01")-(0.049*16889), ymin = -0.02-(.3*0.05), ymax = -0.02) +
  coord_cartesian(clip = "off") +
  annotate("rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1980-07-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1981-07-01"), xmax = as.Date("1982-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1990-07-01"), xmax = as.Date("1991-03-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4)
  
ICECORPORATE_Graph <- ggplot() + #plotting ice corporate index
  geom_line(data=ICECORPORATE, aes(x=date,y= value/100,color= "ICE BofA US Corporate Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.045), breaks = c(0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Financial Conditions were Tightening even before the Federal Reserve Raised Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1000), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.045), ymax = 0) +
  coord_cartesian(clip = "off")

MOVE_Graph <- ggplot() + #MOVE
  geom_line(data = MOVE, aes(x = date, y = close, color = "MOVE Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,200), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("MOVE-ing Up") +
  labs(caption = "Graph created by @JosephPolitano using NYSE data",subtitle = "Interest Rate Volatility is Increasing") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

T5YIE <- ggplot() + #plotting inflation breakevens
  geom_line(data=FIVEYEARBREAKEVEN, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARFWDBREAKEVEN, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "In a Recent Reversal, Short Term Inflation Expectations Are Higher Than Long-Term Expectations") +
  theme_apricitas + theme(legend.position = c(.60,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6950), xmax = as.Date("2003-01-01")-(0.049*6950), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TWOYR_Graph, "TWOYR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Yield_Curve_Graph, "Yield Curve.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T10Y2Y2018_Graph, "T10Y2Y2018.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T10Y2Y_Graph, "T10Y2Y Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T10Y3M2018_Graph, "T10Y3M Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ICECORPORATE_Graph, "ICE Corporate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = MOVE_Graph, "MOVE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5YIE, "T5YIE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
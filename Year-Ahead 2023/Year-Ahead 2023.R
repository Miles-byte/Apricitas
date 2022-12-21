pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Five Year Inflation Breakevens
FIVEYEARBREAKEVEN2019 <- fredr(series_id = "T5YIE", observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN2019 <- fredr(series_id = "T5YIFR",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

FIVEYEARBREAKEVEN2019 <- drop_na(FIVEYEARBREAKEVEN2019)
FIVEYEARFWDBREAKEVEN2019 <- drop_na(FIVEYEARFWDBREAKEVEN2019)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


T5YIE2019 <- ggplot() + #plotting inflation breakevens
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.0225, ymax = 0.0275, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=FIVEYEARFWDBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  annotate("text", label = "Breakevens Approximately Consistent With 2% Inflation Target", x = as.Date("2020-07-01"), y = 0.0287, color = "#EE6055", alpha = 0.6, size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Market-Based Inflation Expectations are Slightly Below a Level Consistent with the Fed's Target") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today() - as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today() - as.Date("2019-01-01"))), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = T5YIE2019, "T5YIE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STEO_WTI <- eia_series("STEO.WTIPUUS.M", start = "2019", end = "2026") 
STEO_WTI <- as.data.frame(STEO_WTI$data)

WTI_STEO_Graph <- ggplot() + #plotting Crude Futures
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2023-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-05-30"), y = 20, color = "#EE6055", size = 5, alpha = 0.6) +
  annotate("text", label = "SPR Refill Target", x = as.Date("2023-05-15"), y = 62, color = "white", size = 4.85) +
  annotate("segment", x = floor_date(as.Date(today() -33), "month"), xend = as.Date("2023-12-01"), y = 72, yend = 72, linetype = "dashed", color = "white", size = 1.25) +
  annotate("segment", x = floor_date(as.Date(today() -33), "month"), xend = as.Date("2023-12-01"), y = 67, yend = 67, linetype = "dashed", color = "white", size = 1.25) +
  geom_line(data=drop_na(STEO_WTI), aes(x=date,y= value, color= "Crude Oil (WTI)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,125), expand = c(0,0)) +
  ylab("Dollars Per Barrel") +
  ggtitle("Price Pullback") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Oil Prices Have Given Up Most of Their Post-Invasion Gains, But are Expected to Remain High") +
  theme_apricitas + theme(legend.position = c(.4,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI)","Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+365-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+365-as.Date("2019-01-01"))), ymin = 0-(.3*125), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WTI_STEO_Graph, "WTI STEO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#STEO Prod
STEO_Crude_Production <- eia_series("STEO.COPRPUS.M", start = "2019", end = "2026")
STEO_Crude_Production <- as.data.frame(STEO_Crude_Production$data)
STEO_SPR_Withdrawls <- eia_series("STEO.COSQ_DRAW.M", start = "2019", end = "2026")
STEO_SPR_Withdrawls <- as.data.frame(STEO_SPR_Withdrawls$data)

STEO_Prod_SPR_Graph <- ggplot() + #plotting US Crude Production
  annotate("rect", xmin = floor_date(as.Date(today() -33), "month"), xmax = as.Date("2023-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2022-04-30"), y = 10.25, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=STEO_SPR_Prod, aes(x=date,y= value.x, color= "US Crude Oil Production"), size = 1.25) +
  geom_line(data=STEO_SPR_Prod, aes(x=date,y= value.x+value.y, color= "US Crude Oil Production With Net SPR Withdrawls"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(9,14),breaks = c(9,10,11,12,13,14), expand = c(0,0)) +
  ylab("Mbbl Per Day") +
  ggtitle("Sand Trap") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Marginal US Crude Oil Production Will Be Handed From the SPR to the Private Sector") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01")+365)), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01")+365)), ymin = 9-(.3*5), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STEO_Prod_SPR_Graph, "STEO Prod Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#BIE
BIE_1YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ATL_BIE_1YR.csv") %>%
  mutate(date = as.Date(date))

BIE_Graph <- ggplot() + #plotting total quits
  geom_line(data=BIE_1YR, aes(x=date,y= bie,color= "Business Unit Cost Inflation Expectations: Next Year"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ggtitle("What Were You Expecting?") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Business Unit Cost Inflation Expectations Are High But Declining") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Business Unit Cost Inflation Expectations: Next Year","Business Unit Cost Inflation Expectations: Next 5-10 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-10-21")-(.1861*(today()-as.Date("2011-10-21"))), xmax = as.Date("2011-10-21")-(0.049*(today()-as.Date("2011-10-21"))), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_Graph, "BIE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#ICE Inflation Expectations
ICE_INF <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ICE_USD_Inflation_Expectations_Index_Family.csv") %>%
  mutate(date = as.Date(date))

ICE_INF_Graph <- ggplot() + #plotting ICE inflation expectations
  geom_line(data=subset(ICE_INF, date > as.Date("2021-12-31")), aes(x=date,y= X1Y/100,color= "ICE US Inflation Expectations: Next 12 Months"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05), limits = c(0,.055), expand = c(0,0)) +
  ggtitle("Expect the Unexpected") +
  labs(caption = "Graph created by @JosephPolitano using ICE data", subtitle = "Market Based Measures of Inflation Expectations Expect Price Rises to Fade in 2023") +
  theme_apricitas + theme(legend.position = c(.40,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-04")-(.1861*(today()-as.Date("2022-01-04"))), xmax = as.Date("2022-01-04")-(0.049*(today()-as.Date("2022-01-04"))), ymin = 0-(.3*.055), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ICE_INF_Graph, "ICE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
#Business employment and revenue growth

BUS_EMP_GROWTH <- fredr(series_id = "ATLSBUEGEP",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data
BUS_REV_GROWTH <- fredr(series_id = "ATLSBUSRGEP",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

BUS_GROWTH_Graph <- ggplot() + #plotting Atlanta Fed Business Employment and Revenue Growth Forecasts
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=BUS_REV_GROWTH, aes(x=date,y= value/100,color= "Revenue Growth"), size = 1.25)+ 
  geom_line(data=BUS_EMP_GROWTH, aes(x=date,y= value/100,color= "Employment Growth"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07), limits = c(-0.006,.07), expand = c(0,0)) +
  ggtitle("Half-Dampened Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Revenue Expectations are Holding Up, But Employment Expectations are Deteriorating") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +
  scale_color_manual(name= "Business Expectations, Next Year",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.006-(.3*.076), ymax = -0.006) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BUS_GROWTH_Graph, "Bus Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#FFR Futures
FFR_FUTURES_MEGA_MERGE_12_17 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Year-Ahead%202023/30-day-fed-funds-prices-intraday-12-17-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

EFFR <- fredr(series_id = "EFFR",observation_start = as.Date("2019-01-01"), frequency = "m", aggregation_method = "avg")

FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  geom_line(data=EFFR, aes(x=date,y= value/100,color= "Effective Federal Funds Rate"), size = 1.25) +
  geom_line(data=FFR_FUTURES_MEGA_MERGE_12_17, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path December 16th"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.07), breaks = c(0.01,0.02,0,.03,.04,.05,.06,0.07), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Higher For How Long?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and CME data",subtitle = "Federal Funds Futures Price In Higher Interest Rates") +
  theme_apricitas + theme(legend.position = c(.40,.88)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Effective Federal Funds Rate","Futures Implied Federal Funds Rate Path December 16th","Futures Implied Federal Funds Rate Path September 27th","Futures Implied Federal Funds Rate Path November 4th")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+1825-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+1825-as.Date("2019-01-01"))), ymin = 0-(.3*.07), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph, "FFR Futures.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Yield curves

T10Y2Y <- fredr(series_id = "T10Y2Y", observation_start = as.Date("2000-01-01")) %>%
  drop_na()

T10Y3MO <- fredr(series_id = "T10Y3M", observation_start = as.Date("2000-01-01")) %>%
  drop_na()

Curve_Graph <- ggplot() + #yield curve
  #annotate("rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1980-07-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  #annotate("rect", xmin = as.Date("1981-07-01"), xmax = as.Date("1982-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  #annotate("rect", xmin = as.Date("1990-07-01"), xmax = as.Date("1991-03-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = T10Y2Y, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity"), size = 1.25) +
  geom_line(data = T10Y3MO, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.01,0.046), breaks = c(-0.02,-.01,0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("An Early Warning?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Yield Curve Inversions Have Preceeded All of America's Recent Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.01-(.3*0.056), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Curve_Graph, "Curve Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#GDP growth
SPF_GDP_Growth <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Year-Ahead%202023/SPF%20GDP%20growth.csv") %>%
  mutate(date = as.Date(date))

SPF_GDP_Growth_graph <- ggplot() + #plotting GDP Growth Changes
  geom_line(data=SPF_GDP_Growth, aes(x=date,y= pct,color= "Survey of Professional Forecasters: Mean 1-Year Real GDP Growth Forecast"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.05,0.095), breaks = c(-0.04,-0.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Growing Pessimism") +
  labs(caption = "Graph created by @JosephPolitano using Philadelphia Fed data",subtitle = "Professional Forecasters Expect Relatively Low Growth Over the Next Year") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.05-(.3*.145), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SPF_GDP_Growth_graph, "SPF GDP Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
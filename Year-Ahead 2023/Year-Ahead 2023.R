pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


#Five Year Inflation Breakevens
FIVEYEARBREAKEVEN2019 <- fredr(series_id = "T5YIE", observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN2019 <- fredr(series_id = "T5YIFR",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

FIVEYEARBREAKEVEN2019 <- drop_na(FIVEYEARBREAKEVEN2019)
FIVEYEARFWDBREAKEVEN2019 <- drop_na(FIVEYEARFWDBREAKEVEN2019)


T5YIE2019 <- ggplot() + #plotting inflation breakevens
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.0225, ymax = 0.0275, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=FIVEYEARFWDBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  annotate("text", label = "Breakevens Approximately Consistent With 2% Inflation Target", x = as.Date("2020-01-01"), y = 0.0287, color = "#EE6055", alpha = 0.6, size = 4) +
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


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
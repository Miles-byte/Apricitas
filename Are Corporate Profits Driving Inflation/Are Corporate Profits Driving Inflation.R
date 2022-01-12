pacman::p_load(rvest,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

NFC_PRETAX_PROFIT <- fredr(series_id = "A463RD3Q052SBEA")

NFC_POSTTAX_PROFIT <- fredr(series_id = "A466RD3Q052SBEA") #downloading Profit per unit of Gross Value Added for Nonfinancial Corporations data
NFC_PRICE <- fredr(series_id = "A455RD3Q052SBEA") #downloading Price per unit of Gross Value Added for Nonfinancial Corporations data

POSTTAX_PROFIT_MARGIN <- merge(NFC_POSTTAX_PROFIT, NFC_PRICE, by = "date") #merging price and profit margin data per unit of gross value added for nonfinancial corporations
POSTTAX_PROFIT_MARGIN <- subset(POSTTAX_PROFIT_MARGIN, select = c("date","value.x","value.y")) #deleting unneeded variables from the data frame
colnames(POSTTAX_PROFIT_MARGIN) <- c("date","NFC_POSTTAX_PROFIT","NFC_PRICE") #renaming vairables for ease of use

PRETAX_PROFIT_MARGIN <- merge(NFC_PRETAX_PROFIT, NFC_PRICE, by = "date") #merging price and profit margin data per unit of gross value added for nonfinancial corporations
PRETAX_PROFIT_MARGIN <- subset(PRETAX_PROFIT_MARGIN, select = c("date","value.x","value.y")) #deleting unneeded variables from the data frame
colnames(PRETAX_PROFIT_MARGIN) <- c("date","NFC_PRETAX_PROFIT","NFC_PRICE") #renaming vairables for ease of use

Imports <- fredr(series_id = "A255RC1Q027SBEA", observation_start = as.Date("2018-01-01"))

#manually adding 4% personal income and outlays growth trend line for later chart on personal income and outlays
DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-11-01"), "months")), trend = 16622.8*1.003274^(0:22)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-11-01"), "months")), trend = 15328.8*1.003274^(0:22))


theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

POSTTAX_PROFIT_MARGIN_GRAPH <- ggplot() + #plotting corporate profit margins
  geom_line(data=POSTTAX_PROFIT_MARGIN, aes(x=date,y= NFC_POSTTAX_PROFIT/NFC_PRICE ,color= "Nonfinancial Corporate Business Profit Margins"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.0,.23), breaks = c(0,.05,.1,.15,.2), expand = c(0,0)) +
  ylab("Margin, %") +
  ggtitle("Prices and Profits") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "During an Inflation Surge, Corporate Profit Margins Appear to be at Record Highs") +
  theme_apricitas + theme(legend.position = c(.60,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*27333), xmax = as.Date("1947-01-01")-(0.049*27333), ymin = 0-(.3*.23), ymax = 0) +
  coord_cartesian(clip = "off")

PROFIT_MARGIN_GRAPH <- ggplot() + #plotting corporate profit margins
  geom_line(data=POSTTAX_PROFIT_MARGIN, aes(x=date,y= NFC_POSTTAX_PROFIT/NFC_PRICE ,color= "After-Tax Nonfinancial Corporate Business Profit Margins"), size = 1.25) +
  geom_line(data=PRETAX_PROFIT_MARGIN, aes(x=date,y= NFC_PRETAX_PROFIT/NFC_PRICE ,color= "Pre-Tax Nonfinancial Corporate Business Profit Margins"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.0,.23), breaks = c(0,.05,.1,.15,.2), expand = c(0,0)) +
  ylab("Margin, %") +
  ggtitle("Prices and Profits") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Pre-Tax Margins, However, are Barely Above Recent Highs and Within Historical Averages") +
  theme_apricitas + theme(legend.position = c(.60,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*27333), xmax = as.Date("1947-01-01")-(0.049*27333), ymin = 0-(.3*.23), ymax = 0) +
  coord_cartesian(clip = "off")

CARS_PROFIT <- data.frame(date = seq(as.Date("2019-01-01"), as.Date("2021-07-01"), "3 months"), profit = c(3.3,5.8,5.6,3.2,7.9,6.6,8.9,-1.1,-3,-10.7,-14.3)) 
OIL_PROFIT <- data.frame(date = seq(as.Date("2019-01-01"), as.Date("2021-07-01"), "3 months"), profit = c(11.1,14.0,24.9,16.9,1.2,-45.6,-55.6,-51.7,-21,2.7,11.2))

INDUSTRY_PROFIT_GRAPH <- ggplot() + #plotting corporate profits
  geom_line(data=CARS_PROFIT, aes(x=date,y= profit ,color= "Motor Vehicles, Bodies and Trailers, and Parts"), size = 1.25) +
  geom_line(data=OIL_PROFIT, aes(x=date,y= profit ,color= "Petroleum and Coal Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(-60,60), breaks = c(-60,-30,0,30,60), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("Prices and Profits") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Industries With Price Shocks Don't Necessarily See Profit Shocks") +
  theme_apricitas + theme(legend.position = c(.60,.85)) +
  scale_color_manual(name= "Corporate Profits By Industry",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*912), xmax = as.Date("2019-01-01")-(0.049*912), ymin = -60-(.3*120), ymax = -60) +
  coord_cartesian(clip = "off")

IMPORTS_GRAPH <- ggplot() + #plotting imports
  geom_line(data=Imports, aes(x=date,y= value/1000, color= "Imports of Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(1.9,3.1), breaks = c(2,2.5,3), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("Prices and Profits") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Imports Have Shot Up After the Initial Pandemic Shock") +
  theme_apricitas + theme(legend.position = c(.60,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1300), xmax = as.Date("2018-01-01")-(0.049*1300), ymin = 1.9-(.3*1.2), ymax = 1.9) +
  coord_cartesian(clip = "off")

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Personal Income and Outlays are on Trend, But Consumers Have Significant Excess Savings") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = POSTTAX_PROFIT_MARGIN_GRAPH, "Post-Tax Margins.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PROFIT_MARGIN_GRAPH, "Pre-Tax Margins.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = INDUSTRY_PROFIT_GRAPH, "Industry Profits.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = IMPORTS_GRAPH, "Imports.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
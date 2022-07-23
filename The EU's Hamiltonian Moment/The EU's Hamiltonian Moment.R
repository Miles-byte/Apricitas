pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#taking key ECB rates
ECB_REFI_RATE <- fredr(series_id = "ECBMRRFR", observation_start = as.Date("2008-11-01")) #downloading ECB REFI Rate
ECB_DEPOSIT_RATE <- fredr(series_id = "ECBDFR", observation_start = as.Date("2008-11-01")) #downloading ECB REFI Rate

#taking spreads
ITA10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/ITALY10YR.csv")
GER10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/GERMANY10YR.csv")
GRE10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/GREECE10YR.csv")

ITASPREADS <- merge(ITA10YR,GER10YR, by = "date")
GRESPREADS <- merge(GRE10YR,GER10YR, by = "date")

TSY_MBS_Graph <- ggplot() + #plotting spread
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=ECB_REFI_RATE, aes(x=date,y= value/100, color= "ECB Main Refinance Rate"), size = 1.25) +
  geom_line(data=ECB_DEPOSIT_RATE, aes(x=date,y= value/100, color= "ECB Deposit Facility Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.0075,.04), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Housing Bust?") +
  labs(caption = "Graph created by @JosephPolitano using Realtor.com data",subtitle = "Listing Prices Have Grown Dramatically During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1996-12-01")-(.1861*(today()-as.Date("1996-12-01"))), xmax = as.Date("1996-12-01")-(0.049*(today()-as.Date("1996-12-01"))), ymin = 0-(.3*.03), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

EU_Spreads_Graph_2 <- ggplot() + #plotting bond spreads in the EU
  geom_line(data=GRESPREADS, aes(x=date,y= (value.x-value.y)/100, color= "Greece"), size = 1.25) +
  geom_line(data=ITASPREADS, aes(x=date,y= (value.x-value.y)/100, color= "Italy"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Spreads with German Bonds, %") +
  ggtitle("When All You Have is A Hammer...") +
  labs(caption = "Graph created by @JosephPolitano using Investing.com data",subtitle = "Financial Tightening is Increasing Eurozone Bond Spreads Again") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "Spreads Between 10 Year German Government Bonds",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1000), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PERMITS_Graph, "Permits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
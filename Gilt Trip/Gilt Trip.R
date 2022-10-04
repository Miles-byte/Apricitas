pacman::p_load(stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

GBPUSD <- tq_get("GBPUSD=X", from = "2022-07-01")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


GBPUSD <- ggplot() + #plotting MOVE
  geom_line(data=GBPUSD, aes(x=date,y= close,color= "GBP/USD Exchange Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .01),limits = c(1,1.3), breaks = c(1,1.1,1.2,1.3), expand = c(0,0)) +
  ylab("Exchange Rate") +
  ggtitle("Gilt Trip") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "The Pound Tanked Early in the Week But Rallied Back After BoE's Intervention") +
  theme_apricitas + theme(legend.position = c(.25,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-07-01")-(.1861*(today()-as.Date("2022-07-01"))), xmax = as.Date("2022-07-01")-(0.049*(today()-as.Date("2022-07-01"))), ymin = 1-(.3*.3), ymax = 1) +
  coord_cartesian(clip = "off")

BoEReserves <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Gilt%20Trip/boereserves.csv")%>%
  mutate(Date = as.Date(Date))

BoEReserves_Graph <- ggplot() + #plotting MOVE
  geom_line(data=BoEReserves, aes(x=Date,y= reserves/1000,color= "BoE Liabilities: Commercial Bank Reserves"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "£"),limits = c(0,1000), breaks = c(0,250,500,750,1000), expand = c(0,0)) +
  ylab("Billions of Pounds") +
  ggtitle("Minimum Viable Balances") +
  labs(caption = "Graph created by @JosephPolitano using Bank of England data",subtitle = "Central Banks, Including the Bank of England, Are Tryin to Unwind Their Balance Sheets") +
  theme_apricitas + theme(legend.position = c(.35,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*1000), ymax = 1) +
  coord_cartesian(clip = "off")

UKREAL5 <-  read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Gilt%20Trip/impliedreal5rates.csv")%>%
  mutate(date = as.Date(date))

UKREAL5_GRAPH <- ggplot() + #plotting MOVE
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=UKREAL5, aes(x=date,y= implied_real_spot_yield/100,color= "Implied Real 5-Year Gilt Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.01), breaks = c(-0.04,-0.03,-0.02,-0.01,0,0.01), expand = c(0,0)) +
  ylab("Implied Real Rate") +
  ggtitle("Gilt Trip") +
  labs(caption = "Graph created by @JosephPolitano using Bank of England data",subtitle = "Real Yields are Rapidly Rising in the UK") +
  theme_apricitas + theme(legend.position = c(.25,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-07-01")-(.1861*(today()-as.Date("2022-07-01"))), xmax = as.Date("2022-07-01")-(0.049*(today()-as.Date("2022-07-01"))), ymin = 1-(.3*.3), ymax = 1) +
  coord_cartesian(clip = "off")

UKYIELDCURVE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Gilt%20Trip/yieldcurve.csv")%>%
  select(MATURITY,SEP_22,SEP_29)

UKYIELDCURVE_Graph <- ggplot() + #plotting MOVE
  geom_line(data=UKYIELDCURVE, aes(x=MATURITY/12,y= SEP_22/100,color= "September 22nd"), size = 1.25) +
  geom_line(data=UKYIELDCURVE, aes(x=MATURITY/12,y= SEP_29/100,color= "September 29th"), size = 1.25) +
  xlab("Years") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Fitted Yield") +
  ggtitle("Gilt Trip") +
  labs(caption = "Graph created by @JosephPolitano using Bank of England data",subtitle = "Gilt Yields Have Moved Up Significantly in the Last Week") +
  theme_apricitas + theme(legend.position = c(.25,.45)) +
  scale_color_manual(name= "Gilt Yield Curve",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = (5/12)-(.1861*5), xmax = (5/12)-(.1861*5), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GBPUSD, "GBPUSD.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BoEReserves_Graph, "BoEReserves.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = UKREAL5_GRAPH, "UKREAL5.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = UKYIELDCURVE, "UKYIELDCURVE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
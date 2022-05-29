pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

NFCI <- fredr(series_id = "NFCI",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
NFCICREDIT <- fredr(series_id = "NFCICREDIT",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

ICECORPORATE <- fredr(series_id = "BAMLC0A0CM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECORPORATE <- drop_na(ICECORPORATE)

ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)

Corp_Issuance <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/CORP_ISSUANCE.csv")
Corp_Issuance$Date <- as.Date(Corp_Issuance$Date)

Yield_Curve <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/YIELD_CURVE.csv")

WORKINGAGEPOP <- fredr(series_id = "LNU00000060",realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "avg")

Top1pct <- fredr(series_id = "WFRBST01108",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)

T5 <- fredr(series_id = "DGS5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 year nominal interest rates
T5IE <- fredr(series_id = "T5YIE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 year inflation expectations
T5RL <- fredr(series_id = "DFII5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates

T5Bind <- rbind(T5IE,T5RL) #binding real yields and inflation expectations
T5Bind <- drop_na(T5Bind)
T5Bind$series_id <- gsub("T5YIE","5-Year Breakeven Inflation Expectations",T5Bind$series_id)
T5Bind$series_id <- gsub("DFII5","5-Year Real Bond Yield",T5Bind$series_id)
T5 <- drop_na(T5)

IORB <- fredr(series_id = "IORB",realtime_start = NULL, realtime_end = NULL)
IOER <- fredr(series_id = "IOER",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL)

TENYR <- fredr(series_id = "DGS10",observation_start = as.Date("1980-01-01"),realtime_start = NULL, realtime_end = NULL)
TENYRREAL <- fredr(series_id = "DFII10",realtime_start = NULL, realtime_end = NULL)
TENYR <- drop_na(TENYR)
TENYRREAL <- drop_na(TENYRREAL)

SPY <- tq_get("VOO", from = "2019-01-01")

T5RATES_Graph <- ggplot(T5Bind, aes(fill=series_id, x=date, y=value/100)) + 
  geom_area(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = T5, aes(x=date, y = value/100, color = "5-Year Nominal Bond Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.02,0.04), breaks = c(-0.02,-.01,0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Effects of Monetary Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Interest Rates are Rising and Inflation Expectations are Falling as the Fed Tightens") +
  theme_apricitas + theme(legend.position = c(.42,.87), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("5-Year Real Bond Yield","5-Year Breakeven Inflation Expectations")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -.02-(.3*0.06), ymax = -0.02) +
  coord_cartesian(clip = "off")

NFCI_Graph <- ggplot() + #plotting national financial conditions indexes
  geom_line(data=NFCI, aes(x=date,y= value,color= "Chicago Fed National Financial Conditions Index"), size = 1.25) +
  geom_line(data=NFCICREDIT, aes(x=date,y= value,color= "Chicago Fed National Financial Conditions Index: Credit Subindex"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1,-0.5,0,0.5,1), expand = c(0,0)) +
  ylab("Index, 0 = Average, Higher Numbers are Tighter") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Financial Conditions were Tightening even before the Federal Reserve Raises Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -1-(.3*2), ymax = -1) +
  coord_cartesian(clip = "off")

SPY_Graph <- ggplot() + #plotting SPY
  geom_line(data=SPY, aes(x=date,y= (close/230)-1,color= "S&P 500"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.1,1), breaks = c(0,.25,.5,0.75,1), expand = c(0,0)) +
  ylab("Total Return") +
  ggtitle("Risk and Reward") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "The S&P 500 is Down in 2022-Possibly Indicating a Rising Equity Premium") +
  theme_apricitas + theme(legend.position = c(.40,.65)) +
  scale_color_manual(name= "Total Return Since Jan 2019",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -.1-(.3*1.1), ymax = -.1) +
  coord_cartesian(clip = "off")

Corp_Issuance_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=Corp_Issuance, aes(x=Date,y= Investment_Grade,color= "Investment Grade"), size = 1.25) +
  geom_line(data=Corp_Issuance, aes(x=Date,y= High_Yield,color= "High Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,300), breaks = c(100,200,300), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using SIFMA data",subtitle = "Corporate Bond Issuance is Decreasing-Especially for High Yield Bonds") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= "Corporate Bond Issuance",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*750), xmax = as.Date("2020-01-01")-(0.049*750), ymin = 0-(.3*300), ymax = 0) +
  coord_cartesian(clip = "off")

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
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.045), ymax = 0) +
  coord_cartesian(clip = "off")

ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Financial Conditions are Rapidly Tightening as the Federal Reserve Raises Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")


IOER_IORB_Graph <- ggplot() + #plotting durables v services inflation
  geom_line(data=IORB, aes(x=date,y= value/100,color= "IORB"), size = 1.25) +
  geom_line(data=IOER, aes(x=date,y= value/100,color= "IOER"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.025), breaks = c(0,0.005,.01,.015,.02,.025), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Federal Reserve Could not Sustain Rates Above 2% Pre-Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*4150), xmax = as.Date("2010-01-01")-(0.049*4150), ymin = 0-(.3*.025), ymax = 0) +
  coord_cartesian(clip = "off")

TenYR_Graph <- ggplot() + #plotting durables v services inflation
  geom_line(data=TENYR, aes(x=date,y= value/100,color= "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity"), size = 1.25) +
  geom_line(data=TENYRREAL, aes(x=date,y= value/100,color= "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Inflation-Indexed"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(-0.015,.2), breaks = c(0,0.05,.1,.15,.2), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Nominal (and Real) Treasury Yields have Been Declining for Decades") +
  theme_apricitas + theme(legend.position = c(.50,.94),legend.text = element_text(size = 13, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*15411), xmax = as.Date("1980-01-01")-(0.049*15411), ymin = -0.015-(.3*.215), ymax = -0.015) +
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
  xlab("Years") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.03), breaks = c(0,0.005,.01,.015,.02,.025,0.03), expand = c(0,0)) +
  scale_x_continuous(labels = c("1yr","2yr","3yr","5yr","7yr","10yr","20yr","30yr"), breaks = c(1,2,3,5,7,10,20,30)) +
  ylab("Yield, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "The Yield Curve is Flattening") +
  theme_apricitas + theme(legend.position = c(.20,.85), panel.grid.minor = element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*30), xmax = 0-(0.049*30), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

Workingagepop_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=WORKINGAGEPOP, aes(x=date,y=value/1000,color= "US Working Age (25-54) Population"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"), limits = c(50,150), breaks = c(50,100,150), expand = c(0,0)) +
  ylab("Millions") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "America's Working Age Population is Not Growing Anymore") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1948-01-01")-(.1861*26664), xmax = as.Date("1948-01-01")-(0.049*26664), ymin = 50-(.3*100), ymax = 50) +
  coord_cartesian(clip = "off")

Top1Pct_Graph <- ggplot() + #plotting top 1% share of total assets
  geom_line(data=Top1pct, aes(x=date,y= value/100,color= "Share of Total Assets Held by the Top 1%"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.2,.3), breaks = c(.2,.25,.3), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Wealth and Income Inequality is Rising, Which Keeps Real Rates Down") +
  theme_apricitas + theme(legend.position = c(.50,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11323), xmax = as.Date("1990-01-01")-(0.049*11323), ymin = .20-(.3*.1), ymax = .20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NFCI_Graph, "NFCI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = IOER_IORB_Graph, "IOER IORB.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TenYR_Graph, "Ten Year.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ICECORPORATE_Graph, "ICE Corporate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "ICE CCC Corporate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Corp_Issuance_Graph, "Corporate Issuance.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Yield_Curve_Graph, "Yield Curve.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Workingagepop_Graph, "Working Age Population.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Top1Pct_Graph, "Top 1 pct.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5RATES_Graph, "T5 Rates.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SPY_Graph, "SPY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
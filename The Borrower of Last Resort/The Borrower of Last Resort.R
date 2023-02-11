pacman::p_load(remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

Deficit <- fredr(series_id = "FYFSGDA188S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data
Interest <- fredr(series_id = "FYOIGDA188S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data


TENYRTIPS <- fredr(series_id = "DFII10",observation_start = as.Date("2003-01-02"),realtime_start = NULL, realtime_end = NULL) #prime age epop data

TENYRTIPS <- drop_na(TENYRTIPS)

fyoint <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/FYOINT.csv")

fyoint$FRBREMIT <- gsub(",","",fyoint$FRBREMIT)#removing commas from FRBREMIT

fyoint$FRBREMIT <- as.numeric(fyoint$FRBREMIT)#forcing Fed remittances and GDP numbers to be numeric instead of char
fyoint$GDP <- as.numeric(fyoint$GDP)
fyoint$DATE <- as.Date(fyoint$DATE, "%m/%d/%Y") #forcing date
fyoint$DATE <- as.IDate(fyoint$DATE)

EnglandDebt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/England_Debt.csv")

GermanDebt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/German_Debt.csv")

JPN_ITA_Int <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/Italy_Japan_Debt_Interest.csv")

JPN_ITA_Int$Date <- as.Date(JPN_ITA_Int$Date, "%m/%d/%Y") #forcing date
JPN_ITA_Int$Date <- as.IDate(JPN_ITA_Int$Date) #forcing date

GermanDebt$Date <- as.Date(GermanDebt$Date, "%m/%d/%Y") #forcing date
GermanDebt$Date <- as.IDate(GermanDebt$Date)

EnglandDebt$Date <- as.Date(EnglandDebt$Date, "%d/%m/%Y") #forcing date
EnglandDebt$Date <- as.IDate(EnglandDebt$Date)
EnglandDebt[,2:10] <- sapply(EnglandDebt[,2:10],as.numeric)

GermanDebt$Date <- as.Date(GermanDebt$Date, "%m/%d/%Y") #forcing date
GermanDebt$Date <- as.IDate(GermanDebt$Date)

GDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/GDP.csv")
Wages <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/Wage%20and%20Salary.csv")
Compensation <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/Compensation.csv")

Compensation$DATE <- as.Date(Compensation$DATE)#forcing date on characters
GDP$DATE <- as.Date(GDP$DATE)
Wages$DATE <- as.Date(Wages$DATE)

colnames(Wages) <- c("DATE","Wage_Pct") #changing colnames
colnames(Compensation) <- c("DATE","Compensation_Pct")

Comp_Mean <- Compensation[Compensation$DATE < "2007-11-01", ] #trimming to get mean compensation

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Deficit_Graph <- ggplot() + #plotting federal deficit data
  geom_line(data=Deficit, aes(x=date,y= value/100 ,color= "Federal Budget Deficit, % of GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.16,0.024), breaks = c(0,-0.05,-0.1,-0.15), expand = c(0,0)) +
  ylab("Federal Budget Deficit, % of GDP") +
  ggtitle("Deficit Finance") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Federal Budget Deficits Have Grown Since the Turn of the Millenium") +
  theme_apricitas + theme(legend.position = c(.60,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7300), xmax = as.Date("2000-01-01")-(0.049*7300), ymin = -.16-(.3*.184), ymax = -.16) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Deficit_Interest_Graph <- ggplot() + #plotting federal deficit data
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=Deficit, aes(x=date,y= value/100 ,color= "Federal Budget Deficit, % of GDP"), size = 1.25) +
  geom_line(data=Interest, aes(x=date,y= -value/100 ,color= "Federal Interest Expenses, % of GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.16,0.024), breaks = c(0,-0.05,-0.1,-0.15), expand = c(0,0)) +
  ylab("Federal Budget Deficit, % of GDP") +
  ggtitle("Deficit Finance") +
  labs(caption = "Graph created by @JosephPolitano using Treasury data",subtitle = "Federal Budget Deficits Have Grown Since the Turn of the Millenium") +
  theme_apricitas + theme(legend.position = c(.30,.25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7300), xmax = as.Date("2000-01-01")-(0.049*7300), ymin = -.16-(.3*.184), ymax = -.16) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")



InterestPct <- ggplot() + #plotting interest as a %of GDP
  geom_line(data=fyoint, aes(x=DATE,y=OINT/GDP,color= "Interest"), size = 1.25) +
  geom_line(data=fyoint, aes(x=DATE,y=(OINT-FRBREMIT)/GDP,color= "Interest Less Fed Remittances"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1947-01-01"),as.IDate("2020-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), expand = c(0,0),limits = c(0,0.035)) +
  ylab("Percent of GDP") +
  ggtitle("A Light Burden") +
  labs(caption = "Graph created by @JosephPolitano using OMB and BEA data",subtitle = "Net Interest Expenses Have Remained Low Despite High Debt Levels") +
  theme_apricitas + theme(legend.position = c(.30,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*26663), xmax = as.Date("1947-01-01")-(0.049*26663), ymin = 0-(.3*.035), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

JapanEUInterestGDP <- ggplot() + #plotting Interest/GDP for many countries
  geom_line(data=fyoint, aes(x=DATE,y=(OINT-FRBREMIT)/GDP,color= "USA"), size = 1.25) + 
  geom_line(data=EnglandDebt, aes(x=Date,y=IntAPFGDP,color= "UK"), size = 1.25) +
  geom_line(data=GermanDebt, aes(x=Date,y=((NominalInterest-NominalRemit)/(NGDP*1000)), color = "Germany"),size = 1.25) +
  geom_line(data=JPN_ITA_Int, aes(x=Date,y=JPNIntRemitGDP,color= "Japan"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1990-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.035),  breaks = c(0,.01,.02,.03,0.04,0.05,0.06), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("The Easing Burden") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed, IMF, Bundesbank, Banca D'Italia, BoJ, and BoE data",subtitle = "Debt Servicing Costs for High-Income Nations Have Been Dropping") +
  theme_apricitas + theme(legend.position = c(.17,.30)) +
  scale_color_manual(name= "Net Interest as a % of GDP",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#EE6055"), breaks = c("USA","UK","Japan","Germany")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11323), xmax = as.Date("1990-01-01")-(0.049*11323), ymin = 0-(.3*.035), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Nominal_Growth <- ggplot() + #plotting nominal growth
  geom_line(data=GDP, aes(x=DATE,y= GDP_PC1/100,color= "GDP"), size = 1.25) +
  geom_line(data=Compensation, aes(x=DATE, y = Compensation_Pct/100, color = "Compensation"), size = 1.25) +
  geom_line(data=Wages, aes(x=DATE,y= Wage_Pct/100, color = "Wages and Salaries"), size = 1.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.06,.1),  breaks = c(-0.05,0,.05,.1), expand = c(0,0)) +
  xlab("Date") +
  ylab("Annual Percent Growth, Nominal USD") +
  ggtitle("The Nominal Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Nominal Growth Tanked After the GFC, Bringing Real Growth Down With it") +
  theme_apricitas + theme(legend.position = c(.17,.30)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "hline", y = mean(Comp_Mean$Compensation_Pct)/100, yintercept = mean(Comp_Mean$Compensation_Pct)/100, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Pre-Crisis Trend", x = as.Date("1998-06-01"), y = mean(Comp_Mean$Compensation_Pct)/100-0.005, color ="#FFE98F",size = 3.5) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11323), xmax = as.Date("1990-01-01")-(0.049*11323), ymin = -0.06-(.3*.16), ymax = -0.06) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

TENYRTIPS_Graph <- ggplot() + #plotting TIPS
  geom_line(data=TENYRTIPS, aes(x=date,y= value/100 ,color= "10 Year TIPS Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.015,0.035), breaks = c(-0.01,0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("Federal Budget Deficit, % of GDP") +
  ggtitle("Lower For Longer") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Real Interest Rates Continue To Slide Downwards") +
  theme_apricitas + theme(legend.position = c(.60,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6918), xmax = as.Date("2003-01-01")-(0.049*6918), ymin = -.015-(.3*.05), ymax = -.015) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Deficit_Graph, "Deficit.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Deficit_Interest_Graph, "Deficit Interest Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = InterestPct, "InterestPct.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = JapanEUInterestGDP, "JPN EU Interest.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Nominal_Growth, "Nominal Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = TENYRTIPS_Graph, "Ten Year Tips.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

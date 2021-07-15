pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes)

US_Stocks <- tq_get("VTI", from = "2006-01-01")#importing stock market data
EU_Stocks <- tq_get("VGK", from = "2006-01-01")
Japan_Stocks <- tq_get("EWJ", from = "2006-01-01")

GDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/GDP.csv")
Wages <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/Wage%20and%20Salary.csv")
Compensation <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/Compensation.csv")
PCEPI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Tight%20Monetary%20Policy%2C%20Not%20Loose%20Monetary%20Policy%2C%20has%20Caused%20Inequality/PCEPI.csv")


Compensation$DATE <- as.Date(Compensation$DATE)
GDP$DATE <- as.Date(GDP$DATE)
Wages$DATE <- as.Date(Wages$DATE)
PCEPI$DATE <- as.Date(PCEPI$DATE)

colnames(Wages) <- c("DATE","Wage_Pct")
colnames(Compensation) <- c("DATE","Compensation_Pct")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

QE_Stock <- ggplot() + #plotting stock market returns 
  geom_line(data=EU_Stocks, aes(x=date,y=adjusted/adjusted[1]*100,color= "Europe"), size = 1.25) +
  geom_line(data=US_Stocks, aes(x=date,y=adjusted/adjusted[1]*100, color = "US"), size = 1.25) +
  geom_line(data=Japan_Stocks, aes(x=date,y=adjusted/adjusted[1]*100, color = "Japan"), size = 1.25) +
  xlab("Date") +
  ylab("Index - 2006-01-01 = 100, USD") +
  ggtitle("QE and Stock Market Performance") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data") +
  theme_apricitas + theme(legend.position = "right") +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "vline",x = c(as.Date("2008-11-01"),as.Date("2014-08-21"),as.Date("2019-09-01"),as.Date("2015-01-22"),as.Date("2012-12-01")),xintercept = c(as.Date("2008-11-01"),as.Date("2014-08-21"),as.Date("2019-09-01"),as.Date("2015-01-22"),as.Date("2012-12-01")), size = 1.25,linetype = "dashed",color = c("#EE6055","#EE6055","#EE6055","#FFE98F","#00A99D")) + #annotating important QE events
  annotate(geom = "text",label = c(as.character("Fed Starts QE"),as.character("Fed Tapers QE"),as.character("Fed Restarts QE"),as.character("ECB Starts QE"),as.character("BOJ Buys ETFs")), x=c(as.Date("2007-05-01"),as.Date("2013-01-21"),as.Date("2018-01-01"),as.Date("2016-07-22"),as.Date("2011-04-01")), y = c(500,500,500,400,300), color = c("#EE6055","#EE6055","#EE6055","#FFE98F","#00A99D"))

Nominal_Growth <- ggplot() + #plotting nominal growth
  geom_line(data=GDP, aes(x=DATE,y= GDP_PC1/100,color= "GDP"), size = 1.25) +
  geom_line(data=Compensation, aes(x=DATE, y = Compensation_Pct/100, color = "Compensation"), size = 1.25) +
  geom_line(data=Wages, aes(x=DATE,y= Wage_Pct/100, color = "Wages and Salaries"), size = 1.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab("Date") +
  ylab("Annual Percent Growth, Nominal USD") +
  ggtitle("Nominal Growth Lagged the Pre-Crisis Trend") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  theme_apricitas +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))
  
PCEPI <- ggplot() + #plotting PCEPI growth
  geom_line(data=PCEPI, aes(x=DATE,y= PCEPI_PC1/100,color= "PCE Price Index"), size = 1.25) +
  xlab("Date") +
  ylab("PCEPI Annual Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Inflation Remained Below Target") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  theme_apricitas +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "hline", y = 0.02, yintercept = 0.02, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "2% Target", x = as.Date("1990-08-01"), y = 0.0225, color ="#FFE98F")

ggsave(dpi = "retina",plot = QE_Stock, "QE and Stock Market Performance.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Nominal_Growth, "Nominal_Growth.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = PCEPI, "Personal Consumption Expenditures.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
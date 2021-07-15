pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes)

US_Stocks <- tq_get("VTI", from = "2006-01-01")#importing stock market data
EU_Stocks <- tq_get("VGK", from = "2006-01-01")
Japan_Stocks <- tq_get("EWJ", from = "2006-01-01")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

QE_Stock <- ggplot() + #plotting stock market returns 
  geom_line(data=EU_Stocks, aes(x=date,y=adjusted/adjusted[1]*100,color= "Europe"), size = 1.25) +
  geom_line(data=US_Stocks, aes(x=date,y=adjusted/adjusted[1]*100, color = "US"), size = 1.25) +
  geom_line(data=Japan_Stocks, aes(x=date,y=adjusted/adjusted[1]*100, color = "Japan"), size = 1.25) +
  xlab("Date") +
  ylab("Index - 2006-01-01 = 100, USD") +
  ggtitle("QE and Stock Market Performance") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data") +
  theme_apricitas +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "vline",x = c(as.Date("2008-11-01"),as.Date("2014-08-21"),as.Date("2019-09-01"),as.Date("2015-01-22"),as.Date("2012-12-01")),xintercept = c(as.Date("2008-11-01"),as.Date("2014-08-21"),as.Date("2019-09-01"),as.Date("2015-01-22"),as.Date("2012-12-01")), size = 1.25,linetype = "dashed",color = c("#EE6055","#EE6055","#EE6055","#FFE98F","#00A99D")) + #annotating important QE events
  annotate(geom = "text",label = c(as.character("Fed Starts QE"),as.character("Fed Tapers QE"),as.character("Fed Restarts QE"),as.character("ECB Starts QE"),as.character("BOJ Buys ETFs")), x=c(as.Date("2007-05-01"),as.Date("2013-01-21"),as.Date("2018-01-01"),as.Date("2016-07-22"),as.Date("2011-04-01")), y = c(500,500,500,400,300), color = c("#EE6055","#EE6055","#EE6055","#FFE98F","#00A99D"))


ggsave(dpi = "retina",plot = QE_Stock, "QE and Stock Market Performance.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

  
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
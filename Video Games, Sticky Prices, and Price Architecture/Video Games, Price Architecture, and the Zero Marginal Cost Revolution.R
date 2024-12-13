pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,ggpubr)

Gaming_Prices <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Video%20Games%2C%20Sticky%20Prices%2C%20and%20Price%20Architecture/Gaming_Prices.csv")

MC_Curves <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Video%20Games%2C%20Sticky%20Prices%2C%20and%20Price%20Architecture/MC_Curves.csv")

colnames(Gaming_Prices) <- c("DATE","PCEPI", "Nintendo_Nominal","Nintendo_Real","Microsoft_Nominal","Microsoft_Real","Sony_Nominal","Sony_Real")

Gaming_Prices[, 3:8] <- lapply(Gaming_Prices[, 3:8], function(x) as.numeric(gsub("[$,]", "", x))) #forcing numeric on char values and removing , or $

Gaming_Prices$DATE <- as.Date(Gaming_Prices$DATE, "%m/%d/%Y")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

LoZIndexGraph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=Gaming_Prices, aes(x=DATE, y=Nintendo_Nominal , color= "Nominal Price"), size = 1.25) +
  geom_line(data=Gaming_Prices, aes(x=DATE, y=Nintendo_Real , color= "Real Price (2021 Dollars)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,120), breaks = c(0,30,60,90,120), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1985-01-01"),as.Date("2021-07-01"))) +
  ylab("Dollars, Real (2021 Dollars) and Nominal") +
  ggtitle("The Legend of Zelda Index") +
  labs(caption = "Graph created by @JosephPolitano using BEA and assorted historical data",subtitle = "Nominal Prices for Legend of Zelda Games Remained Between $50-$60, but Real Prices Halved") +
  theme_apricitas + theme(legend.position = c(.80,.75)) +
  scale_color_manual(name= "",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))

ConsolePricesGraph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=Gaming_Prices, aes(x=DATE, y=Sony_Real , color= "Sony"), size = 1.25) +
  geom_line(data=Gaming_Prices, aes(x=DATE, y=Microsoft_Real , color= "Microsoft"), size = 1.25) +
  geom_line(data=Gaming_Prices, aes(x=DATE, y=Nintendo_Real , color= "Nintendo"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,120), breaks = c(0,30,60,90,120), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1985-01-01"),as.Date("2021-07-01"))) +
  ylab("Dollars, Real (2021 Dollars)") +
  ggtitle("Real Prices of AAA Games are Decreasing Together") +
  labs(caption = "Graph created by @JosephPolitano using BEA and assorted historical data",subtitle = "Regardless of Game or Console, the Real Prices of AAA Video Games Have Decreased in Unison") +
  theme_apricitas + theme(legend.position = c(.80,.75)) +
  scale_color_manual(name= "",values = c("#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E","#6A4C93"))


MC_Curves_Traditional <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=MC_Curves, aes(x=Quantity, y=AC_Traditional , color= "Average Cost"), size = 1.25) +
  geom_line(data=MC_Curves, aes(x=Quantity, y=MC_Traditional , color= "Marginal Cost"), size = 1.25) +
  xlab("Q") +
  scale_y_continuous(limits = c(0,70), breaks = c(0,30,60,90,120), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,35), expand = c(0,0)) +
  ylab("P") +
  ggtitle("Increasing Marginal Cost") +
  #labs(caption = subtitle = "Traditional MC Curves Show A High Priced Equilibrium as Marginal Costs Increase") +
  theme_apricitas + theme(legend.position = c(.80,.30), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_manual(name= "",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E","#6A4C93")) +
  annotate("segment", x = 0, xend = 13.5,y = 21, yend = 21, linetype = "dashed", color = "white", size = 1.25) +
  annotate("segment", x = 13.5, xend = 13.5, y = 0, yend = 21, linetype = "dashed", color = "white", size = 1.25)
  
MC_Curves_ZMC <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=MC_Curves, aes(x=Quantity, y=AC_ZMC , color= "Average Cost"), size = 1.25) +
  geom_line(data=MC_Curves, aes(x=Quantity, y=MC_ZMC+1 , color= "Marginal Cost"), size = 1.25) +
  xlab("Q") +
  scale_y_continuous(limits = c(0,120), breaks = c(0,30,60,90,120), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,70), expand = c(0,0)) +
  ylab("P") +
  ggtitle("Zero Marginal Cost") +
  #labs(caption = "Graph created by @JosephPolitano") +
  theme_apricitas + theme(legend.position = c(.80,.30), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_manual(name= "",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E","#6A4C93"))

MC_Curves_Arrage <- ggarrange(MC_Curves_Traditional, MC_Curves_ZMC)



ggsave(dpi = "retina",plot = LoZIndexGraph, "Legend Of Zelda Index.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = ConsolePricesGraph, "Prices By Console.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = MC_Curves_Arrage, "MC Curves.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

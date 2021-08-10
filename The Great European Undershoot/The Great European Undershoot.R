pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)

OECD_GDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Great%20European%20Undershoot/GDPPerCap.csv")
OECD_Bond <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Great%20European%20Undershoot/Bond_Spreads.csv")
EU_Debt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Great%20European%20Undershoot/EU_Debt.csv")
EU_Deficit <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Great%20European%20Undershoot/EU_Deficit.csv")

Spread <- spread(OECD_Bond,ï..LOCATION,Value)
Spread$TIME <- as.Date(paste(Spread$TIME,"-01",sep="")) #converting to date and adding Day

GDPPC <- subset(OECD_GDP, SUBJECT == "T_GDPPOP") #OECD Data GDP Per Capita
GDPPC <- subset(GDPPC, Measure == "USD, constant prices, 2015 PPPs") #filtering by unit
GDPPC <- GDPPC[GDPPC$Country %in% c("France", "Germany", "Italy", "Greece","Euro area (19 countries)"), ]
EUTrend <- data.frame(TIME = seq(1995,2020),GDPTrend = 33120 * 1.018^(seq(0,25))) #1.8% EU GDP per capita growth trend

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

Undershoot_GDPPerCapita <- ggplot() + #plotting gdp per capita
  geom_line(data=GDPPC[GDPPC$Country %in% c("France", "Germany", "Italy", "Greece"), ], aes(x=TIME,y=Value,color= Country), size = 1.25) +
  geom_line(data=GDPPC[GDPPC$Country %in% c("Euro area (19 countries)"), ], aes(x=TIME,y=Value,color= Country), size = 1.75) + #plotting EU total second so that it is superimposed and can easily be seen
  geom_line(data=EUTrend[EUTrend$TIME > 2007, ], aes(x=TIME,y=GDPTrend,color= "Euro area Pre-crisis Trend"), size = 1.25, linetype = "dashed") + #plotting trend line created through manual calculations
  xlab("Year") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(15000,55000), breaks = c(20000,30000,40000,50000), expand = c(0,0)) +
  ylab("GDP Per Capita, 2015 PPP $") +
  ggtitle("The Great European Undershoot") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "GDP Per Capita Growth Broke a Forty Year Trend") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), guide=guide_legend(override.aes=list(linetype=c(1,2,1,1,1,1), lwd = c(1.25,1,1.25,1.25,1.25,1.25)))) +
  theme(legend.key.width =  unit(1.1, "cm"))

Bond_Spreads <- ggplot() + #plotting bond spreads
  geom_line(data=Spread, aes(x=TIME,y=(FRA - DEU)/100 ,color= "France"), size = 1.25) +
  geom_line(data=drop_na(Spread,GRC), aes(x=TIME,y=(GRC - DEU)/100 ,color= "Greece"), size = 1.25) +
  geom_line(data=Spread, aes(x=TIME,y=(ITA - DEU)/100 ,color= "Italy"), size = 1.25) +
  geom_line(data=Spread, aes(x=TIME,y=(ESP - DEU)/100 ,color= "Spain"), size = 1.25) +
  geom_line(data=Spread, aes(x=TIME,y=(PRT - DEU)/100 ,color= "Portugal"), size = 1.25) +
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,0.3), breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("Spread with German Bunds, %") +
  ggtitle("Euro Area 10yr Bond Spreads") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Spreads Exploded During the Euro Crisis") +
  theme_apricitas + theme(legend.position = c(.85,.60)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#A7ACD9","#9A348E","#FFE98F","#EE6055")) +
  annotate(geom = "vline",x = as.Date("2012-07-26"),xintercept = as.Date("2012-07-26"), size = 1.25,linetype = "dashed",color = "white") +#annotating "whatever it takes" speech
  annotate(geom = "text", label = as.character("Draghi: 'Whatever it takes'"),x = as.Date("2015-07-26"),y= 0.25,color = "white")

EU_Debt_Graph <- ggplot() + 
  geom_line(data = EU_Debt, aes(x=TIME, y = Germany..until.1990.former.territory.of.the.FRG./100, color = "Germany"), size = 1.25) +
  geom_line(data = EU_Debt, aes(x=TIME, y = France/100, color = "France"), size = 1.25) +
  geom_line(data = EU_Debt, aes(x=TIME, y = Italy/100, color = "Italy"), size = 1.25) +
  geom_line(data = EU_Debt, aes(x=TIME, y = Greece/100, color = "Greece"), size = 1.25) +
  geom_line(data = EU_Debt, aes(x=TIME, y = Euro.area...19.countries...from.2015./100, color = "Euro area (19 countries)"), size = 1.25) +
  xlab("Year") +
  #xlim(c(2000,2020)) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,2.1), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) +
  ylab("Gross General Government Debt/GDP") +
  ggtitle("The Maastricht Treaty") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "The Treaty's Restrictions are Incredibly Burdensome") +
  theme_apricitas + theme(legend.position = c(.55,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotate(geom = "hline",y = 0.6,yintercept = 0.6, size = 1.25,linetype = "dashed",color = "white") +#annotating "whatever it takes" speech
  annotate(geom = "text", label = as.character("Maastricht Treaty Debt Limit: 60% Debt/GDP"),x = 2015,y= 0.54,color = "white")

EU_Deficit_Graph <- ggplot() + 
  geom_line(data = EU_Deficit, aes(x=TIME, y = Germany..until.1990.former.territory.of.the.FRG./-100, color = "Germany"), size = 1.25) +
  geom_line(data = EU_Deficit, aes(x=TIME, y = France/-100, color = "France"), size = 1.25) +
  geom_line(data = EU_Deficit, aes(x=TIME, y = Italy/-100, color = "Italy"), size = 1.25) +
  geom_line(data = EU_Deficit, aes(x=TIME, y = Greece/-100, color = "Greece"), size = 1.25) +
  geom_line(data = EU_Deficit, aes(x=TIME, y = Euro.area...19.countries...from.2015./-100, color = "Euro area (19 countries)"), size = 1.25) +
  xlab("Year") +
  #xlim(c(2000,2020))+
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.05,.15), breaks = c(-0.05,0,0.05,.1,.15), expand = c(0,0)) +
  ylab("Net Deficit/GDP") +
  ggtitle("The Maastricht Treaty") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "The Treaty's Restrictions are Incredibly Burdensome") +
  theme_apricitas + theme(legend.position = c(.87,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotate(geom = "hline",y = 0.03,yintercept = 0.03, size = 1.25,linetype = "dashed",color = "white") +#annotating "whatever it takes" speech
  annotate(geom = "text", label = as.character("Maastricht Treaty Deficit Limit: 3% Deficit/GDP"),x = 2000,y= 0.005,color = "white")

  

ggsave(dpi = "retina",plot = Undershoot_GDPPerCapita, "Undershoot_GDPPerCapita.png", type = "cairo-png") #Saving Image of 
ggsave(dpi = "retina",plot = Bond_Spreads, "Bond_Spreads.png", type = "cairo-png") #Saving Image of 
ggsave(dpi = "retina",plot = EU_Debt_Graph, "EU_Debt.png", type = "cairo-png") #Saving Image of 
ggsave(dpi = "retina",plot = EU_Deficit_Graph, "EU_Deficit.png", type = "cairo-png") #Saving Image of 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

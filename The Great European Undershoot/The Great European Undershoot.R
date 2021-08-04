pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)

OECD <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Great%20European%20Undershoot/GDPPerCap.csv")


GDPPC <- subset(OECD, SUBJECT == "T_GDPPOP") #OECD Data GDP Per Capita
GDPPC <- subset(GDPPC, Measure == "USD, constant prices, 2015 PPPs") #filtering by unit
GDPPC <- GDPPC[GDPPC$Country %in% c("France", "Germany", "Italy", "Greece","Euro area (19 countries)"), ]
EUTrend <- data.frame(TIME = seq(1995,2020),GDPTrend = 33120 * 1.018^(seq(0,25))) #1.8% EU GDP per capita growth trend

GDPPH <- subset(OECD, SUBJECT == "T_GDPHRS") #OECD Data GDP Per Hour Worked

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

Undershoot_GDPPerCapita <- ggplot() + #plotting interest as a %of GDP
  geom_line(data=GDPPC[GDPPC$Country %in% c("France", "Germany", "Italy", "Greece"), ], aes(x=TIME,y=Value,color= Country), size = 1.25) +
  geom_line(data=GDPPC[GDPPC$Country %in% c("Euro area (19 countries)"), ], aes(x=TIME,y=Value,color= Country), size = 1.75) +
  geom_line(data=EUTrend[EUTrend$TIME > 2007, ], aes(x=TIME,y=GDPTrend,color= "Euro area Pre-crisis Trend"), size = 1.25, linetype = "dashed") +
  xlab("Year") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(15000,55000), breaks = c(20000,30000,40000,50000), expand = c(0,0)) +
  ylab("GDP Per Capita, 2015 PPP $") +
  ggtitle("The Great European Undershoot") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "GDP Per Capita Growth Broke a Forty Year Trend") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), guide=guide_legend(override.aes=list(linetype=c(1,2,1,1,1,1), lwd = c(1.25,1,1.25,1.25,1.25,1.25)))) +
  theme(legend.key.width =  unit(1.1, "cm"))

ggsave(dpi = "retina",plot = Undershoot_GDPPerCapita, "Undershoot_GDPPerCapita.png", type = "cairo-png") #Saving Image of 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

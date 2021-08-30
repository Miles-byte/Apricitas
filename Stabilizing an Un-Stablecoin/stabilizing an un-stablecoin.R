pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)

USDT <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stabilizing%20an%20Un-Stablecoin/usdt-usd-max.csv")
USDC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stabilizing%20an%20Un-Stablecoin/usdc-usd-max.csv")
BUSD <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stabilizing%20an%20Un-Stablecoin/busd-usd-max.csv")
Titan <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stabilizing%20an%20Un-Stablecoin/titan-usd-max.csv")
Iron <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stabilizing%20an%20Un-Stablecoin/iron-usd-max.csv")
Tether <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Stabilizing%20an%20Un-Stablecoin/Tether-Reserves.csv")



BUSD$snapped_at <- as.Date(BUSD$snapped_at)
USDT$snapped_at <- as.Date(USDT$snapped_at)
USDC$snapped_at <- as.Date(USDC$snapped_at)
Titan$snapped_at <- as.Date(Titan$snapped_at)
Iron$snapped_at <- as.Date(Iron$snapped_at)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

MktCap_Graph <- ggplot() + #Plotting stablecoin volume and market cap Growth Rates
  geom_line(data=USDT, aes(x=snapped_at, y= market_cap/1000000000 , color= "USDT"), size = 1.25) +
  geom_line(data=USDC, aes(x=snapped_at, y= market_cap/1000000000 , color= "USDC"), size = 1.25) +
  geom_line(data=BUSD, aes(x=snapped_at, y= market_cap/1000000000 , color= "BUSD"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,70), breaks = c(20,40,60), expand = c(0,0)) + #adding 'B' suffix to represent Billions of dollars
  scale_x_date(limits = c(as.Date("2018-01-01"),as.Date("2021-08-30"))) +
  ylab("Market Cap, Billions of Dollars") +
  ggtitle("The Rise of the Digital Dollar") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Stablecoins are a Multi-Billion Dollar Financial System, and They're Still Growing") +
  theme_apricitas + theme(legend.position = c(.73,.75)) +
  scale_color_manual(name= "", breaks = c("USDT","USDC","BUSD"), values = c("#EE6055","#00A99D","#FFE98F","#A7ACD9","#9A348E","#6A4C93")) #manually assigning colors and order

Volume_Graph <- ggplot() + #Plotting stablecoin volume and market cap Growth Rates
  geom_line(data=USDT, aes(x=snapped_at, y= total_volume/1000000000 , color= "USDT"), size = 1.25) +
  geom_line(data=USDC, aes(x=snapped_at, y= total_volume/1000000000 , color= "USDC"), size = 1.25) +
  geom_line(data=BUSD, aes(x=snapped_at, y= total_volume/1000000000 , color= "BUSD"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,300), breaks = c(100,200,300), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2018-01-01"),as.Date("2021-08-30"))) +
  ylab("24hr Volume, Billions of Dollars") +
  ggtitle("Very Volatile Volume") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Stablecoins Process Billions in Transactions Every Day, Often More Than Their Total Market Cap") +
  theme_apricitas + theme(legend.position = c(.73,.75)) +
  scale_color_manual(name= "", breaks = c("USDT","USDC","BUSD"), values = c("#EE6055","#00A99D","#FFE98F","#A7ACD9","#9A348E","#6A4C93"))

Titan_Graph <- ggplot() + 
  geom_line(data=Titan, aes(x=snapped_at, y = price , color= "Titan Price"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2021-06-02"),as.Date("2021-08-30"))) +
  ylab("Price, Dollars") +
  ggtitle("Titan-ic Collapse") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Titan's Price Collapsed After a Bank Run Triggered Algorithmic Coin Minting") +
  theme_apricitas + theme(legend.position = c(.73,.75)) +
  scale_color_manual(name= "", values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93"))

Titan_Graph <- ggplot() + 
  geom_line(data=Titan, aes(x=snapped_at, y = price , color= "Titan Price"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(0,40), breaks = c(0,10,20,30,40)) +
  scale_x_date(limits = c(as.Date("2021-06-02"),as.Date("2021-08-30"))) +
  ylab("Price, Dollars") +
  ggtitle("Titan-ic Collapse") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Titan's Price Collapsed After a 'Bank Run' Triggered Algorithmic Coin Minting and ") +
  theme_apricitas + theme(legend.position = c(.73,.75)) +
  scale_color_manual(name= "", values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93"))

Iron_Graph <- ggplot() + 
  geom_line(data=Iron, aes(x=snapped_at, y = price , color= "Iron Price"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(0,2), breaks = c(0,.5,1,1.5,2), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2021-06-02"),as.Date("2021-07-10"))) +
  ylab("Price, Dollars") +
  ggtitle("Iron-ic Failure") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Iron's Dollar Peg Destabilized With Titan's Collapse, Forcing Iron Finance To Redesign The Coin") +
  theme_apricitas + theme(legend.position = c(.73,.75)) +
  scale_color_manual(name= "", values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93")) +
  annotate("vline", x = as.Date("2021-07-01"), xintercept = as.Date("2021-06-16"), linetype = "dashed", color = "white", size = 1.25) +
  annotate("text", x = as.Date("2021-06-21"), y = 1.25, label = "Iron Breaks Dollar Peg", color = "white")

Tether_Graph <- ggplot(unemployedincome, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  xlab("") +
  ylab("") +
  theme_economist() +
  ggtitle("UI Compared to Prior Work Income") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank()) +
  scale_fill_economist(name="")


ggsave(dpi = "retina",plot = MktCap_Graph, "Stablecoin_Graph_MktCap.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = Volume_Graph, "Stablecoin_Graph_Volume.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = Titan_Graph, "Titan_Graph.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = Iron_Graph, "Iron_Graph.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = Tether_Graph, "Stablecoin_Graph.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
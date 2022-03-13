pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Hashrate_By_Country <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Mr.%20Crypto%20Goes%20to%20Washington/Hashrate_by_Country.csv")

USDT <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Mr.%20Crypto%20Goes%20to%20Washington/usdt-usd-max.csv")
USDC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Mr.%20Crypto%20Goes%20to%20Washington/usdc-usd-max.csv")
BUSD <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Mr.%20Crypto%20Goes%20to%20Washington/busd-usd-max.csv")

Tether_Reserves <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Mr.%20Crypto%20Goes%20to%20Washington/Tether_Reserves.csv")
colnames(Tether_Reserves) <- c("Asset","March 2021","December 2021")
Tether_Reserves <- pivot_longer(Tether_Reserves, cols = c("March 2021","December 2021"), names_to = "Date", values_to = "Percent")
Tether_Reserves$Asset <- factor(Tether_Reserves$Asset, levels = c("Reverse_Repo","Other_Digital_Tokens","Corporate_Bonds_Funds_Metals","Secured_Loans","Cash_MMF_Bank_Deposits","Treasury_Bills","Commercial_Paper"), ordered = TRUE)

USDT$snapped_at <- as.Date(USDT$snapped_at)
USDC$snapped_at <- as.Date(USDC$snapped_at)
BUSD$snapped_at <- as.Date(BUSD$snapped_at)

Hashrate_By_Country$date <- as.Date(Hashrate_By_Country$date, "%m/%d/%Y")
Hashrate_By_Country <- subset(Hashrate_By_Country, country == "Mainland China")
Hashrate_By_Country$monthly_hashrate_. <- gsub("%", "" ,Hashrate_By_Country$monthly_hashrate_.)
Hashrate_By_Country$monthly_hashrate_. <- as.numeric(Hashrate_By_Country$monthly_hashrate_.)

Hashrate_By_Country_Graph <- ggplot() + #plotting China's BTC hashrate
  geom_line(data = Hashrate_By_Country, aes(x = date, y = monthly_hashrate_./100, color = "Mainland China's Share of Global Bitcoin Hashrate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,.25,.5,.75,1), expand = c(0,0)) +
  ylab("% of Total Global Hashrate") +
  ggtitle("The Bitcoin Ban") +
  labs(caption = "Graph created by @JosephPolitano using Cambridge Bitcoin Electricity Consumption Index data",subtitle = "China's Bitcoin Miners Were Shut Down by Government Decree") +
  theme_apricitas + theme(legend.position = c(.60,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-09-01")-(.1861*700), xmax = as.Date("2019-09-01")-(0.049*700), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

MktCap_Graph <- ggplot() + #Plotting stablecoin volume and market cap Growth Rates
  geom_line(data=USDT, aes(x=snapped_at, y= market_cap/1000000000 , color= "USDT"), size = 1.25) +
  geom_line(data=USDC, aes(x=snapped_at, y= market_cap/1000000000 , color= "USDC"), size = 1.25) +
  geom_line(data=BUSD, aes(x=snapped_at, y= market_cap/1000000000 , color= "BUSD"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,90), breaks = c(20,40,60,80), expand = c(0,0)) + #adding 'B' suffix to represent Billions of dollars
  scale_x_date(limits = c(as.Date("2018-01-01"),as.Date("2022-2-13"))) +
  ylab("Market Cap, Billions of Dollars") +
  ggtitle("The Rise of the Digital Dollar") +
  labs(caption = "Graph created by @JosephPolitano Using CoinGecko Data",subtitle = "Stablecoins are a Multi-Billion Dollar Financial System, and They're Still Growing") +
  theme_apricitas + theme(legend.position = c(.63,.75)) +
  scale_color_manual(name= "", breaks = c("USDT","USDC","BUSD"), values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93")) + #manually assigning colors and order
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1504), xmax = as.Date("2018-01-01")-(0.049*1504), ymin = 0-(.3*90), ymax = 0) +
  coord_cartesian(clip = "off")

Tether_Reserves_Graph <- ggplot(Tether_Reserves, aes(x = Asset, y = Percent))+
  geom_bar(aes(fill = Date), position = "dodge", stat = "identity", width = 0.7, color = NA) +
  scale_x_discrete(labels = c("Reverse Repo Notes","Other (Including Digital Tokens)","Corporate Bonds, Metals, and Funds","Secured Loans","Cash and Cash Equivalents","Treasury Bills","Commercial Paper")) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.6), breaks = c(0,.2,.4,.6), expand = c(0,0)) + #adding % format
  theme_apricitas +  theme(legend.position = c(.68,.55)) +
  ggtitle("The Rise of the Digital Dollar") +
  labs(caption = "Graph created by @JosephPolitano Using Tether Data",subtitle = "Tether's Reserve Assets are Getting Safer, But Remain Relatively Risky") +
  scale_fill_manual(values = c("#FFE98F","#00A99D"), breaks = c("March 2021", "December 2021")) +
  coord_flip()

ggsave(dpi = "retina",plot = Hashrate_By_Country_Graph, "Hashrate By Country.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = MktCap_Graph, "Stablecoin Market Cap.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Tether_Reserves_Graph, "Tether Reserves.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
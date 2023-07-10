pacman::p_load(bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("seokhoonj/ecos", force = TRUE)
library("ecos")

list <- statTableList(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en")

WONUSD <- fredr(series_id = "DEXKOUS", observation_start = as.Date("2018-01-01")) %>%
  drop_na()

WONUSD_Graph <- ggplot() + #plotting Dollar Won Exchange Rate
  geom_line(data=WONUSD, aes(x=date,y= value, color= "South Korean Won: USD Exchange Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "₩", accuracy = 1), limits = c(1000,1500),breaks = c(1000,1100,1200,1300,1400,1500), expand = c(0,0)) +
  ylab("South Korean Won to One US Dollar") +
  ggtitle("There and Back Again") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Korean Won Depreciated Against the Dollar Throughout 2022, but Has Appreciated Recently") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 1000-(.3*500), ymax = 1000) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WONUSD_Graph, "WONUSD Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#EDIT OUT API KEY
GDP <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "200Y004", item_code1 = "1400", start_time = "2018Q1", cycle = "Q") %>% 
  mutate(time = as.Date(as.yearqtr(time, "%YQ%q")))

GDP_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=GDP, aes(x=time,y= data_value/1000, color= "South Korean Real GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "₩",suffix = "T", accuracy = 1), limits = c(445,500),breaks = c(450,460,470,480,490,500), expand = c(0,0)) +
  ylab("Trillions of 2015 South Korean Won, Quarterly") +
  ggtitle("Korea's Trade Toubles") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "South Korea's Economy Shrank in Q4—Thanks in Part to Weakening Net Exports") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 445-(.3*55), ymax = 445) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_Graph, "GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INDPRO <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "901Y033", item_code1 = "AB00", item_code2 = "2", start_time = "201812", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))
SERVPRO <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "901Y033", item_code1 = "AC00", item_code2 = "2", start_time = "201812", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))
CONSPRO <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "901Y033", item_code1 = "AD00", item_code2 = "2", start_time = "201812", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

PRO_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=CONSPRO, aes(x=time,y= data_value/data_value[1]*100, color= "Construction"), size = 1.25) +
  geom_line(data=SERVPRO, aes(x=time,y= data_value/data_value[1]*100, color= "Services"), size = 1.25) +
  geom_line(data=INDPRO, aes(x=time,y= data_value/data_value[1]*100, color= "Industry"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,117.5),breaks = c(90,95,100,105,110,115), expand = c(0,0)) +
  ylab("Index: Dec 2018 = 100") +
  ggtitle("Korea's Economic Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "South Korea's Slowdown Has Been Driven by a Massive Fall in Industrial Output") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Production Index, South Korea" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Industry", "Services", "Construction")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-12-01")-(.1861*(today()-as.Date("2018-12-01"))), xmax = as.Date("2018-12-01")-(0.049*(today()-as.Date("2018-12-01"))), ymin = 90-(.3*27.5), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PRO_Graph, "PRO Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INDPRO_CAR <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "901Y032", item_code1 = "I11ACU",item_code2 = "2", start_time = "201801", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

KOREA_IP_CARS_graph <- ggplot() + #plotting MOVE
  geom_line(data=INDPRO_CAR, aes(x=time,y= data_value/data_value[1]*100,color= "Motor Vehicles, Trailers, and Semitrailers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,140), breaks = c(60,80,100,120,140), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Korea's Car Industry") +
  labs(caption = "Graph created by @JosephPolitano using KOSTAT Data",subtitle = "South Korean Vehicle Production Is at a Record High") +
  theme_apricitas + theme(legend.position = c(.425,.85)) +
  scale_color_manual(name= "Industrial Production, South Korea",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*80), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = KOREA_IP_CARS_graph, "Korea IP Cars Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BUS_NATIONAL_TENDENCY_EXPORTS <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "512Y015", item_code1 = "C0000", item_code2 = "AM",start_time = "201801", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

BUS_NATIONAL_TENDENCY_PRODUCTION <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "512Y015", item_code1 = "C0000", item_code2 = "AC", start_time = "201801", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))


BSI_Graph <- ggplot() + #plotting US Crude Production
  annotate(geom = "hline",y = 100.0,yintercept = 100, size = .25,color = "white") +
  geom_line(data=BUS_NATIONAL_TENDENCY_EXPORTS, aes(x=time,y= data_value, color= "Exports Growth"), size = 1.25) +
  geom_line(data=BUS_NATIONAL_TENDENCY_PRODUCTION, aes(x=time,y= data_value, color= "Production Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(45,135),breaks = c(60,80,100,120), expand = c(0,0)) +
  ylab("Index, > 100 = Expansion") +
  ggtitle("Korea's Economic Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "South Korea's Industrial Slowdown Has Been Driven by a Massive Fall in Exports") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= "Business Survey Index, National, Manufacturing, Size-Weighted, South Korea" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Exports Growth", "Production Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 45-(.3*90), ymax = 45) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BSI_Graph, "BSI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


NETEXP <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "301Y017", item_code1 = "SA100", start_time = "201801", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

NETEXP_Graph <- ggplot() + #plotting US Crude Production
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=NETEXP, aes(x=time,y= data_value/1000, color= "South Korean Net Goods Exports, Seasonally Adjusted"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(-5,12.5),breaks = c(-5,0,5,10), expand = c(0,0)) +
  ylab("Billions of US Dollars, Monthly") +
  ggtitle("Korea's Trade Toubles") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "South Korea's Is Back to Being a Net Exporter of Goods") +
  theme_apricitas + theme(legend.position = c(.45,.15)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -5-(.3*17.5), ymax = -5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NETEXP_Graph, "NETEXP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EXP_PRICE <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "402Y014",start_time = "201801", item_code1 = "*AA", item_code2 = "C", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))
IMP_PRICE <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "401Y015",start_time = "201801", item_code1 = "*AA", item_code2 = "C", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

EXP_IMP_PI_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=EXP_PRICE, aes(x=time,y= data_value/data_value[1]*100, color= "Exports"), size = 1.25) +
  geom_line(data=IMP_PRICE, aes(x=time,y= data_value/data_value[1]*100, color= "Imports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,130),breaks = c(80,100,120), expand = c(0,0)) +
  ylab("Index: Jan 2018 = 100") +
  ggtitle("Korea's Trade Troubles") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "Korean Export Prices Rose Less and Fell Faster than Import Prices") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Price Index, Contractual Currency Basis, South Korea" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Exports", "Imports")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 80-(.3*50), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXP_IMP_PI_Graph, "EXP IMP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NETEXP_COUNTRY <- read.csv("C:/Users/josep/Documents/South Korea/EXPORTS_COUNTRY.csv") %>%
  mutate(Date = as.Date(Date))%>%
  mutate_if(is.character, as.numeric)

NETEXP_COUNTRY_Graph <- ggplot() + #plotting US Crude Production
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=NETEXP_COUNTRY, aes(x=Date,y= EU.28/1000000, color= "European Union and United Kingdom"), size = 1.25) +
  geom_line(data=NETEXP_COUNTRY, aes(x=Date,y= US/1000000, color= "United States"), size = 1.25) +
  geom_line(data=NETEXP_COUNTRY, aes(x=Date,y= CHINA/1000000, color= "China"), size = 1.25) +
  geom_line(data=NETEXP_COUNTRY, aes(x=Date,y= JAPAN/1000000, color= "Japan"), size = 1.25) +
  geom_line(data=NETEXP_COUNTRY, aes(x=Date,y= AUSTRALIA/1000000, color= "Australia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(-5,8.5),breaks = c(-5,0,5,10), expand = c(0,0)) +
  ylab("Billions of US Dollars, Monthly") +
  ggtitle("Korea's Trade Troubles") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "Korean Net Exports to China Collapsed Amidst the Latter's Slowdown") +
  theme_apricitas + theme(legend.position = c(.45,.82)) +
  scale_color_manual(name= "South Korean Goods Net Exports, NSA" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("China","United States","European Union and United Kingdom","Japan","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -5-(.3*13.5), ymax = -5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NETEXP_COUNTRY_Graph, "NETEXP Country Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NETEXP_FOOD_ENERGY <- read.csv("C:/Users/josep/Documents/South Korea/FOOD_ENERGY.csv") %>%
  mutate(Date = as.Date(Date))%>%
  mutate_if(is.character, as.numeric)

NETEXP_FOOD_ENERGY_Graph <- ggplot() + #plotting US Crude Production
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=NETEXP_FOOD_ENERGY, aes(x=Date,y= Food/1000000, color= "Food (HS 1-24)"), size = 1.25) +
  geom_line(data=NETEXP_FOOD_ENERGY, aes(x=Date,y= Energy/1000000, color= "Energy (HS 27)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(-15,1),breaks = c(-15,-10,-5,0), expand = c(0,0)) +
  ylab("Billions of US Dollars, Monthly") +
  ggtitle("Korea's Trade Toubles") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "Korean Net Exports are Falling Thanks in Large Part to Rising Energy Costs") +
  theme_apricitas + theme(legend.position = c(.45,.22)) +
  scale_color_manual(name= "South Korean Goods Net Exports, NSA" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -15-(.3*16), ymax = -15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NETEXP_FOOD_ENERGY_Graph, "NETEXP Food Energy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EXP_PRICE_SEMICONDUCTOR <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "402Y014", item_code1 = "3091AA", item_code2 = "C",start_time = "201601", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))
EXP_PRICE_COMPUTER <- statSearch(api_key = "2DNSQWJY32YGLL8EM95R", lang = "en",stat_code = "402Y014", item_code1 = "309AA", item_code2 = "C",start_time = "201601", cycle = "M") %>%
  mutate(time = as.Date(as.yearmon(time, "%Y%m")))

EXP_IMP_PI_COMPUTERS_CONDUCTORS_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=EXP_PRICE_SEMICONDUCTOR, aes(x=time,y= data_value/data_value[1]*100, color= "Semiconductors"), size = 1.25) +
  geom_line(data=EXP_PRICE_COMPUTER, aes(x=time,y= data_value/data_value[1]*100, color= "Computers, Electronic & Optical Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(40,140),breaks = c(40,60,80,100,120,140), expand = c(0,0)) +
  ylab("Index: Jan 2016 = 100") +
  ggtitle("Korea's Trade Troubles") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Korea data",subtitle = "Prices for Korean Tech Exports Fell Dramatically at the End of 2022") +
  theme_apricitas + theme(legend.position = c(.45,.15)) +
  scale_color_manual(name= "Export Price Index, Contractual Currency Basis, South Korea" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 40-(.3*100), ymax = 40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXP_IMP_PI_COMPUTERS_CONDUCTORS_Graph, "NETEXP Chips Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


?statSearch
#ADD KOSSIS DATA NEXT TIME
p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()


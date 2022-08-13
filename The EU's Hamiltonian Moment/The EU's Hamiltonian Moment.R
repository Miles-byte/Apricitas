pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#taking key ECB rates
ECB_REFI_RATE <- fredr(series_id = "ECBMRRFR", observation_start = as.Date("2008-11-01")) %>%
  add_row(date = as.Date("2022-07-23"), value = 0.5)#downloading ECB REFI Rate

ECB_DEPOSIT_RATE <- fredr(series_id = "ECBDFR", observation_start = as.Date("2008-11-01")) %>%
  add_row(date = as.Date("2022-07-23"), value = 0)#downloading ECB REFI Rate and manually adding rate hike



#taking spreads
ITA10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/ITALY10YR.csv")
GER10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/GERMANY10YR.csv")
GRE10YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Different%20Inflation%20Problem/GREECE10YR.csv")

#converting to date
ITA10YR$ï..Date <- as.Date(ITA10YR$ï..Date, "%m/%d/%Y")
GER10YR$ï..Date <- as.Date(GER10YR$ï..Date, "%m/%d/%Y")
GRE10YR$ï..Date <- as.Date(GRE10YR$ï..Date, "%m/%d/%Y")
#changing colnames
colnames(ITA10YR) <- c("date","value")
colnames(GER10YR) <- c("date","value")
colnames(GRE10YR) <- c("date","value")
#calculating spreads
ITASPREADS <- merge(ITA10YR,GER10YR, by = "date")
GRESPREADS <- merge(GRE10YR,GER10YR, by = "date")

OECD_Bond <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Great%20European%20Undershoot/Bond_Spreads.csv")
Spread <- spread(OECD_Bond,ï..LOCATION,Value)
Spread$TIME <- as.Date(paste(Spread$TIME,"-01",sep="")) #converting to date and adding Day

#EURO HYOAS
EU_ICECCCCORPORATE <- fredr(series_id = "BAMLHE00EHYIOAS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
EU_ICECCCCORPORATE <- drop_na(EU_ICECCCCORPORATE)

EPOP_ITALY <- fredr(series_id = "LREM25TTITQ156S",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOP_GREECE <- fredr(series_id = "LREM25TTGRQ156S",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOP_GERMANY <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)

ITA_PRI_SURPLUS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20EU's%20Hamiltonian%20Moment/Italy_Primary_Surplus.csv") %>%
  mutate(primary = gsub(")","",primary)) %>%
  mutate(primary = gsub("\\(","-",primary)) %>%
  mutate(primary = as.numeric(primary))

EU_Spreads_2000_Graph <- ggplot() + #plotting bond spreads
  geom_line(data=Spread, aes(x=TIME,y=(FRA - DEU)/100 ,color= "France"), size = 1.25) +
  geom_line(data=drop_na(Spread,GRC), aes(x=TIME,y=(GRC - DEU)/100 ,color= "Greece"), size = 1.25) +
  geom_line(data=Spread, aes(x=TIME,y=(ITA - DEU)/100 ,color= "Italy"), size = 1.25) +
  geom_line(data=Spread, aes(x=TIME,y=(ESP - DEU)/100 ,color= "Spain"), size = 1.25) +
  geom_line(data=Spread, aes(x=TIME,y=(PRT - DEU)/100 ,color= "Portugal"), size = 1.25) +
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,0.3), breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("Spread with German Bunds, %") +
  ggtitle("Mind the Gap") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Spreads Exploded During the Euro Crisis") +
  theme_apricitas + theme(legend.position = c(.28,.70)) +
  scale_color_manual(name= "Spreads Between 10 Year German Bonds",values = c("#00A99D","#A7ACD9","#9A348E","#FFE98F","#EE6055")) +
  annotate(geom = "vline",x = as.Date("2012-07-26"),xintercept = as.Date("2012-07-26"), size = 1,linetype = "dashed",color = "white") +#annotating "whatever it takes" speech
  annotate(geom = "text", label = as.character("Draghi: 'Whatever it takes'"),x = as.Date("2015-07-26"),y= 0.25,color = "white", size = 4) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")
  

ECB_RATES_Graph <- ggplot() + #plotting spread
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=ECB_REFI_RATE, aes(x=date,y= value/100, color= "ECB Main Refinance Rate"), size = 1.25) +
  geom_line(data=ECB_DEPOSIT_RATE, aes(x=date,y= value/100, color= "ECB Deposit Facility Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.0075,.04), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Take A Hike") +
  labs(caption = "Graph created by @JosephPolitano using ECB data",subtitle = "The ECB Instituted the First Rate Hike in a Decade to Combat Inflation") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2008-11-01")-(.1861*(today()-as.Date("2008-11-01"))), xmax = as.Date("2008-11-01")-(0.049*(today()-as.Date("2008-11-01"))), ymin = -.0075-(.3*.0475), ymax = -.0075) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

EU_Spreads_Graph <- ggplot() + #plotting bond spreads in the EU
  geom_line(data=GRESPREADS, aes(x=date,y= (value.x-value.y)/100, color= "Greece"), size = 1.25) +
  geom_line(data=ITASPREADS, aes(x=date,y= (value.x-value.y)/100, color= "Italy"), size = 1.25) +
  annotate("vline", x = as.Date("2022-06-15"), xintercept = as.Date("2022-06-15"), color = "white", size = 1, linetype = "dashed") +
  annotate("text", x = as.Date("2021-12-15"), y = 0.035,label = "ECB Emergency Meeting", color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Spreads with German Bonds, %") +
  ggtitle("Containment Protocol") +
  labs(caption = "Graph created by @JosephPolitano using Investing.com data",subtitle = "The ECB's Emergency Meeting Reduced Spreads-But They Remain High") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_color_manual(name= "Spreads Between 10 Year German Bonds",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

EU_ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  annotate("hline", y = 0.1, yintercept = 0.1, color = "#EE6055", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2012 Cycle Peak", x = as.Date("2019-01-01"), y = 0.105, color ="#EE6055", size = 5) +
  geom_line(data=EU_ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA Euro High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using ICE data",subtitle = "Financial Conditions are Rapidly Tightening as the ECB Raises Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

EU_EPOP_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  geom_line(data=EPOP_GERMANY, aes(x=date,y= value/100,color= "Germany"), size = 1.25) +
  geom_line(data=EPOP_ITALY, aes(x=date,y= value/100,color= "Italy"), size = 1.25) +
  geom_line(data=EPOP_GREECE, aes(x=date,y= value/100,color= "Greece"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.60,0.90), breaks = c(0.60,0.70,0.80,.90), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Falling Behind") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "The 2008 and 2012 Crises Left Italy and Greece Permanently Scarred") +
  theme_apricitas + theme(legend.position = c(.25,.84)) +
  scale_color_manual(name= "Prime Age (25-54) Employment Rate",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = .60-(.3*0.3), ymax = .60) +
  coord_cartesian(clip = "off")

ITA_SURPLUS_Graph <- ggplot() + #plotting spread
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=ITA_PRI_SURPLUS, aes(x=ï..date,y= primary/100, color= "Italian Primary Deficit/Surplus"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.07,.07),breaks = c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Secret Surplus") +
  labs(caption = "Graph created by @JosephPolitano using ECB data",subtitle = "Italy Has Posted a Primary Surplus for the Majority of the Last 25 Years") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 1995-(.1861*(2021-1995)), xmax = 1995-(0.049*(2021-1995)), ymin = -.07-(.3*.14), ymax = -.07) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECB_RATES_Graph, "Rates.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EU_Spreads_Graph, "Spreads 2019.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EU_Spreads_2000_Graph, "Spreads 2000.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EU_ICECCCCORPORATE_Graph, "EU HYOAS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EU_EPOP_Graph, "EU EPOP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ITA_SURPLUS_Graph, "ITA Surplus.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
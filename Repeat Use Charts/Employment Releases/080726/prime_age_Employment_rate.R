pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

EPOP <-import("C:/Users/Joseph/Documents/SubStack/prime_age_employment_rate_monthly.csv")
EPOP <- subset(EPOP, FREQUENCY == "Q")
EPOP <- subset(EPOP, SUBJECT == "25_54")

EPOP$TIME <- gsub("Q1","01-01",EPOP$TIME) #changing quarters to dates 
EPOP$TIME <- gsub("Q2","04-01",EPOP$TIME)
EPOP$TIME <- gsub("Q3","07-01",EPOP$TIME)
EPOP$TIME <- gsub("Q4","010-01",EPOP$TIME)

EPOP$TIME <- as.Date(EPOP$TIME)

EPOP <- EPOP[EPOP$LOCATION %in% c("USA","JPN","FRA","G-7","DEU"), ] #tagging only relevant countries ,"OECD" EA19 #AUS G-7
EPOP <- EPOP[EPOP$TIME >= as.Date("1990-01-01"), ]

EPOP$LOCATION <- gsub("USA","United States",EPOP$LOCATION)
EPOP$LOCATION <- gsub("JPN","Japan",EPOP$LOCATION)
EPOP$LOCATION <- gsub("FRA","France",EPOP$LOCATION)
EPOP$LOCATION <- gsub("G-7","G7",EPOP$LOCATION)
EPOP$LOCATION <- gsub("DEU","Germany",EPOP$LOCATION)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Adding Prime Age for Anglophone Countries
EPOPUSA <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #USA Epop
EPOPNZ <- fredr(series_id = "LREM25TTNZQ156N",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #New Zealand Epop
EPOPCAN <- fredr(series_id = "LREM25TTCAM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #Canada Epop
EPOPAUS <- fredr(series_id = "LREM25TTAUM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #Aus EPop

#Prime Age Female for Selected Countries
EPOPFEMUSA <- fredr(series_id = "LREM25FEUSM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #USA Female Epop
EPOPFEMJPN <- fredr(series_id = "LREM25FEJPM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPFEMCAN <- fredr(series_id = "LREM25FECAM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPFEMAUS <- fredr(series_id = "LREM25FEAUM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)

#Prime Age Male for Selected Countries
EPOPMALUSA <- fredr(series_id = "LREM25MAUSM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #USA Female Epop
EPOPMALJPN <- fredr(series_id = "LREM25MAJPM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPMALCAN <- fredr(series_id = "LREM25MACAM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPMALAUS <- fredr(series_id = "LREM25MAAUM156S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)


EPOP_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=EPOP[EPOP$LOCATION != "United States",], aes(x=TIME, y=Value/100 , color= LOCATION), size = 1.25) +
  geom_line(data=EPOP[EPOP$LOCATION == "United States",], aes(x=TIME, y=Value/100 , color= LOCATION), size = 2) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.69,.875), breaks = c(.7,.75,.8,.85), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-07-01"))) +
  ylab("Prime Age 25-54 Employment-Population Ratio, %") +
  ggtitle("America's Great Employment Failure") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Employment Making New Non-Inflationary Peaks is Normal, but the US has Lagged Behind") +
  theme_apricitas + theme(legend.position = c(.30,.23)) +
  scale_color_manual(name= "Prime Age (25-54) Employment-Population Ratio",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"), breaks = c("United States","France","G7","Germany","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11790), xmax = as.Date("1990-01-01")-(0.049*11790), ymin = .69-(.3*.185), ymax = .69) +
  coord_cartesian(clip = "off")

EPOP_Anglo_Graph <- ggplot() + #Plotting Anglophone Employment Levels
  geom_line(data=EPOPNZ, aes(x=date,y= value/100,color= "New Zealand"), size = 1.25)+ 
  geom_line(data=EPOPCAN, aes(x=date,y= value/100,color= "Canada"), size = 1.25)+ 
  geom_line(data=EPOPAUS, aes(x=date,y= value/100,color= "Australia"), size = 1.25)+ 
  geom_line(data=EPOPUSA, aes(x=date,y= value/100,color= "USA"), size = 2)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.69,.875), breaks = c(.7,.75,.8,.85), expand = c(0,0)) +
  ylab("Prime Age 25-54 Employment-Population Ratio, %") +
  ggtitle("America's Great Employment Failure") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "The US has Lagged Significantly Behind its Closest Peer Nations") +
  theme_apricitas + theme(legend.position = c(.35,.80)) +
  scale_color_manual(name= "Prime Age (25-54) Employment-Population Ratio",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("USA","New Zealand","Canada","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11790), xmax = as.Date("1990-01-01")-(0.049*11790), ymin = .69-(.3*.185), ymax = .69) +
  coord_cartesian(clip = "off")

EPOP_Women_Graph <- ggplot() + #Plotting Female EPOP
  geom_line(data=EPOPFEMJPN, aes(x=date,y= value/100,color= "Japan"), size = 1.25)+ 
  geom_line(data=EPOPFEMCAN, aes(x=date,y= value/100,color= "Canada"), size = 1.25)+ 
  geom_line(data=EPOPFEMAUS, aes(x=date,y= value/100,color= "Australia"), size = 1.25)+ 
  geom_line(data=EPOPFEMUSA, aes(x=date,y= value/100,color= "USA"), size = 2)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.60,.82), breaks = c(.6,.65,.7,.75,.8), expand = c(0,0)) +
  ylab("Prime Age 25-54 Employment-Population Ratio, %") +
  ggtitle("America's Great Employment Failure") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "America Used to Have Among the Best Female Employment Numbers. It has Been Lapped.") +
  theme_apricitas + theme(legend.position = c(.35,.80)) +
  scale_color_manual(name= "Female Prime Age (25-54) Employment-Population Ratio",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("USA","Japan","Canada","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11790), xmax = as.Date("1990-01-01")-(0.049*11790), ymin = .60-(.3*.22), ymax = .60) +
  coord_cartesian(clip = "off")

EPOP_Men_Graph <- ggplot() + #Plotting Anglophone Employment Levels
  geom_line(data=EPOPMALJPN, aes(x=date,y= value/100,color= "Japan"), size = 1.25)+ 
  geom_line(data=EPOPMALCAN, aes(x=date,y= value/100,color= "Canada"), size = 1.25)+ 
  geom_line(data=EPOPMALAUS, aes(x=date,y= value/100,color= "Australia"), size = 1.25)+ 
  geom_line(data=EPOPMALUSA, aes(x=date,y= value/100,color= "USA"), size = 2)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.75,1.02), breaks = c(.75,.8,.85,.9,.95,1), expand = c(0,0)) +
  ylab("Prime Age 25-54 Employment-Population Ratio, %") +
  ggtitle("America's Great Employment Failure") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Many Countries Have Seen Drops in Male Employment Rates, But Few are as Bad as the US") +
  theme_apricitas + theme(legend.position = c(.67,.85)) +
  scale_color_manual(name= "Male Prime Age (25-54) Employment-Population Ratio",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("USA","Japan","Canada","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11790), xmax = as.Date("1990-01-01")-(0.049*11790), ymin = .75-(.3*.27), ymax = .75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_Graph, "EPOPbycountry.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = EPOP_Anglo_Graph, "EPOPAnglo.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = EPOP_Women_Graph, "EPOPWomen.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = EPOP_Men_Graph, "EPOPMen.png", type = "cairo-png") 

EPOPJPN <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPGER <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
EPOPUK <- fredr(series_id = "LREM25TTGBQ156S",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)


EPOP_PEER_Graph <- ggplot() + #Plotting Anglophone Employment Levels
  #geom_line(data=EPOPNZ, aes(x=date,y= value/100,color= "New Zealand"), size = 1.25)+ 
  geom_line(data=subset(EPOPAUS, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Australia"), size = 1.25)+ 
  geom_line(data=subset(EPOPUK, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "United Kingdom"), size = 1.25)+ 
  geom_line(data=subset(EPOPCAN, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Canada"), size = 1.25)+ 
  geom_line(data=subset(EPOPGER, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Germany"), size = 1.25)+ 
  geom_line(data=subset(EPOPJPN, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "Japan"), size = 1.25)+ 
  geom_line(data=subset(EPOPUSA, date >= as.Date("2000-01-01")), aes(x=date,y= value/100,color= "USA"), size = 2)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.695,.885), breaks = c(.7,.75,.8,.85), expand = c(0,0)) +
  ylab("Prime Age 25-54 Employment-Population Ratio, %") +
  ggtitle("America's Great Employment Failure") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "US Employment Rates have Lagged Significantly Behind its Closest Peer Nations") +
  theme_apricitas + theme(legend.position = c(.35,.83), legend.spacing.y = unit(0, 'cm'),legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +
  scale_color_manual(name= "Prime Age (25-54) Employment-Population Ratio",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("USA","Japan","Germany","Canada","United Kingdom","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = .695-(.3*.19), ymax = .695) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_PEER_Graph, "EPOPPEER.png", type = "cairo-png") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

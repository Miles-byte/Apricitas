pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)




PandemicLostWork <- data.frame(date = seq(as.Date("2020-05-01"), as.Date("2022-02-01"), "months"), value = c(48839,40368,31281,24225,19385,15070,14805,15819,14755,13348,11391,9378,7907,6209,5150,5647,5032,3830,3640,3101,6043,4201))
Telework <- data.frame(date = seq(as.Date("2020-05-01"), as.Date("2022-02-01"), "months"), value = c(48703,44644,38194,35800,33501,31954,32737,35501,34484,33839,31553,27643,25168,22004,20271,20562,20348,18052,17553,17358,23938,20399))

Initial_Claims_NSA_14 <- fredr(series_id = "ICNSA",observation_start = as.Date("2014-06-01"), observation_end = as.Date("2019-06-01"), realtime_end = NULL) #weekly initial claims data
Initial_Claims_NSA_19 <- fredr(series_id = "ICNSA",observation_start = as.Date("2020-01-01"),  realtime_end = NULL) #weekly initial claims data

Layoffs_TNSPT_WARE <- bls_api("JTU480099000000000LDL", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
Layoffs_TNSPT_WARE=Layoffs_TNSPT_WARE[order(nrow(Layoffs_TNSPT_WARE):1),]
Layoffs_TNSPT_WARE$date <- seq(as.Date("2017-01-01"), as.Date("2021-10-01"), "months")

Layoffs_RETAIL <- bls_api("JTU440000000000000LDL", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
Layoffs_RETAIL=Layoffs_RETAIL[order(nrow(Layoffs_RETAIL):1),]
Layoffs_RETAIL$date <- seq(as.Date("2017-01-01"), as.Date("2021-10-01"), "months")

Total_Layoffs <- bls_api("JTS000000000000000LDL", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

#have to split black epop into two separate dataframes because BLS API only allows 10 years of data at a time
Black_Epop1 <- bls_api("LNU02300066", startyear = 1994, endyear = 2013, Sys.getenv("BLS_KEY"))
Black_Epop2 <- bls_api("LNU02300066", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)

#binding black epops together and creating date
Black_Epop <- rbind(Black_Epop1,Black_Epop2) %>% mutate(period = gsub("M","",period)) %>% mutate(date = as.Date(as.yearmon(paste(year, period), "%Y %m")))

White_Epop1 <- bls_api("LNU02300063", startyear = 1994, endyear = 2013, Sys.getenv("BLS_KEY"))
White_Epop2 <- bls_api("LNU02300063", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)

White_Epop <- rbind(White_Epop1,White_Epop2) %>% mutate(period = gsub("M","",period)) %>% mutate(date = as.Date(as.yearmon(paste(year, period), "%Y %m")))

Black_White_Epop <- rbind(Black_Epop %>% mutate(race = "Black"),White_Epop %>% mutate(race = "White")) %>%
  select(value,date,race) %>%
  pivot_wider(names_from = race, values_from = value) %>%
  mutate(gap = White-Black)

EPOP_L_NSA=EPOP_L_NSA[order(nrow(EPOP_L_NSA):1),]
EPOP_L_NSA$date <- seq(as.Date("1994-01-01"), as.Date("2022-05-01"), "months")

PAYEMS <- fredr(series_id = "PAYEMS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Nonfarm Payrolls
ELEV <- fredr(series_id = "CE16OV",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Employment Levels
CPSADJ <- bls_api("LNS16000000", startyear = 2019) %>% #headline cpSadj
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


#note: this section is only for when FRED does not update, and the dates must be changed each month
#EPopBLS <- bls_api("LNS12300060", startyear = 2018, endyear = 2022, Sys.getenv("BLS_KEY")) #pulling most recent data from BLS API for EPOP
#EPop[nrow(EPop) + 1,] = list(as.Date("2021-12-01"),"X", EPopBLS$value[1], as.Date("2021-12-01"),as.Date("2021-10-01")) #adding new row with most recent data

PARTTIME <- fredr(series_id = "LNS12032194",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #Part Time For Economic Reasons Level

FOOD_EMP <- fredr(series_id = "CES7072200001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Food Service and Drinking, All Employees
FOOD_SALES <- fredr(series_id = "MRTSSM722USS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Retail Sales: Food Service and Drinking



Total_Quits <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2019-01-01")) #downloading quits data
Total_Quits18 <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2018-01-01")) #downloading quits data




#taking prime age epop for men and women. 
EPOP_MALE_1990 <- bls_api("LNS12300061", startyear = 1990) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_MALE_2000 <- bls_api("LNS12300061", startyear = 2000) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_MALE_2010 <- bls_api("LNS12300061", startyear = 2010) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_MALE_2020 <- bls_api("LNS12300061", startyear = 2020) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPOP_MALE <- rbind(EPOP_MALE_1990,EPOP_MALE_2000,EPOP_MALE_2010,EPOP_MALE_2020)

EPOP_FEMALE_1990 <- bls_api("LNS12300062", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_FEMALE_2010 <- bls_api("LNS12300062", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPOP_FEMALE <- rbind(EPOP_FEMALE_1990,EPOP_FEMALE_2010) %>%
  arrange(date)


#Flows data
UNEMPLOYEMPLOY <- fredr(series_id = c("LNU07100000"), observation_start = as.Date("2000-01-01")) #unemployment flows to employment
NILFEMPLOY <- fredr(series_id = c("LNU07200000"), observation_start = as.Date("2000-01-01")) #NILF to employment
MARGINALEMPLOY <- fredr(series_id = c("LNU07300000"), observation_start = as.Date("2000-01-01")) #marginal flows to employment

Flows_to_Employment <- rbind(UNEMPLOYEMPLOY,NILFEMPLOY,MARGINALEMPLOY)

Flows_to_Employment <- merge(UNEMPLOYEMPLOY,NILFEMPLOY, by = "date") %>%
  merge(MARGINALEMPLOY, by = "date")

Flows_to_Employment <- subset(Flows_to_Employment, select = c("date","value.x","value.y","value"))
colnames(Flows_to_Employment) <- c("date","Unemployment","Not in Labor Force","Marginal")

UNRATE <- fredr(series_id = c("UNRATE"), observation_start = as.Date("1950-01-01")) #unemployment rate

EPOP_RECOVERIES_1990 <- fredr(series_id = "LNS12300060", observation_start = as.Date("1990-02-01")) %>%
  mutate(month = seq(1:nrow(.))) %>%
  mutate(recovery = value - value[1]) %>%
  subset(date <= as.Date("1996-07-01"))

EPOP_RECOVERIES_2000 <- fredr(series_id = "LNS12300060", observation_start = as.Date("2000-04-01")) %>%
  mutate(month = seq(1:nrow(.))) %>%
  mutate(recovery = value - value[1]) %>%
  subset(date <= as.Date("2007-01-01"))

EPOP_RECOVERIES_2007 <- fredr(series_id = "LNS12300060", observation_start = as.Date("2007-01-01")) %>%
  mutate(month = seq(1:nrow(.))) %>%
  mutate(recovery = value - value[1]) %>%
  subset(date <= as.Date("2019-10-01"))

EPOP_RECOVERIES_2020 <- fredr(series_id = "LNS12300060", observation_start = as.Date("2020-01-01")) %>%
  mutate(month = seq(1:nrow(.))) %>%
  mutate(recovery = value - value[1]) %>%
  subset(date <= today())

RECOVERIES_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=EPOP_RECOVERIES_1990, aes(x=month,y= recovery/100,color= "1990 Recession"), size = 1.25)+ 
  geom_line(data=EPOP_RECOVERIES_2000, aes(x=month,y= recovery/100,color= "2000 Recession"), size = 1.25)+ 
  geom_line(data=EPOP_RECOVERIES_2007, aes(x=month,y= recovery/100,color= "2007 Recession"), size = 1.25)+ 
  geom_line(data=EPOP_RECOVERIES_2020, aes(x=month,y= recovery/100,color= "2020 Recession"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Months to Recover to Prior Peak") +
  ylab("Decline Compared to Prior Peak") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.10,-0.05,0), limits = c(-.12,0.01), expand = c(0,0)) +
  ggtitle("A Historic Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using BLS data via Employ America", subtitle = "US Employment Rates Have Recovered to Pre-Recession Levels in Record Time") +
  theme_apricitas + theme(legend.position = c(.53,.3)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Recovery in Prime-Age (25-54) Employment Rates by Recession",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#00A99D","#A7ACD9","#3083DC"), breaks = c("2020 Recession","2007 Recession","2000 Recession", "1990 Recession")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = 0-(.3*.27), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RECOVERIES_Graph, "Recoveries.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


Black_White_Employment_Graph <- ggplot() + #plotting black-white unemployment graph
  geom_line(data=Black_White_Epop, aes(x=date,y= gap/100,color= "Black-White Prime Age (25-54) Employment Gap (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.125),breaks = c(0,0.025,0.05,0.075,0.1,0.125), expand = c(0,0)) +
  ggtitle("A Stronger Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Black-White Employment Gap is at a Record Low") +
  theme_apricitas + theme(legend.position = c(.35,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*10410), xmax = as.Date("1994-01-01")-(0.049*10410), ymin = 0-(.3*.125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Black_White_Employment_Graph, "Black White Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Black_White_Epop_graph <- ggplot() + #plotting black-white unemployment graph
  geom_line(data=Black_White_Epop, aes(x=date,y= Black/100,color= "Black/African-American"), size = 1.25)+ 
  geom_line(data=Black_White_Epop, aes(x=date,y= White/100,color= "White"), size = 1.25)+ 
  #annotate(geom = "hline", y = 0.783, yintercept = .783, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Black/African American Employment Rates are at a Record High", x = as.Date("2012-06-01"), y = 0.79, color ="#FFE98F", size = 5) +
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.64,.85),breaks = c(.65,.70,.75,.80,.85), expand = c(0,0)) +
  ggtitle("25-54 Employment Rates (NSA)") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Black Employment Rates Near Record Highs, and White Employment Rates Have Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.25,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*10410), xmax = as.Date("1994-01-01")-(0.049*10410), ymin = .64-(.3*.21), ymax = .64) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Black_White_Epop_graph, "Black White Epop.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Male_Female_Epop <- ggplot() + #plotting black-white unemployment graph
  annotate(geom = "hline", y = 0.751, yintercept = .753, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Women's Employment Rates are at a Record High", x = as.Date("2007-06-01"), y = 0.76, color ="#FFE98F", size = 5) +
  geom_line(data=EPOP_FEMALE, aes(x=date,y= value/100,color= "Women"), size = 1.25)+ 
  geom_line(data=EPOP_MALE, aes(x=date,y= value/100,color= "Men"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.63,.90),breaks = c(.65,.70,.75,.80,.85,.90), expand = c(0,0)) +
  ggtitle("A Stronger Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Women's Employment Rates are at a Record High") +
  theme_apricitas + theme(legend.position = c(.30,.560)) +
  scale_color_manual(name= "Prime Age (25-54) Employment Population Ratio",breaks = c("Women","Men"),values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .63-(.3*.27), ymax = .63) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Male_Female_Epop, "Male Female Epop.png", type = "cairo-png",width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EPOP_FEMALE_GRAPH <- ggplot() + #plotting black-white unemployment graph
  annotate(geom = "hline", y = EPOP_FEMALE$value[nrow(EPOP_FEMALE)]/100, yintercept = EPOP_FEMALE$value[nrow(EPOP_FEMALE)]/100, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Women's Employment Rates are at a Record High", x = as.Date("2007-06-01"), y = EPOP_FEMALE$value[nrow(EPOP_FEMALE)]/100+0.005, color ="#FFE98F", size = 5) +
  geom_line(data=EPOP_FEMALE, aes(x=date,y= value/100,color= "Women's Prime Age (25-54) Employment Population Ratio"), size = 1.25)+ 
  #geom_line(data=EPOP_MALE, aes(x=date,y= value/100,color= "Men"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.63,.78),breaks = c(.65,.70,.75), expand = c(0,0)) +
  ggtitle("Record Women's Employment Rates") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Women's Employment Rates are at a Record High After a Strong Recovery") +
  theme_apricitas + theme(legend.position = c(.40,.360)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .63-(.3*.15), ymax = .63) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_FEMALE_GRAPH, "EPOP FEMALE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



#PERMJOBLOSERS <- fredr(series_id = c("LNS13026638"), observation_start = as.Date("2000-01-01")) #permanent job losers
#LAYOFFJOBLOSERS <- fredr(series_id = c("LNS13023653"), observation_start = as.Date("2000-01-01")) #temporary job losers

PERMJOBLOSERS <- bls_api("LNS13026638", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("LNS13026638", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

LAYOFFJOBLOSERS <- bls_api("LNS13023653", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("LNS13023653", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

PERM_TEMP_JOBLOSS_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=PERMJOBLOSERS, aes(x=date,y= value/1000,color= "Permanent Job Losers"), size = 1.25)+ 
  geom_line(data=LAYOFFJOBLOSERS, aes(x=date,y= value/1000,color= "Job Losers on Layoff"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,5,10,15,20), limits = c(0,20), expand = c(0,0)) +
  ggtitle("Rapid Rebound") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Policymakers Helped End a 2001-Size Jump in Permanent Job Losses") +
  theme_apricitas + theme(legend.position = c(.40,.87)) +
  scale_color_manual(name= "Unemployment Level",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*20), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PERM_TEMP_JOBLOSS_Graph, "Permanent V Temporary Job Loss.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#UNRATE <- fredr(series_id = c("UNRATE"), observation_start = as.Date("1950-01-01")) #unemployment rate


UNRATE_1950 <- bls_api("LNS14000000", startyear = 1950, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("LNS14000000", startyear = 1970, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(., bls_api("LNS14000000", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(., bls_api("LNS14000000", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)


UNRATE_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNRATE_1950, aes(x=date,y= value/100,color= "Unemployment Rate"), size = 1.25) + 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.15),breaks = c(0,0.05,0.1,0.15), expand = c(0,0)) +
  ggtitle("Back in Business") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Unemployment Rate is Near Historic Lows") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26420), xmax = as.Date("1950-01-01")-(0.049*26420), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNRATE_Graph, "UNRATE graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


Flows_to_Employment_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=Flows_to_Employment, aes(x=date,y= Unemployment/(Unemployment +`Not in Labor Force` + Marginal),color= "Unemployment"), size = 1.25)+ 
  geom_line(data=Flows_to_Employment, aes(x=date,y= `Not in Labor Force`/(Unemployment +`Not in Labor Force` + Marginal),color= "Not in Labor Force"), size = 1.25)+ 
  geom_line(data=Flows_to_Employment, aes(x=date,y= Marginal/(Unemployment +`Not in Labor Force` + Marginal),color= "Marginal"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), expand = c(0,0)) +
  ggtitle("Workers in Waiting") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Vast Majority of New Workers Come from Outside the Labor Force, not Unemployment") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= "Share of Flows Into Employment From:",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Not in Labor Force","Unemployment","Marginal")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8158), xmax = as.Date("2000-01-01")-(0.049*8158), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

# UNLEVEL <- fredr(series_id = c("UNEMPLOY"), observation_start = as.Date("2019-01-01")) #unemployment data
# NILFWJN <- fredr(series_id = c("NILFWJN"), observation_start = as.Date("2019-01-01")) #NILF want jobs now

UNLEVEL <- bls_api("LNS13000000", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  filter(date >= as.Date("2022-01-01"))

NILFWJN <- bls_api("LNS15026639", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  filter(date >= as.Date("2022-01-01"))


NILFUnemploy_Graph <- ggplot() + #plotting total quits
  geom_line(data=UNLEVEL, aes(x=date,y= value/1000,color= "Unemployed"), size = 1.25)+ 
  geom_line(data=NILFWJN, aes(x=date,y= value/1000,color= "Not in Labor Force, Want a Job Now"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(5,6,7,8), limits = c(4.5,7.5), expand = c(0,0)) +
  ggtitle("Unemployment is Slowly Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Unemployment is Slowly Picking Up from Its 2022/2023 Lows") +
  theme_apricitas + theme(legend.position = c(.30,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Unemployed","Not in Labor Force, Want a Job Now")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 4.5-(.3*3), ymax = 4.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NILFUnemploy_Graph, "Not In Labor Force vs Unemployment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#NILFWJN_2002 <- fredr(series_id = c("NILFWJN"), observation_start = as.Date("2002-01-01")) #NILF want jobs now
NILFWJN_2002 <- bls_api("LNS15026639", startyear = 2002, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("LNS15026639", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)


NILF_Graph <- ggplot() + #plotting total quits
  geom_line(data=NILFWJN_2002, aes(x=date,y= value/1000,color= "Not in Labor Force, Want a Job Now"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,2,4,6,8,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("Workers in Waiting") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "About 1M More People Are Outside the Labor Force But Want A Job Now Than Pre-Pandemic") +
  theme_apricitas + theme(legend.position = c(.30,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NILF_Graph, "NILF 2002.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#WAREHOUSE <- fredr(series_id = "CES4349300001",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)

WAREHOUSE <- bls_api("CES4349300001", startyear = 2017, registrationKey = Sys.getenv("BLS_KEY")) %>%
  #rbind(., bls_api("LNS15026639", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

WAREHOUSE_Graph <- ggplot() + #plotting total quits
  geom_line(data=WAREHOUSE, aes(x=date,y= value/1000,color= "All Employees, Warehousing and Storage"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(0.5,1,1.5,2), limits = c(0.5,2), expand = c(0,0)) +
  ggtitle("Total Fulfillment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Warehousing Employment Growth Has Stalled This Year") +
  theme_apricitas + theme(legend.position = c(.40,.17)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0.5-(.3*1.5), ymax = 0.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WAREHOUSE_Graph, "WareHouse.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

DISCOURAGED <- bls_api("LNS15026645", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

MARGINALLYATTACHED <- bls_api("LNS15026642", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


MARGINAL_DISCOURAGED_GRAPH <- ggplot() + #plotting total quits
  geom_line(data=MARGINALLYATTACHED, aes(x=date,y= value/1000,color= "Not in Labor Force, Marginally Attached"), size = 1.25)+ 
  geom_line(data=DISCOURAGED, aes(x=date,y= value/1000,color= "Not in Labor Force, Marginally Attached, Discouraged Workers"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3), limits = c(0,3), expand = c(0,0)) +
  ggtitle("Workers in Waiting") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Number of Discouraged and Marginally Attached Workers is Near Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.40,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MARGINAL_DISCOURAGED_GRAPH, "Marginal Discouraged.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


Total_Quits_Graph <- ggplot() + #plotting total quits
  geom_line(data=Total_Quits, aes(x=date,y= value/1000,color= "Quits, Total Nonfarm"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(2,3,4,5), limits = c(2,5), expand = c(0,0)) +
  ggtitle("The Great Reshuffling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "A Record Number of Americans are Quitting Their Jobs") +
  theme_apricitas + theme(legend.position = c(.65,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*960), xmax = as.Date("2019-01-01")-(0.049*960), ymin = 2-(.3*3), ymax = 2) +
  coord_cartesian(clip = "off")

Total_Quits_Layoffs_Graph <- ggplot() + #plotting total quits and layoffs
  geom_line(data=Total_Quits18, aes(x=date,y= value/1000,color= "Quits, Total Nonfarm"), size = 1.25)+ 
  geom_line(data=Total_Layoffs, aes(x=date,y= value/1000,color= "Layoffs and Discharges, Total Nonfarm"), size = 1.25)+
  annotate(geom = "text", label = "Note: Discontinuity at March 2020, When Layoffs hit 13M", x = as.Date("2020-01-01"), y = 1.15, color ="white", size = 4, alpha = 1) +
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3,4,5), limits = c(0,5), expand = c(0,0)) +
  ggtitle("The End of The Great Reshuffling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Number of Quits is Coming Down From Record Highs, as Layoffs Have Risen a Bit") +
  theme_apricitas + theme(legend.position = c(.26,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Quits, Total Nonfarm","Layoffs and Discharges, Total Nonfarm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

#UNRATEWhite <- fredr(series_id = "LNS14000003",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#UNRATEBlack <- fredr(series_id = "LNS14000006",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#UNRATEHispanic <- fredr(series_id = "LNS14000009",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#UNRATEAsian <- fredr(series_id = "LNU04032183",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

UNRATEWhite <- bls_api("LNS14000003", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
UNRATEBlack <- bls_api("LNS14000006", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
UNRATEHispanic <- bls_api("LNS14000009", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
UNRATEAsian <- bls_api("LNU04032183", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


Race_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNRATEWhite, aes(x=date,y= value/100,color= "Unemployment Rate - White"), size = 1.25)+ 
  geom_line(data=UNRATEBlack, aes(x=date,y= value/100,color= "Unemployment Rate - Black or African American"), size = 1.25)+ 
  geom_line(data=UNRATEHispanic, aes(x=date,y= value/100,color= "Unemployment Rate - Hispanic or Latino"), size = 1.25)+ 
  geom_line(data=UNRATEAsian, aes(x=date,y= value/100,color= "Unemployment Rate - Asian"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.22), expand = c(0,0)) +
  ggtitle("Unemployment Rates by Race/Ethnicity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Black Unemployment Rate Has Risen Significantly Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.75,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Unemployment Rate - White","Unemployment Rate - Asian","Unemployment Rate - Hispanic or Latino","Unemployment Rate - Black or African American")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*.22), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Race_Graph, "Race.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#UNRATETeens <- fredr(series_id = "LNS14000012",observation_start = as.Date("1945-01-01"),realtime_start = NULL, realtime_end = NULL) 

#DO 1945 START DATE


UNRATETeens <- bls_api("LNS14000012", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

Teens_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNRATETeens, aes(x=date,y= value/100,color= "Unemployment Rate - 16-19 Yrs"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.35), expand = c(0,0)) +
  ggtitle("Back in Action") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Unemployment Rate for Teenagers is at its Lowest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1948-01-01")-(.1861*27060), xmax = as.Date("1948-01-01")-(0.049*27060), ymin = 0.0-(.3*0.35), ymax = 0.0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Teens_Graph, "Teens.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#LessThanHS <- fredr(series_id = "LNS14027659",observation_start = as.Date("1992-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

LessThanHS <- bls_api("LNS14027659", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

LessThanHS_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=LessThanHS, aes(x=date,y= value/100,color= "Unemployment Rate - Less Than a High School Diploma, 25 Yrs. & Over"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.225), expand = c(0,0)) +
  ggtitle("Back in Action") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Unemployment for Workers Without A High School Diploma is at Record Lows") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*.225), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LessThanHS_Graph, "Less than HS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


LessThanHS_1992 <- bls_api("LNS14027659", startyear = 1992, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("LNS14027659", startyear = 2012, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

LessThanHS1992_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=LessThanHS_1992, aes(x=date,y= value/100,color= "Unemployment Rate - Less Than a High School Diploma, 25 Yrs. & Over"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.225), expand = c(0,0)) +
  ggtitle("Back in Action") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Unemployment for Workers Without A High School Diploma is at Record Lows") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*(today()-as.Date("1992-01-01"))), xmax = as.Date("1992-01-01")-(0.049*(today()-as.Date("1992-01-01"))), ymin = 0-(.3*.225), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LessThanHS_1992, "Less than HS 1992.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



OwnIllnessNoWork <- bls_api("LNU02006735", startyear = 2018) %>% #LessthanHS
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

OwnIllnessPartTime <- bls_api("LNU02028296", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

OwnIllness_Graph <- ggplot() + #plotting the number of people out due to illnesses
  geom_line(data=OwnIllnessNoWork, aes(x=date,y= value/1000,color= "Employed But Not At Work, Own Illness"), size = 1.25)+ 
  geom_line(data=OwnIllnessPartTime, aes(x=date,y= value/1000,color= "Work Part-time, Usually Work Full Time, Own Illness"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,4.5), expand = c(0,0)) +
  ggtitle("COVID and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Number of People Out Sick From Work Has Fallen Dramatically") +
  theme_apricitas + theme(legend.position = c(.34,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*4.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = OwnIllness_Graph, "OwnIllness.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


UnpaidAbsences <- bls_api("LNU02044495", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


UnpaidAbsences_Graph <- ggplot() + #plotting the number of people out due to illnesses
  geom_line(data=UnpaidAbsences, aes(x=date,y= value/1000,color= "Wage and Salary Workers with Unpaid Absences"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,7), expand = c(0,0)) +
  ggtitle("Omicron and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Some Unpaid Absences-Which Spiked in January-Represent Misclassified Unemployed Workers") +
  theme_apricitas + theme(legend.position = c(.42,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UnpaidAbsences_Graph, "Unpaid Absences.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#DATE SECTION MUST BE UPDATED EACH MONTH
PandemicLostWork_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=PandemicLostWork, aes(x=date,y= value/1000,color= "Unable to Work, Employer Closed or Lost Business Due to the Pandemic"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,17), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2022-02-01"))) +
  ggtitle("Omicron and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Omicron Interrupted Steady Progress in Reducing Pandemic-Driven Unemployment") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*365), xmax = as.Date("2021-01-01")-(0.049*365), ymin = 0-(.3*17), ymax = 0) +
  coord_cartesian(clip = "off")
#DATE SECTION MUST BE UPDATED EACH MONTH
PandemicTelework_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=Telework, aes(x=date,y= value/1000,color= "Persons Who Teleworked Because of the Coronavirus Pandemic"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,40), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2021-01-01"),as.Date("2022-02-01"))) +
  ggtitle("Omicron and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Omicron Drove Many Americans Back into Telework") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*365), xmax = as.Date("2021-01-01")-(0.049*365), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

#ADD TELEWORK CHART

EPOP55Plus <- bls_api("LNS12324230", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


EPop55Plus_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=EPOP55Plus, aes(x=date,y= value/100,color= "55 yrs and Over Employment-Population Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("55yrs & Over Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.33,.4), expand = c(0,0)) +
  ggtitle("Methodological Mixup", subtitle = "Old Age Employment Is Higher, But the January Jump is Driven by Population Adjustments") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.68,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = .33-(.3*.07), ymax = .33) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPop55Plus_Graph, "Epop 55.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PerformingArts <- fredr(series_id = "CES7071100001",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees
MotionPictures <- fredr(series_id = "CES5051200001",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

PerformingArts <- bls_api("CES7071100001", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

MotionPictures <- bls_api("CES5051200001", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

Pictures_Performing_Graph <- ggplot() + #plotting spectator sports vs motion pictures
  geom_line(data=PerformingArts, aes(x=date,y= value/value[66]*100,color= "All Employees, Performing Arts and Spectator Sports"), size = 1.25)+ 
  geom_line(data=MotionPictures, aes(x=date,y= value/value[66]*100,color= "All Employees, Motion Picture and Sound Recording Industries"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(), limits = c(40,140), expand = c(0,0)) +
  ggtitle("Broadway and Hollywood") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Media Employment has Recovered, While in-person Performing Arts Employment Struggles") +
  theme_apricitas + theme(legend.position = c(.42,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 40-(.3*100), ymax = 40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Pictures_Performing_Graph, "Pictures Performing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Industry_Layoffs_Graph <- ggplot() + #plotting layoffs and discharges by industry
  geom_line(data=Layoffs_RETAIL, aes(x=date,y= value,color= "Retail Trade"), size = 1.25)+ 
  geom_line(data=Layoffs_TNSPT_WARE, aes(x=date,y= value,color= "Transportation, Warehousing, and Utilities"), size = 1.25)+ 
  xlab("Date") +
  ylab("Thousands") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,550), expand = c(0,0)) +
  ggtitle("Bucking the Trend?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Big Question: Will 2022 See Fewer Seasonal Layoffs than Normal?") +
  theme_apricitas + theme(legend.position = c(.52,.87)) +
  scale_color_manual(name= "Layoffs and Discharges",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*1460), xmax = as.Date("2017-01-01")-(0.049*1460), ymin = 0-(.3*550), ymax = 0) +
  coord_cartesian(clip = "off")


Childcare <- bls_api("LNU02096055", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

Childcare_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=Childcare, aes(x=date,y= value,color= "Employed But Not At Work, Childcare Problems"), size = 1.25)+ 
  xlab("Date") +
  ylab("Thousands") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,120), expand = c(0,0)) +
  ggtitle("The Childcare Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Chilcare Issues Interfered With People's Ability to Work During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.32,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Childcare_Graph, "Childcare.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Total_Layoffs_Graph <- ggplot() + #plotting total discharges
  geom_line(data=Total_Layoffs, aes(x=date,y= value/1000,color= "Layoffs and Discharges, Total Nonfarm"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3), limits = c(0,3), expand = c(0,0)) +
  ggtitle("The Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Layoffs and Discharges Have Hit Record Lows") +
  theme_apricitas + theme(legend.position = c(.55,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "text", label = "Note: Discontinuity at March 2020, When Layoffs hit 13M", x = as.Date("2019-06-01"), y = 1.525, color ="white", size = 4, alpha = 0.75) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1295), xmax = as.Date("2018-01-01")-(0.049*1395), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")

EPOP_L_SA <- bls_api("LNS12000060", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

EPOP_L_NSA <- bls_api("LNU02000060", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

EPOP_SA_NSA_Graph <- ggplot() + #plotting total discharges
  geom_line(data=EPOP_L_NSA, aes(x=date,y= value/1000,color= "Prime Age (25-54) Employment-Population Level, NSA"), size = 1.25)+ 
  geom_line(data=EPOP_L_SA, aes(x=date,y= value/1000,color= "Prime Age (25-54) Employment-Population Level, SA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(85,90,95,100,105,110), limits = c(85,110), expand = c(0,0)) +
  ggtitle("Reason for the Season") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Seasonally Adjusted Employment Levels are About 700k Below Non-Seasonally Adjusted Levels") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*25), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_SA_NSA_Graph, "EPOP NSA SA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#EPop <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data

EPop1 <- bls_api("LNS12300060", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

EPop2 <- bls_api("LNS12300060", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

EPop <- rbind(EPop1,EPop2)


EPop_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=EPop, aes(x=date,y= value/100,color= "Prime Age (25-54) Employment-Population Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Still Below Full Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.68,.88)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("2000-06-01"), y = 0.825, color ="#FFE98F", size = 5) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0.69-(.3*0.14), ymax = 0.69) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPop_Graph, "EPopUSA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#LAH <- fredr(series_id = "USLAH",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data

LAH <- bls_api("CES7000000001", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

LAH_Graph <- ggplot() + #plotting leisure and hospitality employment
  geom_line(data=LAH, aes(x=date,y= value/1000,color= "All Employees, Leisure and Hospitality"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(8,12,16), limits = c(8,18), expand = c(0,0)) +
  ggtitle("Travel Blues") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Leisure & Hospitality is Lagging Behind") +
  theme_apricitas + theme(legend.position = c(.75,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 8-(.3*10), ymax = 8) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LAH_Graph, "LAH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

U1RATE <- fredr(series_id = "U1RATE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data

U1RATE <- bls_api("LNS13025670", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


U1RATE_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=U1RATE, aes(x=date,y= value/100,color= "Unemployed 15 Weeks and Over, % of Labor Force"), size = 1.25)+ 
  xlab("Date") +
  ylab("Civilian Labor Force Unemployed 15 Weeks and Over, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.0,0.055), expand = c(0,0)) +
  ggtitle("Long-Term Unemployment is Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Long Term Unemployment is Low, but Has Been Increasing in 2023") +
  theme_apricitas + theme(legend.position = c(.5,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*0.055), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = U1RATE_Graph, "U1RATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


PARTTIME <- bls_api("LNS12032194", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("LNS12032194", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

PARTTIME_Graph <- ggplot() + #plotting employed part time for economic reasons 
  geom_line(data=PARTTIME, aes(x=date,y= value/1000,color= "Employed Part-time for Economic Reasons"), size = 1.25)+ 
  xlab("Date") +
  ylab("Thousands of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1),limits = c(0,12.5), expand = c(0,0)) + #paste(c(0, 2.5, 5,7.5, 10,12.5), "M")
  ggtitle("Going Full Time") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Underemployed Population is Decreasing") +
  theme_apricitas + theme(legend.position = c(.60,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*12.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PARTTIME_Graph, "Part Time for Economic Reasons.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#LGOVED <- fredr(series_id = "CES9093161101",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Local Government Education Data

LGOVED <- bls_api("CES9093161101", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

LGOVED_Graph <- ggplot() + #plotting local government education employment
  geom_line(data=LGOVED, aes(x=date,y= value/1000,color= "All Employees, Local Government Education"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(7,7.5,8,8.5), limits = c(7,8.5), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-10-01"))) +
  ggtitle("Back to School?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Local Government Education Employment Still Lags") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 7-(.3*1.5), ymax = 7) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LGOVED_Graph, "LGOVED.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#FOODSERV <- fredr(series_id = "CES7072000001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Accomodation and Food Service, All Employees

FOODSERV <- bls_api("CES7072000001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOODSERV_Graph <- ggplot() + #plotting food service and accommodation employment
  geom_line(data=FOODSERV, aes(x=date,y= value/1000,color= "All Employees, Accommodation and Food Services"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(7.5,10,12.5,15), limits = c(7,15.5), expand = c(0,0)) +
  ggtitle("The Big Takeaway") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Food Service Employment is Stagnating") +
  theme_apricitas + theme(legend.position = c(.65,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 7.5-(.3*7.5), ymax = 7.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FOODSERV_Graph, "FOODSERV.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

TRNSPT <- fredr(series_id = "CES4300000001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Transportation and Warehousing, All Employees

TRNSPT <- bls_api("CES4300000001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

TRNSPT_Graph <- ggplot() + #plotting transportation and warehousing
  geom_line(data=TRNSPT, aes(x=date,y= value/1000,color= "All Employees, Transportation and Warehousing"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(5,5.5,6,6.5,7), limits = c(5,7), expand = c(0,0)) +
  ggtitle("The Goods Boom") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Transportation and Warehousing has Increased as Americans Buy More Goods") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 5-(.3*2), ymax = 5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TRNSPT_Graph, "Transport.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#NURSING <- fredr(series_id = "CES6562310001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Accomodation and Food Service, All Employees

NURSING <- bls_api("CES6562310001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

Nursing_Graph <- ggplot() + #plotting transportation and warehousing
  geom_line(data=NURSING, aes(x=date,y= value/1000,color= "All Employees, Nursing Care Facilities"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(1.3,1.4,1.5,1.6,1.7), limits = c(1.3,1.7), expand = c(0,0)) +
  ggtitle("Morbid Realities") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Passing of Many Nursing Home Residents Has Reduced Employment in the Sector") +
  theme_apricitas + theme(legend.position = c(.65,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 1.3-(.3*0.4), ymax = 1.3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Nursing_Graph, "Nursing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FOODSERV_REVENUE_Graph <- ggplot() + #plotting food service and drinking places employment against retail sales
  geom_line(data=FOOD_EMP, aes(x=date,y= value/120.12,color= "All Employees, Food Services and Drinking Places"), size = 1.25)+ 
  geom_line(data=FOOD_SALES, aes(x=date,y= value/619.37,color= "Retail Sales, Food Services and Drinking Places"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2019 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(60,80,100,120), limits = c(45,125), expand = c(0,0)) +
  ggtitle("The Big Takeaway") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Food Service Employment is Stagnating Even As Sales Rebound") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 45-(.3*80), ymax = 45) +
  coord_cartesian(clip = "off")

#ARTS <- fredr(series_id = "CES7071000001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Arts,Entertainment, and Recreation, All Employees

ARTS <- bls_api("CES7071000001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


ARTS_Graph <- ggplot() + #plotting arts employment
  geom_line(data=ARTS, aes(x=date,y= value/1000,color= "All Employees, Arts, Entertainment, and Recreation"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(1,1.5,2,2.5,3), limits = c(1,3), expand = c(0,0)) +
  ggtitle("Hey, the Big Artiste...") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in the Arts is Rebounding, but Still Remains Below Pre-Pandemic Highs") +
  theme_apricitas + theme(legend.position = c(.6,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 1-(.3*2), ymax = 1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ARTS_Graph, "Arts.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#COURIERS <- fredr(series_id = "CES4349200001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

COURIERS <- bls_api("CES4349200001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

Couriers_Graph <- ggplot() + #plotting couriers and messengers employment
  geom_line(data=COURIERS, aes(x=date,y= value/1000,color= "All Employees, Couriers and Messengers"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(.8,.9,1,1.1,1.2), limits = c(.75,1.25), expand = c(0,0)) +
  ggtitle("Premium Rush") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment for Couriers, Which Includes Food Delivery and Short-Haul Trucking, is Booming") +
  theme_apricitas + theme(legend.position = c(.6,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = .75-(.3*.5), ymax = .75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Couriers_Graph, "Couriers.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#PWD <- fredr(series_id = "LNU02374597",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

PWD <- bls_api("LNU02374597", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

PWD_Graph <- ggplot() + #plotting employment of people with disabilities
  geom_line(data=PWD, aes(x=date,y= value/100,color= "Employment-Population Ratio - With a Disability, 16 Years and Over"), size = 1.25)+ 
  xlab("Date") +
  ylab("Employment-Population Ratio, Percent") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(.15,.175,.20,.225,.25), limits = c(.15,.25), expand = c(0,0)) +
  ggtitle("Employment Rate, People With Disabilities") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment Rates for People With Disabilities are Up") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today() - as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today() - as.Date("2019-01-01"))), ymin = .15-(.3*.1), ymax = .15) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PWD_Graph, "People With Disabilities.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Employment_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = PAYEMS, aes(x=date, y = value/1521.28, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = ELEV, aes(x=date, y = value/1586.53, color = "Employment Level (Household Survey)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(82,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 82-(.3*23), ymax = 82) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Employment_Graph <- ggplot() + #CPS with NFP adjusted concepts
  geom_line(data = PAYEMS, aes(x=date, y = value/1000, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = CPSADJ, aes(x=date, y = value/1000, color = "Employment Ajusted to Nonfarm Payrolls Concepts (Household Survey)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(120,160), breaks = c(120,130,140,150,160), expand = c(0,0)) +
  ylab("Payrolls/Employees, Millions") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 120-(.3*40), ymax = 120) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ECI_WAG <- bls_api("CIS2020000000000I", startyear = 2006, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  mutate(qoqpctann = ((1+(value-dplyr::lead(value, 1))/dplyr::lead(value, 1))^4)-1) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2006-01-01"), length = nrow(.), by = "quarter")))

ECI_WAG_EX_INC <- bls_api("CIU2020000000710I", startyear = 2006, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  mutate(qoqpctann = ((1+(value-dplyr::lead(value, 1))/dplyr::lead(value, 1))^4)-1) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2006-01-01"), length = nrow(.), by = "quarter")))

ECI_WAG_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=ECI_WAG, aes(x=date,y= qoqpctann ,color= "Quarter-on-Quarter Percent Growth, Annualized"), size = 1.25) +
  geom_line(data=ECI_WAG, aes(x=date,y= annualpct ,color= "Annualized Percent Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07), breaks = c(0,0.03,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Core Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "ECI, Private Industry Wages and Salaries Growth Was in Line With Expectations") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= "ECI Private Sector Wages and Salaries",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECI_WAG_Graph, "ECI WAG.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ECI_WAG_Ex_Inc_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=ECI_WAG_EX_INC, aes(x=date,y= annualpct ,color= "Annualized Percent Growth"), size = 1.25) +
  geom_line(data=ECI_WAG_EX_INC, aes(x=date,y= qoqpctann ,color= "Quarter-on-Quarter Percent Growth, Annualized (NSA)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07), breaks = c(0,0.03,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Core Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "ECI, Private Industry Wages and Salaries Growth Was in Line With Expectations") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "ECI Private Sector Wages and Salaries Excluding Incentive Paid",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECI_WAG_Ex_Inc_Graph, "ECI WAG ex Inc.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


GLI_BEA <- fredr(series_id = "A132RC1",observation_start = as.Date("2018-01-01")) #downloading "Compensation of Employees, Received" data from Fred to calculate Gross Labor Income
ECIPRIVWAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2017-10-01")) %>%
  mutate(date = date %m+% months(3)) %>%
  select(date, value)#downloading "Wages and Salaries: Private Industry Workers" data from Fred to calculate Gross Labor Income using a third method

ECIPRIVWAG_Monthly <- seq(ECIPRIVWAG$date[1], tail(ECIPRIVWAG$date,1), by="month")
ECIPRIVWAG_Monthly <- data.frame(date=ECIPRIVWAG_Monthly, value=spline(ECIPRIVWAG, method="fmm", xout=ECIPRIVWAG_Monthly)$y)

ELEV_PRIVATE <- fredr(series_id = "LNS12032189",observation_start = as.Date("2017-10-01"), frequency = "m", aggregation_method = "eop") #%>%
#mutate(date = date %m+% months(3))#downloading "Employment Level - 25-54 Yrs" data from Fred to calculate Gross Labor Income using a second method
GLI_BLS <- fredr(series_id = "CES0500000017",observation_start = as.Date("2018-01-01")) #downloading "All Employees, Total Nonfarm" data from Fred to calculate Gross Labor Income using a third method

GLI_CPS_NCS <- merge(ECIPRIVWAG_Monthly,ELEV_PRIVATE, by = "date") #merging ECI and EPOP data for the second GLI calculation method
GLI_CPS_NCS <- subset(GLI_CPS_NCS, select = c("date","value.x","value.y")) #cleaning up data frame
colnames(GLI_CPS_NCS) <- c("date","ECI","ELEV") #renaming columns for ease of use

GLITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(GLI_BLS$date, n=1), "months")), trend = 100*1.004074^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(GLI_BLS$date, n=1), by = 'month')) - 1))) #trend variable is just compounding income/outlays monthly at a 4% annual rate 

GLI_Graph <- ggplot() +
  geom_line(data = GLI_BEA, aes(x=date, y = value/8144.8*100, color = "Nominal Private Sector Gross Labor Income: BEA Method"), size = 1.25) + 
  geom_line(data = GLI_CPS_NCS, aes(x=date, y = ((ECI*ELEV)/17488899)*100, color = "Nominal Private Sector Gross Labor Income: ECI Method"), size = 1.25) + 
  geom_line(data = GLI_BLS, aes(x=date, y = value/151.4*100, color = "Nominal Private Sector Gross Labor Income: NFP Method"), size = 1.25) +
  geom_line(data = GLITrend, aes(x=date, y = trend, color = "Pre-Covid 5% Annual GLI Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(80,85,90,95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index, January 2020 = 100") +
  ggtitle("Gross Labor Income") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Gross Labor Income is Likely Slightly Above Trend, as is Growth") +
  theme_apricitas + theme(legend.position = c(.37,.88)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,1,2), lwd = c(1.25,1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 80-(.3*45), ymax = 80) +
  coord_cartesian(clip = "off")

#all employees residential building
#RESIDENTIAL_BUILDING <- fredr(series_id = "CES2023610001",observation_start = as.Date("1998-01-01"),realtime_start = NULL, realtime_end = NULL)

RESIDENTIAL_BUILDING <- bls_api("CES2023610001", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(., bls_api("CES2023610001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

RESIDENTIAL_BUILDING_Graph <- ggplot() + #plotting local government education employment
  geom_line(data=RESIDENTIAL_BUILDING, aes(x=date,y= value/1000,color= "All Employees, Residential Building"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(0.5,0.6,0.7,0.8,0.9,1,1.1), limits = c(0.5,1.1), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-10-01"))) +
  ggtitle("The Building Cycle") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Residential Construction Employment is Stalling, but not Falling Yet") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1998-10-01")-(.1861*(today()-as.Date("1998-10-01"))), xmax = as.Date("1998-10-01")-(0.049*(today()-as.Date("1998-01-01"))), ymin = 0.5-(.3*0.6), ymax = 0.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RESIDENTIAL_BUILDING_Graph, "Residential Building Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UNRATE1 <- bls_api("LNS13000000", startyear = 1995, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

UNRATE2 <- bls_api("LNS13000000", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

UNRATE <- rbind(UNRATE1,UNRATE2) %>%
  mutate(name = "Unemployed")

NILF1 <- bls_api("LNS15026639", startyear = 1995, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

NILF2 <- bls_api("LNS15026639", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

NILF <- rbind(NILF1,NILF2) %>%
  mutate(name = "Not in Labor Force but Want a Job Now")

PARTTIME1 <- bls_api("LNS12032194", startyear = 1995, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

PARTTIME2 <- bls_api("LNS12032194", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

PARTTIME <- rbind(PARTTIME1,PARTTIME2) %>%
  mutate(name = "Part Time for Economic Reasons")

LABOR_FORCE1 <- bls_api("LNS11000000", startyear = 1995, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

LABOR_FORCE2 <- bls_api("LNS11000000", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value)

LABOR_FORCE <- rbind(LABOR_FORCE1,LABOR_FORCE2) %>%
  mutate(name = "Civilian Labor Force")

UNDEREMPLOY <- rbind(UNRATE,NILF,PARTTIME,LABOR_FORCE) %>%
  #select(-series_id,-realtime_start,-realtime_end) %>%
  pivot_wider() %>%
  mutate(Unemployed = Unemployed/(`Civilian Labor Force`+`Not in Labor Force but Want a Job Now`)) %>%
  mutate(`Not in Labor Force but Want a Job Now` = `Not in Labor Force but Want a Job Now`/(`Civilian Labor Force`+`Not in Labor Force but Want a Job Now`)) %>%
  mutate(`Part Time for Economic Reasons` = `Part Time for Economic Reasons`/(`Civilian Labor Force`+`Not in Labor Force but Want a Job Now`)) %>%
  select(-`Civilian Labor Force`) %>%
  #mutate(Aggregate = Unemployed + `Part Time for Economic Reasons` + `Not in Labor Force but Want a Job Now`) %>%
  pivot_longer(cols = Unemployed:`Part Time for Economic Reasons`)

UNDEREMPLOY_TOTAL <- UNDEREMPLOY %>%
  group_by(date) %>%
  summarise(value = sum(value))


UNDEREMPLOY_Graph <- ggplot(data = UNDEREMPLOY, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Labor Force Plus and All Who Want a Job Now") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.05,.1,.15,.2,.25), limits = c(0,.27), expand = c(0,0)) +
  ggtitle("Un and Under Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Looking at Underemployment Gives a Better Picture of the Labor Market") +
  theme_apricitas + theme(legend.position = c(.43,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#00A99D","#A7ACD9","#3083DC"), breaks = c("Unemployed","Part Time for Economic Reasons","Not in Labor Force but Want a Job Now")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = 0-(.3*.27), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNDEREMPLOY_Graph, "Underemploy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


EMPLOY_TRADE_TRANSP_UTIL <- bls_api("CEU4000000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  select(date, value) %>%
  mutate(series_id = "Trade, Transportation, and Utilities") %>%
  drop_na()

EMPLOY_PROF_BUSINESS_SERVICES <- bls_api("CEU6000000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  select(date, value) %>%
  mutate(series_id = "Professional and Business Services") %>%
  drop_na()

EMPLOY_EDU_HEALTH_SERVICES <- bls_api("CEU6500000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  select(date, value) %>%
  mutate(series_id = "Private Education and Health Services") %>%
  drop_na()

EMPLOY_LEISURE_HOSPITALITY <- bls_api("CEU7000000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  select(date, value) %>%
  mutate(series_id = "Leisure and Hospitality") %>%
  drop_na()

EMPLOY_GOODS <- bls_api("CEU0600000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  select(date, value) %>%
  mutate(series_id = "Goods-Producing") %>%
  drop_na()

EMPLOY_GOVT <- bls_api("CEU9000000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  select(date, value) %>%
  mutate(series_id = "Government") %>%
  drop_na()

EMPLOY_OTHER_SERVICES <- rbind(bls_api("CES5000000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")),
                                   bls_api("CES5500000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")),
                                   bls_api("CES8000000001", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date,seriesID,value) %>%
  pivot_wider(names_from = seriesID) %>%
  transmute(date, value = CES5000000001 + CES5500000001 + CES8000000001, series_id = "Other Services Incl. Finance & Info") %>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

#Yoy Change in Employment
# EMPLOY_TRADE_TRANSP_UTIL <- fredr(series_id = "USTPU",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Trade, Transportation, and Utilities")
# EMPLOY_PROF_BUSINESS_SERVICES <- fredr(series_id = "USPBS",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Professional and Business Services")
# EMPLOY_EDU_HEALTH_SERVICES <- fredr(series_id = "USEHS",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Private Education and Health Services")
# EMPLOY_LEISURE_HOSPITALITY <- fredr(series_id = "USLAH",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Leisure and Hospitality")
# EMPLOY_GOODS <- fredr(series_id = "USGOOD",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Goods-Producing")
# EMPLOY_GOVT <- fredr(series_id = "USGOVT",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Government")
# EMPLOY_OTHER_SERVICES <- rbind(fredr(series_id = "USINFO",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1"),
#                                fredr(series_id = "USFIRE",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1"),
#                                fredr(series_id = "USSERV",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1")) %>%
#                         select(date,series_id,value) %>%
#                         pivot_wider(names_from = series_id) %>%
#                         transmute(date, value = USINFO + USFIRE + USSERV, series_id = "Other Services Incl. Finance & Info")

EMPLOY_GROWTH_YOY <- rbind(EMPLOY_TRADE_TRANSP_UTIL,EMPLOY_PROF_BUSINESS_SERVICES,EMPLOY_EDU_HEALTH_SERVICES,EMPLOY_LEISURE_HOSPITALITY,EMPLOY_GOODS,EMPLOY_GOVT,EMPLOY_OTHER_SERVICES) %>%
  mutate(series_id = factor(series_id,levels = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Private Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Info")))
  
EMPLOY_GROWTH_YOY_graph <- ggplot(data = EMPLOY_GROWTH_YOY, aes(x = date, y = value/1000, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Jobs Growth, YoY, Millions") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(-20,-10,0,10), limits = c(-20.5,15), expand = c(0,0)) +
  ggtitle("The Shape of Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Job Growth is Broad-Based, With All Major Industries Posting Gains") +
  theme_apricitas + theme(legend.position = c(.825,.30)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Private Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Info")) +
  theme(legend.text = element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -20-(.3*35.5), ymax = -20.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_GROWTH_YOY_graph, "Employ Growth YoY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


EMPLOY_TRADE_TRANSP_UTIL_IND <- bls_api("CES4000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Trade, Transportation, and Utilities")
  
EMPLOY_PROF_BUSINESS_SERVICES_IND <- bls_api("CES6000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Professional and Business Services")

EMPLOY_EDU_HEALTH_SERVICES_IND <- bls_api("CES6500000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Private Education and Health Services")
  
EMPLOY_LEISURE_HOSPITALITY_IND <- bls_api("CES7000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Leisure and Hospitality")
  
EMPLOY_GOODS_IND <- bls_api("CES0600000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Goods-Producing")
  
EMPLOY_GOVT_IND <- bls_api("CES9000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Government")

EMPLOY_OTHER_SERVICES_IND <- rbind(bls_api("CES5000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")),
                                   bls_api("CES5500000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")),
                                   bls_api("CES8000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date,seriesID,value) %>%
  pivot_wider(names_from = seriesID) %>%
  transmute(date, value = CES5000000001 + CES5500000001 + CES8000000001, series_id = "Other Services Incl. Finance & Information") %>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1]))


#Employ Index
# EMPLOY_TRADE_TRANSP_UTIL_IND <- fredr(series_id = "USTPU",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Trade, Transportation, and Utilities") %>%
#   mutate(value = (value-value[1]))
# EMPLOY_PROF_BUSINESS_SERVICES_IND <- fredr(series_id = "USPBS",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Professional and Business Services") %>%
#   mutate(value = (value-value[1]))
# EMPLOY_EDU_HEALTH_SERVICES_IND <- fredr(series_id = "USEHS",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Private Education and Health Services") %>%
#   mutate(value = (value-value[1]))
# EMPLOY_LEISURE_HOSPITALITY_IND <- fredr(series_id = "USLAH",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Leisure and Hospitality") %>%
#   mutate(value = (value-value[1]))
# EMPLOY_GOODS_IND <- fredr(series_id = "USGOOD",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Goods-Producing") %>%
#   mutate(value = (value-value[1]))
# EMPLOY_GOVT_IND <- fredr(series_id = "USGOVT",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
#   select(date, value, series_id) %>%
#   mutate(series_id = "Government") %>%
#   mutate(value = (value-value[1]))
# EMPLOY_OTHER_SERVICES_IND <- rbind(fredr(series_id = "USINFO",observation_start = as.Date("2020-01-01"), realtime_end = NULL),
#                                fredr(series_id = "USFIRE",observation_start = as.Date("2020-01-01"), realtime_end = NULL),
#                                fredr(series_id = "USSERV",observation_start = as.Date("2020-01-01"), realtime_end = NULL)) %>%
#   select(date,series_id,value) %>%
#   pivot_wider(names_from = series_id) %>%
#   transmute(date, value = USINFO + USFIRE + USSERV, series_id = "Other Services Incl. Finance & Information") %>%
#   mutate(value = (value-value[1]))
# 
EMPLOY_GROWTH_IND <- rbind(EMPLOY_TRADE_TRANSP_UTIL_IND,EMPLOY_PROF_BUSINESS_SERVICES_IND,EMPLOY_EDU_HEALTH_SERVICES_IND,EMPLOY_LEISURE_HOSPITALITY_IND,EMPLOY_GOODS_IND,EMPLOY_GOVT_IND,EMPLOY_OTHER_SERVICES_IND) %>%
   mutate(series_id = factor(series_id,levels = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Private Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Information")))

EMPLOY_GROWTH_IND_graph <- ggplot(data = EMPLOY_GROWTH_IND, aes(x = date, y = value/1000, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(-20,-15,-10,-5,0,5,10), limits = c(-22,10), expand = c(0,0)) +
  ggtitle("The Shape of Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "There are Now 7M More Jobs Than Pre-Pandemic—and All Sectors Have Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.725,.325)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Private Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Information")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -22-(.3*29.5), ymax = -22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_GROWTH_IND_graph, "Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#CES CPS QCEW GRAPH

CES_2022 <- bls_api("CEU0000000001", startyear = 2022) %>% #headline CES NSA INDEXED TO JAN 22
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1]))

CPS_ADJ_2022 <- bls_api("LNU06000000", startyear = 2022) %>% #headline CPS NSA INDEXED TO JAN 22
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1]))

QCEW_2022 <- bls_api("ENUUS00010010", startyear = 2022) %>% #headline QCEW INDEXED TO JAN 22
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1]))

CES_CPS_QCEW_Graph <- ggplot() +
  geom_line(data = CES_2022, aes(x=date, y = value/1000, color = "CES"), size = 1.25) + 
  #geom_line(data = CPS_ADJ_2022, aes(x=date, y = value/1000, color = "CPS Adjusted to CES Concepts"), size = 1.25) + 
  geom_line(data = QCEW_2022, aes(x=date, y = value/1000000, color = "QCEW"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(0,12), breaks = c(0,2,4,6,8,10,12), expand = c(0,0)) +
  ylab("Growth Since Jan 2022, NSA") +
  ggtitle("Detailed QCEW Data Points to Downward\nCES (Nonfarm Payrolls) Revisions") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "QCEW Data Suggests CES Data Will Be Revised Down Significantly at the End of This Year") +
  theme_apricitas + theme(legend.position = c(.30,.8)) +
  scale_color_manual(name= "Non-Seasonally Adjusted Job Growth",values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-15")-(.1861*(today()-as.Date("2022-01-15"))), xmax = as.Date("2022-01-15")-(0.049*(today()-as.Date("2022-01-15"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CES_CPS_QCEW_Graph, "CES CPS QCEW Comparison.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#truck and warehouse employment
TRUCK_EMPLOY <- bls_api("CES4348400001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

WAREHOUSE_EMPLOY <- bls_api("CES4349300001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


TRUCK_WAREHOUSE_GRAPH <- ggplot() + #plotting local government education employment
  geom_line(data=TRUCK_EMPLOY, aes(x=date,y= value/1000,color= "All Employees, Truck Transportation"), size = 1.25) + 
  geom_line(data=WAREHOUSE_EMPLOY, aes(x=date,y= value/1000,color= "All Employees, Warehousing and Storage"), size = 1.25) + 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(1.2,1.4,1.6,1.8,2), limits = c(1.1,2.1), expand = c(0,0)) +
  ggtitle("The Transport Taper") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Warehousing Employment is Falling and Trucking Employment is Stagnating") +
  theme_apricitas + theme(legend.position = c(.26,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 1.1-(.3*1), ymax = 1.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TRUCK_WAREHOUSE_GRAPH, "Truck Warehouse Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
#temp help services employment
#EMPLOY_TEMP_HELP_SERVICES <- fredr(series_id = "TEMPHELPS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

EMPLOY_TEMP_HELP_SERVICES <- bls_api("CES6056132001", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #OwnIllnessPartTime
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

EMPLOY_TEMP_HELP_SERVICES_GRAPH <- ggplot() + #plotting local government education employment
  geom_line(data=EMPLOY_TEMP_HELP_SERVICES, aes(x=date,y= value/1000,color= "All Employees, Temporary Help Services"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(2,2.2,2.4,2.6,2.8,3,3.2,3.4), limits = c(1.9,3.4), expand = c(0,0)) +
  ggtitle("Temporary Trouble") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Temporary Help Services Employment, an Important Leading Indicator, is Slightly Dropping") +
  theme_apricitas + theme(legend.position = c(.30,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 1.9-(.3*1.5), ymax = 1.9) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_TEMP_HELP_SERVICES_GRAPH, "Employ Temp Help Services.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


GLI_BLS_YOY <- fredr(series_id = "CES0500000017",observation_start = as.Date("2018-01-01"), units = "pc1")
GLI_BEA_YOY <- fredr(series_id = "A132RC1",observation_start = as.Date("2018-01-01"), units = "pc1")
GLI_EPOP_YOY <- fredr(series_id = "LNS12300060",observation_start = as.Date("2017-01-01"), aggregation_method = "avg", frequency = "q") %>%
  merge(.,fredr(series_id = "ECIWAG",observation_start = as.Date("2017-01-01")),by = "date") %>%
  mutate(value = value.x*value.y) %>%
  mutate(value = (value-lag(value,4))/lag(value,4)) %>%
  drop_na()


GLI_GROWTH_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=GLI_BLS_YOY, aes(x=date,y= value/100,color= "Non-Farm Payrolls Data"), size = 1.25) +
  geom_line(data=GLI_BEA_YOY, aes(x=date,y= value/100,color= "BEA Data"), size = 1.25) +
  geom_line(data=GLI_EPOP_YOY, aes(x=date,y= value,color= "ECI * Prime Age Employment"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "5% Pre-COVID Normal Growth Rate", x = as.Date("2022-07-01"), y =0.042, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.10,0.18), breaks = c(-.1,-0.05,0,0.05,.1,.15), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("US Gross Labor Income Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data",subtitle = "Gross Labor Income Growth Looks to Be Declining Back to Pre-COVID Normal Levels") +
  theme_apricitas + theme(legend.position = c(.33,.75)) +
  scale_color_manual(name= "Private-Sector Gross Labor Income Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Non-Farm Payrolls Data","BEA Data","ECI * Prime Age Employment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.10-(.3*0.28), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_GROWTH_graph, "GLI Growth graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

DATA_PROCESSING <- bls_api("CES5051800001", startyear = 2005, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SOFTWARE_PUBLISHERS <- bls_api("CES5051320001", startyear = 2005, registrationKey = "BLS_KEY") %>% #software employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SEARCH_PORTALS <- bls_api("CES5051929001", startyear = 2005, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
MEDIA_SOCIAL <- bls_api("CES5051620001", startyear = 2005, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

TECH_EMPLOYMENT_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=DATA_PROCESSING, aes(x=date,y= value,color= "Computing Infrastructure, Data Processing, Web Hosting, & Related"), size = 1.25)+ 
  geom_line(data=SOFTWARE_PUBLISHERS, aes(x=date,y= value,color= "Software Publishers"), size = 1.25) + 
  geom_line(data=SEARCH_PORTALS, aes(x=date,y= value,color= "Web Search Portals and All Other Information Services"), size = 1.25) + 
  geom_line(data=MEDIA_SOCIAL, aes(x=date,y= value,color= "Media Streaming Distribution Services, Social Networks, & Related"), size = 1.25) + 
  xlab("Date") +
  ylab("All Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,675), expand = c(0,0)) +
  ggtitle("Tech-Cession?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment is Stalling in Key Digital Tech Sectors") +
  theme_apricitas + theme(legend.position = c(.385,.835), legend.text = element_text(size = 13)) +
  scale_color_manual(name= "All Employees",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Software Publishers","Computing Infrastructure, Data Processing, Web Hosting, & Related","Media Streaming Distribution Services, Social Networks, & Related","Web Search Portals and All Other Information Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOYMENT_Graph, "Tech Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DATA_PROCESSING_IND <- bls_api("CES5051800001", startyear = 2020, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computing Infrastructure, Data Processing, Web Hosting, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

SOFTWARE_PUBLISHERS_IND <- bls_api("CES5051320001", startyear = 2020, registrationKey = "BLS_KEY") %>% #software employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Software Publishers") %>%
  mutate(value = (value-value[nrow(.)]))

SEARCH_PORTALS_IND <- bls_api("CES5051929001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Web Search Portals and All Other Information Services") %>%
  mutate(value = (value-value[nrow(.)]))

MEDIA_SOCIAL_IND <- bls_api("CES5051620001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Streaming Services, Social Networks, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

CUSTOM_COMPUTER_PROG_IND <- bls_api("CES6054151101", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Custom Computer Programming Services") %>%
  mutate(value = (value-value[nrow(.)]))

COMPUTER_SYSTEM_DESIGN_IND <- bls_api("CES6054151201", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computer Systems Design Services") %>%
  mutate(value = (value-value[nrow(.)]))

TECH_EMPLOY_GROWTH_IND <- rbind(DATA_PROCESSING_IND,MEDIA_SOCIAL_IND,SEARCH_PORTALS_IND,SOFTWARE_PUBLISHERS_IND,CUSTOM_COMPUTER_PROG_IND,COMPUTER_SYSTEM_DESIGN_IND) %>%
  group_by(date) %>%
  filter(n() > 5) %>%
  mutate(series_id = factor(series_id,levels = rev(c("Software Publishers","Custom Computer Programming Services","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design Services","Web Search Portals and All Other Information Services","Streaming Services, Social Networks, & Related"))))

TECH_EMPLOY_GROWTH_INDSUM <- TECH_EMPLOY_GROWTH_IND %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE))

TECH_EMPLOY_GROWTH_IND_graph <- ggplot(data = TECH_EMPLOY_GROWTH_IND, aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,200,400,600), limits = c(-75,750), expand = c(0,0)) +
  ggtitle("The Tech Boom and Techcession") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in the Tech Sector May be Starting to Grow Again") +
  theme_apricitas + theme(legend.position = c(0.35,0.82), legend.key.size = unit(0.5,"cm"), legend.spacing.y = unit(0, "cm")) +
  scale_fill_manual(name= "Change in Employment since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Software Publishers","Custom Computer Programming Services","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design Services","Web Search Portals and All Other Information Services","Streaming Services, Social Networks, & Related")) +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -75-(.3*825), ymax = -75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOY_GROWTH_IND_graph, "Tech Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

DATA_PROCESSING_YOY <- bls_api("CEU5051800001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU5051800001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computing Infrastructure, Data Processing, Web Hosting, & Related") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

SOFTWARE_PUBLISHERS_YOY <- bls_api("CES5051320001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #software employment
  select(-latest) %>%
  rbind(bls_api("CES5051320001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Software Publishers") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

SEARCH_PORTALS_YOY <- bls_api("CES5051929001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CES5051929001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Web Search Portals and All Other Information Services") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

MEDIA_SOCIAL_YOY <- bls_api("CES5051620001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CES5051620001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Streaming Services, Social Networks, & Related") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

CUSTOM_COMPUTER_PROG_YOY <- bls_api("CES6054151101", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CES6054151101", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Custom Computer Programming Services") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

COMPUTER_SYSTEM_DESIGN_YOY <- bls_api("CES6054151201", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CES6054151201", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computer Systems Design Services") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TECH_EMPLOY_GROWTH_YOY <- rbind(DATA_PROCESSING_YOY,MEDIA_SOCIAL_YOY,SEARCH_PORTALS_YOY,SOFTWARE_PUBLISHERS_YOY,CUSTOM_COMPUTER_PROG_YOY,COMPUTER_SYSTEM_DESIGN_YOY) %>%
  group_by(date) %>%
  filter(n() > 5) %>%
  mutate(series_id = factor(series_id,levels = rev(c("Software Publishers","Custom Computer Programming Services","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design Services","Web Search Portals and All Other Information Services","Streaming Services, Social Networks, & Related"))))

TECH_EMPLOY_GROWTH_YOYSUM <- TECH_EMPLOY_GROWTH_YOY %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  mutate(series_id = "Sum YoY Growth")

TECH_EMPLOY_GROWTH_YOY_graph <- ggplot(data = filter(TECH_EMPLOY_GROWTH_YOY, date >= as.Date("2017-01-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(TECH_EMPLOY_GROWTH_YOYSUM, date>= as.Date("2017-01-01")), aes(x=date, y = sum_value, color = "Total Tech Employment Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-100,0,100,200,300,400,500,600), limits = c(-100,420), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in US Tech Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment Growth in the Tech Sector Has Been Noticeable Weak") +
  theme_apricitas + theme(legend.position = c(0.35,0.83), legend.margin=margin(0,0,-07,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Software Publishers","Custom Computer Programming Services","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design Services","Web Search Portals and All Other Information Services","Streaming Services, Social Networks, & Related")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -100-(.3*520), ymax = -100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOY_GROWTH_YOY_graph, "Tech Employ Growth YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


TECH_EMPLOY_GROWTH_YOY_LONG_graph <- ggplot(data = TECH_EMPLOY_GROWTH_YOY, aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = TECH_EMPLOY_GROWTH_YOYSUM, aes(x=date, y = sum_value, color = "Total Tech Employment Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-200,-100,0,100,200,300,400,500,600), limits = c(-225,425), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in US Tech Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The 2023 Tech-Cession is Less Bad than 2001 but worse than 2008 or 2020") +
  theme_apricitas + theme(legend.position = c(.35,0.88), legend.margin=margin(0,0,-10,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.4, "cm"),legend.key.height = unit(0.3, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Software Publishers","Custom Computer Programming Services","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design Services","Web Search Portals and All Other Information Services","Streaming Services, Social Networks, & Related")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 12), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1991-01-01")-(.1861*(today()-as.Date("1991-01-01"))), xmax = as.Date("1991-01-01")-(0.049*(today()-as.Date("1991-01-01"))), ymin = -225-(.3*650), ymax = -225) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOY_GROWTH_YOY_LONG_graph, "Tech Employ Growth YOY Long.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


VEHICLE_ASSEMBLY_YOY <- bls_api("CEU3133610001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3133610001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Vehicle Final Assembly") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

BODY_TRAILER_YOY <- bls_api("CEU3133620001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #software employment
  select(-latest) %>%
  rbind(bls_api("CEU3133620001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Bodies & Trailers") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

ENGINE_YOY <- bls_api("CEU3133631001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133631001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Engines & Related Parts") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

ELECTRICAL_ELECTRONIC_YOY <- bls_api("CEU3133632001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133632001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Electrical & Electronic Components") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TRANSMISSION_POWER_YOY <- bls_api("CEU3133635001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133635001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Transmission & Power Train") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

METAL_STAMPING_YOY <- bls_api("CEU3133637001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133637001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Metal Stamping") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

STEERING_SUSPENSION_YOY <- bls_api("CEU3133639001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133639001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Steering & Suspension Components") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

VEHICLE_EMPLOY_GROWTH_YOY <- rbind(VEHICLE_ASSEMBLY_YOY,BODY_TRAILER_YOY,ENGINE_YOY,ELECTRICAL_ELECTRONIC_YOY,TRANSMISSION_POWER_YOY,METAL_STAMPING_YOY,STEERING_SUSPENSION_YOY) %>%
  group_by(date) %>%
  filter(n() > 5) %>%
  mutate(series_id = factor(series_id,levels = rev(c("Vehicle Final Assembly","Bodies & Trailers","Engines & Related Parts","Electrical & Electronic Components","Transmission & Power Train","Metal Stamping","Steering & Suspension Components"))))

VEHICLE_EMPLOY_GROWTH_YOYSUM <- VEHICLE_EMPLOY_GROWTH_YOY %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  mutate(series_id = "Sum YoY Growth")

VEHICLE_EMPLOY_GROWTH_YOY_graph <- ggplot(data = filter(VEHICLE_EMPLOY_GROWTH_YOY, date >= as.Date("2022-01-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(VEHICLE_EMPLOY_GROWTH_YOYSUM, date>= as.Date("2022-01-01")), aes(x=date, y = sum_value, color = "Total Motor Vehicle Manufacturing Job Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-100,-50,0,50,100), limits = c(-50,110), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in Autoworker Jobs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment Has Fallen in the Motor Vehicle Industry Over the Last Year") +
  theme_apricitas + theme(legend.position = c(0.75,0.780), legend.margin=margin(0,0,-6,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#DFB2F4","#EE6055"), breaks = c("Vehicle Final Assembly","Bodies & Trailers","Engines & Related Parts","Electrical & Electronic Components","Transmission & Power Train","Metal Stamping","Steering & Suspension Components")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -50-(.3*160), ymax = -50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VEHICLE_EMPLOY_GROWTH_YOY_graph, "Vehicle Employ Growth YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



TRANSPORT_EQUIP_MANU_YOY <- bls_api("CEU3133600001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3133600001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Transportation Equipment (Cars/Planes/Etc)") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

FOOD_MANU_YOY <- bls_api("CEU3231100001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3231100001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Food") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

MACHINERY_YOY <- bls_api("CEU3133300001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3133300001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Machinery") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

COMPUTER_ELECTRONIC_ELECTRICAL_YOY <- bls_api("CEU3133400001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3133400001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3133500001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #data processing employment
  rbind(bls_api("CEU3133500001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Computer, Electronics, & Electrical") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()


CHEMICAL_PETRO_PLASTIC_YOY <- bls_api("CES3232500001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #chemical
  select(-latest) %>%
  rbind(bls_api("CEU3232500001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3232600001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #plastics and rubber
  rbind(bls_api("CEU3232600001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3232400001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #petroleum
  rbind(bls_api("CEU3232400001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Chemicals, Petroleum, & Plastic") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

WOOD_PRINTING_PAPER_YOY <- bls_api("CEU3132100001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #Wood
  select(-latest) %>%
  rbind(bls_api("CEU3132100001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3232300001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Printing
  rbind(bls_api("CEU3232300001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3232200001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Paper
  rbind(bls_api("CEU3232200001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Wood, Paper, & Printing") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

METALS_MINERALS_YOY <- bls_api("CEU3133100001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #Primary Metal
  select(-latest) %>%
  rbind(bls_api("CEU3133100001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3133200001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Fabricated Metal
  rbind(bls_api("CEU3133200001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3132700001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Nonmetallic Mineral
  rbind(bls_api("CEU3132700001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Metals & Nonmetallic Minerals") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()


OTHER_MANUFACTURING_YOY <- bls_api("CEU3133900001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #Other Durables
  select(-latest) %>%
  rbind(bls_api("CEU3133900001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3231300001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Textile Mills
  rbind(bls_api("CEU3231300001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3231400001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Textile Product Mills
  rbind(bls_api("CEU3231400001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3231500001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Apparel Manufacturing
  rbind(bls_api("CEU3231500001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3232900001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Other Nondurable Manufacturing
  rbind(bls_api("CEU3232900001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("CEU3133700001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>% #Furniture
  rbind(bls_api("CEU3133700001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Other") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()


TOTAL_MANUFACTURING_YOY <- bls_api("CEU3000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #Wood
  select(-latest) %>%
  rbind(bls_api("CEU3000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Machinery") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

MANUFACTURING_RBIND <- rbind(TRANSPORT_EQUIP_MANU_YOY,FOOD_MANU_YOY,MACHINERY_YOY,COMPUTER_ELECTRONIC_ELECTRICAL_YOY,CHEMICAL_PETRO_PLASTIC_YOY,WOOD_PRINTING_PAPER_YOY,METALS_MINERALS_YOY,OTHER_MANUFACTURING_YOY) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))))


MANUFACTURING_GROWTH_YOY_graph <- ggplot(data = filter(MANUFACTURING_RBIND, date >= as.Date("2023-01-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(TOTAL_MANUFACTURING_YOY, date >= as.Date("2023-01-01")), aes(x=date, y = value, color = "Total Manufacturing Employment Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-100,0,100,200,300,400), limits = c(-150,400), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in US Manufacturing Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "America Has Lost Nearly 100k Manufacturing Jobs Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.625,.80)) + theme(plot.title = element_text(size = 23), legend.margin=margin(0,0,-133,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  #scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72", breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72"), breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = -150-(.3*550), ymax = -150) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = MANUFACTURING_GROWTH_YOY_graph, "Manufacturing Growth Yoy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



RESIDENTIAL_BUILDING_CONSTRUCTION <- bls_api("CEU2023610001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU2023610001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Residential Building Construction") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

NONRESIDENTIAL_BUILDING_CONSTRUCTION <- bls_api("CEU2023620001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU2023620001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Nonresidential Building Construction") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

HEAVY_CIVIL_ENGINEERING_CONSTRUCTION <- bls_api("CES2023700001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CES2023700001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Heavy/Civil Engineering Construction") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()


RESIDENTIAL_SPECIALTY_TRADE_CONTRACTORS <- bls_api("CEU2023800101", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU2023800101", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Residential Specialty Trade Contractors") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

NONRESIDENTIAL_SPECIALTY_TRADE_CONTRACTORS <- bls_api("CEU2023800201", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU2023800201", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Nonresidential Specialty Trade Contractors") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TOTAL_CONSTRUCTION_YOY <- bls_api("CEU2000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #Wood
  select(-latest) %>%
  rbind(bls_api("CEU2000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  group_by(date) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(series_id = "Total Construction") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TOTAL_CONSTRUCTION_RBIND <- rbind(RESIDENTIAL_BUILDING_CONSTRUCTION,NONRESIDENTIAL_BUILDING_CONSTRUCTION,HEAVY_CIVIL_ENGINEERING_CONSTRUCTION,RESIDENTIAL_SPECIALTY_TRADE_CONTRACTORS,NONRESIDENTIAL_SPECIALTY_TRADE_CONTRACTORS) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Residential Building Construction","Nonresidential Building Construction","Residential Specialty Trade Contractors","Nonresidential Specialty Trade Contractors","Heavy/Civil Engineering Construction"))))


TOTAL_CONSTRUCTION_GROWTH_YOY_graph <- ggplot(data = filter(TOTAL_CONSTRUCTION_RBIND, date >= as.Date("2023-01-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(TOTAL_CONSTRUCTION_YOY, date >= as.Date("2023-01-01")), aes(x=date, y = value, color = "Total Construction Employment Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-100,0,100,200,300,400), limits = c(-100,425), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in US Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "American Construction Job Growth Is Slowing Down, Especially in the Residential Sector") +
  theme_apricitas + theme(legend.position = c(.625,.85)) + theme(plot.title = element_text(size = 23), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  #scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72", breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#6A4C93","#00A99D","#3083DC","#A7ACD9","#9A348E","#D28E20","#FF8E72"), breaks = c("Residential Building Construction","Nonresidential Building Construction","Residential Specialty Trade Contractors","Nonresidential Specialty Trade Contractors","Heavy/Civil Engineering Construction")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = -100-(.3*500), ymax = -100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TOTAL_CONSTRUCTION_GROWTH_YOY_graph, "Total Construction Growth Yoy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


MINING_YOY <- bls_api("CEU1000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU1000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Mining, Logging, Oil, & Gas") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

CONSTRUCTION_YOY <- bls_api("CEU2000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU2000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Construction") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

MANUFACTURING_YOY <- bls_api("CEU3000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Manufacturing") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()


TRANSPORTATION_YOY <- bls_api("CEU4300000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU4300000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Transportation & Warehousing") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

UTILITIES_YOY <- bls_api("CEU4422000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU4422000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Utilities") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TOTAL_BLUE_COLLAR_RBIND <- rbind(MINING_YOY,CONSTRUCTION_YOY,MANUFACTURING_YOY,TRANSPORTATION_YOY,UTILITIES_YOY) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Manufacturing","Construction","Mining, Logging, Oil, & Gas","Transportation & Warehousing","Utilities"))))

TOTAL_BLUE_COLLAR_YOY <- TOTAL_BLUE_COLLAR_RBIND %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(series_id = "test")



TOTAL_BLUE_COLLAR_YOY_graph <- ggplot(data = filter(TOTAL_BLUE_COLLAR_RBIND, date >= as.Date("2023-01-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(TOTAL_BLUE_COLLAR_YOY, date >= as.Date("2023-01-01")), aes(x=date, y = value, color = "Total Blue Collar Employment Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-200,0,200,400,600,800), limits = c(-200,900), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in US Blue Collar Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "American Blue Collar Job Growth Is Slowing Down, With Job Losses in the Manufacturing Sector") +
  theme_apricitas + theme(legend.position = c(.625,.85)) + theme(plot.title = element_text(size = 23), legend.margin=margin(0,0,-90,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  #scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72", breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#3083DC","#A7ACD9","#6A4C93","#9A348E","#D28E20","#FF8E72"), breaks = c("Manufacturing","Construction","Mining, Logging, Oil, & Gas","Transportation & Warehousing","Utilities")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = -200-(.3*1100), ymax = -200) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TOTAL_BLUE_COLLAR_YOY_graph, "Total Blue Collar Growth Yoy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#
ECI_WAG_YOY <- fredr(series_id = "ECIWAG",observation_start = as.Date("2002-01-01"), units = "pc1")
AHE_YOY <- fredr(series_id = "CES0500000003",observation_start = as.Date("2002-01-01"), units = "pc1")

WAGE_GROWTH_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=AHE_YOY, aes(x=date,y= value/100,color= "Average Hourly Earnings, Private (Not Composition Adjusted)"), size = 1.25) +
  geom_line(data=ECI_WAG_YOY, aes(x=date,y= value/100,color= "Wages, Employment Cost Index, Private (Composition Adjusted)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.0815), breaks = c(0,0.01,.02,.03,0.04,0.05,0.06,0.07,0.08), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Wage Growth is Decelerating") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Nominal Wage Growth Has Slowed Down as the US Labor Market Cools") +
  theme_apricitas + theme(legend.position = c(.4,.72)) +
  scale_color_manual(name= "Percent Growth, Year-on-Year",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Wages, Employment Cost Index, Private (Composition Adjusted)","Average Hourly Earnings, Private (Not Composition Adjusted)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*0.08), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WAGE_GROWTH_Graph, "Wage Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Quits Rate
QUITS_RATE <- fredr(series_id = "JTSQUR",observation_start = as.Date("2000-01-01"))

QUITS_RATE_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=QUITS_RATE, aes(x=date,y= value/100,color= "Quits Rate, Total Nonfarm"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.01,0.032), breaks = c(0,0.01,.02,.03), expand = c(0,0)) +
  ylab("Rate") +
  ggtitle("The Great Reshuffling") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "The Quit Rate, an Important Indicator of Labor Market Tightness, Has Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.42,.72)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-12-01")-(.1861*(today()-as.Date("2000-12-01"))), xmax = as.Date("2000-12-01")-(0.049*(today()-as.Date("2000-12-01"))), ymin = 0.01-(.3*0.022), ymax = 0.01) +
  coord_cartesian(clip = "off")

SF_CONSTRUCTION <- bls_api("CES2023611501", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Single-Family Construction") %>%
  select(date,value,name)
MF_CONSTRUCTION <- bls_api("CES2023611601", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Multi-Family Construction") %>%
  select(date,value,name)
HFS_CONSTRUCTION <- bls_api("CES2023611701", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "For-Sale Builders") %>%
  select(date,value,name)
REMODEL_CONSTRUCTION <- bls_api("CES2023611801", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Residential Remodelers") %>%
  select(date,value,name)

CONSTRUCTION_RBIND <- rbind(SF_CONSTRUCTION,MF_CONSTRUCTION,HFS_CONSTRUCTION,REMODEL_CONSTRUCTION)

CONSTRUCTION_TOTAL <- bls_api("CES2023610001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Residential Construction Total") %>%
  select(date,value,name)

CONSTRUCTION_GROWTH_IND_graph <- ggplot(data = CONSTRUCTION_RBIND, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = CONSTRUCTION_TOTAL, aes(x=date, y = value, color = "Total Residential Construction Employment"), size = 2) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-125,-100,-75,-50,-25,0,25,50,75,100,125,150), limits = c(-125,150), expand = c(0,0)) +
  ggtitle("The Shape of Construction Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Residential Construction Employment Has Recovered Back to Post-2008 Highs") +
  theme_apricitas + theme(legend.position = c(.625,.30)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-97.5,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Single-Family Construction","Residential Remodelers","Multi-Family Construction","For-Sale Builders")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -125-(.3*275), ymax = -125) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_GROWTH_IND_graph, "Construction Growth Ind.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


RES_CONSTRUCTION <- bls_api("CES2023610001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Residential Construction") %>%
  select(date,value,name) %>%
  mutate(value = value-value[nrow(.)])

RES_TRADE_CONTRACTORS <- bls_api("CES2023800101", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Residential Specialty Trade Contractors") %>%
  select(date,value,name) %>%
  mutate(value = value-value[nrow(.)])

RES_LESSOR <- bls_api("CES5553111001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_PROPERTY_MANAGER <- bls_api("CES5553131101", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_AGENTS <- bls_api("CES5553120001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_APPRAISERS <- bls_api("CES5553132001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_OTHER <- bls_api("CES5553139001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

REAL_ESTATE_RBIND <- rbind(RES_LESSOR,RES_PROPERTY_MANAGER,RES_AGENTS,RES_APPRAISERS,RES_OTHER) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES5553111001`+`CES5553131101`+`CES5553120001`+`CES5553132001`+`CES5553139001`) %>%
  mutate(name = "Property Managers, Real Estate Agents, Appraisers, Lessors, & Related") %>%
  mutate(value = value-value[nrow(.)])

CONSTRUCTION_MATERIALS_WHOLE <- bls_api("CES4142330001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
HOUSEHOLD_APPLIANCES_WHOLE <- bls_api("CES4142360001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
FURNITURE_WHOLE <- bls_api("CES4142320001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
HARDWARE_MATERIALS_WHOLE <- bls_api("CES4142370001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CONSTRUCTION_MACHINERY_WHOLE <- bls_api("CES4142381001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

WHOLESALE_RBIND <- rbind(CONSTRUCTION_MATERIALS_WHOLE,HOUSEHOLD_APPLIANCES_WHOLE,FURNITURE_WHOLE,HARDWARE_MATERIALS_WHOLE,CONSTRUCTION_MACHINERY_WHOLE) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES4142330001`+`CES4142360001`+`CES4142320001`+`CES4142370001`+`CES4142381001`) %>%
  mutate(name = "Housing Related Merchant Wholesalers") %>%
  mutate(value = value-value[nrow(.)])

BUILDING_GARDEN_SUPPLIES_RETAIL <- bls_api("CES4244400001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
FURNITURE_HOME_ELECTRONICS_APPLIANCE_RETAIL <- bls_api("CES4244900001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

RETAIL_RBIND <- rbind(BUILDING_GARDEN_SUPPLIES_RETAIL,FURNITURE_HOME_ELECTRONICS_APPLIANCE_RETAIL) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES4244400001`+`CES4244900001`) %>%
  mutate(name = "Housing Related Retailers") %>%
  mutate(value = value-value[nrow(.)])

RE_CREDIT <- bls_api("CES5552229201", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
LOAN_BROKERS <- bls_api("CES5552231001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FINANCE_RBIND <- rbind(RE_CREDIT,LOAN_BROKERS) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES5552229201`+`CES5552231001`) %>%
  mutate(name = "Housing Related Creditors and Loan Brokers") %>%
  mutate(value = value-value[nrow(.)])

ARCHITECTURE <- bls_api("CES6054130001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
INTERIOR_DESIGN <- bls_api("CES6054141001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

SERVICES_RBIND <- rbind(ARCHITECTURE,INTERIOR_DESIGN) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES6054130001`+`CES6054141001`) %>%
  mutate(name = "Architectural, Engineering, and Interior Design Services") %>%
  mutate(value = value-value[nrow(.)])

PLASTICS_PIPE_MANU <- bls_api("CES3232612001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
PAINT_MANU <- bls_api("CES3232550001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
FURNITURE_MANU <- bls_api("CES3133712001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
APPLIANCE_MANU <- bls_api("CES3133520001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CONSTRUCTION_MACHINERY_MANU <- bls_api("CES3133312001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
ARCHITECTURAL_METALS_MANU <- bls_api("CES3133230001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CEMENT_CONCRETE <- bls_api("CES3132730001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
WOOD_PRODUCT <- bls_api("CES3132100001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

LOGGING <- bls_api("CES1011330001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SAND_GRAVEL_MINING <- bls_api("CES1021232101", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

MANUFACTURING_RBIND <- rbind(PLASTICS_PIPE_MANU,PAINT_MANU,FURNITURE_MANU,APPLIANCE_MANU,CONSTRUCTION_MACHINERY_MANU,ARCHITECTURAL_METALS_MANU,CEMENT_CONCRETE,WOOD_PRODUCT,LOGGING,SAND_GRAVEL_MINING) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  rowwise() %>%
  transmute(date, value = sum(c_across(where(is.numeric)))) %>%
  mutate(name = "Housing Related Manufacturing, Mining, and Logging") %>%
  ungroup() %>%
  mutate(value = value-value[nrow(.)])

RETAIL_WHOLESALE_RBIND <- rbind(WHOLESALE_RBIND,RETAIL_RBIND) %>%
  select(date, value, name) %>%
  pivot_wider(names_from = name) %>%
  drop_na() %>%
  rowwise() %>%
  transmute(date, value = sum(c_across(where(is.numeric)))) %>%
  mutate(name = "Housing Related Retailers and Wholesalers") %>%
  ungroup() 

HOUSING_RELATED_EMPLOYMENT_RBIND <- rbind(RES_CONSTRUCTION,RES_TRADE_CONTRACTORS,REAL_ESTATE_RBIND,RETAIL_WHOLESALE_RBIND,FINANCE_RBIND,SERVICES_RBIND,MANUFACTURING_RBIND) %>%
  subset(date <= REAL_ESTATE_RBIND$date[1]) %>%
  mutate(name = factor(name,levels = c("Housing Related Retailers and Wholesalers","Housing Related Creditors and Loan Brokers","Housing Related Manufacturing, Mining, and Logging","Property Managers, Real Estate Agents, Appraisers, Lessors, & Related","Residential Construction","Architectural, Engineering, and Interior Design Services","Residential Specialty Trade Contractors")))


HOUSING_RELATED_EMPLOYMENT_IND_graph <- ggplot(data = HOUSING_RELATED_EMPLOYMENT_RBIND, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "k"), breaks = c(-1000,-750,-500,-250,0,250,500,750,1000), limits = c(-1100,1000), expand = c(0,0)) +
  ggtitle("The Shape of Housing Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Housing Related Employment Has Stalled—Importantly in Credit, Construction, and Contractors") +
  theme_apricitas + theme(legend.position = c(.625,.25), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Housing-Related Employment, Change Since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Residential Specialty Trade Contractors","Architectural, Engineering, and Interior Design Services","Residential Construction","Property Managers, Real Estate Agents, Appraisers, Lessors, & Related","Housing Related Manufacturing, Mining, and Logging","Housing Related Creditors and Loan Brokers","Housing Related Retailers and Wholesalers")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -1100-(.3*2100), ymax = -1100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HOUSING_RELATED_EMPLOYMENT_IND_graph, "Housing Related Employment Growth Ind.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PCE_2018 <- fredr("PCE", observation_start = as.Date("2018-01-01"), units = "pc1")

GLI_BLS_YOY <- bls_api("CES0500000017", startyear = 2017, endyear = format(Sys.Date(), "%Y"), calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 12))/dplyr::lead(value, 12)) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2017-01-01"), length = nrow(.), by = "month"))) %>%
  select(-latest) %>%
  drop_na()

GLI_PCE_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=GLI_BLS_YOY, aes(x=date,y= annualpct,color= "Gross Labor Income: Non-Farm Payrolls"), size = 1.25) +
  geom_line(data=PCE_2018, aes(x=date,y= value/100,color= "Personal Consumption Spending"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "5% Pre-COVID Normal Growth Rate", x = as.Date("2022-05-01"), y =0.036, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.175,0.325), breaks = c(-.15,-.1,-0.05,0,0.05,.1,.15,.2,.25,.3), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Labor Income and Consumption Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data",subtitle = "Gross Labor Income Growth is Cooling, as is Nominal Consumption Spending") +
  theme_apricitas + theme(legend.position = c(.27,.75)) +
  scale_color_manual(name= "Nominal Growth From Year Ago",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Gross Labor Income: Non-Farm Payrolls","Personal Consumption Spending")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.175-(.3*0.50), ymax = -.175) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_PCE_graph, "GLI PCE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

QCEW_FINANCE_YOY <- bls_api("ENUUS00030552", startyear = 2017, endyear = format(Sys.Date(), "%Y"), calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2017-01-01"), length = nrow(.), by = "3 months"))) %>%
  drop_na()

QCEW_INFO_YOY <- bls_api("ENUUS0003051022", startyear = 2017, endyear = format(Sys.Date(), "%Y"), calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2017-01-01"), length = nrow(.), by = "3 months"))) %>%
  drop_na()

QCEW_TOTAL_YOY <- bls_api("ENUUS00030510", startyear = 2017, endyear = format(Sys.Date(), "%Y"), calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2017-01-01"), length = nrow(.), by = "3 months"))) %>%
  drop_na()

QCEW_AGG_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=GLI_FINANCE_YOY, aes(x=date,y= annualpct,color= "Finance"), size = 1.25) +
  geom_line(data=GLI_INFO_YOY, aes(x=date,y= annualpct,color= "Information"), size = 1.25) +
  geom_line(data=QCEW_TOTAL_YOY, aes(x=date,y= annualpct,color= "Total"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.10,0.30), breaks = c(-.10,0,.10,.20,.30), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Total Comp Growth By Industry") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data",subtitle = "Income in Finance/Information is Declining, Thanks in Large Part to Shrinking Bonuses") +
  theme_apricitas + theme(legend.position = c(.43,.75)) +
  scale_color_manual(name= "Growth in QCEW Total Wages Including Bonuses/Stock Options",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total","Finance","Information")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.10-(.3*0.40), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QCEW_AGG_graph, "QCEW AGG Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

AWHI_TOTAL <- fredr("AWHAE", observation_start = as.Date("2018-01-01"), units = "pc1")
AWHI_MANU <- fredr("CES3000000016", observation_start = as.Date("2018-01-01"), units = "pc1")

AWHI_AGG_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=AWHI_TOTAL, aes(x=date,y= value/100,color= "Total"), size = 1.25) +
  geom_line(data=AWHI_MANU, aes(x=date,y= value/100,color= "Manufacturing (Incl. Overtime)"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.20,0.20), breaks = c(-.2,-.10,0,.10,.20), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Total Hours Growth By Industry") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Income in Finance/Information is Declining, Thanks in Large Part to Shrinking Bonuses") +
  theme_apricitas + theme(legend.position = c(.28,.85)) +
  scale_color_manual(name= "Annual Growth in Aggregate Hours Worked",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total","Manufacturing (Incl. Overtime)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.20-(.3*0.40), ymax = -.20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AWHI_AGG_graph, "AWHI AGG Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GLI_BLS_QTR <- fredr("CES0500000017", observation_start = as.Date("2018-01-01"), units = "cca", frequency = "q", aggregation_method = "avg")

NGLI_Growth_QTR_Graph <- ggplot(GLI_BLS_QTR, aes(fill="Quarterly Gross Labor Income Growth From Nonfarm Payrolls Data, Annualized", x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", label = "5% Long-Run Pre-COVID Norm",y = 0.054, x = as.Date("2019-06-20"), color = "white", size = 3.5) +
  annotate("text", label = "*Note: Q1-Q3 2020 Excluded Because of Volatility",y = 0.08, x = as.Date("2019-02-01"), color = "white", size = 3.5) +
  #geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  #guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.15), breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("US Labor Income Growth Has Normalized") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "In Q2, Nominal Labor Income Sunk Below a Level Necessary To Maintain Target Inflation") +
  theme_apricitas + theme(legend.position = c(.5,.9)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGLI_Growth_QTR_Graph, "NGLI Growth Quarter.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


# PAYEMS_MONTHLY <- fredr(series_id = "PAYEMS", observation_start = as.Date("2020-10-01"), units = "chg") %>%
#   mutate(THREEMMA = c(0,0, rollmean(value, k = 3))) %>%
#   filter(date >= as.Date("2021-01-01"))

PAYEMS_MONTHLY <- bls_api("CES0000000001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value-lag(value,1)) %>%
  mutate(THREEMMA = c(0,0, rollmean(value, k = 3))) %>%
  filter(date >= as.Date("2021-01-01"))
  

PAYEMS_MONTHLY_GRAPH <- ggplot(data = PAYEMS_MONTHLY, aes(x = date, y = value/1000, fill = "Nonfarm Payrolls, Monthly Growth")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +#plotting Deposits, Insured and Uninsured
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_line(data = PAYEMS_MONTHLY, aes(x=date, y = THREEMMA/1000, color = "3 Month Moving Average"), size = 2) +
  xlab("State") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.25, suffix = "M"), breaks = c(0,.25,.5,.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("US Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using Bureau of Labor Statistics data Via Guy Berger", subtitle = "US Job Growth Has Decelerated Significantly Over the Last Two Years") +
  theme_apricitas + theme(legend.position = c(.725,.75)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-6,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93")) +
  guides(color = guide_legend(order = 2),fill = guide_legend(order = 1)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(today()-as.Date("2021-01-01"))), xmax = as.Date("2021-01-01")-(0.049*(today()-as.Date("2021-01-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PAYEMS_MONTHLY_GRAPH, "Payems Monthly 3MMA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

INDEED_WAGE_TRACKER <- read.csv("https://raw.githubusercontent.com/hiring-lab/indeed-wage-tracker/main/posted_wage_growth_by_country_export_2023-07-31.csv") %>%
  filter(jobcountry == "US") %>%
  mutate(month = as.Date(paste0("01-", month), format="%d-%b-%y"))
ECI_WAG_YOY <- fredr(series_id = "ECIWAG",observation_start = as.Date("2019-01-01"), units = "pc1")
AHE_YOY <- fredr(series_id = "CES0500000003",observation_start = as.Date("2019-01-01"), units = "pc1")

WAGE_GROWTH_INDEED_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=AHE_YOY, aes(x=date,y= value/100,color= "Average Hourly Earnings, Private (Not Composition Adjusted)"), size = 1.25) +
  geom_line(data=ECI_WAG_YOY, aes(x=date,y= value/100,color= "Wages, Employment Cost Index, Private (Composition Adjusted)"), size = 1.25) +
  geom_line(data=INDEED_WAGE_TRACKER, aes(x=month,y= posted_wage_growth_yoy,color= "Indeed Posted Wage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,0.11), breaks = c(0,0.025,0.05,0.075,0.1), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Wage Growth is Decelerating") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Wage Growth Looks to Be Decelerating Now") +
  theme_apricitas + theme(legend.position = c(.42,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Wages, Employment Cost Index, Private (Composition Adjusted)","Average Hourly Earnings, Private (Not Composition Adjusted)","Indeed Posted Wage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*0.1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WAGE_GROWTH_INDEED_Graph, "Wage Growth Indeed Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

BUS_EMP_GROWTH <- fredr(series_id = "ATLSBUEGEP",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

EMP_EXP_GROWTH_Graph <- ggplot() + #plotting Atlanta Fed Business Employment and Revenue Growth Forecasts
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=BUS_EMP_GROWTH, aes(x=date,y= value/100,color= "Business Expectations, Employment Growth, Next Year"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04), limits = c(0,.04), expand = c(0,0)) +
  ggtitle("Companies' Hiring Expectations Are Poor") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Employment Expectations are at the Lowest Levels in Almost 5 Years") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.04), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_GROWTH_Graph, "Employment Growth Expectations.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NY_SERVICES_DIFF <- fredr(series_id = "EMFDINA066MNFRBNY",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL)
TX_SERVICES_DIFF <- fredr(series_id = "TSSOSFEMPSAMFRBDAL",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL)

SERVICES_DIFF_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = NY_SERVICES_DIFF, aes(x = date, y = value, color = "NY Region Services"), size = 1.25) +
  geom_line(data = TX_SERVICES_DIFF, aes(x = date, y = value, color = "TX Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-50,60), breaks = c(-40,-20,0,20,40,60), expand = c(0,0)) +
  ylab("Diffusion Index, Positive Number Indicates Growth") +
  ggtitle("Services Hiring Plans") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Services Employment Growth Forecasts are Holding Up Better, Likely Due to Sectoral Rotations") +
  theme_apricitas + theme(legend.position = c(.52,.20)) +
  scale_color_manual(name= "Future Employment Diffusion Index",values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = -50-(.3*90), ymax = -50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SERVICES_DIFF_Graph, "SERVICES DIFF Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FED_EMP_IND <- bls_api("CES9091000001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Federal Government") %>%
  select(date,value,name)
STATE_EDU_EMP_IND <- bls_api("CES9092161101", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "State Government Education") %>%
  select(date,value,name)
STATE_XEDU_EMP_IND <- bls_api("CES9092200001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "State Government Excluding Education") %>%
  select(date,value,name)
LOCAL_EDU_EMP_IND <- bls_api("CES9093161101", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Local Government Education") %>%
  select(date,value,name)
LOCAL_XEDU_EMP_IND <- bls_api("CES9093200001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Local Government Excluding Education") %>%
  select(date,value,name)


GOVT_RBIND <- rbind(FED_EMP_IND,STATE_EDU_EMP_IND,STATE_XEDU_EMP_IND,LOCAL_EDU_EMP_IND,LOCAL_XEDU_EMP_IND) %>%
  mutate(name = factor(name, levels = rev(c("Federal Government","State Government Education","State Government Excluding Education","Local Government Education","Local Government Excluding Education"))))

GOVT_TOTAL <- bls_api("CES9000000001", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Government Total") %>%
  select(date,value,name)

GOVT_GROWTH_IND_graph <- ggplot(data = GOVT_RBIND, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = GOVT_TOTAL, aes(x=date, y = value/1000, color = "Total Government Employment"), size = 2) +
  xlab("Date") +
  ylab("Jobs, Change Since Jan 2020") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"), breaks = c(-2,-1.5,-1,-0.5,0,0.5,1), limits = c(-2.25,1), expand = c(0,0)) +
  ggtitle("The Shape of Government Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Government Employment Has Now Recovered From the Pandemic, With Rapid Growth in 2023") +
  theme_apricitas + theme(legend.position = c(.7,.3)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-110,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= "Government Employment, Change Since Jan 2020",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Federal Government","State Government Education","State Government Excluding Education","Local Government Education","Local Government Excluding Education")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -2.25-(.3*3), ymax = -2.25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GOVT_GROWTH_IND_graph, "Govt Growth Ind.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FED_EMP_IND_2025 <- bls_api("CES9091000001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Federal Government") %>%
  select(date,value,name,pct)

FED_EMP_IND_2025_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = FED_EMP_IND_2025, aes(x = date, y = value, color = "Change in Federal Employment\nSince January 2025"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(floor(min(FED_EMP_IND_2025$value, na.rm = TRUE) / 10) * 10,0), breaks = c(-100,-80,-60,-40,-20,0), expand = c(0,0)) +
  ylab("Change Since Jan 2025") +
  ggtitle("US Federal Employment is Falling") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = paste0("Federal Employment has Decreased by ", -FED_EMP_IND_2025$value[1], "k, or ", -round(FED_EMP_IND_2025$pct[1],4)*100,"% since January amidst DOGE cuts")) +
  theme_apricitas + theme(legend.position = c(.28,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-01")-(.1861*(today()-30-as.Date("2025-01-01"))), xmax = as.Date("2025-01-01")-(0.049*(today()-30-as.Date("2025-01-01"))), ymin = (floor(min(FED_EMP_IND_2025$value, na.rm = TRUE) / 10) * 10)-(.3*-(floor(min(FED_EMP_IND_2025$value, na.rm = TRUE) / 10) * 10)), ymax = (floor(min(FED_EMP_IND_2025$value, na.rm = TRUE) / 10) * 10)) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_EMP_IND_2025_Graph, "Fed Employment Indexed Jan 2025.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


QUITS_Q <- fredr(series_id = "JTSQUL",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "q", aggregation_method = "sum")
LAYOFFS_Q <- fredr(series_id = "JTSLDL",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "q", aggregation_method = "sum")
HIRES_Q <- fredr(series_id = "JTSHIL",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "q", aggregation_method = "sum")

LABOR_TURNOVER_QUARTERLY_Graph <- ggplot() + #plotting job flows
  geom_line(data = QUITS_Q, aes(x=date, y = value/1000, color = "Quits"), size = 1.25) +
  geom_line(data = LAYOFFS_Q, aes(x=date, y = value/1000, color = "Layoffs and Discharges"), size = 1.25) +
  geom_line(data = HIRES_Q, aes(x=date, y = value/1000, color = "Hires"), size = 1.25) +
  xlab("Date") +
  ylab("Nonfarm Labor Turnover, Quarterly") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(0,5,10,15,20), limits = c(0,21), expand = c(0,0)) +
  ggtitle("US Labor Turnover, Quarterly") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Pace of Job Transitions Via Hires and Quits Have Decelerated As Layoffs Rise Slightly") +
  theme_apricitas + theme(legend.position = c(.175,.95)) + theme(legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Hires","Quits","Layoffs and Discharges")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*21), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LABOR_TURNOVER_QUARTERLY_Graph, "Labor Turnover Quarterly.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ATL_WAG_QUART_1 <- fredr("FRBATLWGT12MMUMHWGWD1WP", observation_start = as.Date("2015-01-01"))
ATL_WAG_QUART_2 <- fredr("FRBATLWGT12MMUMHWGWD26WP", observation_start = as.Date("2015-01-01")) 
ATL_WAG_QUART_3 <- fredr("FRBATLWGT12MMUMHWGWD51WP", observation_start = as.Date("2015-01-01"))
ATL_WAG_QUART_4 <- fredr("FRBATLWGT12MMUMHWGWD76WP", observation_start = as.Date("2015-01-01"))

ATLANTA_FED_WAGE_TRACKER_DISTRIB_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=ATL_WAG_QUART_1, aes(x=date,y= value/100,color= "First (Lowest) Wage Quartile"), size = 1.25)+ 
  geom_line(data=ATL_WAG_QUART_2, aes(x=date,y= value/100,color= "Second Wage Quartile"), size = 1.25)+ 
  geom_line(data=ATL_WAG_QUART_3, aes(x=date,y= value/100,color= "Third Wage Quartile"), size = 1.25)+ 
  geom_line(data=ATL_WAG_QUART_4, aes(x=date,y= value/100,color= "Fourth (Highest) Wage Quartile"), size = 1.25)+ 
  annotate("text",label = "NOTE: Data Lags Significantly Due to Being a 12M Moving Average of Annual Growth", x = as.Date("2019-09-01"), y =0.02, color = "white", size = 5) +
  xlab("Date") +
  ylab("12MMA of Median Annual Wage Growth. %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.08),breaks = c(0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ggtitle("Wage Compression is Cooling") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Low-Paid Workers are No Longer Seeing Significantly Faster Wage Growth Than High-Paid Workers") +
  theme_apricitas + theme(legend.position = c(.25,.8)) +
  scale_color_manual(name= "Median Wage Growth by Wage Quartile\n12MMA of Median 12M Wage Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("First (Lowest) Wage Quartile","Second Wage Quartile","Third Wage Quartile","Fourth (Highest) Wage Quartile")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*.08), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ATLANTA_FED_WAGE_TRACKER_DISTRIB_Graph, "Atlanta Fed Wage Tracker Distrib Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



ggsave(dpi = "retina",plot = QUITS_RATE_Graph, "Quits Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FOODSERV_REVENUE_Graph, "Food Service.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ICNSA14_Graph, "ICNSA14.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ICNSA19_Graph, "ICNSA19.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Industry_Layoffs_Graph, "Industry Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Layoffs_Graph, "Total Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PandemicLostWork_Graph, "Lost Work.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PandemicTelework_Graph, "Telework.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Flows_to_Employment_Graph, "Flows to Employment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Quits_Graph, "Total Quits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Quits_Layoffs_Graph, "Total Quits and Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


ggsave(dpi = "retina",plot = GLI_Graph, "GLI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

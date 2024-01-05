pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Childcare <- bls_api("LNU02096055", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
Childcare=Childcare[order(nrow(Childcare):1),]
Childcare$date <- seq(as.Date("2018-01-01"), as.Date("2023-08-01"), "months")

OwnIllnessNoWork <- bls_api("LNU02006735", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
OwnIllnessNoWork=OwnIllnessNoWork[order(nrow(OwnIllnessNoWork):1),]
OwnIllnessNoWork$date <- seq(as.Date("2018-01-01"), as.Date("2023-07-01"), "months")

OwnIllnessPartTime <- bls_api("LNU02028296", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
OwnIllnessPartTime=OwnIllnessPartTime[order(nrow(OwnIllnessNoWork):1),]
OwnIllnessPartTime$date <- seq(as.Date("2018-01-01"), as.Date("2023-07-01"), "months")

PandemicLostWork <- data.frame(date = seq(as.Date("2020-05-01"), as.Date("2022-02-01"), "months"), value = c(48839,40368,31281,24225,19385,15070,14805,15819,14755,13348,11391,9378,7907,6209,5150,5647,5032,3830,3640,3101,6043,4201))
Telework <- data.frame(date = seq(as.Date("2020-05-01"), as.Date("2022-02-01"), "months"), value = c(48703,44644,38194,35800,33501,31954,32737,35501,34484,33839,31553,27643,25168,22004,20271,20562,20348,18052,17553,17358,23938,20399))

EPOP55Plus <- bls_api("LNS12324230", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
EPOP55Plus=EPOP55Plus[order(nrow(EPOP55Plus):1),]
EPOP55Plus$date <- seq(as.Date("2018-01-01"), as.Date("2023-03-01"), "months")

UnpaidAbsences <- bls_api("LNU02044495", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
UnpaidAbsences=UnpaidAbsences[order(nrow(UnpaidAbsences):1),]
UnpaidAbsences$date <- seq(as.Date("2018-01-01"), as.Date("2023-03-01"), "months")

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

EPOP_L_SA <- bls_api("LNS12000060", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
EPOP_L_SA=EPOP_L_SA[order(nrow(EPOP_L_SA):1),]
EPOP_L_SA$date <- seq(as.Date("2018-01-01"), as.Date("2021-12-01"), "months")

EPOP_L_NSA <- bls_api("LNU02000060", startyear = 2018, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY"))
EPOP_L_NSA=EPOP_L_NSA[order(nrow(EPOP_L_NSA):1),]
EPOP_L_NSA$date <- seq(as.Date("2018-01-01"), as.Date("2021-12-01"), "months")

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

DISCOURAGED <- bls_api("LNS15026645", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

MARGINALLYATTACHED <- bls_api("LNS15026642", startyear = 2019) %>% #discouraged workers
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

WAREHOUSE <- fredr(series_id = "CES4349300001",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)

EPop <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL) #prime age epop data
#note: this section is only for when FRED does not update, and the dates must be changed each month
#EPopBLS <- bls_api("LNS12300060", startyear = 2018, endyear = 2022, Sys.getenv("BLS_KEY")) #pulling most recent data from BLS API for EPOP
#EPop[nrow(EPop) + 1,] = list(as.Date("2021-12-01"),"X", EPopBLS$value[1], as.Date("2021-12-01"),as.Date("2021-10-01")) #adding new row with most recent data

LAH <- fredr(series_id = "USLAH",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Leisure and Hospitality Data
U1RATE <- fredr(series_id = "U1RATE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #u1Rate Extended Unemployment Data
LGOVED <- fredr(series_id = "CES9093161101",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Local Government Education Data
PARTTIME <- fredr(series_id = "LNS12032194",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #Part Time For Economic Reasons Level
TRNSPT <- fredr(series_id = "CES4300000001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Transportation and Warehousing, All Employees
FOODSERV <- fredr(series_id = "CES7072000001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Accomodation and Food Service, All Employees
NURSING <- fredr(series_id = "CES6562310001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Accomodation and Food Service, All Employees

FOOD_EMP <- fredr(series_id = "CES7072200001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Food Service and Drinking, All Employees
FOOD_SALES <- fredr(series_id = "MRTSSM722USS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Retail Sales: Food Service and Drinking

ARTS <- fredr(series_id = "CES7071000001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Arts,Entertainment, and Recreation, All Employees
COURIERS <- fredr(series_id = "CES4349200001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees
PWD <- fredr(series_id = "LNU02374597",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

PerformingArts <- fredr(series_id = "CES7071100001",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees
MotionPictures <- fredr(series_id = "CES5051200001",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

LessThanHS <- fredr(series_id = "LNS14027659",observation_start = as.Date("1992-01-01"),realtime_start = NULL, realtime_end = NULL) #Couriers and Messengers, All Employees

UNRATEWhite <- fredr(series_id = "LNS14000003",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
UNRATEBlack <- fredr(series_id = "LNS14000006",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
UNRATEHispanic <- fredr(series_id = "LNS14000009",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
UNRATEAsian <- fredr(series_id = "LNU04032183",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

UNRATETeens <- fredr(series_id = "LNS14000012",observation_start = as.Date("1945-01-01"),realtime_start = NULL, realtime_end = NULL) 
Total_Quits <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2019-01-01")) #downloading quits data
Total_Quits18 <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2018-01-01")) #downloading quits data

UNLEVEL <- fredr(series_id = c("UNEMPLOY"), observation_start = as.Date("2019-01-01")) #unemployment data
NILFWJN <- fredr(series_id = c("NILFWJN"), observation_start = as.Date("2019-01-01")) #NILF want jobs now
NILFWJN_2002 <- fredr(series_id = c("NILFWJN"), observation_start = as.Date("2002-01-01")) #NILF want jobs now



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

PERMJOBLOSERS <- fredr(series_id = c("LNS13026638"), observation_start = as.Date("2000-01-01")) #permanent job losers
LAYOFFJOBLOSERS <- fredr(series_id = c("LNS13023653"), observation_start = as.Date("2000-01-01")) #temporary job losers

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
  geom_line(data=Black_White_Epop, aes(x=date,y= gap/100,color= "Black-White Prime Age (25-54) Employment Gap"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.125),breaks = c(0,0.025,0.05,0.075,0.1,0.125), expand = c(0,0)) +
  ggtitle("A Stronger Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Black Employment Gap is at a Record Low") +
  theme_apricitas + theme(legend.position = c(.35,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*10410), xmax = as.Date("1994-01-01")-(0.049*10410), ymin = 0-(.3*.125), ymax = 0) +
  coord_cartesian(clip = "off")

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


PERM_TEMP_JOBLOSS_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=PERMJOBLOSERS, aes(x=date,y= value/1000,color= "Permanent Job Losers"), size = 1.25)+ 
  geom_line(data=LAYOFFJOBLOSERS, aes(x=date,y= value/1000,color= "Job Losers on Layoff"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,5,10,15,20), limits = c(0,20), expand = c(0,0)) +
  ggtitle("Rapid Rebound") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Policymakers Helped End a 2001-Size Jump in Permanent Job Losses") +
  theme_apricitas + theme(legend.position = c(.70,.87)) +
  scale_color_manual(name= "Unemployment Level",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8158), xmax = as.Date("2000-01-01")-(0.049*8158), ymin = 0-(.3*20), ymax = 0) +
  coord_cartesian(clip = "off")

UNRATE_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNRATE, aes(x=date,y= value/100,color= "Unemployment Rate"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.15),breaks = c(0,0.05,0.1,0.15), expand = c(0,0)) +
  ggtitle("Back in Business") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Unemployment Rate is Near Historic Lows") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26420), xmax = as.Date("1950-01-01")-(0.049*26420), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

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

NILFUnemploy_Graph <- ggplot() + #plotting total quits
  geom_line(data=UNLEVEL, aes(x=date,y= value/1000,color= "Unemployed"), size = 1.25)+ 
  geom_line(data=NILFWJN, aes(x=date,y= value/1000,color= "Not in Labor Force, Want a Job Now"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ggtitle("Workers in Waiting") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "About as Many People are Not in the Labor Force but Want a Job as are Unemployed") +
  theme_apricitas + theme(legend.position = c(.70,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Unemployed","Not in Labor Force, Want a Job Now")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

NILF_Graph <- ggplot() + #plotting total quits
  geom_line(data=NILFWJN_2002, aes(x=date,y= value/1000,color= "Not in Labor Force, Want a Job Now"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,2,4,6,8,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("Workers in Waiting") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "About 1M More People Are Outside the Labor Force But Want A Job Now Than Pre-Pandemic") +
  theme_apricitas + theme(legend.position = c(.60,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

WAREHOUSE_Graph <- ggplot() + #plotting total quits
  geom_line(data=WAREHOUSE, aes(x=date,y= value/1000,color= "All Employees, Warehousing and Storage"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(0.5,1,1.5,2), limits = c(0.5,2), expand = c(0,0)) +
  ggtitle("Total Fulfillment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Warehousing Employment Growth Has Stalled This Year") +
  theme_apricitas + theme(legend.position = c(.40,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0.5-(.3*1.5), ymax = 0.5) +
  coord_cartesian(clip = "off")

MARGINAL_DISCOURAGED_GRAPH <- ggplot() + #plotting total quits
  geom_line(data=MARGINALLYATTACHED, aes(x=date,y= value/1000,color= "NILF, Marginally Attached"), size = 1.25)+ 
  geom_line(data=DISCOURAGED, aes(x=date,y= value/1000,color= "NILF, Marginally Attached, Discouraged Workers"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3), limits = c(0,3), expand = c(0,0)) +
  ggtitle("Workers in Waiting") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Number of Discouraged and Marginally Attached Workers is Near Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.60,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")

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
  theme_apricitas + theme(legend.position = c(.30,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Quits, Total Nonfarm","Layoffs and Discharges, Total Nonfarm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

Race_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNRATEWhite, aes(x=date,y= value/100,color= "Unemployment Rate - White"), size = 1.25)+ 
  geom_line(data=UNRATEBlack, aes(x=date,y= value/100,color= "Unemployment Rate - Black or African American"), size = 1.25)+ 
  geom_line(data=UNRATEHispanic, aes(x=date,y= value/100,color= "Unemployment Rate - Hispanic or Latino"), size = 1.25)+ 
  geom_line(data=UNRATEAsian, aes(x=date,y= value/100,color= "Unemployment Rate - Asian"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.22), expand = c(0,0)) +
  ggtitle("A Strong Recovery") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Racial Unemployment Gap is Closing, but Remains High") +
  theme_apricitas + theme(legend.position = c(.75,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Unemployment Rate - White","Unemployment Rate - Asian","Unemployment Rate - Hispanic or Latino","Unemployment Rate - Black or African American")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 0-(.3*.22), ymax = 0) +
  coord_cartesian(clip = "off")

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

LessThanHS_Graph <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=LessThanHS, aes(x=date,y= value/100,color= "Unemployment Rate - Less Than a High School Diploma, 25 Yrs. & Over"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.225), expand = c(0,0)) +
  ggtitle("Back in Action") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Unemployment for Workers Without A High School Diploma is at Record Lows") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*10989), xmax = as.Date("1992-01-01")-(0.049*10989), ymin = 0.0-(.3*0.225), ymax = 0.0) +
  coord_cartesian(clip = "off")

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
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*2000), xmax = as.Date("2018-01-01")-(0.049*2000), ymin = 0-(.3*4.5), ymax = 0) +
  coord_cartesian(clip = "off")

UnpaidAbsences_Graph <- ggplot() + #plotting the number of people out due to illnesses
  geom_line(data=UnpaidAbsences, aes(x=date,y= value/1000,color= "Wage and Salary Workers with Unpaid Absences"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,7), expand = c(0,0)) +
  ggtitle("Omicron and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Some Unpaid Absences-Which Spiked in January-Represent Misclassified Unemployed Workers") +
  theme_apricitas + theme(legend.position = c(.42,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1295), xmax = as.Date("2018-01-01")-(0.049*1295), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")
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

EPop55Plus_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=EPOP55Plus, aes(x=date,y= value/100,color= "55 yrs and Over Employment-Population Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.33,.4), expand = c(0,0)) +
  ggtitle("Methodological Mixup", subtitle = "Old Age Employment Is Higher, But the January Jump is Driven by Population Adjustments") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.68,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1295), xmax = as.Date("2018-01-01")-(0.049*1295), ymin = .33-(.3*0.07), ymax = .33) +
  coord_cartesian(clip = "off")

ICNSA14_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=PeformingArts, aes(x=date,y= value/1000,color= "Weekly Initial Unemployment Insurance Claims, NSA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Initial Claims") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,600), expand = c(0,0)) +
  ggtitle("Seasonal Layoffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Unemployment Claims Usually Surge at the End of the Holiday Season") +
  theme_apricitas + theme(legend.position = c(.52,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-06-01")-(.1861*2000), xmax = as.Date("2014-06-01")-(0.049*2000), ymin = 0-(.3*600), ymax = 0) +
  coord_cartesian(clip = "off")

Pictures_Performing_Graph <- ggplot() + #plotting spectator sports vs motion pictures
  geom_line(data=PerformingArts, aes(x=date,y= value/5.142,color= "All Employees, Performing Arts and Spectator Sports"), size = 1.25)+ 
  geom_line(data=MotionPictures, aes(x=date,y= value/4.333,color= "All Employees, Motion Picture and Sound Recording Industries"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(), limits = c(40,120), expand = c(0,0)) +
  ggtitle("Broadway and Hollywood") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Media Employment has Recovered, While in-person Performing Arts Employment Struggles") +
  theme_apricitas + theme(legend.position = c(.42,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1295), xmax = as.Date("2018-01-01")-(0.049*1295), ymin = 40-(.3*80), ymax = 40) +
  coord_cartesian(clip = "off")

ICNSA19_Graph <- ggplot() + #plotting weekly initial claims for 2014-2019
  geom_line(data=Initial_Claims_NSA_19, aes(x=date,y= value/1000000,color= "Weekly Initial Unemployment Insurance Claims, NSA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Initial Claims") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,6.5), expand = c(0,0)) +
  ggtitle("Pandemic Unemployment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "COVID Trends Have Overpowered Normal Seasonal Variance") +
  theme_apricitas + theme(legend.position = c(.52,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*720), xmax = as.Date("2020-01-01")-(0.049*720), ymin = 0-(.3*6.5), ymax = 0) +
  coord_cartesian(clip = "off")

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

Childcare_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=Childcare, aes(x=date,y= value,color= "Employed But Not At Work, Childcare Problems"), size = 1.25)+ 
  xlab("Date") +
  ylab("Thousands") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,120), expand = c(0,0)) +
  ggtitle("The Childcare Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Chilcare Issues are Interfering With People's Ability to Work During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.32,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

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

EPOP_SA_NSA_Graph <- ggplot() + #plotting total discharges
  geom_line(data=EPOP_L_NSA, aes(x=date,y= value/1000,color= "Prime Age (25-54) Employment-Population Level, NSA"), size = 1.25)+ 
  geom_line(data=EPOP_L_SA, aes(x=date,y= value/1000,color= "Prime Age (25-54) Employment-Population Level, SA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(85,90,95,100,105), limits = c(85,107), expand = c(0,0)) +
  ggtitle("Reason for the Season") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Seasonally Adjusted Employment Levels are About 700k Below Non-Seasonally Adjusted Levels") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1295), xmax = as.Date("2018-01-01")-(0.049*1395), ymin = 85-(.3*22), ymax = 85) +
  coord_cartesian(clip = "off")

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

PARTTIME_Graph <- ggplot() + #plotting employed part time for economic reasons 
  geom_line(data=PARTTIME, aes(x=date,y= value/1000,color= "Employed Part-time for Economic Reasons"), size = 1.25)+ 
  xlab("Date") +
  ylab("Thousands of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1),limits = c(0,12.5), expand = c(0,0)) + #paste(c(0, 2.5, 5,7.5, 10,12.5), "M")
  ggtitle("Going Full Time") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Underemployed Population is Decreasing") +
  theme_apricitas + theme(legend.position = c(.60,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7200), xmax = as.Date("2000-01-01")-(0.049*7200), ymin = 0-(.3*12.5), ymax = 0) +
  coord_cartesian(clip = "off")

LGOVED_Graph <- ggplot() + #plotting local government education employment
  geom_line(data=LGOVED, aes(x=date,y= value/1000,color= "All Employees, Local Government Education"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(7,7.5,8,8.5), limits = c(7,8.5), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-10-01"))) +
  ggtitle("Back to School?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Local Government Education Employment Still Lags") +
  theme_apricitas + theme(legend.position = c(.65,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 7-(.3*1.5), ymax = 7) +
  coord_cartesian(clip = "off")

FOODSERV_Graph <- ggplot() + #plotting food service and accommodation employment
  geom_line(data=FOODSERV, aes(x=date,y= value/1000,color= "All Employees, Accommodation and Food Services"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(7.5,10,12.5,15), limits = c(7,15.5), expand = c(0,0)) +
  ggtitle("The Big Takeaway") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Food Service Employment is Stagnating") +
  theme_apricitas + theme(legend.position = c(.65,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 7-(.3*8.5), ymax = 7) +
  coord_cartesian(clip = "off")

TRNSPT_Graph <- ggplot() + #plotting transportation and warehousing
  geom_line(data=TRNSPT, aes(x=date,y= value/1000,color= "All Employees, Transportation and Warehousing"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(5,5.5,6), limits = c(5,6.5), expand = c(0,0)) +
  ggtitle("The Goods Boom") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Transportation and Warehousing has Increased as Americans Buy More Goods") +
  theme_apricitas + theme(legend.position = c(.55,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 5-(.3*1.5), ymax = 5) +
  coord_cartesian(clip = "off")

Nursing_Graph <- ggplot() + #plotting transportation and warehousing
  geom_line(data=NURSING, aes(x=date,y= value/1000,color= "All Employees, Nursing Care Facilities"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(1.3,1.4,1.5,1.6,1.7), limits = c(1.3,1.7), expand = c(0,0)) +
  ggtitle("Morbid Realities") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Passing of Many Nursing Home Residents Has Reduced Employment in the Sector") +
  theme_apricitas + theme(legend.position = c(.65,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 1.3-(.3*0.4), ymax = 1.3) +
  coord_cartesian(clip = "off")

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

ARTS_Graph <- ggplot() + #plotting arts employment
  geom_line(data=ARTS, aes(x=date,y= value/1000,color= "All Employees, Arts, Entertainment, and Recreation"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(1.5,2,2.5), limits = c(1.1,2.75), expand = c(0,0)) +
  ggtitle("Hey, the Big Artiste...") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in the Arts is Rebounding, but Still Remains Below Pre-Pandemic Highs") +
  theme_apricitas + theme(legend.position = c(.6,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = 1.1-(.3*1.65), ymax = 1.1) +
  coord_cartesian(clip = "off")

Couriers_Graph <- ggplot() + #plotting couriers and messengers employment
  geom_line(data=COURIERS, aes(x=date,y= value/1000,color= "All Employees, Couriers and Messengers"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(.8,.9,1,1.1), limits = c(.75,1.15), expand = c(0,0)) +
  ggtitle("Premium Rush") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment for Couriers, Which Includes Food Delivery and Short-Haul Trucking, is Booming") +
  theme_apricitas + theme(legend.position = c(.6,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1011), xmax = as.Date("2019-01-01")-(0.049*1011), ymin = .75-(.3*.4), ymax = .75) +
  coord_cartesian(clip = "off")

PWD_Graph <- ggplot() + #plotting employment of people with disabilities
  geom_line(data=PWD, aes(x=date,y= value/100,color= "Employment-Population Ratio - With a Disability, 16 Years and Over"), size = 1.25)+ 
  xlab("Date") +
  ylab("Employment-Population Ratio, Percent") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(.15,.175,.20,.25), limits = c(.15,.25), expand = c(0,0)) +
  ggtitle("Employment Rate, People With Disabilities") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment Rates for People With Disabilities are Up") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today() - as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today() - as.Date("2019-01-01"))), ymin = .15-(.3*.1), ymax = .15) +
  coord_cartesian(clip = "off")

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

ECI_WAG <- bls_api("CIS2020000000000I", startyear = 2006, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  mutate(qoqpctann = ((1+(value-dplyr::lead(value, 1))/dplyr::lead(value, 1))^4)-1) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2006-01-01"), as.Date("2022-10-01"), by = "quarter")))

ECI_WAG_EX_INC <- bls_api("CIU2020000000710I", startyear = 2006, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  mutate(qoqpctann = ((1+(value-dplyr::lead(value, 1))/dplyr::lead(value, 1))^4)-1) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2006-01-01"), as.Date("2022-09-01"), by = "quarter")))

ECI_WAG_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=ECI_WAG, aes(x=date,y= annualpct ,color= "Annualized Percent Growth"), size = 1.25) +
  geom_line(data=ECI_WAG, aes(x=date,y= qoqpctann ,color= "Quarter-on-Quarter Percent Growth, Annualized"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07), breaks = c(0,0.03,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Core Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "ECI, Private Industry Wages and Salaries Growth Was in Line With Expectations") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "ECI Private Sector Wages and Salaries",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

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
RESIDENTIAL_BUILDING <- fredr(series_id = "CES2023610001",observation_start = as.Date("1998-01-01"),realtime_start = NULL, realtime_end = NULL)

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

UNRATE <- fredr(series_id = "UNEMPLOY",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Unemployed")
NILF <- fredr(series_id = "NILFWJN",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Not in Labor Force but Want a Job Now")
PARTTIME <- fredr(series_id = "LNS12032194",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Part Time for Economic Reasons")
LABOR_FORCE <- fredr(series_id = "CLF16OV",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Civilian Labor Force")

UNDEREMPLOY <- rbind(UNRATE,NILF,PARTTIME,LABOR_FORCE) %>%
  select(-series_id,-realtime_start,-realtime_end) %>%
  pivot_wider() %>%
  mutate(Unemployed = Unemployed/(`Civilian Labor Force`+`Not in Labor Force but Want a Job Now`)) %>%
  mutate(`Not in Labor Force but Want a Job Now` = `Not in Labor Force but Want a Job Now`/(`Civilian Labor Force`+`Not in Labor Force but Want a Job Now`)) %>%
  mutate(`Part Time for Economic Reasons` = `Part Time for Economic Reasons`/(`Civilian Labor Force`+`Not in Labor Force but Want a Job Now`)) %>%
  select(-`Civilian Labor Force`) %>%
  #mutate(Aggregate = Unemployed + `Part Time for Economic Reasons` + `Not in Labor Force but Want a Job Now`) %>%
  pivot_longer(cols = Unemployed:`Part Time for Economic Reasons`)

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

#Yoy Change in Employment
EMPLOY_TRADE_TRANSP_UTIL <- fredr(series_id = "USTPU",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Trade, Transportation, and Utilities")
EMPLOY_PROF_BUSINESS_SERVICES <- fredr(series_id = "USPBS",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Professional and Business Services")
EMPLOY_EDU_HEALTH_SERVICES <- fredr(series_id = "USEHS",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Education and Health Services")
EMPLOY_LEISURE_HOSPITALITY <- fredr(series_id = "USLAH",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Leisure and Hospitality") 
EMPLOY_GOODS <- fredr(series_id = "USGOOD",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Goods-Producing")
EMPLOY_GOVT <- fredr(series_id = "USGOVT",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1") %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Government")
EMPLOY_OTHER_SERVICES <- rbind(fredr(series_id = "USINFO",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1"),
                               fredr(series_id = "USFIRE",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1"),
                               fredr(series_id = "USSERV",observation_start = as.Date("2019-01-01"), realtime_end = NULL, units = "ch1")) %>%
                        select(date,series_id,value) %>%
                        pivot_wider(names_from = series_id) %>%
                        transmute(date, value = USINFO + USFIRE + USSERV, series_id = "Other Services Incl. Finance & Info")

EMPLOY_GROWTH_YOY <- rbind(EMPLOY_TRADE_TRANSP_UTIL,EMPLOY_PROF_BUSINESS_SERVICES,EMPLOY_EDU_HEALTH_SERVICES,EMPLOY_LEISURE_HOSPITALITY,EMPLOY_GOODS,EMPLOY_GOVT,EMPLOY_OTHER_SERVICES) %>%
  mutate(series_id = factor(series_id,levels = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Info")))
  
EMPLOY_GROWTH_YOY_graph <- ggplot(data = EMPLOY_GROWTH_YOY, aes(x = date, y = value/1000, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Jobs Growth, YoY, Millions") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(-20,-10,0,10), limits = c(-20.5,15), expand = c(0,0)) +
  ggtitle("The Shape of Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Job Growth is Broad-Based, With All Major Industries Posting Gains") +
  theme_apricitas + theme(legend.position = c(.825,.30)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Info")) +
  theme(legend.text = element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -20-(.3*35.5), ymax = -20.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOY_GROWTH_YOY_graph, "Employ Growth YoY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


EMPLOY_TRADE_TRANSP_UTIL_IND <- bls_api("CES4000000001", startyear = 2020) %>% #headline CES NSA INDEXED TO JAN 22
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  mutate(value = (value-value[1])) %>%
  select(date, value) %>%
  mutate(series_id = "Trade, Transportation, and Utilities")
  
EMPLOY_PROF_BUSINESS_SERVICES_IND <-

EMPLOY_EDU_HEALTH_SERVICES_IND <-
  
EMPLOY_LEISURE_HOSPITALITY_IND <-
  
#Employ Index
EMPLOY_TRADE_TRANSP_UTIL_IND <- fredr(series_id = "USTPU",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Trade, Transportation, and Utilities") %>%
  mutate(value = (value-value[1]))
EMPLOY_PROF_BUSINESS_SERVICES_IND <- fredr(series_id = "USPBS",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Professional and Business Services") %>%
  mutate(value = (value-value[1]))
EMPLOY_EDU_HEALTH_SERVICES_IND <- fredr(series_id = "USEHS",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Private Education and Health Services") %>%
  mutate(value = (value-value[1]))
EMPLOY_LEISURE_HOSPITALITY_IND <- fredr(series_id = "USLAH",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Leisure and Hospitality") %>%
  mutate(value = (value-value[1]))
EMPLOY_GOODS_IND <- fredr(series_id = "USGOOD",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Goods-Producing") %>%
  mutate(value = (value-value[1]))
EMPLOY_GOVT_IND <- fredr(series_id = "USGOVT",observation_start = as.Date("2020-01-01"), realtime_end = NULL) %>%
  select(date, value, series_id) %>%
  mutate(series_id = "Government") %>%
  mutate(value = (value-value[1]))
EMPLOY_OTHER_SERVICES_IND <- rbind(fredr(series_id = "USINFO",observation_start = as.Date("2020-01-01"), realtime_end = NULL),
                               fredr(series_id = "USFIRE",observation_start = as.Date("2020-01-01"), realtime_end = NULL),
                               fredr(series_id = "USSERV",observation_start = as.Date("2020-01-01"), realtime_end = NULL)) %>%
  select(date,series_id,value) %>%
  pivot_wider(names_from = series_id) %>%
  transmute(date, value = USINFO + USFIRE + USSERV, series_id = "Other Services Incl. Finance & Information") %>%
  mutate(value = (value-value[1]))

EMPLOY_GROWTH_IND <- rbind(EMPLOY_TRADE_TRANSP_UTIL_IND,EMPLOY_PROF_BUSINESS_SERVICES_IND,EMPLOY_EDU_HEALTH_SERVICES_IND,EMPLOY_LEISURE_HOSPITALITY_IND,EMPLOY_GOODS_IND,EMPLOY_GOVT_IND,EMPLOY_OTHER_SERVICES_IND) %>%
  mutate(series_id = factor(series_id,levels = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Private Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Information")))

EMPLOY_GROWTH_IND_graph <- ggplot(data = EMPLOY_GROWTH_IND, aes(x = date, y = value/1000, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(-20,-15,-10,-5,0,5), limits = c(-22,5.5), expand = c(0,0)) +
  ggtitle("The Shape of Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "There are Now More Jobs Than Pre-Pandemic—and Most Sectors Have Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.725,.325)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Leisure and Hospitality","Trade, Transportation, and Utilities","Goods-Producing","Private Education and Health Services","Professional and Business Services","Government","Other Services Incl. Finance & Information")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -22-(.3*27.5), ymax = -22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(0,10), breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Growth Since Jan 2022, NSA") +
  ggtitle("Detailed QCEW Data Points to Downward\nCES (Nonfarm Payrolls) Revisions") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "QCEW Data Suggests CES Data Will Be Revised Downward Slightly at the End of This Year") +
  theme_apricitas + theme(legend.position = c(.30,.8)) +
  scale_color_manual(name= "Non-Seasonally Adjusted Job Growth",values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-15")-(.1861*(today()-as.Date("2022-01-15"))), xmax = as.Date("2022-01-15")-(0.049*(today()-as.Date("2022-01-15"))), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CES_CPS_QCEW_Graph, "CES CPS QCEW Comparison.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#truck and warehouse employment
TRUCK_EMPLOY <- fredr(series_id = "CES4348400001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
WAREHOUSE_EMPLOY <- fredr(series_id = "CES4349300001",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

TRUCK_WAREHOUSE_GRAPH <- ggplot() + #plotting local government education employment
  geom_line(data=TRUCK_EMPLOY, aes(x=date,y= value/1000,color= "All Employees, Truck Transportation"), size = 1.25)+ 
  geom_line(data=WAREHOUSE_EMPLOY, aes(x=date,y= value/1000,color= "All Employees, Warehousing and Storage"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8), limits = c(1.1,1.8), expand = c(0,0)) +
  ggtitle("The Transport Taper") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Warehousing Employment is Falling and Trucking Employment is Stagnating") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 1.1-(.3*0.7), ymax = 1.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TRUCK_WAREHOUSE_GRAPH, "Truck Warehouse Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
#temp help services employment
EMPLOY_TEMP_HELP_SERVICES <- fredr(series_id = "TEMPHELPS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

EMPLOY_TEMP_HELP_SERVICES_GRAPH <- ggplot() + #plotting local government education employment
  geom_line(data=EMPLOY_TEMP_HELP_SERVICES, aes(x=date,y= value/1000,color= "All Employees, Temporary Help Services"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(2,2.5,3), limits = c(1.9,3.25), expand = c(0,0)) +
  ggtitle("Temporary Trouble") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Temporary Help Services Employment, an Important Leading Indicator, is Slightly Dropping") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 1.9-(.3*1.35), ymax = 1.9) +
  coord_cartesian(clip = "off")



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

TECH_EMPLOY_GROWTH_IND_graph <- ggplot(data = TECH_EMPLOY_GROWTH_IND, aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,200,400,600), limits = c(-75,600), expand = c(0,0)) +
  ggtitle("The Tech Boom and Techcession") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "So Far, Employment in the Tech Sector Has Stalled But Not Really Contracted") +
  theme_apricitas + theme(legend.position = c(0.35,0.82), legend.key.size = unit(0.5,"cm"), legend.spacing.y = unit(0, "cm")) +
  scale_fill_manual(name= "Change in Employment since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Software Publishers","Custom Computer Programming Services","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design Services","Web Search Portals and All Other Information Services","Streaming Services, Social Networks, & Related")) +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -75-(.3*600), ymax = -75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOY_GROWTH_IND_graph, "Tech Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



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
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Wage Growth Looks to Be Decelerating Now") +
  theme_apricitas + theme(legend.position = c(.42,.72)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Wages, Employment Cost Index, Private (Composition Adjusted)","Average Hourly Earnings, Private (Not Composition Adjusted)")) +
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

SF_CONSTRUCTION <- bls_api("CES2023611501", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Single-Family Construction") %>%
  select(date,value,name)
MF_CONSTRUCTION <- bls_api("CES2023611601", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Multi-Family Construction") %>%
  select(date,value,name)
HFS_CONSTRUCTION <- bls_api("CES2023611701", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "For-Sale Builders") %>%
  select(date,value,name)
REMODEL_CONSTRUCTION <- bls_api("CES2023611801", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Residential Remodelers") %>%
  select(date,value,name)

CONSTRUCTION_RBIND <- rbind(SF_CONSTRUCTION,MF_CONSTRUCTION,HFS_CONSTRUCTION,REMODEL_CONSTRUCTION)

CONSTRUCTION_TOTAL <- bls_api("CES2023610001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
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
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-125,-100,-75,-50,-25,0,25,50,75,100,125), limits = c(-125,125), expand = c(0,0)) +
  ggtitle("The Shape of Construction Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "New Jobs Come From SF Construction and Remodelers Vulnerable to Higher Rates") +
  theme_apricitas + theme(legend.position = c(.625,.25)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-11,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Single-Family Construction","Residential Remodelers","Multi-Family Construction","For-Sale Builders")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -125-(.3*250), ymax = -125) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_GROWTH_IND_graph, "Construction Growth Ind.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


RES_CONSTRUCTION <- bls_api("CES2023610001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Residential Construction") %>%
  select(date,value,name) %>%
  mutate(value = value-value[nrow(.)])

RES_TRADE_CONTRACTORS <- bls_api("CES2023800101", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Residential Specialty Trade Contractors") %>%
  select(date,value,name) %>%
  mutate(value = value-value[nrow(.)])

RES_LESSOR <- bls_api("CES5553111001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_PROPERTY_MANAGER <- bls_api("CES5553131101", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_AGENTS <- bls_api("CES5553120001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_APPRAISERS <- bls_api("CES5553132001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
RES_OTHER <- bls_api("CES5553139001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

REAL_ESTATE_RBIND <- rbind(RES_LESSOR,RES_PROPERTY_MANAGER,RES_AGENTS,RES_APPRAISERS,RES_OTHER) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES5553111001`+`CES5553131101`+`CES5553120001`+`CES5553132001`+`CES5553139001`) %>%
  mutate(name = "Property Managers, Real Estate Agents, Appraisers, Lessors, & Related") %>%
  mutate(value = value-value[nrow(.)])

CONSTRUCTION_MATERIALS_WHOLE <- bls_api("CES4142330001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
HOUSEHOLD_APPLIANCES_WHOLE <- bls_api("CES4142360001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
FURNITURE_WHOLE <- bls_api("CES4142320001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
HARDWARE_MATERIALS_WHOLE <- bls_api("CES4142370001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CONSTRUCTION_MACHINERY_WHOLE <- bls_api("CES4142381001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

WHOLESALE_RBIND <- rbind(CONSTRUCTION_MATERIALS_WHOLE,HOUSEHOLD_APPLIANCES_WHOLE,FURNITURE_WHOLE,HARDWARE_MATERIALS_WHOLE,CONSTRUCTION_MACHINERY_WHOLE) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES4142330001`+`CES4142360001`+`CES4142320001`+`CES4142370001`+`CES4142381001`) %>%
  mutate(name = "Housing Related Merchant Wholesalers") %>%
  mutate(value = value-value[nrow(.)])

BUILDING_GARDEN_SUPPLIES_RETAIL <- bls_api("CES4244400001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
FURNITURE_HOME_ELECTRONICS_APPLIANCE_RETAIL <- bls_api("CES4244900001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
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
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "k"), breaks = c(-1000,-750,-500,-250,0,250,500,750), limits = c(-1100,750), expand = c(0,0)) +
  ggtitle("The Shape of Housing Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Housing Related Employment Has Stalled—Importantly in Credit, Construction, and Contractors") +
  theme_apricitas + theme(legend.position = c(.625,.25), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Housing-Related Employment, Change Since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Residential Specialty Trade Contractors","Architectural, Engineering, and Interior Design Services","Residential Construction","Property Managers, Real Estate Agents, Appraisers, Lessors, & Related","Housing Related Manufacturing, Mining, and Logging","Housing Related Creditors and Loan Brokers","Housing Related Retailers and Wholesalers")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -1100-(.3*1850), ymax = -1100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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

AL <- fredr(series_id = "ALNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Alabama")
AK <- fredr(series_id = "AKNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Alaska")
AZ <- fredr(series_id = "AZNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Arizona")
AR <- fredr(series_id = "ARNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Arkansas")
CA <- fredr(series_id = "CANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "California")
CO <- fredr(series_id = "CONA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Colorado")
CT <- fredr(series_id = "CTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Connecticut")
DE <- fredr(series_id = "DENA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Delaware")
DC <- fredr(series_id = "DCNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "District of Columbia")
FL <- fredr(series_id = "FLNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Florida")
GA <- fredr(series_id = "GANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Georgia")
HI <- fredr(series_id = "HINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Hawaii")
ID <- fredr(series_id = "IDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Idaho")
IL <- fredr(series_id = "ILNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Illinois")
IN <- fredr(series_id = "INNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Indiana")
IA <- fredr(series_id = "IANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Iowa")
KS <- fredr(series_id = "KSNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Kansas")
KY <- fredr(series_id = "KYNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Kentucky")
LA <- fredr(series_id = "LANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Louisiana")
ME <- fredr(series_id = "MENA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Maine")
MD <- fredr(series_id = "MDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Maryland")
MA <- fredr(series_id = "MANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Massachusetts")
MI <- fredr(series_id = "MINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Michigan")
MN <- fredr(series_id = "MNNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Minnesota")
MS <- fredr(series_id = "MSNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Mississippi")
MO <- fredr(series_id = "MONA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Missouri")
MT <- fredr(series_id = "MTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Montana")
NE <- fredr(series_id = "NENA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Nebraska")
NV <- fredr(series_id = "NVNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Nevada")
NH <- fredr(series_id = "NHNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "New Hampshire")
NJ <- fredr(series_id = "NJNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "New Jersey")
NM <- fredr(series_id = "NMNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "New Mexico")
NY <- fredr(series_id = "NYNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "New York")
NC <- fredr(series_id = "NCNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "North Carolina")
ND <- fredr(series_id = "NDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "North Dakota")
OH <- fredr(series_id = "OHNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Ohio")
OK <- fredr(series_id = "OKNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Oklahoma")
OR <- fredr(series_id = "ORNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Oregon")
PA <- fredr(series_id = "PANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Pennsylvania")
RI <- fredr(series_id = "RINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Rhode Island")
SC <- fredr(series_id = "SCNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "South Carolina")
SD <- fredr(series_id = "SDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "South Dakota")
TN <- fredr(series_id = "TNNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Tennessee")
TX <- fredr(series_id = "TXNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Texas")
UT <- fredr(series_id = "UTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Utah")
VT <- fredr(series_id = "VTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Vermont")
VA <- fredr(series_id = "VANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Virginia")
WA <- fredr(series_id = "WANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Washington")
WV <- fredr(series_id = "WVNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "West Virginia")
WI <- fredr(series_id = "WINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Wisconsin")
WY <- fredr(series_id = "WYNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Wyoming")
PR <- fredr(series_id = "SMS72000000000000001",observation_start = as.Date("2020-01-01")) %>%
  mutate(series_id = "PR") %>%
  mutate(name = "Puerto Rico")
VI <- fredr(series_id = "SMS78000000000000001",observation_start = as.Date("2020-01-01")) %>%
  mutate(series_id = "VI") %>%
  mutate(name = "Virgin Islands")

JOB_GROWTH <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, PR, VI) %>%
  select(date, value, name, series_id) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(CAGR = (value / first(value)) ^ (1 / ((row_number() - 1) / 12)) - 1) %>%
  mutate(Growth = ((value - first(value)) / first(value))) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states_job_growth <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_job_growth <- states_job_growth %>%
  mutate(name = state_name)

states_job_growth <- left_join(states_job_growth, JOB_GROWTH, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_job_growth, .) %>%
  st_centroid()

states_territories_labls <- get_urbn_labels(map = "territories") %>%
  left_join(states_job_growth, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

JOB_GROWTH_STATE <- states_job_growth  %>%
  ggplot(aes(fill = Growth_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states_job_growth, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1.5%", "1.5-3%", "3-4.5%", "4.5-6%","6%+")) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("PR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("VI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle("    Change in Nonfarm Payrolls Since Jan 2020") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE, "Job Growth By State Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PAYEMS_2020 <- fredr(series_id = "PAYEMS",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Rest of America")

TX_FL_US_PAYEMS <- rbind(FL,TX,PAYEMS_2020) %>%
  select(date, value, name) %>%
  group_by(name) %>%
  mutate(value = (value - first(value))) %>%
  pivot_wider() %>%
  mutate(`Rest of America` = `Rest of America`-Texas-Florida) %>%
  drop_na() %>%
  pivot_longer(cols = Florida:`Rest of America`) %>%
  mutate(name = factor(name, levels = rev(c("Texas", "Florida", "Rest of America"))))
  
TX_FL_US_PAYEMS_graph <- ggplot(data = TX_FL_US_PAYEMS, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "M"), breaks = c(-20,-15,-10,-5,0,5), limits = c(-22,5), expand = c(0,0)) +
  ggtitle("The State of US Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Texas and Florida Have Created the Most New Jobs Since the Pandemic") +
  theme_apricitas + theme(legend.position = c(.625,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Nonfarm Payrolls, Change Since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Texas","Florida","Rest of America")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -22-(.3*27), ymax = -22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_FL_US_PAYEMS_graph, "TX FL Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Adding Puerto Rico
  
JOB_GROWTH <- JOB_GROWTH %>%
  mutate(series_id = gsub("NA","",series_id)) %>%
  .[order(.$Growth), ] %>%
  mutate(series_id = factor(series_id, levels = .$series_id))

JOB_GROWTH_STATE_BAR_GRAPH <- ggplot(data = JOB_GROWTH, aes(x = series_id, y = Growth, fill = "Nonfarm Payrolls Growth Since Jan 2020")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +#plotting Deposits, Insured and Uninsured
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("State") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.05,0,0.05,0.10), limits = c(-.12,.11), expand = c(0,0)) +
  ggtitle("Job Growth By State/Territory") +
  labs(caption = "Graph created by @JosephPolitano using US Census data", subtitle = "Most States Have Now Seen Payrolls Fully Recover From the Pandemic") +
  theme_apricitas + theme(legend.position = c(.67,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  theme(axis.text.x = element_text(size = 7))

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE_BAR_GRAPH, "Growth State Bar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PAYEMS_MONTHLY <- fredr(series_id = "PAYEMS", observation_start = as.Date("2020-10-01"), units = "chg") %>%
  mutate(THREEMMA = c(0,0, rollmean(value, k = 3))) %>%
  filter(date >= as.Date("2021-01-01"))

PAYEMS_MONTHLY_GRAPH <- ggplot(data = PAYEMS_MONTHLY, aes(x = date, y = value/1000, fill = "Nonfarm Payrolls, Monthly Growth")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +#plotting Deposits, Insured and Uninsured
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_line(data = PAYEMS_MONTHLY, aes(x=date, y = THREEMMA/1000, color = "3 Month Moving Average"), size = 2) +
  xlab("State") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.25, suffix = "M"), breaks = c(0,.25,.5,.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("US Job Growth Is Slowing") +
  labs(caption = "Graph created by @JosephPolitano using Bureau of Labor Statistics data Via Guy Berger", subtitle = "US Job Growth Has Decelerated Significantly Over the Last Two Years") +
  theme_apricitas + theme(legend.position = c(.725,.75)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-11,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
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


ggsave(dpi = "retina",plot = QUITS_RATE_Graph, "Quits Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EMPLOY_TEMP_HELP_SERVICES_GRAPH, "Employ Temp Help Services.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EPop_Graph, "EPopUSA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LAH_Graph, "LAH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = U1RATE_Graph, "U1RATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PARTTIME_Graph, "Part Time for Economic Reasons.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LGOVED_Graph, "LGOVED.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FOODSERV_Graph, "FOODSERV.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Nursing_Graph, "Nursing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FOODSERV_REVENUE_Graph, "Food Service.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ARTS_Graph, "Arts.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Couriers_Graph, "Couriers.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PWD_Graph, "People With Disabilities.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = TRNSPT_Graph, "Transport.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Childcare_Graph, "Childcare.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ICNSA14_Graph, "ICNSA14.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ICNSA19_Graph, "ICNSA19.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Industry_Layoffs_Graph, "Industry Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Layoffs_Graph, "Total Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EPOP_SA_NSA_Graph, "EPOP NSA SA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = OwnIllness_Graph, "OwnIllness.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PandemicLostWork_Graph, "Lost Work.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PandemicTelework_Graph, "Telework.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EPop55Plus_Graph, "Epop 55.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = UnpaidAbsences_Graph, "Unpaid Absences.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Pictures_Performing_Graph, "Pictures Performing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LessThanHS_Graph, "Less than HS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Race_Graph, "Race.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Teens_Graph, "Teens.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = PERM_TEMP_JOBLOSS_Graph, "Permanent V Temporary Job Loss.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = NILFUnemploy_Graph, "Not In Labor Force vs Unemployment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = UNRATE_Graph, "UNRATE graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Flows_to_Employment_Graph, "Flows to Employment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Quits_Graph, "Total Quits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Quits_Layoffs_Graph, "Total Quits and Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Black_White_Employment_Graph, "Black White Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Black_White_Epop_graph, "Black White Epop.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ggsave(dpi = "retina",plot = NILF_Graph, "NILF 2002.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = MARGINAL_DISCOURAGED_GRAPH, "Marginal Discouraged.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ggsave(dpi = "retina",plot = Male_Female_Epop, "Male Female Epop.png", type = "cairo-png",width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = WAREHOUSE_Graph, "WareHouse.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ECI_WAG_Graph, "ECI WAG.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ECI_WAG_Ex_Inc_Graph, "ECI WAG ex Inc.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GLI_Graph, "GLI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RESIDENTIAL_BUILDING_Graph, "Residential Building Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = UNDEREMPLOY_Graph, "Underemploy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

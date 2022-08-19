pacman::p_load(RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("quantmod")
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)


CPI <- bls_api("CUSR0000SA0", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CPIUSEDCARS <- bls_api("CUSR0000SETA02", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi used cars data
CPINEWCARS <- bls_api("CUSR0000SETA01", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi new cars data
CPIRENT <- bls_api("CUSR0000SEHA", startyear = 2019, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi rent data
CPIORENT <- bls_api("CUSR0000SEHC", startyear = 2019, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi owners equivalent rent
CPIGAS <- bls_api("CUSR0000SETB01", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi gasoline data
CPIPIPEDGAS <- bls_api("CUSR0000SEHF02", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi piped gas data
CPILODGINGHOME <- bls_api("CUSR0000SEHB", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi lodging away from home data
CPIDURABLE <- bls_api("CUSR0000SAD", startyear = 2005, endyear = 2022, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi durables data
CPISERVICE <- bls_api("CUSR0000SAS", startyear = 2005, endyear = 2022, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #cpi services data
CPIBREAD <- bls_api("CUUR0000SEFB01", startyear = 2005, endyear = 2022, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) #CPI Bread data

CPIRent <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"), units = "pc1")
CPIServicesLessRent <- fredr(series_id = "CUSR0000SASL2RS",observation_start = as.Date("2019-01-01"), units = "pc1")

CPIRENTmonth <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"), units = "pch")
CPIOERmonth <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"), units = "pch")

US_Regular_All_Form_Gas <- fredr(series_id = "GASREGW",observation_start = as.Date("2019-01-01"))


CPIRENT$calculations <- str_sub(CPIRENT$calculations, start= -3) #correcting percent growth calculations to remove excess data and convert to numeric
CPIRENT$calculations <- as.numeric(CPIRENT$calculations)
CPIORENT$calculations <- str_sub(CPIORENT$calculations, start= -3)
CPIORENT$calculations <- as.numeric(CPIORENT$calculations)


CPI=CPI[order(nrow(CPI):1),] #blscraper puts rows in reverse order, so I am reversing them again here
#CPIUSEDCARS=CPIUSEDCARS[order(nrow(CPIUSEDCARS):1),]
#CPINEWCARS=CPINEWCARS[order(nrow(CPINEWCARS):1),]
#CPIRENT=CPIRENT[order(nrow(CPIRENT):1),]
#CPIORENT=CPIORENT[order(nrow(CPIORENT):1),]
#CPIGAS=CPIGAS[order(nrow(CPIGAS):1),]
#CPIPIPEDGAS=CPIPIPEDGAS[order(nrow(CPIPIPEDGAS):1),]
#CPILODGINGHOME=CPILODGINGHOME[order(nrow(CPILODGINGHOME):1),]
#CPIDURABLE=CPIDURABLE[order(nrow(CPIDURABLE):1),]
#CPISERVICE=CPISERVICE[order(nrow(CPISERVICE):1),]


#CPI$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months") #adding a "date" sequence. Note: manual backup, do not use unless initial mutates do not work
#CPIUSEDCARS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPINEWCARS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPIRENT$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPIORENT$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPIGAS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPIPIPEDGAS$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPILODGINGHOME$date <- seq(as.Date("2019-01-01"), as.Date("2021-12-01"), "months")
#CPIDURABLE$date <- seq(as.Date("2005-01-01"), as.Date("2021-12-01"), "months")
#PISERVICE$date <- seq(as.Date("2005-01-01"), as.Date("2021-12-01"), "months")

#FRED Backups
#CPIUSEDCARS <- fredr(series_id = "CUSR0000SETA02",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#CPINEWCARS <- fredr(series_id = "CUSR0000SETA01",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") 
#CPIOERENT <- fredr(series_id = "CUSR0000SEHC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") 
#CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
PPIIDC <- fredr(series_id = "PPIIDC",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
#PROFIT_MARGIN <- fredr(series_id = "A463RD3Q052SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

PCE <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #personal consumption expenditured data
PCEGD <- fredr(series_id = "DGDSRC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pce goods data
PCESV <- fredr(series_id = "PCES",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pce services data
CPIGAS <- fredr(series_id = "CUUR0000SETB01",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #WTI oil prices data
CPIBreadCereal <- fredr(series_id = "CUUR0000SAF111",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL)
FIVEYEARBREAKEVEN <- fredr(series_id = "T5YIE",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN <- fredr(series_id = "T5YIFR",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

FIVEYEARBREAKEVEN <- drop_na(FIVEYEARBREAKEVEN)
FIVEYEARFWDBREAKEVEN <- drop_na(FIVEYEARFWDBREAKEVEN)

FIVEYEARBREAKEVEN2019 <- fredr(series_id = "T5YIE", observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN2019 <- fredr(series_id = "T5YIFR",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

FIVEYEARBREAKEVEN2019 <- drop_na(FIVEYEARBREAKEVEN2019)
FIVEYEARFWDBREAKEVEN2019 <- drop_na(FIVEYEARFWDBREAKEVEN2019)

CPILFESL_Monthly <- fredr(series_id = "CPILFESL",observation_start = as.Date("2019-01-01"), units = "pch")
CPIAUCSL_Monthly <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"), units = "pch")

WTI <- tq_get("CL=F", from = "2019-01-01")
Wheat <- tq_get("KE=F", from = "2005-01-01")
Corn <- tq_get("ZC=F", from = "2020-01-01")
Palladium <- tq_get("PA=F", from = "2020-01-01")

#ECISERV <- fredr(series_id = "CIS201S000000000I",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading Employment Cost Index (ECI) services data
#ECIGOOD <- fredr(series_id = "CIU201G000000000I",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading ECI goods data
#PCESERV <- fredr(series_id = "DSERRG3M086SBEA",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading PCE services
#PCEGOOD <- fredr(series_id = "DGDSRG3M086SBEA",observation_start = as.Date("2018-01-01"),observation_end = as.Date("2021-10-30"), units = "pc1") #downloading PCE goods

CPI_SERVICES <- fredr(series_id = "CUSR0000SAS",observation_start = as.Date("2018-01-01"))
CPI_SERVICESTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(CPI_SERVICES$date, n=1), "months")), trend = 330.424*1.002466^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(CPI_SERVICES$date, n=1), by = 'month')) - 1))) #3% annual trend variable
         
         
#Wage_Price_Merge <- do.call("rbind", list(ECISERV,ECIGOOD,PCESERV,PCEGOOD)) #binding ECI and PCE data for service and goods sector
#renaming series IDs in the merged data set to plain language explanations
#Wage_Price_Merge$series_id <- gsub("CIS201S000000000I","Services Compensation (ECI)",Wage_Price_Merge$series_id)
#Wage_Price_Merge$series_id <- gsub("CIU201G000000000I","Goods Compensation (ECI)",Wage_Price_Merge$series_id)
#Wage_Price_Merge$series_id <- gsub("DSERRG3M086SBEA","Services Prices (PCE)",Wage_Price_Merge$series_id)
#Wage_Price_Merge$series_id <- gsub("DGDSRG3M086SBEA","Goods Prices (PCE)",Wage_Price_Merge$series_id)


PCEPIPCT <- fredr(series_id = "PCEPI",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #PCEPI yoy percent growth data
COREPCEPI <- fredr(series_id = "PCEPILFE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #Core PCEPI yoy percent growth data
TRIMMEDPCEPI <- fredr(series_id = "PCETRIM12M159SFRBDAL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Trimmed Mean PCEPI data

PCEPIIND <- fredr(series_id = "PCEPI",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pcepi index data

AIRFARES <- fredr(series_id = "CUSR0000SETG01",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pcepi index data

#downloading aggregate PCE and PCE durable/nondurable goods for spending comparisons graph
PCE2 <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01")) #downloading PCE
PCEDG <- fredr(series_id = "PCEDG",observation_start = as.Date("2019-01-01")) #downloading PCE durable goods
PCEND <- fredr(series_id = "PCEND",observation_start = as.Date("2019-01-01")) #downloading PCE nondurable goods
PCEDGmerge <- merge(PCE2, PCEDG, by = "date")
PCENDmerge <- merge(PCE2, PCEND, by = "date")


#manually adding 2% CPI growth trend for later chart on above-trend CPI
CPI$CPITREND <- c(seq(0,0,length.out = 13), 258.824*1.001652^(0:27)) #the sequence of zeroes is for the part of the chart where the trendline is excluded, and the second sequence is compounding CPI monthly at a 2% annual rate

#manually adding 4% personal income and outlays growth trend line for later chart on personal income and outlays
DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(DSPI$date, n=1), "months")), trend = 16622.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(DSPI$date, n=1), by = 'month')) - 1))) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(POUT$date, n=1), "months")), trend = 15328.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(POUT$date, n=1), by = 'month')) - 1)))

CPIPCT <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #CPI pct 
CPILFEPCT <- fredr(series_id = "CPILFESL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #CPI lfe

PPICORESERV <- fredr(series_id = "PCUATTDSVATTDSV",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #PPI Core Services

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI_SERV_DURABLE <- merge(CPIDURABLE,CPISERVICE, by = "date") #merging cpi services and durables data

WTIEIA <- eia_series("PET.RWTC.D", start = "2019", end = today())
WTIEIA <- as.data.frame(WTIEIA$data)

Relative_Importance <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/RelativeImportance.csv") %>%
  `colnames<-`(c("Category","2018-01-01","2020-01-01","2022-01-01")) %>%
  pivot_longer(cols=c(-Category),names_to="Original_Vars")%>%
  pivot_wider(names_from=c(Category)) %>%
  mutate(Original_Vars = as.Date(Original_Vars)) %>%
  select(Original_Vars, `All items`,Food,Energy,`Commodities less food and energy commodities`,`Services less energy services`)%>%
  `colnames<-`(c("date","All","Food","Energy","Goods_LFE","Services_LE")) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  `colnames<-`(c("date","Category","value","Indicator")) %>%
  mutate(Indicator = "Relative_Importance")

CPI_ALL <- fredr(series_id = "CPIAUCNS",observation_start = as.Date("2017-12-01")) %>%
  mutate(Category = "All") %>%
  select(date,value,Category)
CPI_FOOD <- fredr(series_id = "CPIUFDNS",observation_start = as.Date("2017-12-01")) %>%
  mutate(Category = "Food")%>%
  select(date,value,Category)
CPI_ENERGY <- fredr(series_id = "CPIENGNS",observation_start = as.Date("2017-12-01"))%>%
  mutate(Category = "Energy")%>%
  select(date,value,Category)
CPI_COM_LFE <- fredr(series_id = "CUUR0000SACL1E",observation_start = as.Date("2017-12-01"))%>%
  mutate(Category = "Goods_LFE")%>%
  select(date,value,Category)
CPI_SERV_LE <- fredr(series_id = "CUUR0000SASLE",observation_start = as.Date("2017-12-01"))%>%
  mutate(Category = "Services_LE")%>%
  select(date,value,Category)

CPI_Indices <- rbind(CPI_ALL,CPI_FOOD,CPI_ENERGY,CPI_COM_LFE,CPI_SERV_LE) %>%
  mutate(Indicator = "Index")

CPI_CONTRIBUTION <- rbind(CPI_Indices,Relative_Importance) %>%
  pivot_wider(names_from = "Indicator") %>%
  mutate_cond(Category == "All" & date < as.Date("2020-01-01") & date > as.Date("2018-01-01"), Relative_Importance = Index/246.524*100) %>%
  mutate_cond(Category == "All" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index/256.974*100) %>%
  mutate_cond(Category == "All" & date > as.Date("2022-01-01"), Relative_Importance = Index/278.802*100) %>%
  mutate_cond(Category == "Food" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index/251.238*13.38400) %>%
  mutate_cond(Category == "Food" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index/259.823*13.771) %>%
  mutate_cond(Category == "Food" & date > as.Date("2022-01-01"), Relative_Importance = Index/286.966*13.37) %>%
  mutate_cond(Category == "Energy" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index/206.598*7.513) %>%
  mutate_cond(Category == "Energy" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index/212.982*6.706) %>%
  mutate_cond(Category == "Energy" & date > as.Date("2022-01-01"), Relative_Importance = Index/256.207*7.348) %>%
  mutate_cond(Category == "Goods_LFE" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index/142.647*19.849000) %>%
  mutate_cond(Category == "Goods_LFE" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index/142.920*20.137000) %>%
  mutate_cond(Category == "Goods_LFE" & date > as.Date("2022-01-01"), Relative_Importance = Index/160.850*21.699000) %>%
  mutate_cond(Category == "Services_LE" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index/322.250*59.254000) %>%
  mutate_cond(Category == "Services_LE" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index/341.347*59.387) %>%
  mutate_cond(Category == "Services_LE" & date > as.Date("2022-01-01"), Relative_Importance = Index/359.559*57.583)

#making updated relative importance calculations
CPI_RI_FINAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index), names_from = "Category", values_from = Relative_Importance) %>%
  mutate(Food = Food/All*100) %>%
  mutate(Energy = Energy/All*100) %>%
  mutate(Goods_LFE = Goods_LFE/All*100) %>%
  mutate(Services_LE = Services_LE/All*100) %>%
  mutate(All = All/All*100) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  arrange(match(name, c("All","Food","Energy","Goods_LFE","Services_LE")))

CPI_CONTRIBUTION$Relative_Importance <- CPI_RI_FINAL_CALCULATIONS$value


#NOTE: NEED TO RECALCULATE TO ACCOUNT FOR SLIGHT ADJUSTMENTS MADE BY BIANNUAL WEIGHT REBALANCING
CPI_CONTRIBUTION_FINAL <- CPI_CONTRIBUTION %>%
  mutate(Monthly_Contribution = (Index/lag(Index))*lag(Relative_Importance)-lag(Relative_Importance)) %>%
  mutate(Yearly_Contribution = (Index/lag(Index,12))*lag(Relative_Importance,12)-lag(Relative_Importance,12)) %>%
  drop_na() %>%
  subset(date >= as.Date("2019-01-01"))

CPI_CONTRIBUTION_ANNUAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = Yearly_Contribution/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.1), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is Broad-Based, but Food and Energy are Still Having an Outsized Influence") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.025-(.3*.125), ymax = -0.025) +
  coord_cartesian(clip = "off")

CPI_CONTRIBUTION_MONTHLY_GRAPH <- ggplot() + #plotting components of monthly inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = Monthly_Contribution/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.01,.015), breaks = c(-.01,-0.005,0,0.005,.01,.015), expand = c(0,0)) +
  ylab("Monthly Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Dropping Energy Prices Pulled Inflation Down In July") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_fill_manual(name= "Contributions to Monthly CPI Inflation (NSA)",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.01-(.3*.025), ymax = -0.01) +
  coord_cartesian(clip = "off")

CPI_PCEPI_PCT_merge <- merge(CPIPCT,PCEPIPCT,by = "date") %>%
  mutate(CPI_PCE_DIFF = value.x - value.y)

CPI_PCEPI_PCT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CPI_PCEPI_PCT_merge, aes(x=date,y= (CPI_PCE_DIFF/100) ,color= "CPI-PCEPI Differential"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.01,0.03), breaks = c(-0.01,0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("Gap, %-%") +
  ggtitle("Mind The Gap") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "The Gap Between CPI and PCEPI-the Fed's Preferred Inflation Index-is at a Modern Day High") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -0.01-(.3*0.04), ymax = -0.01) +
  coord_cartesian(clip = "off")

PCE_Graph <- ggplot() + #plotting Personal Consumption Expenditures as well as PCE Goods/Services
  geom_line(data=PCE, aes(x=date,y= (value/141.04) ,color= "Total Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCEGD, aes(x=date,y= (value/43.46) ,color= "Goods Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCESV, aes(x=date,y= (value/97.73) ,color= "Services Personal Consumption Expenditures"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(83,140), breaks = c(90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Personal Consumption Expenditures: January 2019 = 100") +
  ggtitle("Good to Go?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Goods Shot Up after the Pandemic Hit, and Has Remained High") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 83-(.3*57), ymax = 83) +
  coord_cartesian(clip = "off")

PCE_Goods_Graph <- ggplot() + #plotting nondurable and durable share of PCE
  geom_line(data=PCEDGmerge, aes(x=date,y= (value.y/value.x) ,color= "Durable Goods (lhs)"), size = 1.25) +
  geom_line(data=PCENDmerge, aes(x=date,y= (value.y/value.x)-.1 ,color= "Nondurable Goods (rhs)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.095,.145), breaks = c(.09,.10,.11,.12,.13,.14), expand = c(0,0), sec.axis = sec_axis(~.+.1, name="Share of Total PCE", labels = scales::percent_format(accuracy = 1))) +
  ylab("Share of Total PCE") +
  ggtitle("Good to Go?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Goods Shot Up after the Pandemic Hit, and has Remained High") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "Share of Total Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .095-(.3*.050), ymax = 0.095) +
  coord_cartesian(clip = "off")

AIRFARES_Graph <- ggplot() + #plotting Personal Consumption Expenditures as well as PCE Goods/Services
  geom_line(data=AIRFARES, aes(x=date,y= (value/2.58) ,color= "CPI: Airline Fares"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(70,135), breaks = c(70,80,90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Personal Consumption Expenditures: January 2019 = 100") +
  ggtitle("Taking Off") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Flight Prices Shot Up as an Acute Jet Fuel Shortage Mixed With Reopening Demand") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 70-(.3*70), ymax = 70) +
  coord_cartesian(clip = "off")

Wage_Price_Graph <- ggplot() + #plotting service/goods industry wages/prices using ECI/PCE
  geom_line(data = Wage_Price_Merge, aes(x=date, y = value/100, color = series_id, alpha = series_id), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.025,0.065), breaks = c(-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Spiral? Not so Fast!") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "Price Growth is in Goods, but Wage Growth is in Services") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  scale_alpha_manual(name= NULL,values = c(1,.5,1,.5)) + #scale alpha is making some of the lines have less opacity
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1600), xmax = as.Date("2018-01-01")-(0.049*1600), ymin = -.025-(.3*.09), ymax = -0.025) +
  coord_cartesian(clip = "off")

CPI_Durable_Services <- ggplot() + #plotting durables v services inflation
  geom_line(data=CPI_SERV_DURABLE, aes(x=date,y= (value.x/value.y)/.5132*100 ,color= "CPI Durables/CPI Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,100), breaks = c(60,70,80,90,100), expand = c(0,0)) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Relative Price of Durable Goods Has Rocketed Upwards in 2021") +
  theme_apricitas + theme(legend.position = c(.50,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*6200), xmax = as.Date("2005-01-01")-(0.049*6200), ymin = 60-(.3*40), ymax = 60) +
  coord_cartesian(clip = "off")

T5YIE <- ggplot() + #plotting inflation breakevens
  geom_line(data=FIVEYEARBREAKEVEN, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARFWDBREAKEVEN, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "In a Recent Reversal, Short Term Inflation Expectations Are Higher Than Long-Term Expectations") +
  theme_apricitas + theme(legend.position = c(.60,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*6950), xmax = as.Date("2003-01-01")-(0.049*6950), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

T5YIE2019 <- ggplot() + #plotting inflation breakevens
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.0225, ymax = 0.0275, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=FIVEYEARBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARFWDBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  annotate("text", label = "Breakevens Approximately Consistent With 2% Inflation Target", x = as.Date("2020-01-01"), y = 0.0287, color = "#EE6055", alpha = 0.6, size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Short and Long Term Inflation Expectations are Falling Fast") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

CPI_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=CPI, aes(x=date,y= (value/2.52) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPI, aes(x=date,y= CPITREND/2.52 ,color= "2% Target"), size = 1.25, linetype = "dashed") +
  geom_line(data=PCEPIIND, aes(x=date,y= (value/1.0814) ,color= "PCEPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(100,117.5), breaks = c(100,105,110,115,120), expand = c(0,0)) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "The CPI and PCE Price Indexes are Way Above Trend") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= NULL,breaks = c("CPI","PCEPI","2% Target"),values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 100-(.3*17.5), ymax = 100) +
  coord_cartesian(clip = "off")

CPI_SERVICES_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=CPI_SERVICES, aes(x=date,y= (value/3.12717) ,color= "CPI: Services"), size = 1.25) +
  geom_line(data=CPI_SERVICESTrend, aes(x=date,y= trend/3.12717,color= "3% Pre-COVID Trend"), size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(limits = c(100,117.5), breaks = c(100,105,110,115,120), expand = c(0,0)) +
  ylab("Consumer Price Index: January 2019 = 100") +
  ggtitle("The Inflation Miscalculation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Services Prices-The Core Driver of 'Normal' Inflation, Has Overshot the Pre-COVID Trend") +
  theme_apricitas + theme(legend.position = c(.40,.60)) +
  scale_color_manual(name= NULL,breaks = c("CPI: Services","3% Pre-COVID Trend"),values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1500), xmax = as.Date("2018-01-01")-(0.049*1500), ymin = 100-(.3*20), ymax = 100) +
  coord_cartesian(clip = "off")

CPIPCT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=CPIPCT, aes(x=date,y= (value/100) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPILFEPCT, aes(x=date,y= value/100 ,color= "Core CPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.09), breaks = c(0,0.03,0.06,0.09), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("The Inflation Miscalculation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation Has Hit a 40 Year High") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.09), ymax = 0) +
  coord_cartesian(clip = "off")

PPIPCT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=PPICORESERV, aes(x=date,y= (value/100) ,color= "PPI: Services Less Trade, Transportation, and Warehousing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.01,0.06), breaks = c(0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Getting to the Core") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "PPI Core Services Prices Have Already Decelerated Significantly") +
  theme_apricitas + theme(legend.position = c(.40,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -0.01-(.3*0.07), ymax = -0.01) +
  coord_cartesian(clip = "off")

PCE_Inflation_Rates_Graph <- ggplot() + #plotting PCE Inflation Rates
  geom_line(data=PCEPIPCT, aes(x=date,y= value/100 ,color= "PCE Inflation"), size = 1.25) +
  geom_line(data=COREPCEPI, aes(x=date,y= value/100 ,color= "Core PCE Inflation"), size = 1.25) +
  geom_line(data=TRIMMEDPCEPI, aes(x=date,y= value/100 ,color= "Trimmed Mean PCE Inflation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.07), breaks = c(0,.02,.04,0.06), expand = c(0,0)) +
  ylab("Percent Change from One Year Ago") +
  ggtitle("Is Inflation Transitory?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Traditional 'Core' Inflation Metric Does Not Eliminate Pandemic Volatility") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E"))+
  annotate(geom = "hline",y = 0.02,yintercept = 0.02, size = 1.25,linetype = "dashed",color = "white") +#annotating 2% target inflation
  annotate(geom = "text", label = as.character("2% Inflation Target"),x = as.Date("2019-05-01"),y = 0.023,color = "white") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

CPI_New_Used_Car_Vehicles_Graph <- ggplot() + #plotting "Used Cars and Trucks" and "New Vehicles" price Indexes
  geom_line(data=CPIUSEDCARS, aes(x=date,y= (value/141)*100 ,color= "Used Cars and Trucks"), size = 1.25) +
  geom_line(data=CPINEWCARS, aes(x=date,y= (value/146)*100 ,color= "New Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,160), breaks = c(90,120,150), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Used Cars and Trucks Experienced Unprecedented Price Increases") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 80-(.3*80), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ZORI <- read.csv("https://files.zillowstatic.com/research/public_csvs/zori/Metro_ZORI_AllHomesPlusMultifamily_Smoothed.csv?t=1657959787") %>%
  select(-RegionID, -SizeRank) %>%
  subset(RegionName == "United States") %>%
  transpose() %>%
  `colnames<-`(.[1, ]) %>%
  mutate(date = c(seq(as.Date("2013-12-01"), as.Date("2022-06-01"), "months"))) %>%
  .[-1, ] %>%
  mutate(`United States` = as.numeric(`United States`)) %>%
  mutate(`United States` = (`United States`-lag(`United States`,12))/lag(`United States`,12))
  
CPI_Rent <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  geom_line(data=CPIRENT, aes(x=date,y= (calculations/100) ,color= "CPI Rent: Annual Percentage Growth"), size = 1.25) +
  geom_line(data=CPIORENT, aes(x=date,y= (calculations/100) ,color= "CPI Owner's Equivalent Rent: Annual Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.07), breaks = c(0,.01,0.02,0.03,0.04,0.05,0.06,0.07), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Housing Price Growth Had Slowed, But is Rebounding") +
  theme_apricitas + theme(legend.position = c(.40,.20)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent: Annual Percentage Growth","CPI Owner's Equivalent Rent: Annual Percentage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.06), ymax = 0.00) +
  coord_cartesian(clip = "off")

CPI_Rent_Zillow <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  geom_line(data=CPIRENT, aes(x=date,y= (calculations/100) ,color= "CPI Rent"), size = 1.25) +
  geom_line(data=CPIORENT, aes(x=date,y= (calculations/100) ,color= "CPI Owner's Equivalent Rent"), size = 1.25) +
  geom_line(data=subset(ZORI, date > as.Date("2018-06-01")), aes(x=date+180,y= (`United States`) ,color= "Zillow Observed Rent Index, Lagged 6 Months"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.20), breaks = c(0,.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Zillow data",subtitle = "Whether Rent Growth Actually Peaks Will Be Critical to Future Inflation Prints") +
  theme_apricitas + theme(legend.position = c(.35,.70)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent","CPI Owner's Equivalent Rent","Zillow Observed Rent Index, Lagged 6 Months")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1400), xmax = as.Date("2019-01-01")-(0.049*1400), ymin = 0-(.3*0.20), ymax = 0.00) +
  coord_cartesian(clip = "off")

CPI_Rent_Month <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  geom_line(data=CPIRENTmonth, aes(x=date,y= (value/100) ,color= "CPI Rent: Monthly Percentage Growth"), size = 1.25) +
  geom_line(data=CPIOERmonth, aes(x=date,y= (value/100) ,color= "CPI Owner's Equivalent Rent: Monthly Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .25),limits = c(0,.0080), breaks = c(0,.0025,0.005,.0075), expand = c(0,0)) +
  ylab("Monthly Percent Growth, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Housing Price Growth is Accelerating") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("CPI Rent: Monthly Percentage Growth","CPI Owner's Equivalent Rent: Monthly Percentage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.0085), ymax = 0.00) +
  coord_cartesian(clip = "off")

CPI_Core_Month <- ggplot() + #plotting core and headline monthly Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("vline", x= tail(CPIAUCSL_Monthly$date, n=1) - 365, xintercept = tail(CPIAUCSL_Monthly$date, n=1) - 365, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text",label = "One Year Ago", x= tail(CPIAUCSL_Monthly$date, n=1) - 230, y = -0.005, color = "white", size = 5.5) +
  geom_line(data=CPIAUCSL_Monthly, aes(x=date,y= (value/100) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPILFESL_Monthly, aes(x=date,y= (value/100) ,color= "CPI Less Food and Energy"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .25),limits = c(-0.01,.0135), breaks = c(-0.01,-0.005,0,.005,.01), expand = c(0,0)) +
  ylab("Monthly Percent Growth, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Year-on-Year Core Inflation is Likely to Increase Thanks to Base Effects") +
  theme_apricitas + theme(legend.position = c(.40,.82)) +
  scale_color_manual(name= "Monthly Percentage Growth",values = c("#00A99D","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("CPI","CPI Less Food and Energy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -0.01-(.3*0.0235), ymax = -0.01) +
  coord_cartesian(clip = "off")


CPI_Lodging_Graph <- ggplot() + #plotting lodging away from home
  geom_line(data=CPILODGINGHOME, aes(x=date,y= (value/162)*100 ,color= "CPI: Lodging Away From Home"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(85,120), breaks = c(90,100,110,120), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Lodging Away From Home (Hotels, Motels, etc) Have Bounced Back") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 85-(.3*35), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_PIPED_GAS <- ggplot() + #plotting piped gas price index
  geom_line(data=CPIPIPEDGAS, aes(x=date,y= (value/176)*100 ,color= "CPI: Utility (Piped) Gas Service in U.S. City Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(94,160), breaks = c(100,120,140,160), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("The Energy Crunch") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Utility Gas Service Are Near Record Highs") +
  theme_apricitas + theme(legend.position = c(.45,.72)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 94-(.3*66), ymax = 94) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_GAS <- ggplot() + #plotting gasoline price index against WTI prices
  geom_line(data=WTIEIA, aes(x=date,y= (value/46.31)*100 ,color= "Crude Oil (WTI)"), size = 1.25) +
  geom_line(data=US_Regular_All_Form_Gas, aes(x=date,y= (value/2.24)*100 ,color= "US Regular All Formulations Gas Price"), size = 1.25) +
  geom_line(data=CPIGAS, aes(x=date,y= (value/201)*100 ,color= "CPI: Gasoline"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,275), breaks = c(50,75,100,125,150,175,200,225,250,275), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("The Energy Crunch") +
  labs(caption = "Graph created by @JosephPolitano using BLS, EIA, and Yahoo! Finance data",subtitle = "Oil and Gas Prices Are Pulling Back A Bit From Their Highs") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 50-(.3*225), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CPI_Wheat <- ggplot() + #plotting bread/cereal products price index against wheat prices
  geom_line(data=CPIBreadCereal, aes(x=date,y= (value/275)*100 ,color= "CPI: Cereals and Bakery Products"), size = 1.25) +
  geom_line(data=CPIBREAD, aes(x=date,y= (value/185.053)*100 ,color= "CPI: Bread"), size = 1.25) +
  geom_line(data=Wheat, aes(x=date,y= (close/492)*100 ,color= "Wheat Futures"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,275), breaks = c(50,75,100,125,150,175,200,225,250,275), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("The Empty Breadbasket") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The War in Ukraine Will Likely Keep Global Wheat Prices Elevated, and Will Boost Inflation") +
  theme_apricitas + theme(legend.position = c(.70,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*6240), xmax = as.Date("2005-01-01")-(0.049*6240), ymin = 50-(.3*225), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Personal Income Remains on Trend, But Spending is Above Trend as Excess Savings Decrease") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1600), xmax = as.Date("2018-01-01")-(0.049*1600), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

Rent_LessRent_Graph <- ggplot() + 
  geom_line(data = CPIRent, aes(x = date, y = value/100, color = "CPI: Rent of Primary Residences"), size = 1.25) +
  geom_line(data = CPIServicesLessRent, aes(x = date, y = value/100, color = "CPI: Services Less Rent of Shelter"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.07), breaks = c(0,.01,.02,.03,.04,0.05,0.06,0.07), expand = c(0,0)) +
  ylab("Change from Year Ago, %") +
  ggtitle("Services Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is Becoming More Broad Based as Prices for Rent and Other Services Jump") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*.07), ymax = 0) +
  coord_cartesian(clip = "off")

#Saving png images of all graphs
ggsave(dpi = "retina",plot = CPI_Graph, "CPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPIPCT_Graph, "CPI PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = PCE_Inflation_Rates_Graph, "PCE Inflation.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPI_New_Used_Car_Vehicles_Graph, "CPI CARS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = CPI_Rent, "CPI Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_GAS, "CPI Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_PIPED_GAS, "CPI Piped Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Lodging_Graph, "CPI Lodging.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Durable_Services, "Durable Services.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5YIE, "Inflation Expectations.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5YIE2019, "Inflation Expectations 2019.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Wage_Price_Graph, "Wage Price Spiral.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PCE_Graph, "PCE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Rent_LessRent_Graph, "Rent and Services Less Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Wheat, "Wheat.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Rent_Month, "CPI Rent Month.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Core_Month, "CPI Core Month.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AIRFARES_Graph, "Airfares Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PPIPCT_Graph, "PPI PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PCE_Goods_Graph, "PCE Goods Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_SERVICES_Graph, "CPI Services Trend.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_ANNUAL_GRAPH, "CPI Contribution.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_MONTHLY_GRAPH, "CPI Monthly Contribution.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Rent_Zillow, "CPI Rent Zillow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_PCEPI_PCT_Graph, "CPI PCEPI PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

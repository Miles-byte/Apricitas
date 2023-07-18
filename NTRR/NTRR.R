pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

install_github("keberwein/blscrapeR")
library(blscrapeR)

NTRR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/NTRR/NTRR.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPIRENT <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2005-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") # CPI rent
#NTRR CPI
NTRR_Graph <- ggplot() + #plotting NTRR & ATRR
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(CPIRENT,date >= as.Date("2005-01-01")), aes(x=date,y= value/100,color= "CPI: Rent of Primary Residence"), size = 1.25)+ 
  geom_line(data=subset(NTRR,date >= as.Date("2005-01-01")), aes(x=date,y= ATRR/100,color= "All Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=subset(NTRR,date >= as.Date("2005-01-01")), aes(x=date,y= NTRR/100,color= "New Tenant Repeat Rent Index"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,.125), breaks = c(-0.05,0,.05,0.1), expand = c(0,0)) +
  ggtitle("The New Key Dis-Inflation Indicator") +
  labs(caption = "Graph created by @JosephPolitano using Cleveland Fed & BLS data via Adams, Loewenstein, Montag, and Vebrugge (2022)", subtitle = "Rent Inflation for New Tenants Has Declined Substantially") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("New Tenant Repeat Rent Index","All Tenant Repeat Rent Index","CPI: Rent of Primary Residence")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-04-01")-(.1861*(today()-as.Date("2005-04-01"))), xmax = as.Date("2005-04-01")-(0.049*(today()-as.Date("2005-04-01"))), ymin = -0.05-(.3*.175), ymax = -.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NTRR_Graph, "NTRR Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#NTRR ZORI and Apartment List

ZORI <- read.csv("https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_month.csv?t=1665666510") %>%
  subset(RegionName == "United States") %>%
  select(-RegionID, -SizeRank, -RegionType, -StateName) %>%
  transpose() %>%
  `colnames<-`(.[1, ]) %>%
  mutate(date = c(seq(as.Date("2014-12-01"), as.Date("2023-06-01"), "months"))) %>%
  .[-1, ] %>%
  mutate(`United States` = as.numeric(`United States`)) %>%
  mutate(`United States` = (`United States`-lag(`United States`,12))/lag(`United States`,12))

ApartmentList <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/091322/apartmentlist.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

CPI_Rent_Zillow <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(CPIRENT,date >= as.Date("2019-01-01")), aes(x=date,y= (value/100) ,color= "CPI: Rent of Primary Residence"), size = 1.25) +
  geom_line(data=subset(ZORI, date >= as.Date("2018-03-01")), aes(x=date+365,y= (`United States`) ,color= "Zillow Observed Rent Index, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(ApartmentList, date >= as.Date("2018-03-01")), aes(x=date+365,y= annualpct ,color= "ApartmentList Median New Lease, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(NTRR,date >= as.Date("2018-01-01")), aes(x=date+365,y= NTRR/100,color= "New Tenant Repeat Rent Index, Lagged 1 Year"), size = 1.25)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,.20), breaks = c(0,.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("The Great Rent Disinflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS, Zillow, and ApartmentList data",subtitle = "Rent Growth Has Peaked, and Leading Indicators Suggest Further Decelerations") +
  theme_apricitas + theme(legend.position = c(.32,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI: Rent of Primary Residence","New Tenant Repeat Rent Index, Lagged 1 Year","Zillow Observed Rent Index, Lagged 1 Year","ApartmentList Median New Lease, Lagged 1 Year")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+365-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+365-as.Date("2019-01-01"))), ymin = -0.025-(.3*0.225), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Rent_Zillow, "CPI Rent Zillow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#GLI YoY growth
GLI_CES <- fredr(series_id = "CES0500000017",observation_start = as.Date("2005-04-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1", frequency = "q") # CPI rent
ELEV2554 <- fredr(series_id = "LNS12000060",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "q", aggregation_method = "avg") # CPI rent
ECIWAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2003-01-01"),realtime_start = NULL, realtime_end = NULL) # CPI rent

GLI_ECI <- merge(ELEV2554,ECIWAG, by = "date") %>%
  select(date, value.x, value.y) %>%
  mutate(pct = (value.x*value.y)) %>%
  mutate(pct = (pct-lag(pct, 4))/lag(pct,4))
  
GLI_NTRR <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= NTRR/100,color= "New Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=GLI_CES, aes(x=date+90,y= value/100,color= "Gross Labor Income, Nonfarm Payrolls"), size = 1.25)+ 
  geom_line(data=subset(GLI_ECI,date > as.Date("2005-01-01")), aes(x=date+90,y= pct,color= "Gross Labor Income, Prime Age Employment * ECI"), size = 1.25)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.1,.15), breaks = c(-.10,-0.05,0,0.05,0.10,0.15), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using Cleveland Fed & BLS data via Adams, Loewenstein, Montag, and Vebrugge (2022)",subtitle = "New Tenant Rents are Partially Driven by Gross Labor Income Growth") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Tenant Repeat Rent Index","Gross Labor Income, Nonfarm Payrolls","Gross Labor Income, Prime Age Employment * ECI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-04-01")-(.1861*(today()-as.Date("2005-04-01"))), xmax = as.Date("2005-04-01")-(0.049*(today()-as.Date("2005-04-01"))), ymin = -0.10-(.3*.25), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_NTRR, "GLI NTRR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#NTRR 95% CI Graph
NTRR_95_Graph <- ggplot() + #plotting NTRR & ATRR
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= NTRR/100,color= "New Tenant Repeat Rent Index"), size = 1.25, alpha = 1)+ 
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= NTRR_lower_confinterval/100,color= "Upper Bound of 95% Confidence Interval"), size = 1.25, alpha = 0.4)+ 
  geom_line(data=subset(NTRR,date > as.Date("2005-01-01")), aes(x=date,y= NTRR_upper_confinterval/100,color= "Lower Bound of 95% Confidence Interval"), size = 1.25, alpha = 0.4)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,.15), breaks = c(-0.05,0,.05,0.1,0.15), expand = c(0,0)) +
  ggtitle("NTRR Confidence Intervals") +
  labs(caption = "Graph created by @JosephPolitano using Cleveland Fed & BLS data via Adams, Loewenstein, Montag, and Vebrugge (2022)", subtitle = "Most Recent Quarters Have Higher Confidence Intervals As Full Data is Not Yet Available") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#EE6055","#00A99D","#EE6055","#A7ACD9"), breaks = c("New Tenant Repeat Rent Index","Upper Bound of 95% Confidence Interval","Lower Bound of 95% Confidence Interval"), guide = guide_legend(override_aes = list(alpha = c(1,0.4,0.4)))) +
  scale_alpha_manual(name= NULL,values = c(1,.4,.4)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-04-01")-(.1861*(today()-as.Date("2005-04-01"))), xmax = as.Date("2005-04-01")-(0.049*(today()-as.Date("2005-04-01"))), ymin = -0.05-(.3*.175), ymax = -.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NTRR_95_Graph, "GLI NTRR 95.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPIRENT_INDEX <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2005-04-01"),realtime_start = NULL, realtime_end = NULL,frequency = "q", aggregation_method = "avg") # CPI rent

CPI_NTRR_INDEXED <- ggplot() + #plotting NTRR & ATRR
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=subset(NTRR,date >= as.Date("2019-01-01")), aes(x=date,y= NTRR_INDEX/NTRR_INDEX[1]*100,color= "New Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=subset(NTRR,date >= as.Date("2019-01-01")), aes(x=date,y= ATRR_index/ATRR_index[1]*100,color= "All Tenant Repeat Rent Index"), size = 1.25)+ 
  geom_line(data=subset(CPIRENT_INDEX,date >= as.Date("2019-01-01")), aes(x=date,y= value/value[1]*100,color= "CPI Rent"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Q1 2005 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(100,127), breaks = c(100,105,110,115,120,125), expand = c(0,0)) +
  ggtitle("The CPI has Caught Up to NTRR and ATRR") +
  labs(caption = "Graph created by @JosephPolitano using Cleveland Fed & BLS data via Adams, Loewenstein, Montag, and Vebrugge (2022)", subtitle = "Concerns about `Catch Up Growth` Can Be Reduced—CPI Has Already Caught Up to NTRR") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= "Quartely Data, Indexed to Q1 2019",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  scale_alpha_manual(name= NULL,values = c(1,.4,.4)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 100-(.3*27), ymax = 100) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_NTRR_INDEXED, "CPI NTRR Indexed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#NORTHEAST
NYC_CPI <- bls_api("CUURS12ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "New York, NY")
PHI_CPI <- bls_api("CUURS12BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Philadelphia, PA")
BOS_CPI <- bls_api("CUURS11ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Boston, MA")

#MIDWEST
CHI_CPI <- bls_api("CUURS23ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Chicago, IL")
DET_CPI <- bls_api("CUURS23BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Detroit, MI")
MIN_CPI <- bls_api("CUURS24ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Minneapolis, MN")
STL_CPI <- bls_api("CUURS24BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "St. Louis, MO")

#SOUTH
ATL_CPI <- bls_api("CUURS35CSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Atlanta, GA")
DAL_CPI <- bls_api("CUURS37ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Dallas, TX")
HOU_CPI <- bls_api("CUURS37BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Houston, TX")
MIA_CPI <- bls_api("CUURS35BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Miami, FL")
TPA_CPI <- bls_api("CUURS35DSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Tampa, FL")
WAS_CPI <- bls_api("CUURS35ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Washington, DC")
BAL_CPI <- bls_api("CUURS35ESEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Baltimore, MD")

#WEST
DEN_CPI <- bls_api("CUURS48BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Denver, CO")
LA_CPI <- bls_api("CUURS49ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Los Angeles, CA")
RIV_CPI <- bls_api("CUURS49CSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Riverside, CA")
PHO_CPI <- bls_api("CUURS48ASEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Phoenix, AZ")
SND_CPI <- bls_api("CUURS49ESEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "San Diego, CA")
SFO_CPI <- bls_api("CUURS49BSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "San Francisco, CA")
SEA_CPI <- bls_api("CUURS49DSEHA", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(growth = (value-lead(value, 12))/lead(value, 12)) %>%
  mutate(metro = "Seattle, WA")

CPI_RBIND <- rbind(SEA_CPI,SFO_CPI,SND_CPI,PHO_CPI,RIV_CPI,LA_CPI,DEN_CPI,BAL_CPI,WAS_CPI,TPA_CPI,MIA_CPI,HOU_CPI,DAL_CPI,ATL_CPI,STL_CPI,MIN_CPI,DET_CPI,CHI_CPI,BOS_CPI,PHI_CPI,NYC_CPI) %>%
  transmute(date, CPI_GROWTH = growth, metro)

ZORI_METRO <- read.csv("https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_month.csv?t=1689398576") %>%
  select(-RegionID, -SizeRank, -RegionType, -StateName) %>%
  transpose() %>%
  `colnames<-`(.[1, ]) %>%
  mutate(date = seq.Date(from = as.Date("2014-12-01"), length = nrow(.), by = "month")) %>%
  .[-1, ] %>%
  select(date, `New York, NY`:`St. Louis, MO`) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.numeric, ~(. - lag(., 12))/lag(., 12)) %>%
  pivot_longer(cols = `New York, NY`:`St. Louis, MO`) %>%
  transmute(date, metro = name, ZORI_GROWTH = value) %>%
  drop_na()
  
min_values_ZORI_CPI <-  merge(CPI_RBIND, ZORI_METRO, by = c("date", "metro")) %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2022-03-01")) %>%
  group_by(metro) %>%
  summarise(min_CPI_GROWTH = min(CPI_GROWTH, na.rm = TRUE),
            min_ZORI_GROWTH = min(ZORI_GROWTH, na.rm = TRUE),
            min_date_CPI = date[which.min(CPI_GROWTH)],
            min_date_ZORI = date[which.min(ZORI_GROWTH)])

max_values_ZORI_CPI <-  merge(CPI_RBIND, ZORI_METRO, by = c("date", "metro")) %>%
  filter(date >= as.Date("2021-06-01") & date <= Sys.Date()) %>%
  group_by(metro) %>%
  summarise(max_CPI_GROWTH = max(CPI_GROWTH, na.rm = TRUE),
            max_ZORI_GROWTH = max(ZORI_GROWTH, na.rm = TRUE),
            max_date_CPI = date[which.max(CPI_GROWTH)],
            max_date_ZORI = date[which.max(ZORI_GROWTH)])

combined_data_ZORI_CPI <- left_join(min_values_ZORI_CPI, max_values_ZORI_CPI, by = "metro")

combined_data_ZORI_CPI <- combined_data_ZORI_CPI %>%
  mutate(gap_months_min = interval(min_date_ZORI, min_date_CPI) / months(1),
         gap_months_max = interval(max_date_ZORI, max_date_CPI) / months(1))


APARTMENTLIST_METRO <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/NTRR/ApartmentList_DATA.csv") %>%
  subset(location_type == "Metro") %>%
  select(-location_name,-location_type,-location_fips_code,-population,-state,-county) %>%
  .[1:21, ] %>%
  transpose() %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), length = nrow(.), by = "month")) %>%
  setNames(c("New York, NY","Los Angeles, CA","Chicago, IL","Dallas, TX","Houston, TX","Washington, DC","Miami, FL","Philadelphia, PA","Atlanta, GA","Boston, MA","Phoenix, AZ","San Francisco, CA","Riverside, CA","Detriot, MI","Seattle, WA","Minneapolis, MN","San Diego, CA","Tampa, FL","Denver, CO","St. Louis, MO","Baltimore, MD","date")) %>%
  pivot_longer(cols = `New York, NY`:`Baltimore, MD`) %>%
  transmute(date, metro = name, APARTMENTLIST_GROWTH = as.numeric(value)) %>%
  drop_na()
  
min_values_APARTMENTLIST_CPI <-  merge(CPI_RBIND, APARTMENTLIST_METRO, by = c("date", "metro")) %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2022-03-01")) %>%
  group_by(metro) %>%
  summarise(min_CPI_GROWTH = min(CPI_GROWTH, na.rm = TRUE),
            min_APARTMENTLIST_GROWTH = min(APARTMENTLIST_GROWTH, na.rm = TRUE),
            min_date_CPI = date[which.min(CPI_GROWTH)],
            min_date_APARTMENTLIST = date[which.min(APARTMENTLIST_GROWTH)])

max_values_APARTMENTLIST_CPI <-  merge(CPI_RBIND, APARTMENTLIST_METRO, by = c("date", "metro")) %>%
  filter(date >= as.Date("2021-06-01") & date <= Sys.Date()) %>%
  group_by(metro) %>%
  summarise(max_CPI_GROWTH = max(CPI_GROWTH, na.rm = TRUE),
            max_APARTMENTLIST_GROWTH = max(APARTMENTLIST_GROWTH, na.rm = TRUE),
            max_date_CPI = date[which.max(CPI_GROWTH)],
            max_date_APARTMENTLIST = date[which.max(APARTMENTLIST_GROWTH)])

combined_data_APARTMENTLIST_CPI <- left_join(min_values_APARTMENTLIST_CPI, max_values_APARTMENTLIST_CPI, by = "metro")

combined_data_APARTMENTLIST_CPI <- combined_data_APARTMENTLIST_CPI %>%
  mutate(gap_months_min = interval(min_date_APARTMENTLIST, min_date_CPI) / months(1),
         gap_months_max = interval(max_date_APARTMENTLIST, max_date_CPI) / months(1))

ZORI_APARTMENTLIST_CPI_MERGE <- combined_data_APARTMENTLIST_CPI %>%
  transmute(metro, gap_min_APARTMENTLIST = gap_months_min, gap_max_APARTMENTLIST = gap_months_max) %>%
  merge(., combined_data_ZORI_CPI, by = "metro") %>%
  transmute(metro, gap_min_APARTMENTLIST, gap_max_APARTMENTLIST, gap_min_ZORI = gap_months_min, gap_max_ZORI = gap_months_max)
  
AVERAGE(ZORI_APARTMENTLIST_CPI_MERGE$gap_min_ZORI)
AVERAGE(ZORI_APARTMENTLIST_CPI_MERGE$gap_max_ZORI)
AVERAGE(ZORI_APARTMENTLIST_CPI_MERGE$gap_min_APARTMENTLIST)
AVERAGE(ZORI_APARTMENTLIST_CPI_MERGE$gap_max_APARTMENTLIST)

ZORI_APARTMENTLIST_CPI_MERGE_graph <- ggplot(ZORI_APARTMENTLIST_CPI_MERGE, aes(x = gap_min_APARTMENTLIST, color = "ApartmentList Median New Lease to CPI Rent: Trough to Trough")) +
  geom_freqpoly(binwidth = 4, size = 0.75, linetype = "dashed") +
  geom_freqpoly(aes(x = gap_max_APARTMENTLIST, color = "ApartmentList Median New Lease to CPI Rent: Peak to Peak"), binwidth = 4, size = 1.25) +
  geom_freqpoly(aes(x = gap_min_ZORI, color = "Zillow Observed Rent Index to CPI Rent: Trough to Trough"), binwidth = 4, size = 0.75, linetype = "dashed") +
  geom_freqpoly(aes(x = gap_max_ZORI, color = "Zillow Observed Rent Index to CPI Rent: Peak to Peak"), binwidth = 4, size = 1.25) +
  labs(color = "Variable", x = "Lag (Months)", y = "Count (Metro Areas)",
       title = "Frequency polygon of num1 and num2") +
  theme_apricitas + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,12), breaks = c(0,5,10), expand = c(0,0)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1), limits = c(-12,26), breaks = c(-10,0,10,20), expand = c(0,0)) +
  ggtitle("Metro Area Lags Between Private Rent Data and CPI") +
  labs(caption = "Graph created by @JosephPolitano using BLS, Zillow, and ApartmentList Data", subtitle = "The Lags Between Troughs of Private Rent Indexes and CPI Was Shorter Than Between Peaks") +
  theme_apricitas + theme(legend.position = c(.33,.85)) +
  theme(legend.key.width =  unit(.82, "cm"), plot.title = element_text(size =22), legend.title = element_text(size = 13), legend.text = element_text(size = 11)) +
  scale_color_manual(name= "Distribution of Lags Between Pandemic-era Peaks and Troughs",values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"), guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "solid", "dashed"), lwd = c(1.25,0.75,1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 100-(.3*27), ymax = 100) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ZORI_APARTMENTLIST_CPI_MERGE_graph, "ZORI APARTMENTLIST CPI MERGE GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Relative_Importance_ALL_SHELTER <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/RelativeImportance.csv") %>%
  `colnames<-`(c("Category","2018-01-01","2020-01-01","2022-01-01","2023-01-01")) %>%
  pivot_longer(cols=c(-Category),names_to="Original_Vars")%>%
  pivot_wider(names_from=c(Category)) %>%
  mutate(Original_Vars = as.Date(Original_Vars)) %>%
  select(Original_Vars, `All items`,Shelter)%>%
  `colnames<-`(c("date","All","Shelter")) %>%
  pivot_longer(cols = c("All","Shelter")) %>%
  `colnames<-`(c("date","Category","value","Indicator")) %>%
  mutate(Indicator = "Relative_Importance")

CPI_ALL <- bls_api("CUUR0000SA0", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "All") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_SHELTER <- bls_api("SUUR0000SAH1", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Shelter") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_Indices <- rbind(CPI_ALL,CPI_SHELTER) %>%
  mutate(Indicator = "Index_NSA")

CPI_CONTRIBUTION <- rbind(CPI_Indices,Relative_Importance_ALL_SHELTER) %>%
  pivot_wider(names_from = "Indicator") %>%
  mutate_cond(Category == "All" & date < as.Date("2020-01-01") & date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/246.524*100) %>%
  mutate_cond(Category == "All" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/256.974*100) %>%
  mutate_cond(Category == "All" & date < as.Date("2023-01-01") & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/278.802*100) %>%
  mutate_cond(Category == "All" & date > as.Date("2023-01-01"), Relative_Importance = Index_NSA/296.797*100) %>%
  mutate_cond(Category == "Shelter" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/158.264*32.843) %>%
  mutate_cond(Category == "Shelter" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/168.064*32.946) %>%
  mutate_cond(Category == "Shelter" & date < as.Date("2023-01-01") & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/178.483*33.158) %>%
  mutate_cond(Category == "Shelter" & date > as.Date("2023-01-01"), Relative_Importance = Index_NSA/256.974*34.4)
  
CPI_RI_ANNUAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index_NSA), names_from = "Category", values_from = Relative_Importance) %>%
  pivot_longer(cols = c("All","Shelter")) %>%
  arrange(match(name, c("All","Shelter")))

CPI_CONTRIBUTION_ANNUAL <- CPI_CONTRIBUTION
CPI_CONTRIBUTION_ANNUAL$Relative_Importance <- CPI_RI_ANNUAL_CALCULATIONS$value
CPI_CONTRIBUTION_ANNUAL <- drop_na(CPI_CONTRIBUTION_ANNUAL)

CPI_RI_FINAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index_NSA), names_from = "Category", values_from = Relative_Importance) %>%
  mutate(Shelter = Shelter/All*100) %>%
  mutate(All = All/All*100) %>%
  pivot_longer(cols = c("All","Shelter")) %>%
  arrange(match(name, c("All","Shelter")))

CPI_CONTRIBUTION$Relative_Importance <- CPI_RI_FINAL_CALCULATIONS$value
CPI_CONTRIBUTION <- drop_na(CPI_CONTRIBUTION)

CPI_CONTRIBUTION_FINAL <- CPI_CONTRIBUTION %>%
  subset(., date > as.Date("2017-12-01")) %>%
  mutate(Yearly_Contribution = (Index_NSA/lead(Index_NSA,12))*lead(Relative_Importance,12)-lead(Relative_Importance,12)) %>%
  drop_na() %>%
  subset(date >= as.Date("2019-01-01")) %>%
  select(Category, date, Yearly_Contribution) %>%
  pivot_wider(names_from = Category, values_from = Yearly_Contribution) %>%
  transmute(date, `Everything Else`= All-Shelter, `Rent of Shelter` = Shelter) %>%
  pivot_longer(cols = `Everything Else`:`Rent of Shelter`)

CPI_CONTRIBUTION_SHELTER_ANNUAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL), aes(x = date, y = value/100, fill = name), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.01,.1), breaks = c(0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("Inflation is Now Mostly Driven by Rent") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation Now Mostly Comes From Increases in Core Services Prices, Particularly Rent") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#00A99D","#9A348E","#EE6055","#A7ACD9","#3083DC"), breaks = c("Rent of Shelter","Everything Else")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.01-(.3*.11), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_SHELTER_ANNUAL_GRAPH, "CPI Shelter Annual.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
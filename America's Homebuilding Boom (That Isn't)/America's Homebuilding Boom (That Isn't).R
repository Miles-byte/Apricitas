pacman::p_load(nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)


TOTAL_HOUSING_GROWTH <- fredr(series_id = "ETOTALUSQ176N", units = "pc1") #downloading total housing growth
TOTAL_HOUSING_STARTS <- fredr(series_id = "HOUST") #downloading total housing starts
TOTAL_HOUSING_STARTS_SUBSET <- fredr(series_id = "HOUST", observation_start = as.Date("2000-01-01")) #downloading total housing starts

TOTAL_HOUSING_COMPLETIONS <- fredr(series_id = "COMPUTSA", observation_start = as.Date("2000-01-01")) #downloading total completions
TOTAL_HOUSING_UNDERCONSTRUCTION <- fredr(series_id = "UNDCONTSA", observation_start = as.Date("2000-01-01")) #total under construction

HOUSING_STARTS_5PLUS <- fredr(series_id = "HOUST5F", observation_start = as.Date("2000-01-01")) #5 or More Units
HOUSING_STARTS_SFH <- fredr(series_id = "HOUST1F", observation_start = as.Date("2000-01-01")) #Single Family Home
HOUSING_COMPS_SFH <- fredr(series_id = "COMPU1USA", observation_start = as.Date("2000-01-01")) #Single Family Home

THIRTY_YR_FIXED <- fredr(series_id = "MORTGAGE30US", observation_start = as.Date("2000-01-01")) #Single Family Home

RENTAL_VACANCY_RATE <- fredr(series_id = "RRVRUSQ156N", observation_start = as.Date("2000-01-01")) #Single Family Home
HOMEOWNER_VACANCY_RATE <- fredr(series_id = "RHVRUSQ156N", observation_start = as.Date("2000-01-01")) #Single Family Home

AUTHORIZED_NOT_STARTED <- fredr(series_id = "AUTHNOTTSA", observation_start = as.Date("2000-01-01")) #Single Family Home

PRIV_CONS_SPEND <- fredr(series_id = "PRRESCONS", observation_start = as.Date("2000-01-01")) #Total Private Construction Spending

CPI_SF <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/Construction_Price_Index_SF.csv")
CPI_MF <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/Construction_Price_Index_MF.csv")

colnames(CPI_MF) <- c("Date", "Multifamily", "Annual_Growth")
colnames(CPI_SF) <- c("Date", "Singlefamily", "Annual_Growth")

CPI_SF$Date <- as.Date(CPI_SF$Date,"%Y-%m-%d")
CPI_MF$Date <- as.Date(CPI_MF$Date, "%m/%d/%Y")

POPULATION <- fredr(series_id = "POPTHM") #population

Starts_Population_Merge <- merge(TOTAL_HOUSING_STARTS, POPULATION, by = "date")

LISTINGS_CUT <- fredr(series_id = "PRIREDCOUUS") #Active Lisings with Price Reduced
LISTINGS_TOTAL <- fredr(series_id = "ACTLISCOUUS") #Total Active Listings

LISTINGS_MEDIAN <- fredr(series_id = "MEDLISPRIPERSQUFEEUS", units = "pc1") %>% drop_na() #Percent Growth in Median Listing Price

LISTINGS_CUT_SHARE <- merge(LISTINGS_CUT, LISTINGS_TOTAL, by = "date") %>%
  transmute(date, value = value.x/value.y)

PERMIT <- fredr(series_id = "PERMIT", observation_start = as.Date("2000-01-01"))
PERMIT_SFH <- fredr(series_id = "PERMIT1", observation_start = as.Date("2000-01-01"))

#employment levels for NDP credit intermediation and residential building
NDP_CREDIT_INTERMEDIATION <- bls_api("CES5552220001", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

RESIDENTIAL_BUILDING <- bls_api("CES2023610001", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

TSY_MBS_SPREAD <- fredr(series_id = "MORTGAGE30US") %>% #calculating treasury spreads
  merge(., fredr(series_id = "DGS30"), by = "date") %>%
  transmute(date, value = value.x-value.y) %>%
  subset(date > as.Date("1996-12-31")) %>%
  group_by(yw = paste(year(date), month(date))) %>%
  drop_na() %>%
  mutate_if(is.numeric, ~mean(.))

#Downloading Rent data for class A and B/C cities
CPIRENTA <- bls_api("CUSR0000SEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -3))) 
CPIRENTBC <- bls_api("CUURN000SEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -3))) 

#Downloading Rent data for Superstar vs Secondary Cities
CPIRENTNY <- bls_api("CUURS12ASEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -4))) 
CPIRENTLA <- bls_api("CUURS49ASEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -4))) 
CPIRENTSF <- bls_api("CUURS49BSEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -4)))

CPIRENTATL <- bls_api("CUURS35CSEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -4))) 
CPIRENTMIA <- bls_api("CUURS35BSEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -4))) 
CPIRENTPHO <- bls_api("CUURS48ASEHA", startyear = 2015, endyear = 2022, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(calculations = as.numeric(str_sub(.$calculations, start= -4)))

RESIDENTIAL_BUILDING <- bls_api("CES2023610001", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

Permits_California <- fredr(series_id = "CABPPRIVSA", observation_start = as.Date("2000-01-01"))
Permits_Texas <- fredr(series_id = "TXBPPRIV", observation_start = as.Date("2000-01-01"))
Permits_Florida <- fredr(series_id = "FLBPPRIV", observation_start = as.Date("2000-01-01"))
Permits_NC <- fredr(series_id = "NCBPPRIVSA", observation_start = as.Date("2000-01-01"))
Permits_Dallas <- fredr(series_id = "DALL148BPPRIVSA", observation_start = as.Date("2000-01-01"))


Household_Equity <- fredr(series_id = "HOEREPHRE", observation_start = as.Date("2000-01-01"))

REDFIN_RENT_OWN <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/redfin_rent_own.csv") %>%
  mutate(Average_Rent = gsub(",","",.$Average_Rent)) %>%
  mutate(Average_Rent = gsub("\\$","",.$Average_Rent)) %>%
  mutate(Average_Mortgage = gsub(",","",.$Average_Mortgage)) %>%
  mutate(Average_Mortgage = gsub("\\$","",.$Average_Mortgage)) %>%
  mutate(Average_Mortgage = as.numeric(Average_Mortgage)) %>%
  mutate(Average_Rent = as.numeric(Average_Rent)) %>%
  mutate(date = as.Date(?..Date))
  

RENT_ABC_Graph<- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=CPIRENTA, aes(x=date,y= calculations/100, color= "CPI Rent of Primary Residence: Largest Metro Areas"), size = 1.25) +
  geom_line(data=CPIRENTBC, aes(x=date,y= calculations/100, color= "CPI Rent of Primary Residence: Medium/Small Metro Areas"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.08), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Superstar Upset") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices Growth in Smaller Metros Has Caught Up to Larger Metros") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*.08), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

RENT_STAR_CITIES_Graph<- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=CPIRENTNY, aes(x=date,y= calculations/100, color= "New York"), size = 1.25) +
  geom_line(data=CPIRENTLA, aes(x=date,y= calculations/100, color= "Los Angeles"), size = 1.25) +
  geom_line(data=CPIRENTSF, aes(x=date,y= calculations/100, color= "San Francisco"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.01,.22), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Superstar Upset") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices Growth in Smaller Metros Has Caught Up to Larger Metros") +
  theme_apricitas + theme(legend.position = c(.45,.65)) +
  scale_color_manual(name= "CPI: Rent of Primary Residence by Metro Area" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.01-(.3*.22), ymax = -0.01) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

RENT_SMALL_CITIES_Graph<- ggplot() + #plotting rent by minor city
  geom_line(data=CPIRENTATL, aes(x=date,y= calculations/100, color= "Atlanta"), size = 1.25) +
  geom_line(data=CPIRENTMIA, aes(x=date,y= calculations/100, color= "Miami"), size = 1.25) +
  geom_line(data=CPIRENTPHO, aes(x=date,y= calculations/100, color= "Phoenix"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.01,.22), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Superstar Upset") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices Growth in Smaller Metros Has Caught Up to Larger Metros") +
  theme_apricitas + theme(legend.position = c(.45,.65)) +
  scale_color_manual(name= "CPI: Rent of Primary Residence by Metro Area" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.01-(.3*.22), ymax = -0.01) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Graphing permits by state
PERMITS_STATE_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=Permits_California, aes(x=date,y= value/1000, color= "California"), size = 1.25) +
  geom_line(data=Permits_Texas, aes(x=date,y= value/1000, color= "Texas"), size = 1.25) +
  geom_line(data=Permits_Florida, aes(x=date,y= value/1000, color= "Florida"), size = 1.25) +
  geom_line(data=Permits_NC, aes(x=date,y= value/1000, color= "North Carolina"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,30), expand = c(0,0)) +
  ylab("Permits, Monthly") +
  ggtitle("Going South for the Winter") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Permits are Rising in States With Less-Restrictive Land Use Regulations") +
  theme_apricitas + theme(legend.position = c(.65,.75)) +
  scale_color_manual(name= "Private Housing Units Authorized by Permits" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*30), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

PERMITS_Dallas_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=Permits_California, aes(x=date,y= value/1000, color= "California"), size = 1.25) +
  geom_line(data=Permits_Dallas, aes(x=date,y= value/1000, color= "Dallas Metropolitan Area"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,21), expand = c(0,0)) +
  ylab("Permits, Monthly") +
  ggtitle("Don't Texas My California") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "In July, More Housing Permits Were Issued in the Dallas MSA than in All of California") +
  theme_apricitas + theme(legend.position = c(.65,.75)) +
  scale_color_manual(name= "Private Housing Units Authorized by Permits" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*21), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

PERMIT_SF <- fredr(series_id = "SANF806BPPRIVSA", observation_start = as.Date("2000-01-01"))
PERMIT_Lakeland <- fredr(series_id = "LAKE412BPPRIVSA", observation_start = as.Date("2000-01-01"))
PERMIT_Boise <- fredr(series_id = "BOIS216BPPRIVSA", observation_start = as.Date("2000-01-01"))

PERMITS_SF_Boise_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=PERMIT_SF, aes(x=date,y= value/1000, color= "SF-Oakland-Berkeley, CA (MSA)"), size = 1.25) +
  geom_line(data=PERMIT_Boise, aes(x=date,y= value/1000, color= "Boise City, ID (MSA)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,3), expand = c(0,0)) +
  ylab("Permits, Monthly") +
  ggtitle("Zoned Out") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Boise, ID Permits about as Much Housing as SF") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= "Private Housing Units Authorized by Permits" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("SF-Oakland-Berkeley, CA (MSA)","Boise City, ID (MSA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

PERMITS_SF_Lakeland_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=PERMIT_SF, aes(x=date,y= value/1000, color= "SF-Oakland-Berkeley, CA (MSA)"), size = 1.25) +
  geom_line(data=PERMIT_Lakeland, aes(x=date,y= value/1000, color= "Lakeland-Winter Haven, FL (MSA)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,3), expand = c(0,0)) +
  ylab("Permits, Monthly") +
  ggtitle("Zoned Out") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Lakeland, FL Permits About as Much Housing as SF") +
  theme_apricitas + theme(legend.position = c(.55,.85)) +
  scale_color_manual(name= "Private Housing Units Authorized by Permits" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("SF-Oakland-Berkeley, CA (MSA)","Lakeland-Winter Haven, FL (MSA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


REDFIN_Graph <- ggplot() + #plotting redfin data
  geom_line(data=REDFIN_RENT_OWN, aes(x=date,y= Average_Rent, color= "Average Monthly Rent"), size = 1.25) +
  geom_line(data=REDFIN_RENT_OWN, aes(x=date,y= Average_Mortgage, color= "Average Monthly Mortgage Payment, 20% Down"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), limits = c(0,2500), expand = c(0,0)) +
  ylab("Dollars, Monthly") +
  ggtitle("Rent to Buy") +
  labs(caption = "Graph created by @JosephPolitano using Redfin data",subtitle = "Monthly Mortgage Payments are Rising") +
  theme_apricitas + theme(legend.position = c(.55,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*2500), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Household_Equity <- ggplot() + #plotting spread
  geom_line(data=Household_Equity, aes(x=date,y= value/100, color= "Owners' Equity in Real Estate as a Percentage of Household Real Estate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ylab("Percent, Monthly Average") +
  ggtitle("Deleveraging") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "In Aggregate, Homeowners Have A Higher Equity Share of Their Homes") +
  theme_apricitas + theme(legend.position = c(.5,.8)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CREDIT_RESIDENTIAL_Graph <- ggplot() + #plotting authorized not started
  geom_line(data=NDP_CREDIT_INTERMEDIATION, aes(x=date,y= value, color= "All Employees, Nondepository Credit Intermediation"), size = 1.25) +
  geom_line(data=RESIDENTIAL_BUILDING, aes(x=date,y= value, color= "All Employees, Residential Building"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "K", accuracy = 1), limits = c(550,950), expand = c(0,0)) +
  ylab("Thousands") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Residential Construction Employment Has Held Up Even With Higher Mortgage Rates") +
  theme_apricitas + theme(legend.position = c(.40,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("All Employees, Residential Building","All Employees, Nondepository Credit Intermediation")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 550-(.3*400), ymax = 550) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

LISTINGS_CUT_SHARE_Graph<- ggplot() + #plotting total listings
  geom_line(data=LISTINGS_CUT_SHARE, aes(x=date,y= value, color= "Share of Total Active Listings With a Price Cut"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.50), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Housing Bust?") +
  labs(caption = "Graph created by @JosephPolitano using Realtor.com data",subtitle = "Price Cuts are More Common as Mortgage Rates Rise") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-07-01")-(.1861*(today()-as.Date("2016-07-01"))), xmax = as.Date("2016-07-01")-(0.049*(today()-as.Date("2016-07-01"))), ymin = 0-(.3*.50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

LISTINGS_MEDIAN_Graph<- ggplot() + #plotting total listings
  geom_line(data=LISTINGS_MEDIAN, aes(x=date,y= value/100, color= "Median Listing Price per Square Feet"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.25), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Housing Bust?") +
  labs(caption = "Graph created by @JosephPolitano using Realtor.com data",subtitle = "Listing Prices Have Grown Dramatically During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-07-01")-(.1861*(today()-as.Date("2017-07-01"))), xmax = as.Date("2017-07-01")-(0.049*(today()-as.Date("2017-07-01"))), ymin = 0-(.3*.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

TSY_MBS_Graph <- ggplot() + #plotting spread
  geom_line(data=TSY_MBS_SPREAD, aes(x=date,y= value/100, color= "30 Year Fixed Mortgage Spread Over Treasuries"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.03), expand = c(0,0)) +
  ylab("Percent, Monthly Average") +
  ggtitle("Mind the Gap") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Spread Between Mortgage Rates and Treasuries Has Increased") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1996-12-01")-(.1861*(today()-as.Date("1996-12-01"))), xmax = as.Date("1996-12-01")-(0.049*(today()-as.Date("1996-12-01"))), ymin = 0-(.3*.03), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

STARTS_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=TOTAL_HOUSING_STARTS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Boom?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Total Housing Starts are at the Highest Levels in More than A Decade") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Privately-Owned Housing Units Started","New Privately-Owned Housing Units Completed")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*28000), xmax = as.Date("1960-01-01")-(0.049*28000), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

PERMITS_Graph <- ggplot() + #plotting permits
  geom_line(data=PERMIT, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Authorized in Permit-Issuing Places"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Bust?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Permits are Down More Than 10% from Their Recent Highs") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

PERMIT_SFH_Graph <- ggplot() + #plotting permits
  geom_line(data=PERMIT_SFH, aes(x=date,y= value/1000, color= "New Single-Family Housing Units Authorized in Permit-Issuing Places"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Bust?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Permits are Down Significantly for Single-Family Units") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*2), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

PRIV_CONS_SPEND_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=PRIV_CONS_SPEND , aes(x=date,y= value/1000, color= "Private Residential Construction Spending"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,1000), expand = c(0,0)) +
  ylab("Dollar, Billions, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Boom?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Rising Prices and Increasing Starts Has Pushed Construction Spending Higher") +
  theme_apricitas + theme(legend.position = c(.35,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*1000), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

STARTS_PERCAPITA_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=Starts_Population_Merge, aes(x=date,y= (value.x/value.y), color= "New Privately-Owned Housing Units Started Per Capita"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(), limits = c(0,.012), expand = c(0,0)) +
  ylab("Units Per Capita, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Boom?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "When Adjusted for Population, Housing Starts Still Look Weak") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*28000), xmax = as.Date("1960-01-01")-(0.049*28000), ymin = 0-(.3*.012), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

TOTAL_HOUSING_GROWTH_Graph <- ggplot() + #plotting growth in total housing units
  geom_line(data=TOTAL_HOUSING_GROWTH, aes(x=date,y= value/100, color= "Annual Growth in Total Housing Units"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(0,.02),breaks = c(0,0.005,0.01,0.015,0.02), expand = c(0,0)) +
  ylab("Percent Change From Year Ago, %") +
  ggtitle("Away from Home") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Growth in the Housing Stock is Extremely Low") +
  theme_apricitas + theme(legend.position = c(.7,.60)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-04-01")-(.1861*8250), xmax = as.Date("2000-04-01")-(0.049*8250), ymin = 0-(.3*.02), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SF_MF_CPI_Graph <- ggplot() + #plotting growth in total housing units
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CPI_SF, aes(x=Date,y= Annual_Growth, color= "Construction Price Index: Single Family"), size = 1.25) +
  geom_line(data=CPI_MF, aes(x=Date,y= Annual_Growth, color= "Construction Price Index: Multi-Family"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.08,.15),breaks = c(-0.05,0,0.05,0.1,0.15), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2022-01-01")))+
  ylab("Percent Change From Year Ago, %") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Construction Costs are Rising Rapidly, Especially for Single-Family Homes") +
  theme_apricitas + theme(legend.position = c(.27,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Construction Price Index: Single Family","Construction Price Index: Multi-Family")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*12250), xmax = as.Date("1990-01-01")-(0.049*12250), ymin = -0.08-(.3*.23), ymax = -0.08) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

VACANCY_RATE_Graph <- ggplot() + #plotting rental vacancy rate
  geom_line(data=RENTAL_VACANCY_RATE, aes(x=date,y= value/100, color= "Rental Vacancy Rate"), size = 1.25) +
  geom_line(data=HOMEOWNER_VACANCY_RATE, aes(x=date,y= value/100, color= "Homeowner Vacancy Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(0,.12),breaks = c(0,0.04,0.08,0.12), expand = c(0,0)) +
  ylab("Vacancy Rate, %") +
  ggtitle("No Vacancies") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Vacancy Rates are at the Lowest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.75,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Rental Vacancy Rate","Homeowner Vacancy Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*.12), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

AUTHORIZED_NOT_STARTED_Graph <- ggplot() + #plotting authorized not started
  geom_line(data=AUTHORIZED_NOT_STARTED, aes(x=date,y= value, color= "New Privately-Owned Housing Units Authorized but Not Started"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "K", accuracy = 1), limits = c(0,300), expand = c(0,0)) +
  ylab("Units, Thousands, Seasonally Adjusted") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "The Number of Authorized Units that Haven't Broke Ground is at the Highest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SFH_MF_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=HOUSING_STARTS_5PLUS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started: Units in Buildings with 5 Units or More"), size = 1.25) +
  geom_line(data=HOUSING_STARTS_SFH, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started: Single-Family Units"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Starts Have Fallen From 2022 Highs") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Single Family Housing Starts Have Dropped Significantly—But Multifamily Starts Held Up Better") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SFH_STARTS_COMPS_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=HOUSING_COMPS_SFH, aes(x=date,y= value/1000, color= "Single-Family Housing Completions"), size = 1.25) +
  geom_line(data=HOUSING_STARTS_SFH, aes(x=date,y= value/1000, color= "Single-Family Housing Starts"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Single-Family Completions Now Exceed Starts") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Single Family Housing Starts Dropped 30% as Mortgage Rates Rose") +
  theme_apricitas + theme(legend.position = c(.5,.93), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Single-Family Housing Starts","Single-Family Housing Completions")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

UNDER_CONSTRUCTION_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=TOTAL_HOUSING_UNDERCONSTRUCTION, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Under Construction"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Supply Chain Issues Have Meant that Many Housing Starts Haven't Been Finished") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

STARTS_COMPLETIONS_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=TOTAL_HOUSING_STARTS_SUBSET, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started"), size = 1.25) +
  geom_line(data=TOTAL_HOUSING_COMPLETIONS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Completed"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Supply Chain Issues Have Meant that Many Housing Starts Haven't Been Finished") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Privately-Owned Housing Units Started","New Privately-Owned Housing Units Completed")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

THIRTY_YEAR_FIXED_Graph <- ggplot() + #plotting growth in total housing units
  geom_line(data=THIRTY_YR_FIXED, aes(x=date,y= value/100, color= "30-Year Fixed Rate Mortgage Average in the United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.09),breaks = c(0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Mortgage Rates Have Risen Rapidly") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Mortgage Rates Have Fallen Slightly From 20-Year Highs, But Remain Elevated") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.09), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#redfin list price graph
REDFIN_LIST_PRICE <- fredr(series_id = "MEDLISPRIPERSQUFEEUS")

REDFIN_LIST_PRICE_Graph <- ggplot() + #plotting redfin data
  geom_line(data=REDFIN_LIST_PRICE, aes(x=date,y= value, color= "Redfin Median Listing Price per Square Feet in the United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), limits = c(100,250), expand = c(0,0)) +
  ylab("Dollars, Monthly") +
  ggtitle("Cooling Off") +
  labs(caption = "Graph created by @JosephPolitano using Redfin data",subtitle = "Listing Prices are Declining as Mortgage Rates Rise") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-07-01")-(.1861*(today()-as.Date("2016-07-01"))), xmax = as.Date("2016-07-01")-(0.049*(today()-as.Date("2016-07-01"))), ymin = 100-(.3*150), ymax = 100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SF_HOUSING_UNDERCONSTRUCTION <- fredr(series_id = "UNDCON1USA", observation_start = as.Date("2000-01-01")) #total under construction
MF_HOUSING_UNDERCONSTRUCTION <- fredr(series_id = "UNDCON5MUSA", observation_start = as.Date("2000-01-01")) #total under construction

SF_MF_UNDER_CONSTRUCTION_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=MF_HOUSING_UNDERCONSTRUCTION, aes(x=date,y= value/1000, color= "Housing Units Under Construction: Units in Buildings With 5 Units or More"), size = 1.25) +
  geom_line(data=SF_HOUSING_UNDERCONSTRUCTION, aes(x=date,y= value/1000, color= "Housing Units Under Construction: Single-Family Units"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.25), limits = c(0,1.25), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted") +
  ggtitle("The Construction Backlog is Dwindling") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Single-Family Homes Under Construction Is Dropping as Multi-Family Hits New Highs") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*1.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SF_MF_UNDER_CONSTRUCTION_Graph, "SF MF Under Construction.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
#plotting home price indices

CASE_SHILLER <- fredr(series_id = "CSUSHPINSA", observation_start = as.Date("2000-01-01")) #total under construction
ALL_TRANS <- fredr(series_id = "USSTHPI", observation_start = as.Date("2000-01-01")) #total under construction

HOME_PRICE_graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=ALL_TRANS, aes(x=date,y= value/value[21]*100, color= "US All-Transactions House Price Index"), size = 1.25) +
  geom_line(data=CASE_SHILLER, aes(x=date,y= value/value[61]*100, color= "S&P/Case-Shiller US National Home Price Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(), limits = c(50,200), expand = c(0,0)) +
  ylab("Index, Jan 2005 = 100") +
  ggtitle("Price Pressures") +
  labs(caption = "Graph created by @JosephPolitano using FHA & S&P data",subtitle = "Housing Prices Look to Be on the Decline for the First Time in Years") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 50-(.3*150), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HOME_PRICE_graph, "Home Price Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

# for sale/sold
SOLD <- fredr(series_id = "HSN1F", observation_start = as.Date("2000-01-01")) #total under construction
FOR_SALE <- fredr(series_id = "HNFSEPUSSA", observation_start = as.Date("2000-01-01")) #total under construction


SF_FOR_SALE_graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=SOLD, aes(x=date,y= value/1000, color= "New Single-Family Houses Sold (Annual Rate)"), size = 1.25) +
  geom_line(data=FOR_SALE, aes(x=date,y= value/1000, color= "New Single-Family Houses For Sale"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.25), limits = c(0,1.5), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted") +
  ggtitle("Sell-Side") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Completed Single Family Sales are Falling—As Units For Sale Hits a Post-2008 Peak") +
  theme_apricitas + theme(legend.position = c(.65,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*1.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SF_FOR_SALE_graph, "SF for Sale Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FIXED_RESI_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50406',
  'Frequency' = 'Q',
  'Year' = '2015,2016,2017,2018,2019,2020,2021,2022',
  'ResultFormat' = 'json'
)

FIXED_RESI_INVEST <- beaGet(FIXED_RESI_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2015-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVESTMENT_Residential_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a944rx_37_single_family_structures_chained_dollars_level_6/u50406_a944rx_37_single_family_structures_chained_dollars_level_6[1]*100, color = "Real Fixed Investment: Single-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST, aes(x=date, y = u50406_a946rx_42_improvements_chained_dollars_level_6/u50406_a946rx_42_improvements_chained_dollars_level_6[1]*100, color = "Real Fixed Investment: Residential Improvements"), size = 1.25) + 
  #geom_line(data = FIXED_INDUSTRIAL, aes(x=date, y = value/2.42, color = "Real Fixed Investment: Industrial Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(90,150), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Unfixed Problems") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Single-Family Homes and Home Improvements are Declining") +
  theme_apricitas + theme(legend.position = c(.60,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Real Fixed Investment: Single-Family Structures","Real Fixed Investment: Residential Improvements")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 90-(.3*60), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_RESI_INVEST_SPECS_2018 <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50406',
  'Frequency' = 'Q',
  'Year' = '2018,2019,2020,2021,2022,2023',
  'ResultFormat' = 'json'
)

FIXED_RESI_INVEST_2018 <- beaGet(FIXED_RESI_INVEST_SPECS_2018, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2018-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

FIXED_INVESTMENT_RESIDENTIAL_COMPONENT_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESI_INVEST_2018, aes(x=date, y = u50406_a944rx_37_single_family_structures_chained_dollars_level_6/u50406_a944rx_37_single_family_structures_chained_dollars_level_6[1]*100, color = "Single-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST_2018, aes(x=date, y = u50406_c292rx_38_multifamily_structures_chained_dollars_level_6/u50406_c292rx_38_multifamily_structures_chained_dollars_level_6[1]*100, color = "Multi-Family Structures"), size = 1.25) + 
  geom_line(data = FIXED_RESI_INVEST_2018, aes(x=date, y = u50406_a946rx_42_improvements_chained_dollars_level_6/u50406_a946rx_42_improvements_chained_dollars_level_6[1]*100, color = "Residential Improvements"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,135), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Real Fixed Residential Investment Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Fixed Investment in Single-Family Homes and Home Improvements are Declining") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= "Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Single-Family Structures","Multi-Family Structures","Residential Improvements")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*45), ymax = 85) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_RESIDENTIAL_COMPONENT_Graph, "Fixed Residential Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


MORTGAGE_ORIGINATIONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/ORIGINATIONS_FRBNY.csv") %>%
  select(Originations) %>%
  mutate(Originations = as.numeric(Originations)) %>%
  ts(., frequency = 4, start = c(2003, 1)) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2003-04-01"), to = as.Date("2023-04-01"),by = "3 months"))

MORTGAGE_ORIGINS_graph <- ggplot() + #plotting mortgage originations
  geom_line(data=MORTGAGE_ORIGINATIONS, aes(x=date,y= x/1000,color= "US Quarterly Mortgage Loan Originations"), size = 1.25)+ 
  #geom_line(data=MORTGAGE_ORIGINATIONS_NSA, aes(x=date,y= x,color= "US Quarterly Mortgage Loan Originations NSA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Billions of US Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"), breaks = c(0.5,1,1.5),limits = c(0,1.5), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("The Pandemic Mortgage Boom is Over") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY consumer credit data seasonally adjusted with X-13ARIMA", subtitle = "US Mortgage Loan Originations Have Fallen From Pandemic-era Highs as Mortgage Rates Rose") +
  theme_apricitas + theme(legend.position = c(.46,.60)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-04-01")-(.1861*(today()-as.Date("2003-04-01"))), xmax = as.Date("2003-04-01")-(0.049*(today()-as.Date("2003-04-01"))), ymin = 0-(.3*1.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MORTGAGE_ORIGINS_graph, "Mortgage Origins Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MORTAGE_ORIGINS_BREAKDOWN <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/ORIGINS_REFINANCE_NEW.csv") %>%
  select(purchase, refinance) %>%
  mutate(purchase = as.numeric(purchase)) %>%
  mutate(refinance = as.numeric(refinance)) %>%
  ts(., frequency = 4, start = c(2000, 1)) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2000-04-01"), to = as.Date("2023-04-01"),by = "3 months")) %>%
  pivot_longer(cols = c(purchase, refinance))

MORTAGE_ORIGINS_BREAKDOWN_graph <- ggplot() + #plotting mortgage originations
  geom_line(data=MORTAGE_ORIGINS_BREAKDOWN, aes(x=date,y= refinance,color= "Refinance"), size = 1.25)+
  geom_line(data=MORTAGE_ORIGINS_BREAKDOWN, aes(x=date,y= purchase,color= "Purchase"), size = 1.25)+ 
  #geom_line(data=MORTGAGE_ORIGINATIONS_NSA, aes(x=date,y= x,color= "US Quarterly Mortgage Loan Originations NSA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Billions of US Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), breaks = c(250,500,750),limits = c(0,750), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("The Pandemic Mortgage Boom is Over") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY consumer credit data seasonally adjusted with X-13ARIMA", subtitle = "Refinances Have Collapsed But Mortgages for Home Purchases Only Fell to 2019 Levels") +
  theme_apricitas + theme(legend.position = c(.46,.78)) +
  scale_color_manual(name= "Quarterly Mortgage Originations by Type",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-04-01")-(.1861*(today()-as.Date("2000-04-01"))), xmax = as.Date("2000-04-01")-(0.049*(today()-as.Date("2000-04-01"))), ymin = 0-(.3*750), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MORTAGE_ORIGINS_BREAKDOWN_graph, "Mortgage Origins Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MORTAGE_ORIGINS_BREAKDOWN_STACKED <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/ORIGINS_REFINANCE_NEW.csv") %>%
  select(purchase, refinance) %>%
  mutate(purchase = as.numeric(purchase)) %>%
  mutate(refinance = as.numeric(refinance)) %>%
  ts(., frequency = 4, start = c(2000, 1)) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2000-04-01"), to = as.Date("2023-04-01"),by = "3 months")) %>%
  pivot_longer(cols = c(purchase, refinance)) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = factor(name,levels = c("Refinance","Purchase")))

MORTAGE_ORIGINS_BREAKDOWN_STACKED_graph <- ggplot(data = MORTAGE_ORIGINS_BREAKDOWN_STACKED, aes(x = date, y = value/1000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "T"), breaks = c(0,0.5,1,1.5), limits = c(0,1.5), expand = c(0,0)) +
  ggtitle("The Pandemic Mortgage Boom is Over") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY consumer credit data seasonally adjusted with X-13ARIMA", subtitle = "Refinances Have Collapsed But Mortgages for Home Purchases Only Fell to 2019 Levels") +
  theme_apricitas + theme(legend.position = c(.5,.67)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Quarterly Mortgage Originations by Type",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Purchase","Refinance")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-04-01")-(.1861*(today()-as.Date("2000-04-01"))), xmax = as.Date("2000-04-01")-(0.049*(today()-as.Date("2000-04-01"))), ymin = 0-(.3*1.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MORTAGE_ORIGINS_BREAKDOWN_STACKED_graph, "Mortgage Origins Stacked Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

UMICH <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/UMich_Survey.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  drop_na()
  
UMICH_Graph<- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=UMICH, aes(x=Date,y= Bad.Time.Br.Prices.are.High/100, color= "% Saying it's a Bad Time to Buy a Home b/c Prices are High"), size = 1.25) +
  geom_line(data=UMICH, aes(x=Date,y= Bad.Time.br.Interest.Rates.High/100, color= "% Saying it's a Bad Time to Buy a Home b/c Interest Rates are High"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Americans are Souring on the Housing Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "People Currently Believe it is a Historically Terrible Time to Buy a Home") +
  theme_apricitas + theme(legend.position = c(.565,.89), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "University of Michigan Consumer Survey" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1978-01-01")-(.1861*(today()-as.Date("1978-01-01"))), xmax = as.Date("1978-01-01")-(0.049*(today()-as.Date("1978-01-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UMICH_Graph, "UMich Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Construction Stacked Graph
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

RE_CREDIT <- bls_api("CES5552229201", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
LOAN_BROKERS <- bls_api("CES5552231001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FINANCE_RBIND <- rbind(RE_CREDIT,LOAN_BROKERS) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES5552229201`+`CES5552231001`) %>%
  mutate(name = "Housing Related Creditors and Loan Brokers") %>%
  mutate(value = value-value[nrow(.)])

ARCHITECTURE <- bls_api("CES6054130001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
INTERIOR_DESIGN <- bls_api("CES6054141001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

SERVICES_RBIND <- rbind(ARCHITECTURE,INTERIOR_DESIGN) %>%
  select(date, value, seriesID) %>%
  pivot_wider(names_from = seriesID) %>%
  drop_na() %>%
  transmute(date, value = `CES6054130001`+`CES6054141001`) %>%
  mutate(name = "Architectural, Engineering, and Interior Design Services") %>%
  mutate(value = value-value[nrow(.)])

PLASTICS_PIPE_MANU <- bls_api("CES3232612001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
PAINT_MANU <- bls_api("CES3232550001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
FURNITURE_MANU <- bls_api("CES3133712001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
APPLIANCE_MANU <- bls_api("CES3133520001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CONSTRUCTION_MACHINERY_MANU <- bls_api("CES3133312001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
ARCHITECTURAL_METALS_MANU <- bls_api("CES3133230001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
CEMENT_CONCRETE <- bls_api("CES3132730001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
WOOD_PRODUCT <- bls_api("CES3132100001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

LOGGING <- bls_api("CES1011330001", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
SAND_GRAVEL_MINING <- bls_api("CES1021232101", startyear = 2020, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
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

ZHVI_FIPS <- read.csv("https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1684841233") %>%
  select(-RegionID, -SizeRank, - RegionType, - StateName, -StateName, - State, - Metro) %>%
  #transpose() %>%
  gather(key = "date", value = "value",-3, -2, -1) %>%
  mutate(date = as.Date(gsub("X","",date), "%Y.%m.%d")) %>%
  mutate(StateCodeFIPS = str_pad(StateCodeFIPS, 2, pad = 0)) %>%
  mutate(MunicipalCodeFIPS = str_pad(MunicipalCodeFIPS , 3, pad = 0)) %>%
  mutate(fips = paste0(StateCodeFIPS,MunicipalCodeFIPS)) %>%
  group_by(fips) %>%
  mutate(value = (value-lag(value,12))/lag(value,12)) %>%
  subset(date == max(date)) %>%
  ungroup()

# Load the US county map data
counties_map <- us_map(regions = "counties")

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

counties <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(fips = county_fips)

counties <- left_join(counties, ZHVI_FIPS, by = "fips")

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

ZHVI_Counties <- ggplot() +
  geom_sf(data = counties %>% mutate(value = case_when(
    value > 0.10 ~ 0.10,
    value < -0.10 ~ -0.10,
    TRUE ~ value
  )), aes(fill = value)) +
  geom_sf(data = counties, color = "black", fill = NA, lwd = 0.1) + # Black borders for counties
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  ggtitle("   Zillow Home Value Change Over The Last 12M") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Zillow data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = ZHVI_Counties, "ZHVI_Counties.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#SELECTED MAJOR METRO AREAS GRAPH

ZHVI_ZIP <- read.csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1685429158") %>%
  select(-RegionID, -SizeRank, - RegionType, - StateName) %>%
  #transpose() %>%
  gather(key = "date", value = "value",-5,-4,-3,-2,-1) %>%
  mutate(date = as.Date(gsub("X","",date), "%Y.%m.%d")) %>%
  mutate(ZIP = str_pad(RegionName, 5, pad = 0)) %>%
  group_by(ZIP) %>%
  mutate(value = (value-lag(value,12))/lag(value,12)) %>%
  subset(date == max(date)) %>%
  ungroup() %>%
  mutate(value = case_when(
    value > 0.10 ~ 0.10,
    value < -0.10 ~ -0.10,
    TRUE ~ value
  ))

options(tigris_use_sf = TRUE)
options(tigris_use_cache = TRUE)

ZIP <- zctas(year = 2020)
ZIP_ZHVI_MERGE <- left_join(ZIP, ZHVI_ZIP, by = c("ZCTA5CE20" = "ZIP"))

ZIP_NYC <- ZIP_ZHVI_MERGE %>%
  subset(State == "NY") %>%
  subset(City == "New York")
ZIP_NYC <- st_union(ZIP_NYC) %>%
  st_cast("POLYGON") 
ZIP_NYC <- nngeo::st_remove_holes(ZIP_NYC)


NYC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("New York-Newark-Jersey City, NY-NJ-PA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_NYC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("New York") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) + 
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_LA <- ZIP_ZHVI_MERGE %>%
  subset(State == "CA") %>%
  subset(City == "Los Angeles")
ZIP_LA <- st_union(ZIP_LA) %>%
  st_cast("POLYGON") 
ZIP_LA <- nngeo::st_remove_holes(ZIP_LA)

LA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Los Angeles-Long Beach-Anaheim, CA","Riverside-San Bernardino-Ontario, CA") & ZCTA5CE20 != "90704" & ZCTA5CE20 !="93562"), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_LA, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Greater Los Angeles") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))
  
ZIP_DC <- ZIP_ZHVI_MERGE %>%
  subset(State == "DC")
ZIP_DC <- st_union(ZIP_DC) %>%
  st_cast("POLYGON") 
ZIP_DC <- nngeo::st_remove_holes(ZIP_DC)

DC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Washington-Arlington-Alexandria, DC-VA-MD-WV")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Washington DC") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_Chicago <- ZIP_ZHVI_MERGE %>%
  subset(State == "IL") %>%
  subset(City == "Chicago")
ZIP_Chicago <- st_union(ZIP_Chicago) %>%
  st_cast("POLYGON") 
ZIP_Chicago <- nngeo::st_remove_holes(ZIP_Chicago)

CHI <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Chicago-Naperville-Elgin, IL-IN-WI")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_Chicago, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Chicago") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_SF <- ZIP_ZHVI_MERGE %>%
  subset(State == "CA") %>%
  subset(City == "San Francisco")
ZIP_SF <- st_union(ZIP_SF) %>%
  st_cast("POLYGON") 
ZIP_SF <- nngeo::st_remove_holes(ZIP_SF)

SF <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("San Francisco-Oakland-Berkeley, CA","San Jose-Sunnyvale-Santa Clara, CA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_SF, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("San Francisco Bay Area") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_DFW <- ZIP_ZHVI_MERGE %>%
  subset(State == "TX") %>%
  subset(City == "Dallas")
ZIP_DFW <- st_union(ZIP_DFW) %>%
  st_cast("POLYGON") 
ZIP_DFW <- nngeo::st_remove_holes(ZIP_DFW)

DFW <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Dallas-Fort Worth-Arlington, TX")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DFW, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Dallas-Fort Worth") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_Boston <- ZIP_ZHVI_MERGE %>%
  subset(State == "MA") %>%
  subset(City == "Boston")
ZIP_Boston <- st_union(ZIP_Boston) %>%
  st_cast("POLYGON") 
ZIP_Boston <- nngeo::st_remove_holes(ZIP_Boston)


BOS <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Boston-Cambridge-Newton, MA-NH")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_Boston, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Boston") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_HOU <- ZIP_ZHVI_MERGE %>%
  subset(State == "TX") %>%
  subset(City == "Houston")
ZIP_HOU <- st_union(ZIP_HOU) %>%
  st_cast("POLYGON") 
ZIP_HOU <- nngeo::st_remove_holes(ZIP_HOU)

HOU <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Houston-The Woodlands-Sugar Land, TX")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_HOU, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Houston") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_PHI <- ZIP_ZHVI_MERGE %>%
  subset(State == "PA") %>%
  subset(City == "Philadelphia")
ZIP_PHI <- st_union(ZIP_PHI) %>%
  st_cast("POLYGON") 
ZIP_PHI <- nngeo::st_remove_holes(ZIP_PHI)

PHI <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_PHI, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Philadelphia") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_ATL <- ZIP_ZHVI_MERGE %>%
  subset(State == "GA") %>%
  subset(City == "Atlanta")
ZIP_ATL <- st_union(ZIP_ATL) %>%
  st_cast("POLYGON") 
ZIP_ATL <- nngeo::st_remove_holes(ZIP_ATL)

ATL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Atlanta-Sandy Springs-Alpharetta, GA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_ATL, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Atlanta") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_DEN <- ZIP_ZHVI_MERGE %>%
  subset(State == "CO") %>%
  subset(City == "Denver")
ZIP_DEN <- st_union(ZIP_DEN) %>%
  st_cast("POLYGON") 
ZIP_DEN <- nngeo::st_remove_holes(ZIP_DEN)

DEN <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Denver-Aurora-Lakewood, CO")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DEN, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Denver") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_MIA <- ZIP_ZHVI_MERGE %>%
  subset(State == "FL") %>%
  subset(City == "Miami")
ZIP_MIA <- st_union(ZIP_MIA) %>%
  st_cast("POLYGON") 
ZIP_MIA <- nngeo::st_remove_holes(ZIP_MIA)

MIA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Miami-Fort Lauderdale-Pompano Beach, FL")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_MIA, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Miami") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_PHO <- ZIP_ZHVI_MERGE %>%
  subset(State == "AZ") %>%
  subset(City == "Phoenix")
ZIP_PHO <- st_union(ZIP_PHO) %>%
  st_cast("POLYGON") 
ZIP_PHO <- nngeo::st_remove_holes(ZIP_PHO)

PHO <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Phoenix-Mesa-Chandler, AZ")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_PHO, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Phoenix") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_DET <- ZIP_ZHVI_MERGE %>%
  subset(State == "MI") %>%
  subset(City == "Detroit")
ZIP_DET <- st_union(ZIP_DET) %>%
  st_cast("POLYGON") 
ZIP_DET <- nngeo::st_remove_holes(ZIP_DET)

DET <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Detroit-Warren-Dearborn, MI")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DET, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Detroit") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_SEA <- ZIP_ZHVI_MERGE %>%
  subset(State == "WA") %>%
  subset(City == "Seattle")
ZIP_SEA <- st_union(ZIP_SEA) %>%
  st_cast("POLYGON") 
ZIP_SEA <- nngeo::st_remove_holes(ZIP_SEA)

SEA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Seattle-Tacoma-Bellevue, WA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_SEA, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Seattle") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_MIN <- ZIP_ZHVI_MERGE %>%
  subset(State == "MN") %>%
  subset(City == "Minneapolis")
ZIP_MIN <- st_union(ZIP_MIN) %>%
  st_cast("POLYGON") 
ZIP_MIN <- nngeo::st_remove_holes(ZIP_MIN)


MIN <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Minneapolis-St. Paul-Bloomington, MN-WI")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_MIN, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Minneapolis") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_VEGAS <- ZIP_ZHVI_MERGE %>%
  subset(State == "NV") %>%
  subset(City == "Las Vegas")
ZIP_VEGAS <- st_union(ZIP_VEGAS) %>%
  st_cast("POLYGON") 
ZIP_VEGAS <- nngeo::st_remove_holes(ZIP_VEGAS)

VEGAS <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Las Vegas-Henderson-Paradise, NV")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_VEGAS, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Las Vegas") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_TAMPA <- ZIP_ZHVI_MERGE %>%
  subset(State == "FL") %>%
  subset(City == "Tampa")
ZIP_TAMPA <- st_union(ZIP_TAMPA) %>%
  st_cast("POLYGON") 
ZIP_TAMPA <- nngeo::st_remove_holes(ZIP_TAMPA)

TAMPA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Tampa-St. Petersburg-Clearwater, FL")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_TAMPA, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Tampa") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_SAN_DIEGO <- ZIP_ZHVI_MERGE %>%
  subset(State == "CA") %>%
  subset(City == "San Diego")
ZIP_SAN_DIEGO <- st_union(ZIP_SAN_DIEGO) %>%
  st_cast("POLYGON") 
ZIP_SAN_DIEGO <- nngeo::st_remove_holes(ZIP_SAN_DIEGO)

SAN_DIEGO <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("San Diego-Chula Vista-Carlsbad, CA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_SAN_DIEGO, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("San Diego") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_BALTIMORE <- ZIP_ZHVI_MERGE %>%
  subset(State == "MD") %>%
  subset(City == "Baltimore")
ZIP_BALTIMORE <- st_union(ZIP_BALTIMORE) %>%
  st_cast("POLYGON") 
ZIP_BALTIMORE <- nngeo::st_remove_holes(ZIP_BALTIMORE)

BAL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Baltimore-Columbia-Towson, MD")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_BALTIMORE, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Baltimore") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_STL <- ZIP_ZHVI_MERGE %>%
  subset(State == "MO") %>%
  subset(City == "Saint Louis")
ZIP_STL <- st_union(ZIP_STL) %>%
  st_cast("POLYGON") 
ZIP_STL <- nngeo::st_remove_holes(ZIP_STL)

STL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("St. Louis, MO-IL")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_STL, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("St. Louis") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_ORL <- ZIP_ZHVI_MERGE %>%
  subset(State == "FL") %>%
  subset(City == "Orlando")
ZIP_ORL <- st_union(ZIP_ORL) %>%
  st_cast("POLYGON") 
ZIP_ORL <- nngeo::st_remove_holes(ZIP_ORL)

ORL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Orlando-Kissimmee-Sanford, FL")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_ORL, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Orlando") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_CHAR <- ZIP_ZHVI_MERGE %>%
  subset(State == "NC") %>%
  subset(City == "Charlotte")
ZIP_CHAR <- st_union(ZIP_CHAR) %>%
  st_cast("POLYGON") 
ZIP_CHAR <- nngeo::st_remove_holes(ZIP_CHAR)

CHAR <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Charlotte-Concord-Gastonia, NC-SC")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_CHAR, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Charlotte") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_SAN_ANTONIO <- ZIP_ZHVI_MERGE %>%
  subset(State == "TX") %>%
  subset(City == "San Antonio")
ZIP_SAN_ANTONIO <- st_union(ZIP_SAN_ANTONIO) %>%
  st_cast("POLYGON") 
ZIP_SAN_ANTONIO <- nngeo::st_remove_holes(ZIP_SAN_ANTONIO)

SAN_ANTONIO <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("San Antonio-New Braunfels, TX")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_SAN_ANTONIO, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("San Antonio") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_POR <- ZIP_ZHVI_MERGE %>%
  subset(State == "OR") %>%
  subset(City == "Portland")
ZIP_POR <- st_union(ZIP_POR) %>%
  st_cast("POLYGON") 
ZIP_POR <- nngeo::st_remove_holes(ZIP_POR)

POR <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Portland-Vancouver-Hillsboro, OR-WA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_POR, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Portland") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_AUSTIN <- ZIP_ZHVI_MERGE %>%
  subset(State == "TX") %>%
  subset(City == "Austin")
ZIP_AUSTIN <- st_union(ZIP_AUSTIN) %>%
  st_cast("POLYGON") 
ZIP_AUSTIN <- nngeo::st_remove_holes(ZIP_AUSTIN)

AUSTIN <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Austin-Round Rock-Georgetown, TX")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_AUSTIN, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Austin") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_SAC <- ZIP_ZHVI_MERGE %>%
  subset(State == "CA") %>%
  subset(City == "Sacramento")
ZIP_SAC <- st_union(ZIP_SAC) %>%
  st_cast("POLYGON") 
ZIP_SAC <- nngeo::st_remove_holes(ZIP_SAC)

SAC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Sacramento-Roseville-Folsom, CA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_SAC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Sacramento") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_PITT <- ZIP_ZHVI_MERGE %>%
  subset(State == "PA") %>%
  subset(City == "Pittsburgh")
ZIP_PITT <- st_union(ZIP_PITT) %>%
  st_cast("POLYGON") 
ZIP_PITT <- nngeo::st_remove_holes(ZIP_PITT)

PITT <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Pittsburgh, PA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_PITT, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Pittsburgh") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))


ZIP_CIN <- ZIP_ZHVI_MERGE %>%
  subset(State == "OH") %>%
  subset(City == "Cincinnati")
ZIP_CIN <- st_union(ZIP_CIN) %>%
  st_cast("POLYGON") 
ZIP_CIN <- nngeo::st_remove_holes(ZIP_CIN)

CIN <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Cincinnati, OH-KY-IN")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_CIN, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Cincinnati") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_KC <- ZIP_ZHVI_MERGE %>%
  subset(State == "MO") %>%
  subset(City == "Kansas City")
ZIP_KC <- st_union(ZIP_KC) %>%
  st_cast("POLYGON") 
ZIP_KC <- nngeo::st_remove_holes(ZIP_KC)

KC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Kansas City, MO-KS")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_KC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Kansas City") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_COL <- ZIP_ZHVI_MERGE %>%
  subset(State == "OH") %>%
  subset(City == "Columbus")
ZIP_COL <- st_union(ZIP_COL) %>%
  st_cast("POLYGON") 
ZIP_COL <- nngeo::st_remove_holes(ZIP_COL)

COL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Columbus, OH")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_COL, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Columbus") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_IND <- ZIP_ZHVI_MERGE %>%
  subset(State == "IN") %>%
  subset(City == "Indianapolis")
ZIP_IND <- st_union(ZIP_IND) %>%
  st_cast("POLYGON") 
ZIP_IND <- nngeo::st_remove_holes(ZIP_IND)

IND <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Indianapolis-Carmel-Anderson, IN")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_IND, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Indianapolis") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

MEGA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Trenton-Princeton, NJ","Washington-Arlington-Alexandria, DC-VA-MD-WV","Baltimore-Columbia-Towson, MD","Philadelphia-Camden-Wilmington, PA-NJ-DE-MD","Boston-Cambridge-Newton, MA-NH","New York-Newark-Jersey City, NY-NJ-PA","New Haven-Milford, CT","Providence-Warwick, RI-MA","Bridgeport-Stamford-Norwalk, CT","Hartford-East Hartford-Middletown, CT","Worcester, MA-CT","Barnstable Town, MA","Norwich-New London, CT")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_PHI, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_NYC, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_Boston, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("          Zillow 12M Home Price Growth in the Acela Corridor") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = MEGA, "MEGA ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


DC_PLUS_BAL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, Metro %in% c("Washington-Arlington-Alexandria, DC-VA-MD-WV","Baltimore-Columbia-Towson, MD")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_BALTIMORE, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("DC & Baltimore") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))




ZIP_ZHVI_ARRANGE <- ggarrange(NYC,LA,SF,CHI,DFW,HOU,DC,PHI,ATL,  ncol = 3, nrow = 3, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

ZIP_ZHVI_ARRANGE <- annotate_figure(ZIP_ZHVI_ARRANGE, 
                                  top = text_grob("Zillow 12M Home Value Change in Major Metros
                                                  ", face = "bold", size = 28, color = "white")) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = ZIP_ZHVI_ARRANGE, "ZIP ZHVI ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#WESTERN
WESTERN_ARRANGE <- ggarrange(LA,SF,PHO,SEA,SAN_DIEGO,DEN,POR,SAC,VEGAS, ncol = 3, nrow = 3, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

WESTERN_ARRANGE <- annotate_figure(WESTERN_ARRANGE, 
                                    top = text_grob("Zillow 12M Home Value Change in Western Metros
                                                  ", face = "bold", size = 27, color = "white")) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = WESTERN_ARRANGE, "WEST ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#SOUTHERN
SOUTHERN_ARRANGE <- ggarrange(DFW,HOU,ATL,MIA,TAMPA,ORL,CHAR,SAN_ANTONIO,AUSTIN, ncol = 3, nrow = 3, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

SOUTHERN_ARRANGE <- annotate_figure(SOUTHERN_ARRANGE, 
                                   top = text_grob("Zillow 12M Home Value Change in Southern Metros
                                                  ", face = "bold", size = 26, color = "white")) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = SOUTHERN_ARRANGE, "SOUTH ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#MIDWEST
MIDWEST_ARRANGE <- ggarrange(CHI,DET,MIN,STL,PITT,CIN,KC,COL,IND, ncol = 3, nrow = 3, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

MIDWEST_ARRANGE <- annotate_figure(MIDWEST_ARRANGE, 
                                    top = text_grob("Zillow 12M Home Value Change in Midwestern Metros
                                                  ", face = "bold", size = 25, color = "white")) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = MIDWEST_ARRANGE, "MIDWEST ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NORTHEAST_ARRANGE <- ggarrange(NYC,DC_PLUS_BAL,PHI,BOS, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

NORTHEAST_ARRANGE <- annotate_figure(NORTHEAST_ARRANGE, 
                                   top = text_grob("Zillow 12M Home Value Change in Northeastern Metros
                                                  ", face = "bold", size = 24, color = "white")) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = NORTHEAST_ARRANGE, "NORTHEAST ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")





# states_boundaries <- states(class = "sf")
# 
# DONUT_DC_STATES <- subset(states_boundaries, STUSPS %in% c("DC", "MD", "VA"))
# 
# DONUT_DC_STATES <- st_transform(DONUT_DC_STATES, st_crs(ZIP_ZHVI_MERGE))

ZIP_DC <- ZIP_ZHVI_MERGE %>%
  subset(State == "DC")
ZIP_DC <- st_union(ZIP_DC) %>%
  st_cast("POLYGON") 
ZIP_DC <- nngeo::st_remove_holes(ZIP_DC)

DONUT_DC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, State %in% c("DC","MD","VA") & CountyName %in% c("District of Columbia","Arlington County","Prince Georges County","Fairfax County","Loudoun County","Prince William County","Charles County","Manassas Park City","Manassas City","Fairfax County","Falls Church City","Alexandria City","Howard County","Anne Arundel County")|
                          (State == "MD" & CountyName == "Montgomery County")), aes(fill = value, color = value))+
  #geom_sf(data = DONUT_DC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Washington DC") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))



DONUT_NYC_STATES <- subset(states_boundaries, STUSPS %in% c("NY", "NJ", "CT"))

DONUT_NYC_STATES <- st_transform(DONUT_NYC_STATES, st_crs(ZIP_ZHVI_MERGE))

ZIP_NYC <- ZIP_ZHVI_MERGE %>%
  subset(State == "NY") %>%
  subset(City == "New York")

ZIP_NYC <- st_union(ZIP_NYC) %>%
  st_cast("POLYGON") 
ZIP_NYC <- nngeo::st_remove_holes(ZIP_NYC)

  
DONUT_NYC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, 
                        (State %in% c("NY","NJ","CT") & CountyName %in% c("New York County","Kings County","Queens County","Richmond County","Bronx County","Nassau County","Suffolk County","Westchester County","Rockland County","Putnam County","Bergen County","Hudson County","Passaic County","Union County","Middlesex County","Monmouth County","Somerset County","Morris County","Fairfield County","New Haven County")) 
                        | (State == "NJ" & CountyName == "Essex County")), 
          aes(fill = value, color = value)) +#geom_sf(data = DONUT_DC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #geom_sf(data = DONUT_NYC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_NYC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("New York") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))


DONUT_CHI_STATES <- subset(states_boundaries, STUSPS %in% c("IL","IN","WI"))

DONUT_CHI_STATES <- st_transform(DONUT_CHI_STATES, st_crs(ZIP_ZHVI_MERGE))

ZIP_Chicago <- ZIP_ZHVI_MERGE %>%
  subset(State == "IL") %>%
  subset(City == "Chicago")

ZIP_Chicago <- st_union(ZIP_Chicago) %>%
  st_cast("POLYGON") 
ZIP_Chicago <- nngeo::st_remove_holes(ZIP_Chicago)


DONUT_CHI <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, State %in% c("IL","IN","WI") & CountyName %in% c("Cook County","Lake County","DuPage County","Will County","Lake County","Kenosha County")), 
                        aes(fill = value, color = value)) +#geom_sf(data = DONUT_DC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #geom_sf(data = DONUT_CHI_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_Chicago, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.10,0.10)) +
  ggtitle("Chicago") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))

ZIP_Boston <- ZIP_ZHVI_MERGE %>%
  subset(State == "MA") %>%
  subset(City == "Boston")

ZIP_Boston <- st_union(ZIP_Boston) %>%
  st_cast("POLYGON") 
ZIP_Boston <- nngeo::st_remove_holes(ZIP_Boston)

DONUT_BOS <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE, State %in% c("MA") & CountyName %in% c("Suffolk County","Essex County","Middlesex County","Norfolk County","Plymouth County","Bristol County")), 
          aes(fill = value, color = value)) +
  geom_sf(data = ZIP_Boston, fill = NA, color = "black", lwd = 0.40) +
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  scale_color_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = NULL,
                      aesthetics = "color",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.10,0.10)) +
  ggtitle("Boston") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))


ggsave(dpi = "retina",plot = DONUT_BOS, "DONUT BOS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


DONUT_ZHVI_ARRANGE <- ggarrange(DONUT_NYC,DONUT_CHI,DONUT_BOS,DONUT_DC,  ncol = 2, nrow = 2, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

DONUT_ZHVI_ARRANGE <- annotate_figure(DONUT_ZHVI_ARRANGE,top = text_grob("The 'Donut Effect' is Hitting Many Cities—Falls in Downtown Home Prices as Suburbs Hold Up Better
                                                  ", face = "bold", size = 10, color = "white")) + bgcolor("#252A32") + border("#252A32")
DONUT_ZHVI_ARRANGE <- annotate_figure(DONUT_ZHVI_ARRANGE, 
                                    top = text_grob("Zillow 12M Home Value Change", face = "bold", size = 28, color = "white")) + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = DONUT_ZHVI_ARRANGE, "DONUT ZHVI ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



ggsave(dpi = "retina",plot = FIXED_INVESTMENT_Residential_Graph, "Fixed Investment Residential.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = REDFIN_LIST_PRICE_Graph, "Redfin Listing Prices.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = THIRTY_YEAR_FIXED_Graph, "Thirty Year Fixed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_COMPLETIONS_Graph, "Starts Completions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = UNDER_CONSTRUCTION_Graph, "Under Construction.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SFH_MF_Graph, "Single Family Multi Family.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TOTAL_HOUSING_GROWTH_Graph, "Total Housing Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_PERCAPITA_Graph, "Starts Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_Graph, "Starts.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AUTHORIZED_NOT_STARTED_Graph, "Authorized Not Started.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = VACANCY_RATE_Graph, "Vacancy Rate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SF_MF_CPI_Graph, "SF MF CPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SFH_STARTS_COMPS_Graph, "SF Starts Comps.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = LISTINGS_MEDIAN_Graph, "Listings Median.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = LISTINGS_CUT_SHARE_Graph, "Cut Share.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TSY_MBS_Graph, "TSY MBS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CREDIT_RESIDENTIAL_Graph, "Credit and Residential Employment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PERMITS_Graph, "Permits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PERMIT_SFH_Graph, "Permits SFH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Household_Equity, "Household Equity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PERMITS_STATE_Graph, "Permits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RENT_ABC_Graph, "RENT ABC.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RENT_STAR_CITIES_Graph, "RENT STAR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RENT_SMALL_CITIES_Graph, "RENT SMALL.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = REDFIN_Graph, "REDFIN.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PERMITS_Dallas_Graph, "Dallas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ggsave(dpi = "retina",plot = PERMITS_SF_Boise_Graph, "SF Boise.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PERMITS_SF_Lakeland_Graph, "SF Lakeland.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
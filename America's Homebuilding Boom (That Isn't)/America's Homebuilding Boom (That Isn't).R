pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "June Saw the First Emplyoment Decreases in Housing Related Sectors Since Rates Increased") +
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
  ggtitle("La Vie Boheme") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Multifamily Housing Starts Are Above Pre-2008 Levels, But Single Family Starts Aren't") +
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
  ggtitle("Demand Destruction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Single Family Housing Starts Dropped 30% as Mortgage Rates Rose") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
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
  ggtitle("Adjusting Rates") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Mortgage Rates Have Rapidly Rebounded to Nearly 6%") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7250), xmax = as.Date("2000-01-01")-(0.049*7250), ymin = 0-(.3*.09), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
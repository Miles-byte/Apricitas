pacman::p_load(ggrepel,dots,ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

BEA_GOV_FIXED_INVEST_NOM_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "FixedAssets", # Specify dataset
  "TableName" = "FAAt705", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "Year" =  paste(seq(from = 1900, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GOV_FIXED_INVEST_NOM <- beaGet(BEA_GOV_FIXED_INVEST_NOM_SPECS, iTableStyle = FALSE) %>%
  select(`FAAt705 i3gstlc1sn00 65 Transportation Historical Cost Level 6`, `FAAt705 i3gstlc1sq00 67 Highways and streets Historical Cost Level 6`) %>%
  setNames(c("Transportation_N","Highways_N")) %>%
  mutate(date = (seq(as.Date("1901-01-01"), length.out = nrow(.), by = "1 year")))

BEA_GOV_FIXED_INVEST_QUAN_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "FixedAssets", # Specify dataset
  "TableName" = "FAAt706", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "Year" =  paste(seq(from = 1900, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GOV_FIXED_INVEST_QUAN <- beaGet(BEA_GOV_FIXED_INVEST_QUAN_SPECS, iTableStyle = FALSE) %>%
  select(`FAAt706 icgstlc1sn00 65 Transportation Fisher Quantity Index Level 0`, `FAAt706 icgstlc1sq00 67 Highways and streets Fisher Quantity Index Level 0`) %>%
  setNames(c("Transportation_Q","Highways_Q")) %>%
  mutate(date = (seq(as.Date("1901-01-01"), length.out = nrow(.), by = "1 year")))

BEA_GOV_FIXED_INVEST_REAL <- merge(BEA_GOV_FIXED_INVEST_NOM, BEA_GOV_FIXED_INVEST_QUAN, by = "date") %>%
  transmute(date, Highways = Highways_N[117]*Highways_Q/100, Transportation = Transportation_N[117]*Transportation_Q/100)

POPULATION <- fredr("B230RC0A052NBEA") %>%
  select(date,value)

BEA_GOV_FIXED_INVEST_REAL_POPULATION <- merge(BEA_GOV_FIXED_INVEST_REAL,POPULATION, by = "date") %>%
  mutate(Highways = Highways/value, Transportation = Transportation/value)

BEA_GOV_FIXED_INVEST_REAL_graph <- ggplot() + #plotting fixed gov investment
  geom_line(data= BEA_GOV_FIXED_INVEST_REAL_POPULATION, aes(x=date,y= Highways*1000, color= "Highways & Streets"), size = 1.25) +
  geom_line(data= BEA_GOV_FIXED_INVEST_REAL_POPULATION, aes(x=date,y= Transportation*1000, color= "Mass Transit (Including Airports)"), size = 1.25) +
  xlab("Year") +
  ylab("2017 Dollars Per Capita") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), breaks = c(0,100,200,300,400,500,600), limits = c(0,600), expand = c(0,0)) +
  ggtitle("US Transportation Investment Has Stagnated") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "American Real Per-Capita Investment In Highways & Mass Transit Has Been Flat for Decades") +
  theme_apricitas + theme(legend.position = c(.71,.87), plot.title = element_text(size = 26)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Real Per Capita Government Fixed Investment",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1929-01-01")-(.1861*(today()-as.Date("1929-01-01"))), xmax = as.Date("1929-01-01")-(0.049*(today()-as.Date("1929-01-01"))), ymin = 0-(.3*600), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_GOV_FIXED_INVEST_REAL_graph, "BEA Gov Fixed Invest Real.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NY_PUMAS <- pumas(state = "NY", cb = TRUE, year = 2020)
NJ_PUMAS <- pumas(state = "NJ", cb = TRUE, year = 2020)
CT_PUMAS <- pumas(state = "CT", cb = TRUE, year = 2020)

NY_NJ_CT_PUMAS <- rbind(NY_PUMAS,NJ_PUMAS,CT_PUMAS)

#EXTRACTED FROM https://data.census.gov/mdat/#/
TRANSIT_DATA_EXTRACT <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20York/TRANSIT_DATA_CSV.csv") %>%
  setNames(c("Selected.Geographies", "Total", "NOT AVAILABLE", "Car/Truck/Motorcycle", "Bus", "Subway", "Commuter Rail", "Light Rail", "Ferry", "Taxi", "Motorcycle", "Bicycle", "Walking", "WFH", "Other")) %>%
  mutate(`Car/Truck/Motorcycle` = `Car/Truck/Motorcycle` + Motorcycle) %>%
  mutate(Subway = Subway + `Light Rail`) %>%
  select(-`Motorcycle`, - `Light Rail`) %>%
  mutate(NAMELSAD20 = sub(";.*", "", Selected.Geographies)) %>%
  merge(., NY_NJ_CT_PUMAS, by = "NAMELSAD20") %>%
  #mutate_at(vars(Total:Other.method), as.numeric) %>%
  #filter(Total > 4750) %>%
  st_as_sf() %>%
  erase_water(year = 2020)

MANHATTAN_COMMUTER_DOT_MAP <- dots_points(shp = TRANSIT_DATA_EXTRACT, cols = c(Ferry, Taxi, Bicycle, Walking,`Commuter Rail`,`Car/Truck/Motorcycle`, Bus, Subway), divisor = 2000) %>%
  ggplot() +
  geom_sf(data = TRANSIT_DATA_EXTRACT, fill = "grey70", color = 'black') +
  #geom_sf(data = test1, fill = NA, color = "#EE6055") +
  #geom_sf(data = test2, fill = NA, color = "#3083DC", size = 1) +
  geom_sf(aes(color = dots_type), size = 1, alpha = 1) +
  scale_x_continuous(limits = c(-74.4,-73.5)) +
  scale_y_continuous(limits = c(40.45,41.05)) +
  ggtitle("How Do People Commute Into Manhattan?") +
  scale_color_manual(name= "1 Dot = 2k Commuters",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Subway","Car/Truck/Motorcycle","Bus","Commuter Rail","Walking","Bicycle","Taxi","Ferry"), labels = c("Subway (50%)","Car/Truck/Motorcycle (15%)","Bus (12%)","Commuter Rail (9%)","Walking (8%)","Bicycle (2%)","Taxi (1%)","Ferry (1%)"), guide = guide_legend(override.aes = list(size = c(3)))) +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 1-Yr Estimates by PUMA", subtitle = "In 2022, 72% of People Who Worked in Manhattan Commuted by Transit and Only 15% Drove") +
  theme_apricitas + theme(plot.title = element_text(size = 27),legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0.1, 0.1, 0.1), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = MANHATTAN_COMMUTER_DOT_MAP, "Manhattan Commuter Dot Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#copied manually from acs 2022 data
MSA_COMMUTE <- data.frame(MSA = c("NYC","Chicago","Boston","DC","LA","SF","Philly"), value = c(2284759,334379,210004,206430,199734,182625,177582)) %>%
  mutate(MSA = factor(MSA, levels = rev(c("NYC","Chicago","Boston","DC","LA","SF","Philly"))))

MSA_COMMUTE_graph <- ggplot(data = MSA_COMMUTE, aes(x = MSA, y = value/1000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("Transit Commuters") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"), limits = c(0,2.5), expand = c(0,0)) +
  ggtitle("Transit Commuters by Metropolitan Area") +
  labs(caption = "Graph created by @JosephPolitano using 2022 ACS 1-Yr Estimates", subtitle = "45% of America's Transit Commuters Live in the New York MSA—and 1/3 Live in the City Itself") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = MSA_COMMUTE_graph, "MSA Commuter Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PERMITS_MANHATTAN <- fredr("BPPRIV036061") %>%
  mutate(Borough = "Manhattan")
PERMITS_BROOKLYN <- fredr("BPPRIV036047") %>%
  mutate(Borough = "Brooklyn")
PERMITS_QUEENS <- fredr("BPPRIV036081") %>%
  mutate(Borough = "Queens")
PERMITS_BRONX <- fredr("BPPRIV036005") %>%
  mutate(Borough = "The Bronx")
PERMITS_STATEN <- fredr("BPPRIV036085") %>%
  mutate(Borough = "Staten Island")

PERMITS_NYC <- rbind(PERMITS_MANHATTAN,PERMITS_BROOKLYN,PERMITS_QUEENS,PERMITS_BRONX,PERMITS_STATEN) %>%
  mutate(Borough = factor(Borough, levels = c("Manhattan","Brooklyn","Queens","The Bronx",'Staten Island')))

PERMITS_NYC_graph <- ggplot(data = PERMITS_NYC, aes(x = date, y = value/1000, fill = Borough)) + #plotting housing permits
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("date") +
  ylab("Housing Permits") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,60), expand = c(0,0)) +
  scale_fill_manual(name= "Borough", values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotate("text",label = "NOTE: Permits Spike in 2015 Ahead of\nLapse of 421-a Construction Tax Exemption", x = as.Date("2006-06-01"), y = 50, color = "white", size = 4, alpha = 0.75) +
  ggtitle("NYC Housing Permits") +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "NYC Permits Have Mostly Remained Below Pre-08 Levels—and are Increasingly Outside Manhattan") +
  theme_apricitas + theme(legend.position = c(.15,.77)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*60), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PERMITS_NYC_graph, "Permits NYC Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NFP_US <- fredr("PAYEMS", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[25]*100)
NFP_NY <- fredr("NYNA", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[25]*100)
NFP_NYC <- fredr("SMS36935610000000001", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[25]*100)
NFP_NJ <- fredr("NJNA", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[25]*100)


EMPLOYMENT_NY_US_graph <- ggplot() + #plotting employment growth
  annotate("text",label = "Pre-COVID Job Level", x = as.Date("2018-10-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  geom_line(data= NFP_US, aes(x=date,y= value, color= "United States"), size = 1.25) +
  geom_line(data= NFP_NJ, aes(x=date,y= value, color= "New Jersey"), size = 1.25) +
  geom_line(data= NFP_NY, aes(x=date,y= value, color= "New York State"), size = 1.25) +
  geom_line(data= NFP_NYC, aes(x=date,y= value, color= "New York City"), size = 1.25) +
  xlab("date") +
  ylab("Payrolls (Index, Jan 2020 = 100)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(75,110), expand = c(0,0)) +
  scale_color_manual(name= "All Employees\nIndex, Jan 2020 = 100", values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("New York City","New York State","New Jersey","United States")) +
  ggtitle("New York's Weak Job Recovery") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data", subtitle = "NY Has Only Just Reached Pre-COVID Job Levels, Trailing Well Below The National Average") +
  theme_apricitas + theme(legend.position = c(.20,.35)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*35), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMPLOYMENT_NY_US_graph, "Employment NY US Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MONTHLY_STATE_LOCAL_CONST <- read.xlsx("https://www.census.gov/construction/c30/xlsx/slsatime.xlsx") %>%
  { setNames(., make.unique(unlist(.[2, ]))) } %>%
  slice(3:n()) %>%
  select(Date, `Transportation`,`Highway and street`,`Land passenger terminal`,`Mass transit`) %>%
  drop_na() %>%
  mutate(Date = sub("[^0-9]*$", "", Date),
         Date = my(Date)) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(Mass_Transit_Share = `Mass transit`/(`Land passenger terminal`+`Highway and street`+`Mass transit`))
  
MONTHLY_STATE_LOCAL_CONST_graph <- ggplot() + #plotting employment growth
  geom_line(data= MONTHLY_STATE_LOCAL_CONST, aes(x=Date,y= `Land passenger terminal`/(`Land passenger terminal`+`Highway and street`+`Mass transit`), color= "Passenger Terminals"), size = 1.25) +
  geom_line(data= MONTHLY_STATE_LOCAL_CONST, aes(x=Date,y= `Mass transit`/(`Land passenger terminal`+`Highway and street`+`Mass transit`), color= "Mass Transit"), size = 1.25) +
  xlab("date") +
  ylab("% of S&L Transportation Construction Spending") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.12), expand = c(0,0)) +
  ggtitle("Public Transportation Construction as a % of\nAll State & Local Land Transportation Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "Mass Transit's Share of Transportation Construction Has Fallen to The Lowest Level in 15 Years") +
  scale_color_manual(name= "% of Land Transportation Construction", values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Mass Transit","Passenger Terminals")) +
  theme_apricitas + theme(legend.position = c(.30,.75), plot.title = element_text(size = 23)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 0-(.3*.12), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MONTHLY_STATE_LOCAL_CONST_graph, "Monthly State Local Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TRANSIT_COST_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20York/TRANSIT_COST_PROJECTS.csv") %>%
  mutate(isNY = (City == "New York")) %>%
  select(Country, TunnelPer, Phase, Cost.km..2023.dollars., isNY) %>%
  setNames(c("Country","TunnelPer","Phase","Cost per km","isNY")) %>%
  mutate(TunnelPer = gsub("%","",TunnelPer)) %>%
  mutate(TunnelPer = ifelse(TunnelPer == "1", "100", TunnelPer)) %>% #A couple chinese tunnel projects were accidentally coded as 1% tunnel instead of 100% tunnel
  mutate(`Cost per km` = gsub(",","",`Cost per km`)) %>%
  mutate(TunnelPer = as.numeric(TunnelPer), `Cost per km` = as.numeric(`Cost per km`)) %>%
  filter(TunnelPer >= 75) %>%
  group_by(Country) %>%
  drop_na() %>%
  filter(n() > 10) %>%
  mutate(Country = recode(Country,
                          "CN" = "China",
                          "DE" = "Germany",
                          "ES" = "Spain",
                          "FR" = "France",
                          "IT" = "Italy",
                          "KR" = "South Korea",
                          "TR" = "Turkey")) %>%
  mutate(Country = factor(Country, levels = c("South Korea","Spain","Turkey","France","China","Italy","Germany","US")))
  

TRANSIT_COST_DATA_graph <- ggplot() + #plotting employment growth
  geom_point(data= TRANSIT_COST_DATA, aes(x=Country,y= `Cost per km`/1000, color= isNY), size = 3) +
  xlab("Country") +
  ylab("Transit Costs Per km (2023 PPP USD)") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,5.25), expand = c(0,0)) +
  ggtitle("New York's Massive Transit Construction Costs") +
  labs(caption = "Graph created by @JosephPolitano using Marron Institute-NYU Transit Cost Projects Data\n\"Underground\" Defined as 75%+ Tunnel. Countries Listed Are Those with 10+ Underground Transit Projects", subtitle = "New Transit Projects in the NYC Area are Some of the Most Expensive in the World") +
  scale_color_manual(name= "Construction Cost Per km\nUnderground Transit Projects",
                     breaks = "TRUE",
                     values = c("TRUE" = "#FFE98F", "FALSE" = "#00A99D"),
                     labels = c("TRUE" = "New York")) +
  geom_text_repel(
    data = TRANSIT_COST_DATA %>% filter(isNY == TRUE),
    aes(x = Country, y = `Cost per km` / 1000, label = Phase),
    nudge_x = -1.2,
    size = 4, 
    color = "white"
  ) +
  theme_apricitas + theme(legend.position = c(.30,.75), plot.title = element_text(size = 25), axis.text.x = element_text(size = 14))

ggsave(dpi = "retina",plot = TRANSIT_COST_DATA_graph, "Transit Cost Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NYC_TRANSIT_FUNDING <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20York/NEW_YORK_MTA_FUNDING.csv") %>%
  mutate(Year = as.Date(paste0(Year, "-01-01")))

NYC_TRANSIT_FUNDING_graph <- ggplot() + #plotting employment growth
  geom_line(data= NYC_TRANSIT_FUNDING, aes(x=Year,y= Fares/1000000000, color= "Fares"), size = 1.25) +
  geom_line(data= NYC_TRANSIT_FUNDING, aes(x=Year,y= Federal/1000000000, color= "Federal Funds"), size = 1.25) +
  geom_line(data= NYC_TRANSIT_FUNDING, aes(x=Year,y= State/1000000000, color= "State Funds"), size = 1.25) +
  geom_line(data= NYC_TRANSIT_FUNDING, aes(x=Year,y= Local/1000000000, color= "Local Funds"), size = 1.25) +
  xlab("date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,7.5), expand = c(0,0)) +
  ggtitle("MTA NYC Transit Funding, 2012-2022") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNote: Only MTA NYCT and Not Other MTA Branches. Includes Capital & Operational Funding", subtitle = "The NYC Subway Needed to be Rescued by Federal Money to Survive The COVID Ridership Drop") +
  scale_color_manual(name= NULL, values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Fares","Federal Funds","State Funds","Local Funds")) +
  theme_apricitas + theme(legend.position = c(.6,.68)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-06-01")-(.1861*(today()-as.Date("2012-06-01"))), xmax = as.Date("2012-06-01")-(0.049*(today()-as.Date("2012-06-01"))), ymin = 0-(.3*7.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NYC_TRANSIT_FUNDING_graph, "NYC Transit Funding Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

COSTS_KM_CA_HSR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20York/HSR%20Cost%20Data.csv") %>%
  select(Country, Line, Cost.km, Year) %>%
  mutate(isCA = (Country == "US")) %>%
  mutate(Year = if_else(isCA == TRUE, 2021, Year)) %>%
  mutate(Line = recode(Line, "CAHSR Bakersfield-Merced" = "Bakersfield-Merced\nInitial Operating Segment",
         "CAHSR SF-LA" = "Full SF-LA Line")) %>%
  #filter(Country %in% c("UK","FR","CN","JP","ES","DE","IT","US")) %>%
  mutate(Country = recode(Country,
                          "CN" = "China",
                          "DE" = "Germany",
                          "ES" = "Spain",
                          "FR" = "France",
                          "IT" = "Italy",
                          "KR" = "South Korea",
                          "JP" = "Japan")) %>%
  mutate(Country = factor(Country, levels = c("Spain","Germany","China","France","Italy","Japan","UK","US")))
  
COSTS_KM_CA_HSR_CPI <- fredr("CPIAUCSL", frequency = "a") %>%
  mutate(Year = year(date)) %>%
  drop_na() %>%
  mutate(value = value/value[n()]) %>%
  full_join(COSTS_KM_CA_HSR,., by = "Year") %>%
  drop_na() %>%
  mutate(real_cost_km = Cost.km/value)
  
CAHSR_COSTS_graph <- ggplot() + #plotting employment growth
  geom_point(data= COSTS_KM_CA_HSR_CPI, aes(x=Country,y= `real_cost_km`, color= isCA), size = 3) +
  xlab("Country") +
  ylab("High Speed Rail Costs Per km (2023 PPP USD)") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "M"), limits = c(0,290), expand = c(0,0)) +
  ggtitle("The Cost of California High Speed Rail") +
  labs(caption = "Graph created by @JosephPolitano using Marron Institute-NYU Transit Cost Projects Data", subtitle = "CAHSR, If Built at 2023 Projections, Would be Among the World's Most Expensive HSR Systems") +
  scale_color_manual(name= "Construction Cost Per km\nHigh Speed Rail Projects",
                     breaks = "TRUE",
                     values = c("TRUE" = "#FFE98F", "FALSE" = "#00A99D"),
                     labels = c("TRUE" = "California High Speed Rail\n2023 Cost Estimates")) +
  geom_text_repel(
    data = COSTS_KM_CA_HSR_CPI %>% filter(Line == "Full SF-LA Line"),
    aes(x = Country, y = `real_cost_km`, label = Line),
    nudge_x = -1,
    nudge_y = +35,
    size = 4, 
    color = "white"
  ) +
  geom_text_repel(
    data = COSTS_KM_CA_HSR_CPI %>% filter(Line == "Bakersfield-Merced\nInitial Operating Segment"),
    aes(x = Country, y = `real_cost_km`, label = Line),
    nudge_x = -1,
    nudge_y = -40,
    size = 4, 
    color = "white"
  ) +
  geom_text_repel(
    data = COSTS_KM_CA_HSR_CPI %>% filter(Line == "HS2"),
    aes(x = Country, y = `real_cost_km`, label = "UK HS2\n(pre-2023 Plans)"),
    nudge_x = -1,
    nudge_y = -10,
    size = 4, 
    color = "white"
  ) +
  theme_apricitas + theme(legend.position = c(.30,.75), axis.text.x = element_text(size = 16))

ggsave(dpi = "retina",plot = CAHSR_COSTS_graph, "CAHSR Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STATE_LOCAL_ANNUAL_RAILROAD_SPENDING <- read.xlsx("https://www.census.gov/construction/c30/xlsx/state.xlsx") %>%
  slice(-1:-2) %>%
  transpose() %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%# Set the first row as column names
  slice(-1) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(date = seq.Date(from = as.Date("2012-01-01"), by = "1 year", length.out = n()))

STATE_LOCAL_RAILROAD_graph <- ggplot() +
  geom_line(data= STATE_LOCAL_ANNUAL_RAILROAD_SPENDING, aes(x=date,y= railroad/1000, color= "All State & Local Intercity Railroad Construction Spending"), size = 1.25) +
  xlab("date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,3.5), expand = c(0,0)) +
  ggtitle("State & Local Railroad Spending, 2012-2022") +
  labs(caption = "Graph created by @JosephPolitano using Census Data\nNOTE: Railroad does not include Subway, Light Rail, or Other Urban Mass Transit", subtitle = "State & Local Railroad Spending—Including California HSR—is at Record Highs") +
  scale_color_manual(name= NULL, values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  theme_apricitas + theme(legend.position = c(.4,.85), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-06-01")-(.1861*(today()-as.Date("2012-06-01"))), xmax = as.Date("2012-06-01")-(0.049*(today()-as.Date("2012-06-01"))), ymin = 0-(.3*3.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STATE_LOCAL_RAILROAD_graph, "State & Local Railroad Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


MTA_2024_CAPITAL_PLAN <- data.frame(category = c("Congestion Pricing","Federal Funding","Real Estate Transfer Tax &\nEnd of Internet Sales Tax Exemptions","MTA Bonds &\n PAYGO","Other State Funding","Other City Funding"),
                                    value = c(15000,10680,10000,9792,3000,3000)) %>%
  mutate(category = factor(category, levels = rev(c("Congestion Pricing","Federal Funding","Real Estate Transfer Tax &\nEnd of Internet Sales Tax Exemptions","MTA Bonds &\n PAYGO","Other State Funding","Other City Funding")))) %>%
  mutate(pos = cumsum(value) - value/2)
  
MTA_2024_CAPITAL_PLAN <- MTA_2024_CAPITAL_PLAN %>%
  arrange(desc(category)) %>%
  mutate(pos = cumsum(value) - 0.5 * value)

MTA_2024_CAPITAL_PLAN_PIE <- ggplot(MTA_2024_CAPITAL_PLAN, aes(x = "", y = value, fill = category)) +
  geom_bar(width = 1, stat = "identity", color = NA) +
  xlab("") +
  ylab("") +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "",breaks = c("Congestion Pricing","Federal Funding","Real Estate Transfer Tax &\nEnd of Internet Sales Tax Exemptions","MTA Bonds &\n PAYGO","Other State Funding","Other City Funding"), values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93","#89023E")) +
  geom_label_repel(aes(label = paste0(category, "\n$", round(value / 1000, 1), "B"), y = pos), nudge_x = 1) +
  ggtitle("MTA 2020-2024 Capital Funding") +
  labs(caption = "Graph created by @JosephPolitano using MTA Data", subtitle = "Congestion Pricing Was Key to Funding the MTA's Recent Capital Plans") +
  theme_apricitas + theme(plot.title = element_text(size = 27),legend.position = "none", axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.ticks = element_blank(), panel.grid.minor = element_blank(), panel.grid.major=element_blank(), panel.grid = element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0.1, 0.1, -1), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = MTA_2024_CAPITAL_PLAN_PIE, "MTA 2024 Pie Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


  

  
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
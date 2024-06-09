pacman::p_load(dots,ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

??beaGet

list <- beaSets(beaKey = Sys.getenv("BEA_KEY"))

params <- beaParams(beaKey = Sys.getenv("BEA_KEY"), "FixedAssets")

paramvals <- beaParamVals(beaKey = Sys.getenv("BEA_KEY"), setName = "FixedAssets", paramName = "TableName")

list$Dataset

params$Parameter

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

MANHATTAN_COMMUTER_DOT_MAP2 <- dots_points(shp = TRANSIT_DATA_EXTRACT, cols = c(Ferry, Taxi, Bicycle, Walking,`Commuter Rail`,`Car/Truck/Motorcycle`, Bus, Subway), divisor = 2000) %>%
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

ggsave(dpi = "retina",plot = MANHATTAN_COMMUTER_DOT_MAP2, "Manhattan Commuter Dot Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

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


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
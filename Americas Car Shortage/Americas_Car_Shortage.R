pacman::p_load(openxlsx,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
library(blscrapeR)

Inventory <- fredr(series_id = "N864RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
Capacity_Util <- fredr(series_id = "CAPUTLG33611S",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Assemblies <- fredr(series_id = "MVATOTASSS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(date = as.Date(date), Shortfall = value - (mean(value[year(date) == 2019]))) %>%
  mutate(Cumulative_Shortfall = (cumsum(Shortfall))/12) %>%
  mutate(Cumulative_Shortfall = Cumulative_Shortfall-Cumulative_Shortfall[24])
AssembliesNSA <- fredr(series_id = "MVATOTASSN",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

AutoAssemblies <- fredr(series_id = "MVAAUTOASS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
TruckAssemblies <- fredr(series_id = "MVALTTRCKS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

Total_Car_Sales <- fredr(series_id = "TOTALSA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Auto_Car_Sales <- fredr(series_id = "LAUTOSA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Light_Truck_Sales <- fredr(series_id = "LTRUCKSA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
Heavy_Truck_Sales <- fredr(series_id = "HTRUCKSSAAR",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

Auto_Sales_Merge <- do.call("rbind", list(Auto_Car_Sales,Light_Truck_Sales,Heavy_Truck_Sales))
Auto_Sales_Merge$series_id <- gsub("HTRUCKSSAAR","Heavy Trucks",Auto_Sales_Merge$series_id)
Auto_Sales_Merge$series_id <- gsub("LTRUCKSA","Light Trucks",Auto_Sales_Merge$series_id)
Auto_Sales_Merge$series_id <- gsub("LAUTOSA","Automobiles",Auto_Sales_Merge$series_id)

Auto_Industrial_Capacity <- fredr(series_id = "CAPG33611S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)
Capacity_Util_1990s <- fredr(series_id = "CAPUTLG33611S",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)

Auto_Utilized_Capacity_Merge <- merge(Auto_Industrial_Capacity, Capacity_Util_1990s, by = "date")

Auto_Inventory_Sales_Ratio <- fredr(series_id = "AISRSA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

VMT <- fredr(series_id = "TRFVOLUSM227SFWA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), plot.title = element_text(size = 28, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Plant_Underutilization <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americas%20Car%20Shortage/Plant_Underutilization.csv") %>%
  drop_na(Not_Profitable) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"))

Plant_Underutilization_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=Plant_Underutilization, aes(x=Date,y= Insufficient_Materials/100,color= "Insufficient Materials"), size = 1.25)+ 
  geom_line(data=Plant_Underutilization, aes(x=Date,y= Insufficient_Labor/100,color= "Insufficient Labor"), size = 1.25)+ 
  geom_line(data=Plant_Underutilization, aes(x=Date,y= Insufficient_Orders/100,color= "Insufficient New Orders"), size = 1.25)+ 
  xlab("Date") +
  ylab("% Saying Yes") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,.80), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Why isn't America Making Enough Cars?") +
  labs(caption = "Graph created by @JosephPolitano using US Census data", subtitle = "Reasons For Plant Capacity Underutilization in Transportation Equipment Manufacturing") +
  theme_apricitas + theme(legend.position = c(.76,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.8), ymax = 0) +
  coord_cartesian(clip = "off")

Vehicle_Inventory_Graph <- ggplot() + #plotting real private auto inventories
  geom_line(data=Inventory, aes(x=date,y= value,color= "Real Private Inventories: Retail trade: Motor vehicle and Parts Dealers"), size = 1.25)+ 
  xlab("Date") +
  ylab("Inventories, US Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(150,275), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Dealer's Choice") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Vehicle Inventories Have Dropped Almost 25% as Pandemic Demand Combined with a Chip Shortage") +
  theme_apricitas + theme(legend.position = c(.46,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 150-(.3*125), ymax = 150) +
  coord_cartesian(clip = "off")

Capacity_Utilization_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=Capacity_Util, aes(x=date,y= value/100,color= "Capacity Utilization: Automobile and Light Duty Motor Vehicles"), size = 1.25)+ 
  xlab("Date") +
  ylab("Capacity Utilization, %") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Underutilization") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Capacity Utilization and Total Production Decline as the Supply Chain Crunch Hits Automakers") +
  theme_apricitas + theme(legend.position = c(.56,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

Auto_Inventory_Sales_Ratio_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=Auto_Inventory_Sales_Ratio, aes(x=date,y= value,color= "Automobile Inventory/Sales Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("Inventory/Sales Ratio") +
  scale_y_continuous( limits = c(0,4), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Dealbreaker") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "The Automobile Inventory/Sales Ratio Has Sunk to All-Time Lows in 2021") +
  theme_apricitas + theme(legend.position = c(.56,.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*4), ymax = 0) +
  coord_cartesian(clip = "off")

Auto_Sales_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_area(data=Auto_Sales_Merge, aes(x=date,y= value,fill=series_id), color = NA)+ 
  xlab("Date") +
  ylab("Seasonally Adjusted Annual Sales, Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,19), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Go Big or Go Home") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "New Auto Sales Have Dropped Almost 40%, Compared to a 17% Drop for Light Trucks") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*19), ymax = 0) +
  coord_cartesian(clip = "off")

Cumulative_Shortfall_Graph <- ggplot(subset(Assemblies, date > as.Date("2020-01-01")), aes(x = date, y = -Cumulative_Shortfall, fill = "Cumulative Shortfall in US Motor Vehicle Production")) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  xlab("Date") +
  ylab("Millions of Motor Vehicles") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3,4,5), limits = c(0,5), expand = c(0,0)) +
  ggtitle("Falling Short") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Cumulative Shortfall in US Vehicle Production Sits at Nearly 4.75 Million Units") +
  theme_apricitas + theme(legend.position = c(.35,.95)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-03-01")-(.1861*(today()-as.Date("2020-03-01"))), xmax = as.Date("2020-03-01")-(0.049*(today()-as.Date("2020-03-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

VMT_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=VMT, aes(x=date,y= value/1000, color = "Vehicle Miles Travelled"), size = 1.25)+ 
  xlab("Date") +
  ylab("Vehicle Miles Travelled, Billions") +
  scale_y_continuous(labels = scales::number_format(suffix = "B", accuracy = 1), limits = c(160,290), breaks = c(160,200,240,280), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("On The Road Again") +
  labs(caption = "Graph created by @JosephPolitano using FHA data", subtitle = "Vehicle Miles Travelled Have Been Trending Marginally Downward in 2022") +
  theme_apricitas + theme(legend.position = c(0.26,0.55)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 160-(.3*130), ymax = 160) +
  coord_cartesian(clip = "off")

Auto_Industrial_Capacity_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=Auto_Industrial_Capacity, aes(x=date,y= value, color = "Industrial Capacity: Automobile and Light Duty Motor Vehicles"), size = 1.25)+ 
  xlab("Date") +
  ylab("Industrial Capacity, 2017 = 100") +
  scale_y_continuous(limits = c(60,180), breaks = c(60,80,100,120,140,160,180), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Capacity Growth") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "American Automobile Industrial Capacity Still Shows the Scars of 2008") +
  theme_apricitas + theme(legend.position = c(0.40,0.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(11596)), xmax = as.Date("1990-01-01")-(0.049*(11596)), ymin = 60-(.3*120), ymax = 60) +
  coord_cartesian(clip = "off")

Auto_Utilized_Capacity_Graph <- ggplot() + #plotting capacity utilization in Automobiles
  geom_line(data=Auto_Industrial_Capacity, aes(x=date,y= value, color = "Industrial Capacity: Automobile and Light Duty Motor Vehicles"), size = 1.25)+ 
  geom_line(data=Auto_Utilized_Capacity_Merge, aes(x=date,y= value.x*(value.y/100), color = "Industrial Production: Automobile and Light Duty Motor Vehicles"), size = 1.25)+ 
  xlab("Date") +
  ylab("Industrial Capacity, 2017 = 100") +
  scale_y_continuous(limits = c(00,170), breaks = c(0,20,40,60,80,100,120,140,160), expand = c(0,0)) +
  ggtitle("Capacity Recovery") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "American Automobile Industrial Capacity Has Increased More Than 18% Since Early 2020") +
  theme_apricitas + theme(legend.position = c(0.40,0.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*170), ymax = 0) +
  coord_cartesian(clip = "off")

Assemblies <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=G17&series=75edc36fae13b9ecdb1266e03ab34902&lastobs=100&from=&to=&filetype=csv&label=omit&layout=seriescolumn") %>%
  setNames(.[1, ]) %>%
  .[-1, ] %>%
  transmute(date = as.Date(paste0(`Time Period`,"-01")), value = as.numeric(MVA.TOTASS.S)) %>%
  subset(date >= as.Date("2018-01-01"))

Assemblies_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=Assemblies, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies"), size = 1.25)+ 
  annotate(geom = "hline", y = 10.9030, yintercept = 10.9030, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "2019 Average", x = as.Date("2021-07-01"), y = 11.5, color ="#FFE98F") +
  #geom_line(data=AssembliesNSA, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Motor Vehicle Assemblies, Millions, Annual Rate") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,14), breaks = c(0,4,8,12), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Fixing the Assembly Line") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Vehicle Assemblies Had Climbed Back to Their Pre-Pandemic Average Before the UAW Strikes") +
  theme_apricitas + theme(legend.position = c(0.75,0.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

Assemblies_Auto_Truck_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=AutoAssemblies, aes(x=date,y= value/0.0260, color = "Automobile Assemblies"), size = 1.25)+ 
  geom_line(data=TruckAssemblies, aes(x=date,y= value/0.0803, color = "Light Truck Assemblies"), size = 1.25)+ 
  #geom_line(data=AssembliesNSA, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Assemblies, Index, Jan 2018 = 100") +
  scale_y_continuous(labels = scales::number_format( accuracy = 1), limits = c(0,120), breaks = c(0,50,100), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Some Assembly Required") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "There is Still a Major Shortfall in Motor Vehicle Assemblies") +
  theme_apricitas + theme(legend.position = c(0.25,0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  #annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("1996-01-01"), y = 0.825, color ="#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")


Mannheim <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/091322/mannheim.csv") %>%
  mutate(date = as.Date(date))

CPIUSEDCARS <- bls_api("CUSR0000SETA02", startyear = 2019, endyear = 2025, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi used cars data

CPI_Mannheim_Used_Car_Vehicles_Graph <- ggplot() + #plotting "Used Cars and Trucks" and "Mannheim" price Indexes
  geom_line(data=CPIUSEDCARS, aes(x=date,y= (value/141)*100 ,color= "CPI: Used Cars and Trucks"), size = 1.25) +
  geom_line(data=subset(Mannheim, date > as.Date("2018-12-31")), aes(x=date,y= (mannheim/135.4)*100 ,color= "Mannheim Used Vehicles Value Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,180), breaks = c(90,120,150), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Mannheim Wholesale Data Could Be Giving Early Indications of a Fall in Used Car Prices") +
  theme_apricitas + theme(legend.position = c(.30,.70)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 80-(.3*100), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


#employment by stage of vehicle manufacturing
EMP_VEHICLES <- bls_api("CES3133610001", startyear = 2019, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi used cars data
EMP_BODIES <- bls_api("CES3133620001", startyear = 2019,endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi used cars data
EMP_PARTS <- bls_api("CES3133630001", startyear = 2019, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi used cars data

Employment_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=EMP_VEHICLES, aes(x=date,y= value, color = "Manufacturing: Motor Vehicle Assembly"), size = 1.25)+ 
  geom_line(data=EMP_BODIES, aes(x=date,y= value, color = "Manufacturing: Motor Vehicle Bodies and Trailers"), size = 1.25)+ 
  geom_line(data=EMP_PARTS, aes(x=date,y= value, color = "Manufacturing: Motor Vehicle Parts"), size = 1.25)+ 
  xlab("Date") +
  ylab("Employees, Thousands") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,650), breaks = c(0,200,400,600), expand = c(0,0)) +
  ggtitle("Fixing the Assembly Line") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment Has Recovered, But Also Shifted From Parts Manufacturing to Vehicle Assembly") +
  theme_apricitas + theme(legend.position = c(0.72,0.60), legend.title = element_text(size = 14)) +
  scale_color_manual(name= "Employment Levels",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*650), ymax = 0) +
  coord_cartesian(clip = "off")

ALL_EMPLOYEES_MOTOR_VEHICLE <- fredr(series_id = "CES3133600101",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

ALL_EMPLOYEES_MOTOR_VEHICLE_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=ALL_EMPLOYEES_MOTOR_VEHICLE, aes(x=date,y= value/1000, color = "All Employees, Manufacturing: Motor Vehicles & Parts"), size = 1.25)+ 
  xlab("Date") +
  ylab("Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), limits = c(0.6,1.1), breaks = c(0.6,0.7,0.8,0.9,1,1.1), expand = c(0,0)) +
  ggtitle("Employment Impact of the UAW Strikes") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Car Manufacturing Employment Fell by 33k in October Thanks Largely to the UAW Strike") +
  theme_apricitas + theme(legend.position = c(0.35,0.95), legend.title = element_text(size = 14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0.6-(.3*0.5), ymax = 0.6) +
  coord_cartesian(clip = "off")

PPI_MARGINS_NEW_DATA <- fredr(series_id = "PCU441110441110101",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
PPI_MARGINS_USED_DATA <- fredr(series_id = "PCU441110441110102",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

PPI_Car_Margins_Graph <- ggplot() + #plotting "Used Cars and Trucks" and "Mannheim" price Indexes
  geom_line(data=PPI_MARGINS_NEW_DATA, aes(x=date,y= value/value[24]*100,color= "PPI: New Car Dealers: New Car Margins"), size = 1.25) +
  geom_line(data=PPI_MARGINS_USED_DATA, aes(x=date,y= value/value[24]*100,color= "PPI: New Car Dealers: Used Car Margins"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,500), breaks = c(0,100,200,300,400,500), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Dealer Magins are Compressing") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Gross Margins for Car Dealerships are Coming Down From Record Highs") +
  theme_apricitas + theme(legend.position = c(.30,.70)) +
  scale_color_manual(name= "Raw Gross Dealer Margin\n(Retail Prices Less Acquisition Prices)\nIndexed January 2019 = 100",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*500), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

UNFILLED_ORDERS_MV_PARTS <- fredr(series_id = "AMVPUO",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

UNFILLED_ORDERS_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=UNFILLED_ORDERS_MV_PARTS, aes(x=date,y= value/1000, color = "Unfilled Orders: Durable Goods: Motor Vehicles and Parts"), size = 1.25)+ 
  xlab("Date") +
  ylab("Employees") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ggtitle("Vehicle Unfilled Orders Remain High") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "Roughly $35B in Unfilled Orders for Motor Vehicles Still Remain, Up Significantly From Pre-COVID") +
  theme_apricitas + theme(legend.position = c(0.5,0.15), legend.title = element_text(size = 14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

#QUARTER NUMBER NEEDS TO BE UPDATED EVERY TIME YOU RUN THE CHART
DELINQUENT_FRBNY_CONSUMER_CREDIT <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/householdcredit/data/xls/HHD_C_Report_2025Q1.xlsx?sc_lang=en", sheet = "Page 12 Data") %>%
  slice(-(1:3)) %>%
  setnames(c("date","Mortgage","HELOC","Auto","Credit Card","Student Loan","Other","All","X")) %>%
  select(-Other,-All,-X) %>%
  mutate(date = seq.Date(from = as.Date("2003-01-01"), by = "3 months", length = nrow(.))) %>%
  mutate(across(where(is.character), ~ as.numeric(.))) %>%
  pivot_longer(cols = Mortgage:`Student Loan`)

#QUARTER NUMBER NEEDS TO BE UPDATED EVERY TIME YOU RUN THE CHART
NEWLY_DELINQUENT_FRBNY_CONSUMER_CREDIT <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/householdcredit/data/xls/HHD_C_Report_2025Q1.xlsx?sc_lang=en", sheet = "Page 14 Data") %>%
  slice(-(1:4)) %>%
  select(1:8) %>%
  setnames(c("date","Auto","CC","Mortgage","HELOC","Student Loan","Other","All")) %>%
  select(-Other,-All) %>%
  mutate(date = seq.Date(from = as.Date("2003-01-01"), by = "3 months", length = nrow(.))) %>%
  mutate(across(where(is.character), ~ as.numeric(.))) %>%
  pivot_longer(cols = Auto:`Student Loan`)

DELINQUENT_FRBNY_CONSUMER_CREDIT_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = filter(DELINQUENT_FRBNY_CONSUMER_CREDIT,name == "Auto"), aes(x = date, y = value/100, color = "% of Auto Loan Balance 90+ Days Delinquent"), size = 1.25) +
  geom_line(data = filter(NEWLY_DELINQUENT_FRBNY_CONSUMER_CREDIT,name == "Auto"), aes(x = date, y = value/100, color = "% of Auto Loan Balance Newly Transitioning into 90+ Days Delinquent"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.055), breaks = c(0,.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("% of Total Balance") +
  ggtitle("Auto Delinquencies are Rising Again") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Data",subtitle = "The Share of Auto Loans in Delinquency has Risen Above its COVID-era Lows") +
  theme_apricitas + theme(legend.position = c(0.50,0.15)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*0.055), ymax = 0) +
  coord_cartesian(clip = "off")

SCE_APPLY_DENY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americas%20Car%20Shortage/SCE_APPLY_REJECTED.csv") %>%
  mutate(date = as.Date(date))

SCE_APPLY_DENY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = SCE_APPLY_DENY, aes(x = date, y = Applied/100, color = "Likelihood of Applying for Auto Loans in the Next 12M"), size = 1.25) +
  geom_line(data = SCE_APPLY_DENY, aes(x = date, y = Rejected/100, color = "Among Auto Loan Applicants: Expected Likelihood of Rejection"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.35), breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("% Likelihood") +
  ggtitle("Americans Feel Auto Credit Drying Up") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Data",subtitle = "Americans Expect to Apply for Fewer Auto Loans and See a Higher Likelihood of Being Rejected") +
  theme_apricitas + theme(legend.position = c(0.50,0.95)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("Likelihood of Applying for Auto Loans in the Next 12M","Among Auto Loan Applicants: Expected Likelihood of Rejection")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*0.35), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SCE_APPLY_DENY_GRAPH, "SCE Apply Deny Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


ggsave(dpi = "retina",plot = DELINQUENT_FRBNY_CONSUMER_CREDIT_GRAPH, "Auto Delinquencies FRBNY Consumer Credit Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = UNFILLED_ORDERS_Graph, "Unfilled Orders Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = PPI_Car_Margins_Graph, "PPI Car Margins Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Vehicle_Inventory_Graph, "Vehicle Inventory.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Capacity_Utilization_Graph, "Automobile Capacity Utilization.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Auto_Sales_Graph, "Auto Sales Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Auto_Inventory_Sales_Ratio_Graph, "Auto Inventories Sales Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = VMT_Graph, "VMT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Auto_Industrial_Capacity_Graph, "Auto Industrial Capacity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Assemblies_Graph, "Auto Assemblies.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Assemblies_Auto_Truck_Graph, "Auto Truck Assemblies.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Auto_Utilized_Capacity_Graph, "Auto Production Capacity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Plant_Underutilization_Graph, "Plant Underutilization.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = CPI_Mannheim_Used_Car_Vehicles_Graph, "CPI Mannheim.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Cumulative_Shortfall_Graph, "Cumulative Shortfall.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Employment_Graph, "Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = ALL_EMPLOYEES_MOTOR_VEHICLE_Graph, "All Employees Motor Vehicles Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

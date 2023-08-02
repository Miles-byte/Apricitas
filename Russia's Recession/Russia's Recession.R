pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Russia GDP data
RU_GDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/RU_GDP_2021.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(RU_GDP = gsub(",","",RU_GDP)) %>%
  mutate(RU_GDP = as.numeric(RU_GDP))
  
RU_GDP_graph <- ggplot() + #plotting russian GDP data
  geom_line(data=RU_GDP, aes(x=Date,y= RU_GDP/1000,color= "Real GDP, Russia"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of 2021 Rubles") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "₽", suffix = "T"), breaks = c(28,30,32,34), limits = c(28,35), expand = c(0,0)) +
  ggtitle("The Russian Recession") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data", subtitle = "Russian GDP is Recovering From the Fallout of its Invasion of Ukraine and the Ensuing Sanctions") +
  theme_apricitas + theme(legend.position = c(.475,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-04-01")-(.1861*(today()-as.Date("2011-04-01"))), xmax = as.Date("2011-04-01")-(0.049*(today()-as.Date("2011-04-01"))), ymin = 28-(.3*7), ymax = 28) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_GDP_graph, "RU GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#UKR GDP Drop

UKR_GDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/UKR_GDP.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(yoy_gdp_growth = as.numeric(yoy_gdp_growth))

UKR_GDP_graph <- ggplot() + #plotting ukr GDP data
  geom_line(data=UKR_GDP, aes(x=date,y= yoy_gdp_growth/100,color= "Ukraine Real GDP Growth, Year-on-Year"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year-on-Year Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.40,-.30,-.20,-.10,0,.10), limits = c(-.40,.10), expand = c(0,0)) +
  ggtitle("Ukraine's Damaged Economy") +
  labs(caption = "Graph created by @JosephPolitano using State Statistics Service of Ukraine data", subtitle = "Ukrainian Real GDP, Including Territory and Population Loss, Has Dropped Nearly 40%") +
  theme_apricitas + theme(legend.position = c(.3,.64)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-04-01")-(.1861*(today()-as.Date("2016-04-01"))), xmax = as.Date("2016-04-01")-(0.049*(today()-as.Date("2016-04-01"))), ymin = -.40-(.3*.5), ymax = -.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UKR_GDP_graph, "UKR GDP.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Russia Car Prices

RUSSIA_CAR_PRICES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/RU_CAR_PRICE.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(DOMESTIC_CAR = (DOMESTIC_CAR/DOMESTIC_CAR[1])*100) %>%
  mutate(FOREIGN_CAR = (FOREIGN_CAR/FOREIGN_CAR[1])*100)

RUSSIA_CAR_PRICES_graph <- ggplot() + #plotting Russia car Prices
  geom_line(data=RUSSIA_CAR_PRICES, aes(x=date,y= DOMESTIC_CAR,color= "New Domestic Passenger Cars"), size = 1.25)+ 
  geom_line(data=RUSSIA_CAR_PRICES, aes(x=date,y= FOREIGN_CAR,color= "New Foreign Passenger Cars"), size = 1.25)+ 
  xlab("Date") +
  ylab("Price, Rubles, Indexed to Jan 10 2022") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(100,110,120,130,140,150), limits = c(100,150), expand = c(0,0)) +
  ggtitle("Lada Money for a Lada") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data", subtitle = "Russian Car Prices Have Skyrocketed in the Wake of Allied Sanctions") +
  theme_apricitas + theme(legend.position = c(.5,.25)) +
  scale_color_manual(name= "Russian Consumer Prices",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-10")-(.1861*(today()-as.Date("2022-01-10"))), xmax = as.Date("2022-01-10")-(0.049*(today()-as.Date("2022-01-10"))), ymin = 100-(.3*50), ymax = 100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_CAR_PRICES_graph, "Russia Car Prices.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Russia BCI output

RUSSIA_BCI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/RU_BCI.csv") %>%
  mutate(date = as.Date(date))

RUSSIA_BCI_OUTPUT_graph <- ggplot() + #plotting Russia car Prices
  geom_line(data=RUSSIA_BCI, aes(x=date,y= all_actual/100,color = "All Enterprises"), size = 1.25)+ 
  geom_line(data=RUSSIA_BCI, aes(x=date,y= FAE_lending_nsa_manu/100,color = "Manufacturing Enterprises With Foreign Economic Activity"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Net Percent Improving") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.60,-.50,-.40,-.30,-.20,-.10,0,.10), limits = c(-.60,.15), expand = c(0,0)) +
  ggtitle("Cut Off") +
  labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia Data", subtitle = "Manufacturing Industries with Foreign Economic Activity Were Hit Particularly Hard by Sanctions") +
  theme_apricitas + theme(legend.position = c(.38,.15)) +
  scale_color_manual(name= "Current Ratings, Business Conditions Index",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = -.60-(.3*.75), ymax = -.60) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_BCI_OUTPUT_graph, "Russia BCI Output.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Russia BCI credit

RUSSIA_BCI_CREDIT_graph <- ggplot() + #plotting Russian Credit Conditions
  geom_line(data=RUSSIA_BCI, aes(x=date,y= all_lending_nsa/100,color = "All Enterprises"), size = 1.25)+ 
  geom_line(data=RUSSIA_BCI, aes(x=date,y= FAE_lending_nsa_manu/100,color = "Manufacturing Enterprises With Foreign Economic Activity"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Net Percent Improving") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.60,-.50,-.40,-.30,-.20,-.10,0,.10), limits = c(-.60,.15), expand = c(0,0)) +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia Data", subtitle = "Russian Credit Conditions Collapsed When Sanctions Began, and Haven't Improved") +
  theme_apricitas + theme(legend.position = c(.38,.20)) +
  scale_color_manual(name= "BCI: Credit Conditions",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = -.60-(.3*.75), ymax = -.60) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_BCI_CREDIT_graph, "Russia BCI Credit.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Russia Annual Industrial Production

RUSSIA_IDPRO_ANNUAL_COMPLEX <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/RU_IDPRO_ANNUAL.csv") %>%
  select(Name.OKVED2,Production.of.household.appliances,Production.of.vehicles,Production.of.components.and.accessories.for.vehicles,Production.of.vehicles.and.equipment.not.included.in.other.groups)
  
RUSSIA_IDPRO_ANNUAL_COMPLEX_graph <- ggplot() + #plotting industrial production growth
  geom_line(data=RUSSIA_IDPRO_ANNUAL_COMPLEX, aes(x=Name.OKVED2,y= Production.of.household.appliances,color = "Household Appliances"), size = 1.25)+ 
  geom_line(data=RUSSIA_IDPRO_ANNUAL_COMPLEX, aes(x=Name.OKVED2,y= Production.of.components.and.accessories.for.vehicles,color = "Components and Accessories for Vehicles"), size = 1.25)+ 
  geom_line(data=RUSSIA_IDPRO_ANNUAL_COMPLEX, aes(x=Name.OKVED2,y= Production.of.vehicles,color = "Motor Vehicles"), size = 1.25)+ 
  geom_line(data=RUSSIA_IDPRO_ANNUAL_COMPLEX, aes(x=Name.OKVED2,y= Production.of.vehicles.and.equipment.not.included.in.other.groups,color = "Vehicles and Equipment Excluding Passenger Cars, Aircraft, and Trains"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Index, 2015 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,50,100,150), limits = c(0,175), expand = c(0,0)) +
  ggtitle("Some Assembly Required") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data", subtitle = "Russian Production of Select Complex Goods Have Collapsed Amidst Sanctions") +
  theme_apricitas + theme(legend.position = c(.50,.20)) +
  scale_color_manual(name= "Russian Industrial Production, Jan-Oct",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 2015-(.1861*(2022-2015)), xmax = 2015-(0.049*(2022-2015)), ymin = 0-(.3*175), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_IDPRO_ANNUAL_COMPLEX_graph, "Russia IDPRO ANNUAL COMPLEX.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Russia IDPRO Headline Monthly
RUSSIA_IDPRO_MON_HEADLINE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/RU_IDPRO_MON.csv") %>%
  select(Date,General.gas.production.and.gas.condensate,Oil.and.oil..associated..gas.production,Production.production) %>%
  mutate(Date = as.Date(Date))

RUSSIA_IDPRO_MON_HEADLINE_graph <- ggplot() + #plotting russian Monthly Industrial Production Data
  geom_line(data=RUSSIA_IDPRO_MON_HEADLINE, aes(x=Date,y= General.gas.production.and.gas.condensate/100,color= "Natural Gas and Condensate"), size = 1.25)+ 
  geom_line(data=RUSSIA_IDPRO_MON_HEADLINE, aes(x=Date,y= Oil.and.oil..associated..gas.production/100,color= "Oil"), size = 1.25)+ 
  geom_line(data=RUSSIA_IDPRO_MON_HEADLINE, aes(x=Date,y= Production.production/100,color= "Manufacturing"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year-on-Year Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.15,-.10,-0.05,0,0.05,0.1,0.15,0.2,0.25), limits = c(-.175,.20), expand = c(0,0)) +
  ggtitle("Russia's Recession") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data", subtitle = "Russian Oil, Gas, and Manufacturing Output are All Declining") +
  theme_apricitas + theme(legend.position = c(.3,.17)) +
  scale_color_manual(name= "Industrial Production, Change From Year Ago",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -.175-(.3*.375), ymax = -.175) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_IDPRO_MON_HEADLINE_graph, "Russia IDPRO MON HEADLINE.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Oil and Gas Revenues
RUSSIA_OIL_GAS_REV <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/5488aefd7759ac03292b1469febfe9861a60bb45/Russia's%20Recession/Oil_Gas_Revenues.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Oil_Gas_Rev = gsub(",","",Oil_Gas_Rev)) %>%
  mutate(Oil_Gas_Rev = as.numeric(Oil_Gas_Rev))

RUSSIA_OIL_GAS_REV_graph <- ggplot() + #plotting russian gas data
  geom_line(data=RUSSIA_OIL_GAS_REV, aes(x=Date,y= Oil_Gas_Rev/1000000,color= "Volume of Shipped Russian Crude Oil and Natural Gas Production"), size = 1.25)+ 
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Rubles") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, prefix = "₽", suffix = "T"), breaks = c(0,0.5,1,1.5,2,2.5), limits = c(0,2.75), expand = c(0,0)) +
  ggtitle("The Russian Recession") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data", subtitle = "Ruble-Denominated Crude and Gas Revenues Have Fallen Near Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.42,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-02-01")-(.1861*(today()-as.Date("2017-02-01"))), xmax = as.Date("2017-02-01")-(0.049*(today()-as.Date("2017-02-01"))), ymin = 0-(.3*2.75), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_OIL_GAS_REV_graph, "Russia Oil Gas Rev Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

RU_IND_PRO <- read.xlsx("https://rosstat.gov.ru/storage/mediabank/ind_baza_2018.xlsx", sheet = 2)

RU_IND_PRO_CAR <- RU_IND_PRO %>%
  subset(X2 == "29.1") %>%
  select(-К.содержанию,-X2) %>%
  transpose() %>%
  transmute(Monthly_growth = as.numeric(V1)) %>%
  mutate(Vehicle_IND_PRO = cumprod(1 + (Monthly_growth-100)/100)*(10000/Monthly_growth[1])) %>%
  select(Vehicle_IND_PRO) %>%
  ts(., start = c(2015,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2015-01-01"), by = "month", length = nrow(.)), value = x)

RU_IND_PRO_CAR_GRAPH <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= RU_IND_PRO_CAR, aes(x=date,y=value,color= "Russian Industrial Production of Motor Vehicles, Monthly"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,250), breaks = c(0,50,100,150,200,250,300), expand = c(0,0)) +
  ylab("Index, Jan 2015 = 100") +
  ggtitle("Russian Motor Vehicle Production") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data seasonally adjusted usint X-13ARIMA",subtitle = "Russian Motor Vehicle Production Remains Extremely Depressed in the Wake of Sanctions") +
  theme_apricitas + theme(legend.position = c(.415,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = min(RU_IND_PRO_CAR$date)-(.1861*(max(RU_IND_PRO_CAR$date)-min(RU_IND_PRO_CAR$date))), xmax = min(RU_IND_PRO_CAR$date)-(0.049*(max(RU_IND_PRO_CAR$date)-min(RU_IND_PRO_CAR$date))), ymin = 0-(.3*250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_IND_PRO_CAR_GRAPH, "RU Ind Pro Car Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RU_IND_PRO_ADJUSTED <- RU_IND_PRO %>%
  select(-К.содержанию) %>%
  transpose() %>%
  select(-V1,-V2,-V3,-V4,-V136,-V137) %>%
  setNames(.[1,]) %>%
  .[-1,] %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), 
                ~cumprod(1 + (. - 100)/100) * (10000 / .[1])
  )
  ) %>%
  ts(., start = c(2015,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2015-01-01"), by = "month", length = nrow(.)))

start_date1 <- as.Date("2021-09-01")
end_date1 <- as.Date("2022-02-01")
start_date2 <- as.Date("2022-04-01")
end_date2 <- as.Date("2022-09-01")

# calculate the average for each numeric column in the two date ranges
avg_2021 <- RU_IND_PRO_AFFECTED_GRAPH %>% 
  filter(date >= start_date1 & date <= end_date1) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

avg_2023 <- RU_IND_PRO_AFFECTED_GRAPH %>% 
  filter(date >= start_date2 & date <= end_date2) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

percentage_change_long <- percentage_change %>%
  gather(key = "column", value = "percentage_change")

# Calculate the ranks
percentage_change_long <- percentage_change_long %>%
  mutate(rank = rank(-percentage_change, na.last = "keep", ties.method = "min"))

# Sort by rank
percentage_change_long <- percentage_change_long %>%
  arrange(rank)


RU_IND_PRO_DOWNWARD_GRAPH <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`291`/`291`[37]*100,color= "Motor Vehicles"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`309`/`309`[37]*100,color= "Transportation Equipment n.e.c (Bikes, Motorcycles, etc)"), size = 1.25) +
  #geom_line(data= subset(RU_IND_PRO_ADJUSTED, date > as.Date("2019-01-01")), aes(x=date,y=`283`/`283`[37]*100,color= "Agricultural and Forestry Machinery"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`282`/`282`[37]*100,color= "Other General Purpose Machinery"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`275`/`275`[37]*100,color= "Household Appliances"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,135), breaks = c(0,50,100,150,200,250,300), expand = c(0,0)) +
  ylab("Index, Jan 2022 = 100") +
  ggtitle("Russia's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data seasonally adjusted usint X-13ARIMA",subtitle = "Russian Production of High-Complexity Consumer Goods Has Tanked Since the Invasion") +
  theme_apricitas + theme(legend.position = c(.65,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*135), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

RU_IND_PRO_UPWARD_GRAPH <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`25`/`25`[37]*100,color= "Fabricated Metal Products"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`26`/`26`[37]*100,color= "Computer, Electronic, and Optical Products"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`303`/`303`[37]*100,color= "Other Transportation Equipment: Aircraft, Spacecraft, and Related Machinery"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`30`/`30`[37]*100,color= "Other Transportation Equipment"), size = 1.25) +
  geom_line(data= subset(RU_IND_PRO_ADJUSTED, date >= as.Date("2019-01-01")), aes(x=date,y=`27`/`27`[37]*100,color= "Electrical Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,135), breaks = c(0,50,100,150,200,250,300), expand = c(0,0)) +
  ylab("Index, Jan 2022 = 100") +
  ggtitle("Russia's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data seasonally adjusted usint X-13ARIMA",subtitle = "Russian Production of Items Used for or Related to the War are Ramping Up") +
  theme_apricitas + theme(legend.position = c(.515,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*135), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_IND_PRO_UPWARD_GRAPH, "RU Ind Pro Upward Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RU_IND_PRO_DOWNWARD_GRAPH, "RU Ind Pro Downward Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Vacation_CPI <- data.frame(
  Date = as.Date(c("1/1/2022", "2/1/2022", "3/1/2022", "4/1/2022", "5/1/2022", "6/1/2022",
                   "7/1/2022", "8/1/2022", "9/1/2022", "10/1/2022", "11/1/2022", "12/1/2022",
                   "1/1/2023", "2/1/2023", "3/1/2023", "4/1/2023", "5/1/2023", "6/1/2023"),
                 format = "%m/%d/%Y"),
  Value = c(28071.63, 32466.65, 55979.85, 58758.55, 54633.09, 64203.26,
            64076.69, 67311.17, 62246.92, 56349.34, 58566.75, 53790.11,
            52062.93, 54750.76, 61846.10, 68967.99, 76164.69, 81065.95)
)

RU_VACATION_TURKEY_GRAPH <- ggplot() + #plotting Chinese Motor Vehicle Production
  geom_line(data= Vacation_CPI, aes(x=Date,y=Value/Value[1]*100,color= "Russian CPI: Vacation Trip to Turkey"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,300), breaks = c(0,50,100,150,200,250,300), expand = c(0,0)) +
  ylab("Index, Jan 2022 = 100") +
  ggtitle("Russia's Exodus") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data",subtitle = "The Costs of Russian Trips to Turkey Have Almost Triped Since the Start of the Invasion") +
  theme_apricitas + theme(legend.position = c(.515,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-02-01")-(.1861*(today()-as.Date("2022-02-01"))), xmax = as.Date("2022-02-01")-(0.049*(today()-as.Date("2022-02-01"))), ymin = 90-(.3*210), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_VACATION_TURKEY_GRAPH, "RU VACATION TURKEY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

date <- Sys.Date()
LEADING_INDIC_FILE_TEST_BASE_URL <- "https://rosstat.gov.ru/storage/mediabank/IPU_kp_"
LEADING_INDIC_FILE_TEST_FILE_URL <- paste0(LEADING_INDIC_FILE_TEST_BASE_URL, format(date, "%Y-%m"), ".xlsx") 

while (http_status(HEAD(LEADING_INDIC_FILE_TEST_FILE_URL))$category != "Success") {
  date <- date - months(1)
  LEADING_INDIC_FILE_TEST_FILE_URL <- paste0(LEADING_INDIC_FILE_TEST_BASE_URL, format(date, "%Y-%m"), ".xlsx")
}

RU_LEAD_INDIC_UNCERTAINTY <- read.xlsx(LEADING_INDIC_FILE_TEST_FILE_URL,17) %>%
  slice(-(c(1,2,3,4))) %>%
  slice(-((n()-2):n())) %>%
  setnames(c("Industry", "year", "1","2","3","4","5","6","7","8","9","10","11","12")) %>%
  fill(Industry, .direction = "down") %>%
  mutate_at(vars(2:14), as.numeric) %>%
  mutate(Industry = recode(Industry, 
                           "Добыча полезных ископаемых" = "Mining",
                           "Обрабатывающие производства" = "Manufacturing",
                           "Обеспечение электрической энергией, газом и паром; кондиционирование воздуха" = "Supply of electricity, gas, and steam; air conditioning")) %>%
  pivot_longer(
    cols = `1`:`12`,
    names_to = "month",
    values_to = "value"
  ) %>%
  unite("date", year, month, sep = "-", remove = FALSE) %>%
  mutate(date=ym(date)) %>%
  pivot_wider(
    names_from = Industry,
    values_from = value
  )  %>%
  drop_na()

RU_LEAD_INDIC_MATERIAL_SHORTAGE <- read.xlsx(LEADING_INDIC_FILE_TEST_FILE_URL,22) %>%
  slice(-(c(1,2,3,4))) %>%
  slice(-((n()-2):n())) %>%
  setnames(c("Industry", "year", "1","2","3","4","5","6","7","8","9","10","11","12")) %>%
  fill(Industry, .direction = "down") %>%
  mutate_at(vars(2:14), as.numeric) %>%
  mutate(Industry = recode(Industry, 
                           "Добыча полезных ископаемых" = "Mining",
                           "Обрабатывающие производства" = "Manufacturing",
                           "Обеспечение электрической энергией, газом и паром; кондиционирование воздуха" = "Supply of electricity, gas, and steam; air conditioning")) %>%
  pivot_longer(
    cols = `1`:`12`,
    names_to = "month",
    values_to = "value"
  ) %>%
  unite("date", year, month, sep = "-", remove = FALSE) %>%
  mutate(date=ym(date)) %>%
  pivot_wider(
    names_from = Industry,
    values_from = value
  )  %>%
  drop_na()

RU_LEAD_INDIC_LABOR_SHORTAGE <- read.xlsx(LEADING_INDIC_FILE_TEST_FILE_URL,23) %>%
  slice(-(c(1,2,3,4))) %>%
  slice(-((n()-2):n())) %>%
  setnames(c("Industry", "year", "1","2","3","4","5","6","7","8","9","10","11","12")) %>%
  fill(Industry, .direction = "down") %>%
  mutate_at(vars(2:14), as.numeric) %>%
  mutate(Industry = recode(Industry, 
                           "Добыча полезных ископаемых" = "Mining",
                           "Обрабатывающие производства" = "Manufacturing",
                           "Обеспечение электрической энергией, газом и паром; кондиционирование воздуха" = "Supply of electricity, gas, and steam; air conditioning")) %>%
  pivot_longer(
    cols = `1`:`12`,
    names_to = "month",
    values_to = "value"
  ) %>%
  unite("date", year, month, sep = "-", remove = FALSE) %>%
  mutate(date=ym(date)) %>%
  pivot_wider(
    names_from = Industry,
    values_from = value
  ) %>%
  drop_na()

RU_IMPEDIMENTS_PRODUCTION <- ggplot() + #plotting BIE
  #geom_line(data=RU_LEAD_INDIC_UNCERTAINTY, aes(x=date,y= Manufacturing/100,color= "Economic Uncertainty"), size = 1.25) +
  geom_line(data=subset(RU_LEAD_INDIC_MATERIAL_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date,y= Manufacturing/100,color= "Shortage of Raw Materials and Supplies"), size = 1.25) +
  geom_line(data=subset(RU_LEAD_INDIC_LABOR_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date,y= Manufacturing/100,color= "Shortage of Skilled Workers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.3), breaks = c(0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  ylab("Pct (Could Select Multiple Options)") +
  ggtitle("Russia's Manufacturing Shortages") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data",subtitle = "Material Shortages Eased After the Initial Sanctions Shock—But Labor Shortages are Growing") +
  theme_apricitas + theme(legend.position = c(.60,.9)) +
  scale_color_manual(name= "Factors Limiting Production in Russian Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-01-01")))), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_IMPEDIMENTS_PRODUCTION, "RU Impediments Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
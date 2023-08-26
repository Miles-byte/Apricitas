pacman::p_load(openxlsx,read_xl,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  ggtitle("Russia's Wartime Production") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data seasonally adjusted usint X-13ARIMA",subtitle = "Russian Production of High-Complexity Consumer Goods Has Tanked Since the Invasion") +
  theme_apricitas + theme(legend.position = c(.675,.175)) +
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
  ggtitle("Russia's Wartime Production") +
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
                   "1/1/2023", "2/1/2023", "3/1/2023", "4/1/2023", "5/1/2023", "6/1/2023","7/1/2023"),
                 format = "%m/%d/%Y"),
  Value = c(28071.63, 32466.65, 55979.85, 58758.55, 54633.09, 64203.26,
            64076.69, 67311.17, 62246.92, 56349.34, 58566.75, 53790.11,
            52062.93, 54750.76, 61846.10, 68967.99, 76164.69, 81065.95, 83422.64)
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

while (http_status(HEAD(paste0(LEADING_INDIC_FILE_TEST_BASE_URL, format(date, "%Y-%m"), ".xlsx")))$category != "Success") {
  date <- date - months(1)
}

LEADING_INDIC_FILE_TEST_FILE_URL <- paste0(LEADING_INDIC_FILE_TEST_BASE_URL, format(date, "%Y-%m"), ".xlsx")

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
  #geom_line(data=subset(RU_LEAD_INDIC_LABOR_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date,y= Manufacturing/100,color= "Shortage of Skilled Workers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.2), breaks = c(0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  ylab("Pct (Could Select Multiple Options)") +
  ggtitle("Russia's Manufacturing Shortages") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data",subtitle = "Material Shortages Have Eased Significantly After the Initial Sanctions Shock") +
  theme_apricitas + theme(legend.position = c(.40,.8)) +
  scale_color_manual(name= "Factors Limiting Production in Russian Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-01-01")))), ymin = 0-(.3*.2), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_IMPEDIMENTS_PRODUCTION, "RU Impediments Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

GVA_INDUSTRY <- read.xlsx("https://rosstat.gov.ru/storage/mediabank/VDS_kvartal_OKVED2_s2011.xlsx",2) %>%
  slice(4,22) %>%
  select(-1,-2) %>%
  transpose() %>%
  drop_na() %>%
  mutate_if(is.character, as.numeric) %>%
  ts(., start = c(2011,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq.Date(from = as.Date("2011-01-01"), by = "3 months",length.out = nrow(.)), GDP = V1, GVA_DEFENSE = V2, PERCENT_DEFENSE_V1 = V2/V1)
  
GVA_INDUSTRY_graph <- ggplot() + #plotting Russia car Prices
  geom_line(data=GVA_INDUSTRY, aes(x=date,y= PERCENT_DEFENSE_V1,color = "Public Administration, Military security, and Social Security, % of GDP, Russia"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10), limits = c(0,.10), expand = c(0,0)) +
  ggtitle("The Growing Cost of Russia's War") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data", subtitle = "Military and Other Government Activities Now Make Up a Much Larger Share of Russian Spending") +
  theme_apricitas + theme(legend.position = c(.48,.15)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-01-01")-(.1861*(today()-as.Date("2011-01-01"))), xmax = as.Date("2011-01-01")-(0.049*(today()-as.Date("2011-01-01"))), ymin = 0-(.3*.10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GVA_INDUSTRY_graph, "GVA Military Spending Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

REAL_GVA_INDUSTRY <- read.xlsx("https://rosstat.gov.ru/storage/mediabank/VDS_kvartal_OKVED2_s2011.xlsx",6) %>%
  slice(4,9,10) %>%
  select(-1,-2) %>%
  transpose() %>%
  drop_na() %>%
  mutate_if(is.character, as.numeric) %>%
  transmute(date = seq.Date(from = as.Date("2011-01-01"), by = "3 months",length.out = nrow(.)), GDP = V1, GVA_MINING = V2, GVA_MANUFACTURING = V3) %>%
  filter(date >= as.Date("2015-01-01"))

REAL_GVA_INDUSTRY_GRAPH <- ggplot() + #plotting GVA of Mining and Manufacturing
  geom_line(data= REAL_GVA_INDUSTRY, aes(x=date,y=GVA_MINING/GVA_MINING[1]*100,color= "Mining (Including Oil and Gas Extraction)"), size = 1.25) +
  geom_line(data= REAL_GVA_INDUSTRY, aes(x=date,y=GVA_MANUFACTURING/GVA_MANUFACTURING[1]*100,color= "Manufacturing (Including Oil Refining)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,125), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, Q1 2015 = 100") +
  ggtitle("Russia's Industrial Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat Data",subtitle = "Russia's Manufacturing and Mining Industries Have not Fully Recovered from Sanctions") +
  theme_apricitas + theme(legend.position = c(.4,.15)) +
  scale_color_manual(name= "Russia, Real Gross Value Added by Sector",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-02-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 90-(.3*35), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_GVA_INDUSTRY_GRAPH, "RU REAL GVA INDUSTRY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


date <- Sys.Date()
VOLUME_SHIPPED_PRODUCTION_URL <- "https://rosstat.gov.ru/storage/mediabank/otgruz_mes_"

while (http_status(HEAD(paste0(VOLUME_SHIPPED_PRODUCTION_URL, format(date, "%m-%Y"), ".xlsx")))$category != "Success") {
  date <- date - months(1)
}

VOLUME_SHIPPED_PRODUCTION_URL <- paste0(VOLUME_SHIPPED_PRODUCTION_URL, format(date, "%m-%Y"), ".xlsx")

VOLUME_SHIPPED_PRODUCTION <- read.xlsx(VOLUME_SHIPPED_PRODUCTION_URL, 2) %>%
  filter(X2 %in% c("06.1","06.2","19.2")) %>%
  transpose() %>%
  slice(-1,-2) %>%
  setNames(c("Oil","Gas","Petroleum_Products")) %>%
  mutate(date = seq.Date(from = as.Date("2017-01-01"), by = "month", length.out = nrow(.))) %>%
  mutate_if(is.character, as.numeric)
  
#Oil and Gas Revenues

RUSSIA_OIL_GAS_REV_graph <- ggplot() + #plotting russian gas data
  geom_line(data=VOLUME_SHIPPED_PRODUCTION, aes(x=date,y= Gas/1000000,color= "Natural Gas and Condensate"), size = 1.25)+ 
  geom_line(data=VOLUME_SHIPPED_PRODUCTION, aes(x=date,y= Petroleum_Products/1000000,color= "Refined Petroleum Products"), size = 1.25)+ 
  geom_line(data=VOLUME_SHIPPED_PRODUCTION, aes(x=date,y= Oil/1000000,color= "Oil & Associated Petroleum Gas"), size = 1.25)+ 
  xlab("Date") +
  ylab("Trillions of Rubles") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, prefix = "₽", suffix = "T"), breaks = c(0,0.5,1,1.5,2,2.5), limits = c(0,2.25), expand = c(0,0)) +
  ggtitle("Russia's Slowdown in Energy Revenue") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data", subtitle = "Ruble-Denominated Crude and Refined Petroleum Revenues Have Declined From 2022 Peaks") +
  theme_apricitas + theme(legend.position = c(.42,.70)) +
  scale_color_manual(name= "Russian Value of Shipped Production",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Oil & Associated Petroleum Gas","Refined Petroleum Products","Natural Gas and Condensate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-02-01")-(.1861*(today()-as.Date("2017-02-01"))), xmax = as.Date("2017-02-01")-(0.049*(today()-as.Date("2017-02-01"))), ymin = 0-(.3*2.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_OIL_GAS_REV_graph, "Russia Oil Gas Rev Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_GDP <- read.xlsx(paste0("https://rosstat.gov.ru/storage/mediabank/VVP_kvartal_s%201995_1%D0%BB-",year(today()-45),".xlsx"),11) %>%
  slice(4) %>%
  transpose %>%
  transmute(date = seq.Date(from = as.Date("2011-01-01"), by = "3 months",length.out = nrow(.)), GDP = as.numeric(V1)) %>%
  drop_na()

date <- Sys.Date()
IBVO_BASE_URL <- "https://rosstat.gov.ru/storage/mediabank/IVBO_OKVED2_"

while (http_status(HEAD(paste0(IBVO_BASE_URL, format(date, "%m-%Y"), ".xlsx")))$category != "Success") {
  date <- date - months(1)
}

IBVO_BASE_URL <- paste0(IBVO_BASE_URL, format(date, "%m-%Y"), ".xlsx")

REAL_IBVO <- read.xlsx(IBVO_BASE_URL,3) %>%
  slice(-1,-2,-6,-10,-14,-18,-19,-20,-21) %>%
  setNames(c("month", as.character(seq(from = 2013, by = 1, length.out = ncol(.) - 1)))) %>%
  mutate(month = seq(from = 1, by = 1, length.out = nrow(.))) %>%
  pivot_longer(cols = -month, names_to = "year", values_to = "value") %>%
  unite("date", month, year, sep = "-") %>%
  transmute(date = as.Date(as.yearmon(date, "%m-%Y")), value = as.numeric(gsub(",",".",gsub(")","",value)))) %>%
  mutate(value = if_else(row_number() == 1, 100, value)) %>%
  arrange(date) %>%
  mutate(value = cumprod(1 + (value - 100)/100) * (10000 / value[1])) %>%
  select(-date) %>%
  drop_na() %>%
  ts(., start = c(2013,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq(from = as.Date("2013-01-01"), by = "month", length = nrow(.)), value = x)

RU_GDP_graph <- ggplot() + #plotting russian GDP data
  geom_line(data=subset(REAL_IBVO, date >= as.Date("2017-01-01")), aes(x=date,y= value/value[1]*100,color= "Monthly Index of Goods & Services Output, Russia"), size = 1.25)+ 
  geom_line(data=subset(REAL_GDP, date >= as.Date("2017-01-01")), aes(x=date+25,y= GDP/GDP[1]*100,color= "Official Real GDP, Russia"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Index, Jan/Q1 2018 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120), limits = c(90,120), expand = c(0,0)) +
  ggtitle("The Russian Recovery") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data Seasonally Adjusted Using X-13ARIMA", subtitle = "Volatile Higher-Frequency Data Suggest Russia's Economy Has Continued Recovering") +
  theme_apricitas + theme(legend.position = c(.475,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Official Real GDP, Russia", "Monthly Index of Goods & Services Output, Russia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 90-(.3*30), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_GDP_graph, "RU GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RUSSIA_TRADE_DATA <- tempfile(fileext = ".xls") %>% {download.file("https://www.cbr.ru/vfs/eng/statistics/credit_statistics/trade/trade_e.xls", ., mode = "wb"); .} %>% read_xls() %>%
  setNames(c("date","export","export_pct_growth","export_non_CIS","export_non_CIS_pct_growth","export_CIS","export_CIS_pct_growth","import","import_pct_growth","import_non_CIS","import_non_CIS_growth","import_CIS","import_CIS_pct_growth","balance","balance_non_CIS","balance_CIS")) %>%
  filter(date %in% c("January","February","March","April","May","June","July","August","September","October","November","December")) %>%
  transmute(date = seq.Date(from = as.Date("1997-01-01"), by = "month", length = nrow(.)), import = as.numeric(import), export = as.numeric(export), balance = as.numeric(balance)) %>%
  filter(date >= as.Date("2018-01-01"))

RUSSIA_TRADE_DATA_graph <- ggplot() + #plotting russian GDP data
  geom_line(data=RUSSIA_TRADE_DATA, aes(x=date,y= balance/1000,color= "Goods Balance"), size = 1.25)+ 
  geom_line(data=RUSSIA_TRADE_DATA, aes(x=date,y= import/1000,color= "Goods Imports"), size = 2.25)+ 
  geom_line(data=RUSSIA_TRADE_DATA, aes(x=date,y= export/1000,color= "Goods Exports"), size = 2.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,25,50,75), limits = c(0,60), expand = c(0,0)) +
  ggtitle("Russia's Shrinking Surplus") +
  labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia data", subtitle = "Russian Imports are Rising as Exports Decline, Leading to a Shrinking Trade Surplus") +
  theme_apricitas + theme(legend.position = c(.475,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Goods Exports","Goods Imports","Goods Balance"), guide = guide_legend(override.aes = list(lwd = c(2.25,2.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*60), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_TRADE_DATA_graph, "Russia Trade Data graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CHINA_EXPORTS_RUSSIA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/China_Exports_to_Russia.csv") %>%
  mutate(date = as.Date(date))
  
RUSSIA_TRADE_DATA_CHINA_graph <- ggplot() + #plotting russian GDP data
  geom_line(data=RUSSIA_TRADE_DATA, aes(x=date,y= import/1000,color= "Total Russian Goods Imports"), size = 1.25)+ 
  geom_line(data=CHINA_EXPORTS_RUSSIA, aes(x=date,y= value/1000000000,color= "Chinese Exports to Russia"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Monthly") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30), limits = c(0,32.5), expand = c(0,0)) +
  ggtitle("Russia's Growing Trade With China") +
  labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia and GACC data", subtitle = "China is Supplying Russia with New Sources of Imports in the Wake of Allied Sanctions") +
  theme_apricitas + theme(legend.position = c(.475,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Total Russian Goods Imports","Chinese Exports to Russia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*32.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUSSIA_TRADE_DATA_CHINA_graph, "Russia Trade Data China graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#THIS DATA COMES FROM THE CBR's OVERVIEW OF FINANCIAL MARKET RISKS
#https://www.cbr.ru/analytics/finstab/orfr/

EXPORTS_CURRENCY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/CURRENCY_EXPORT_SHARE.csv") %>%
  mutate(date = as.Date(date)) %>%
  setNames(c("date","Dollar", "Euro", "Ruble", "Yuan","Other")) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = rev(c("Ruble","Yuan","Dollar","Euro","Other"))))
  
IMPORTS_CURRENCY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/CURRENCY_IMPORT_SHARE.csv") %>%
  mutate(date = as.Date(date)) %>%
  setNames(c("date","Dollar", "Euro", "Ruble", "Yuan","Other")) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = rev(c("Ruble","Yuan","Dollar","Euro","Other"))))

IMPORTS_CURRENCY_graph <- ggplot(data = IMPORTS_CURRENCY, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Billions of Dollars") +
  ggtitle("Russian Imports by Currency") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1,prefix = "$", suffix = "B"), breaks = c(0,10,20,30,40,50), limits = c(0,50), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia data", subtitle = "Russia Has Struggled to Buy Imports Using Rubles—Hence Increased Reliance on the Yuan") +
  theme_apricitas + theme(legend.position = "right") +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#FFE98F","#9A348E","#3083DC","#6A4C93")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

EXPORTS_CURRENCY_graph <- ggplot(data = EXPORTS_CURRENCY, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Billions of Dollars") +
  ggtitle("Russian Exports by Currency") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1,prefix = "$", suffix = "B"), breaks = c(0,10,20,30,40,50,60), limits = c(0,50), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia data", subtitle = "Russia is Using the Ruble and Yuan to Settle Exports Much More Than Pre-Sanctions") +
  theme_apricitas + theme(legend.position = "right") +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#FFE98F","#9A348E","#3083DC","#6A4C93")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IMPORTS_CURRENCY_graph, "Russia Exports by Currency graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = EXPORTS_CURRENCY_graph, "Russia Imports by Currency graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RU_FX_LOANS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Russia's%20Recession/RU_FX_LOANS_CURRENCY.csv") %>%
  transmute(date = as.Date(Date), Dollar = USD, Francs = as.numeric(CHF), Euro = EUR, Yuan = as.numeric(CNY), Other = as.numeric(Other)) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = rev(c("Francs","Yuan","Dollar","Euro","Other"))))

FX_BORROWING_CURRENCY_graph <- ggplot(data = RU_FX_LOANS, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
    annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
    geom_bar(stat = "identity", position = "stack", color = NA) +
    ylab("Billions of Rubles") +
    ggtitle("Russian Corporate FX-Denominated\nBorrowing by Currency") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1,prefix = "₽", suffix = "B"), breaks = c(0,100,200,300,400), limits = c(0,450), expand = c(0,0)) +
    labs(caption = "Graph created by @JosephPolitano using Central Bank of Russia data", subtitle = "The Yuan has Supplanted the Dollar for Most of Russia's Corporate Foreign Currency Borrowing") +
    theme_apricitas + theme(legend.position = "right") +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
    scale_fill_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#FFE98F","#3083DC","#6A4C93")) +
    theme(legend.text =  element_text(size = 13, color = "white")) +
    annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-06-01")-(.1861*(today()-as.Date("2022-06-01"))), xmax = as.Date("2022-06-01")-(0.049*(today()-as.Date("2022-06-01"))), ymin = 0-(.3*450), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
    coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = FX_BORROWING_CURRENCY_graph, "FX Borrowing Currency graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RU_SERVICES_LABOR_SHORTAGE <- read.xlsx("https://rosstat.gov.ru/storage/mediabank/Obsled_del_activ.xlsx",7) %>%
  setnames(c("year", "1","2","3","4")) %>%
  mutate(Impediment = year) %>%
  mutate(Impediment = if_else(Impediment %in% c("Недостаточный спрос на данный вид услуг", 
                                                "Высокий уровень налогообложения2)",
                                                "Недостаток квалифицированного персонала"), 
                              Impediment, NA_character_)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(Impediment = recode(Impediment, 
                             "Недостаточный спрос на данный вид услуг" = "demandshortfall",
                             "Высокий уровень налогообложения2)" = "Taxes",
                             "Недостаток квалифицированного персонала" = "Workers")) %>%
  fill(Impediment, .direction = "down") %>%
  slice(-1,-2,-3,-4,-17,-30,-43,-44) %>%
  mutate_at(vars(1:5), as.numeric) %>%
  pivot_longer(
    cols = `1`:`4`,
    names_to = "quarter",
    values_to = "value"
  ) %>%
  unite("date", year, quarter, sep = "-", remove = FALSE) %>%
  mutate(date=as.Date(as.yearqtr(date, "%Y-%q"))) %>%
  pivot_wider(
    names_from = Impediment,
    values_from = value
  ) %>%
  drop_na()

RU_CONSTRUCTION_LABOR_SHORTAGE <- read.xlsx("https://rosstat.gov.ru/storage/mediabank/indikatori_DAS_2kv.xlsx", 8) %>%
  slice(-(c(1,2,3))) %>%
  slice(-((n()-2):n())) %>%
  setnames(c("Impediment", "year", "1","2","3","4")) %>%
  fill(Impediment, .direction = "down") %>%
  mutate(year = gsub("2)","",year)) %>%
  mutate_at(vars(2:6), as.numeric) %>%
  mutate(Impediment = recode(Impediment, 
                           "7.1. Недостаток заказов" = "Orders",
                           "7.2. Высокий уровень налогов" = "Taxes",
                           "7.3. Недостаток квалифицированных работников" = "Workers")) %>%
  pivot_longer(
    cols = `1`:`4`,
    names_to = "quarter",
    values_to = "value"
  ) %>%
  unite("date", year, quarter, sep = "-", remove = FALSE) %>%
  mutate(date=as.Date(as.yearqtr(date, "%Y-%q"))) %>%
  pivot_wider(
    names_from = Impediment,
    values_from = value
  ) %>%
  drop_na()


RU_IMPEDIMENTS_PRODUCTION_SECTORS <- ggplot() + #plotting BIE
  geom_line(data=subset(RU_CONSTRUCTION_LABOR_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date+45,y= Workers/100,color= "Construction"), size = 1.25) +
  geom_line(data=subset(RU_LEAD_INDIC_LABOR_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date,y= Mining/100,color= "Mining"), size = 1.25) +
  geom_line(data=subset(RU_LEAD_INDIC_LABOR_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date,y= Manufacturing/100,color= "Manufacturing"), size = 1.25) +
  geom_line(data=subset(RU_SERVICES_LABOR_SHORTAGE, date >= as.Date("2013-01-01")), aes(x=date+45,y= Workers/100,color= "Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.3), breaks = c(0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  ylab("Pct (Could Select Multiple Options)") +
  ggtitle("Russia's Growing Labor Shortages") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data",subtitle = "Conscription and Net Emigration Have Exacerbated Labor Shortages Across Russian Industries") +
  theme_apricitas + theme(legend.position = c(.40,.13), legend.spacing.y = unit(0, "cm"), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "% of Firms Citing Labor Shortages as an Impediment to Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Services","Manufacturing","Construction","Mining")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-01-01")))), ymin = 0-(.3*.3), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RU_IMPEDIMENTS_PRODUCTION_SECTORS, "RU Impediments Production Sectors.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

date <- Sys.Date()
MONTHLY_BUDGET_BASE_URL <- "https://archive.minfin.gov.ru/common/upload/library/"

while (http_status(HEAD(paste0(MONTHLY_BUDGET_BASE_URL, format(date, "%Y/%m"), "/main/3_fedbud_month_eng_%E2%80%94_month.xlsx")))$category != "Success") {
  date <- date - months(1)
}

MONTHLY_BUDGET_BASE_URL <- paste0(MONTHLY_BUDGET_BASE_URL, format(date, "%Y/%m"), "/main/3_fedbud_month_eng_%E2%80%94_month.xlsx")

BUDGET_DATA <- read.xlsx(MONTHLY_BUDGET_BASE_URL) %>%
  slice(-1,-53,-54,-55) %>%
  transpose() %>%
  setNames(make.names(.[2, ], unique = TRUE)) %>% 
  slice(-1,-2) %>%
  mutate(date = seq.Date(from = as.Date("2011-01-01"), by = "month", length.out = nrow(.))) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year) %>%
  mutate(across(where(is.numeric), ~ ifelse(!is.na(lag(.)), . - lag(.), .))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(.,k = 12)))) %>%
  filter(date > as.Date("2018-01-01"))
  
MONTHLY_BUDGET_EXPENDITURES_graph <- ggplot() + #plotting russian GDP data
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=BUDGET_DATA, aes(x=date,y= Total.Expenditure/1000*12,color= "Russia Federal Government Total Expenditures, 12MMT"), size = 1.25)+ 
  xlab("Date") +
  ylab("Trillions of Rubles") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "₽", suffix = "T"), breaks = c(0,10,20,30,40), limits = c(0,40), expand = c(0,0)) +
  ggtitle("The Rising Costs of Russia's War") +
  labs(caption = "Graph created by @JosephPolitano using Russian Ministry of Finance data", subtitle = "Russian Government Expenditures Have Risen Significantly Amidst the Invasion of Ukraine") +
  theme_apricitas + theme(legend.position = c(.475,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*40), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

MONTHLY_BUDGET_DEFICIT_graph <- ggplot() + #plotting russian GDP data
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=BUDGET_DATA, aes(x=date,y= Deficit.......Surplus..../1000*12,color= "Russian Federal Surplus/Deficit, 12MMT"), size = 1.25)+ 
  geom_line(data=BUDGET_DATA, aes(x=date,y= X.Non.oil.gas.deficit/1000*12,color= "Russian Federal Surplus/Deficit Ex Oil and Gas, 12MMT"), size = 1.25)+ 
  xlab("Date") +
  ylab("Trillions of Rubles") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "₽", suffix = "T"), breaks = c(-20,-15,-10,-5,0,5,10), limits = c(-20,5), expand = c(0,0)) +
  ggtitle("The Rising Costs of Russia's War") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat data", subtitle = "Russia's Federal Deficit is Rising as the Country Borrows to Pay for Defense Expenditures") +
  theme_apricitas + theme(legend.position = c(.475,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Russian Federal Surplus/Deficit, 12MMT","Russian Federal Surplus/Deficit Ex Oil and Gas, 12MMT")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -20-(.3*25), ymax = -20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MONTHLY_BUDGET_EXPENDITURES_graph, "Monthly Budget Expenditures.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = MONTHLY_BUDGET_DEFICIT_graph, "Monthly Budget Deficit.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


date <- Sys.Date()
QUARTERLY_BUDGET_BASE_URL <- "https://archive.minfin.gov.ru/common/upload/library/"

while (http_status(HEAD(paste0(QUARTERLY_BUDGET_BASE_URL, format(date, "%Y/%m"), "/main/3_fedbud_month_eng_%E2%80%94_quater.xlsx")))$category != "Success") {
  date <- date - months(1)
}

QUARTERLY_BUDGET_BASE_URL <- paste0(QUARTERLY_BUDGET_BASE_URL, format(date, "%Y/%m"), "/main/3_fedbud_month_eng_%E2%80%94_quater.xlsx")

QUARTERLY_NOMINAL_GDP <- read.xlsx(paste0("https://rosstat.gov.ru/storage/mediabank/VVP_kvartal_s%201995_1%D0%BB-",year(today()-45),".xlsx"),3) %>%
  slice(4) %>%
  transpose %>%
  transmute(date = seq.Date(from = as.Date("2011-01-01"), by = "3 months",length.out = nrow(.)), GDP = as.numeric(V1)) %>%
  drop_na()

QUARTERLY_BUDGET_DATA <- read.xlsx(QUARTERLY_BUDGET_BASE_URL) %>%
  slice(-1,-53,-54,-55) %>%
  transpose() %>%
  setNames(make.names(.[2, ], unique = TRUE)) %>% 
  slice(-1,-2) %>%
  mutate(date = seq.Date(from = as.Date("2011-01-01"), by = "3 months", length.out = nrow(.))) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  mutate(across(where(is.numeric), ~ ifelse(!is.na(lag(.)), . - lag(.), .))) %>%
  ungroup() %>%
  select(Total.Expenditure,Deficit.......Surplus....,X.Non.oil.gas.deficit,date) %>%
  merge(.,QUARTERLY_NOMINAL_GDP, by = "date") %>%
  mutate(across(where(is.numeric), ~ c(NA,NA,NA,rollmean(.,4)))) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  transmute(Deficit = Deficit.......Surplus..../GDP,Expenditures = Total.Expenditure/GDP,Deficit_Ex_Oil_Gas = X.Non.oil.gas.deficit/GDP, date)

#DO ROLLMEAN
QUARTERLY_BUDGET_DEFICIT_graph <- ggplot() + #plotting russian GDP data
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=QUARTERLY_BUDGET_DATA, aes(x=date,y= Deficit,color= "Russian Federal Surplus/Deficit, % of GDP, Rolling 1-Year Total"), size = 1.25)+ 
  geom_line(data=QUARTERLY_BUDGET_DATA, aes(x=date,y= Deficit_Ex_Oil_Gas,color= "Russian Federal Surplus/Deficit Ex Oil and Gas, % of GDP, Rolling 1-Year Total"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.10,-0.05,0,0.05), limits = c(-.125,.05), expand = c(0,0)) +
  ggtitle("The Rising Costs of Russia's War") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat and Ministry of Finance data", subtitle = "Russia's Federal Deficit is Rising as the Country Borrows to Pay for Defense Expenditures") +
  theme_apricitas + theme(legend.position = c(.475,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Russian Federal Surplus/Deficit, % of GDP, Rolling 1-Year Total","Russian Federal Surplus/Deficit Ex Oil and Gas, % of GDP, Rolling 1-Year Total")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.125-(.3*.175), ymax = -.125) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QUARTERLY_BUDGET_DEFICIT_graph, "Quarterly Budget Deficit.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

QUARTERLY_BUDGET_EXPENDITURES_graph <- ggplot() + #plotting russian GDP data
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=QUARTERLY_BUDGET_DATA, aes(x=date,y= Expenditures,color= "Russian Federal Government Expenditures, % of GDP, Rolling 1-Year Total"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.1,.2), limits = c(0,.25), expand = c(0,0)) +
  ggtitle("The Rising Costs of Russia's War") +
  labs(caption = "Graph created by @JosephPolitano using Rosstat and Ministry of Finance data", subtitle = "Russia's Federal Spending is Rising as the Country's Defense Expenditures Rise") +
  theme_apricitas + theme(legend.position = c(.475,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Russian Federal Government Expenditures, % of GDP, Rolling 1-Year Total","Russian Federal Surplus/Deficit Ex Oil and Gas, % of GDP, Rolling 1-Year Total")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QUARTERLY_BUDGET_EXPENDITURES_graph, "Quarterly Budget Expenditures.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
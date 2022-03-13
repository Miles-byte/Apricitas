pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

DurablesQuantity <- fredr(series_id = "DDURRA3M086SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #durable quantity
DurablesNominal <- fredr(series_id = "PCEDG",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #durable nominal

RetailGrocery <- fredr(series_id = "MRTSSM4451USS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #retail sales grocery stores
RetailRestaurant <- fredr(series_id = "MRTSSM722USS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #retail sales food service

GasolineNominal <- fredr(series_id = "DGOERC1Q027SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Gasoline quantity
GasolineReal <- fredr(series_id = "DGOERA3Q086SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Gasoline quantity

PCE <- fredr(series_id = "PCE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #personal consumption expenditured data
PCEGD <- fredr(series_id = "DGDSRC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pce goods data
PCESV <- fredr(series_id = "PCES",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #pce services data

AdvanceRetail <- fredr(series_id = "RSXFS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Gasoline quantity
ElectronicShopping <- fredr(series_id = "MRTSSM4541USS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

Furniture <- fredr(series_id = "MRTSSM442USS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

BuildingMaterials <- fredr(series_id = "MRTSSM4441USS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

MotorVehicleParts <- fredr(series_id = "MRTSSM441USS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

RealFoodOff <- fredr(series_id = "DFXARX1Q020SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
NominalFoodOff <- fredr(series_id = "DFXARC1Q027SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)

PCE_Graph <- ggplot() + #plotting Personal Consumption Expenditures as well as PCE Goods/Services
  geom_line(data=PCE, aes(x=date,y= (value/141.04) ,color= "Total Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCEGD, aes(x=date,y= (value/43.46) ,color= "Goods Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PCESV, aes(x=date,y= (value/97.73) ,color= "Services Personal Consumption Expenditures"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(83,135), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Good to Go?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Spending on Goods Shot Up after the Pandemic Hit, but is Now Stalling") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 83-(.3*47), ymax = 83) +
  coord_cartesian(clip = "off")

DurablesQuantity_Graph <- ggplot() + 
  geom_line(data = DurablesNominal, aes(x = date, y = value/15.508, color = "Nominal Personal Consumption Expenditures: Durable Goods"), size = 1.25) +
  geom_line(data = DurablesQuantity, aes(x = date, y = value/1.58667, color = "Real Personal Consumption Expenditures: Durable Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(75,150), breaks = c(80,100,120,140), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Buying Spree") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Americans Are Buying Record Numbers of Durable Goods") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 75-(.3*75), ymax = 75) +
  coord_cartesian(clip = "off")

GroceryRestaurant_Graph <- ggplot() + 
  geom_line(data = RetailGrocery, aes(x = date, y = value/1000, color = "Retail Sales: Grocery Stores"), size = 1.25) +
  geom_line(data = RetailRestaurant, aes(x = date, y = value/1000, color = "Retail Sales: Food Service and Drinking Places"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(25,80), breaks = c(25,50,75), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("The Big Takeaway") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Americans Are Back to Spending More on Restaurants than Grocery Stores") +
  theme_apricitas + theme(legend.position = c(.40,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 25-(.3*55), ymax = 25) +
  coord_cartesian(clip = "off")

AdvanceRetail_Graph <- ggplot() + 
  geom_line(data = AdvanceRetail, aes(x = date, y = value/1000, color = "Advanced Retail Sales: Retail Trade"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(350,600), breaks = c(400,500,600), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Buying Spree") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Americans Are Spending Tons of Money on Retail Goods") +
  theme_apricitas + theme(legend.position = c(.40,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 350-(.3*250), ymax = 350) +
  coord_cartesian(clip = "off")

ElectronicShopping_Graph <- ggplot() + 
  geom_line(data = ElectronicShopping, aes(x = date, y = value/1000, color = "Retail Sales: Electronic Shopping and Mail-Order Houses"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(45,90), breaks = c(50,70,90), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Buying Spree") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Unsurprisingly, Online Spending is Up Dramatically During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 45-(.3*45), ymax = 45) +
  coord_cartesian(clip = "off")

Furniture_Graph <- ggplot() + 
  geom_line(data = Furniture, aes(x = date, y = value/1000, color = "Retail Sales: Furniture and Home Furnishings Stores"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(4,13), breaks = c(4,8,12), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Buying Spree") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Furniture Sales Collapsed and then Rocketed Upward After Lockdowns Began") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 4-(.3*9), ymax = 4) +
  coord_cartesian(clip = "off")

Building_Materials_Graph <- ggplot() + 
  geom_line(data = BuildingMaterials, aes(x = date, y = value/1000, color = "Retail Sales: Building Materials and Supplies Dealers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(25,40), breaks = c(25,30,35,40), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Residential Refurbishment") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Sales at Building Materials Stores Skyrocketed as Homebound Americans Started Construction") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 25-(.3*15), ymax = 25) +
  coord_cartesian(clip = "off")

Motor_Vehicles_Graph <- ggplot() + 
  geom_line(data = MotorVehicleParts, aes(x = date, y = value/1000, color = "Retail Sales: Motor Vehicle and Parts Dealers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(65,145), breaks = c(80,100,120,140), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Unbalanced Balancing") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Fewer Vehicle Sales at Higher Prices Have Kept Dealer's Sales Mostly On-Trend") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 65-(.3*80), ymax = 65) +
  coord_cartesian(clip = "off")

FoodOff_Graph <- ggplot() + 
  geom_line(data = NominalFoodOff, aes(x = date, y = value/10.41498, color = "Nominal PCE: Food and Beverages for Off-Premises Consumption"), size = 1.25) +
  geom_line(data = RealFoodOff, aes(x = date, y = value/9.94714, color = "Real PCE: Food and Beverages for Off-Premises Consumption"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,125), breaks = c(90,100,110,120), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("The Big Takeaway") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Americans Are Buying Lots of Food, But Price Growth is Starting to Hit") +
  theme_apricitas + theme(legend.position = c(.42,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 90-(.3*30), ymax = 90) +
  coord_cartesian(clip = "off")

GasolineQuantity_Graph <- ggplot() + 
  geom_line(data = GasolineNominal, aes(x = date, y = value/3.43650, color = "Nominal PCE: Gasoline and Other Energy Goods"), size = 1.25) +
  geom_line(data = GasolineReal, aes(x = date, y = value/1.04985, color = "Real PCE: Gasoline and Other Energy Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,125), breaks = c(60,80,100,120), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("Buying Spree") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Americans Are Spending More on Gas, but Consuming Less of it") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 50-(.3*75), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DurablesQuantity_Graph, "Durables.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GroceryRestaurant_Graph, "Grocery Restaurant.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GasolineQuantity_Graph, "Gasoline.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PCE_Graph, "PCE Goods.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AdvanceRetail_Graph, "Retail Trade.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ElectronicShopping_Graph, "Electronic Shopping.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Building_Materials_Graph, "Building Materials.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Motor_Vehicles_Graph, "Motor Vehicles.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = FoodOff_Graph, "Food Off.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
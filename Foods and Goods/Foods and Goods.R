pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("pacman")
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FOOD <- fredr(series_id = "CPIUFDSL",observation_start = as.Date("2018-01-01"), units = "pc1")
COREGOODS <- fredr(series_id = "CUSR0000SACL1E",observation_start = as.Date("2018-01-01"), units = "pc1")

Food_Goods_Graph <- ggplot() + #plotting Food at Home, Away From Home, Durable Goods, and Nondurable Goods
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=FOOD, aes(x=date,y= value/100,color= "Food"), size = 1.25) +
  geom_line(data=COREGOODS, aes(x=date,y= value/100 ,color= "Goods Excluding Food and Energy"), size = 1.25) +
  #geom_line(data=NONDURABLE, aes(x=date,y= value/100 ,color= "Nondurable Goods Less Food and Beverages"), size = 1.25) +
  #geom_line(data=DURABLE, aes(x=date,y= value/100 ,color= "Durable Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.025,0.15), breaks = c(-0.05,0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Foods and Goods") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Food Inflation Remains High Even as Goods Inflation Slows") +
  theme_apricitas + theme(legend.position = c(.30,.70)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Food","Goods Excluding Food and Energy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.025-(.3*.175), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Food_Goods_Graph, "Foods and Goods.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Main Graph—food at home, food away from home
FOODHOME <- fredr(series_id = "CUSR0000SAF11",observation_start = as.Date("2018-01-01"), units = "pc1")
FOODAWAY <- fredr(series_id = "CUSR0000SEFV",observation_start = as.Date("2018-01-01"), units = "pc1")

Food_Graph <- ggplot() + #plotting Food at Home, Away From Home, Durable Goods, and Nondurable Goods
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=FOODHOME, aes(x=date,y= value/100,color= "Food at Home"), size = 1.25) +
  geom_line(data=FOODAWAY, aes(x=date,y= value/100 ,color= "Food Away From Home"), size = 1.25) +
  #geom_line(data=NONDURABLE, aes(x=date,y= value/100 ,color= "Nondurable Goods Less Food and Beverages"), size = 1.25) +
  #geom_line(data=DURABLE, aes(x=date,y= value/100 ,color= "Durable Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.01,0.15), breaks = c(-0.05,0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Foods, Wherever They Are") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Groceries Lead Restaurants on Inflation For Now") +
  theme_apricitas + theme(legend.position = c(.30,.70)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Food at Home","Food Away From Home")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.01-(.3*.16), ymax = -0.01) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = Food_Graph, "Food.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Meats Chart
Beef_Veal <- bls_api("CUSR0000SEFC", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Pork <- bls_api("CUSR0000SEFD", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Poultry <- bls_api("CUSR0000SEFF", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Seafood <- bls_api("CUSR0000SEFG", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Meat_Graph <- ggplot() + #plotting Meat Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=Beef_Veal, aes(x=date,y= pct1,color= "Beef and Veal"), size = 1.25) +
  geom_line(data=Pork, aes(x=date,y= pct1,color= "Pork"), size = 1.25) +
  geom_line(data=Poultry, aes(x=date,y= pct1,color= "Poultry"), size = 1.25) +
  geom_line(data=Seafood, aes(x=date,y= pct1,color= "Fish and Seafood"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,0.25), breaks = c(-0.05,0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Carnivorous Costs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Meat Prices Remain High, Though Most are Seeing Growth Slowdowns") +
  theme_apricitas + theme(legend.position = c(.30,.70)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Beef and Veal","Pork","Poultry","Fish and Seafood")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.05-(.3*.30), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Meat_Graph, "Meat.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Essentials
Milk <- bls_api("CUSR0000SEFJ01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Eggs <- bls_api("CUSR0000SEFH", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Butter <- bls_api("CUSR0000SS10011", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Sugar <- bls_api("CUSR0000SEFR01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Salt <- bls_api("CUSR0000SS18041", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Flour <- bls_api("CUSR0000SEFA01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))


Essentials_Graph <- ggplot() + #plotting Meat Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=Milk, aes(x=date,y= pct1,color= "Milk"), size = 1.25) +
  geom_line(data=Eggs, aes(x=date,y= pct1,color= "Eggs"), size = 1.25) +
  geom_line(data=Butter, aes(x=date,y= pct1,color= "Butter"), size = 1.25) +
  geom_line(data=Sugar, aes(x=date,y= pct1,color= "Sugar and Sugar Substitutes"), size = 1.25) +
  geom_line(data=Salt, aes(x=date,y= pct1,color= "Salt and Other Seasonings"), size = 1.25) +
  geom_line(data=Flour, aes(x=date,y= pct1,color= "Flour and Prepared Flour Mixes"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.2,0.5), breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Ingredient Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Basic, Essential Ingredients are Rising Rapidly") +
  theme_apricitas + theme(legend.position = c(.325,.75)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Eggs","Butter","Flour and Prepared Flour Mixes","Sugar and Sugar Substitutes","Milk","Salt and Other Seasonings")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.2-(.3*.70), ymax = -0.2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Essentials_Graph, "Essentials.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Drinks Chart
Soda <- bls_api("CUSR0000SEFN01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Juice <- bls_api("CUSR0000SEFN03", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Coffee <- bls_api("CUSR0000SEFP01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Tea <- bls_api("CUSR0000SEFP02", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Beer <- bls_api("CUSR0000SEFW01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Wine <- bls_api("CUSR0000SEFW03", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Drinks_Graph <- ggplot() + #plotting Drink Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=Soda, aes(x=date,y= pct1,color= "Soda and Other Nonalcoholic Carbonated Beverages"), size = 1.25) +
  geom_line(data=Juice, aes(x=date,y= pct1,color= "Juice and Other Noncarbonated Nonalcoholic Beverages"), size = 1.25) +
  geom_line(data=Coffee, aes(x=date,y= pct1,color= "Coffee Beans, Grounds, and Other Coffee Materials"), size = 1.25) +
  geom_line(data=Tea, aes(x=date,y= pct1,color= "Tea Bags, Leaves, and Other Beverage Materials"), size = 1.25) +
  geom_line(data=Beer, aes(x=date,y= pct1,color= "Beer, Ale, and Other Malt Beverages"), size = 1.25) +
  geom_line(data=Wine, aes(x=date,y= pct1,color= "Wines"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,0.225), breaks = c(-0.05,0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Raise a Glass") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Drinks—Especially Coffee—Have Been Rising Rapidly") +
  theme_apricitas + theme(legend.position = c(.375,.7)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Coffee Beans, Grounds, and Other Coffee Materials","Soda and Other Nonalcoholic Carbonated Beverages","Juice and Other Noncarbonated Nonalcoholic Beverages","Tea Bags, Leaves, and Other Beverage Materials","Beer, Ale, and Other Malt Beverages","Wines")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.05-(.3*.275), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Drinks_Graph, "Drinks.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Retail Sales in Restaurants and Grocery Stores
RETAIL_RESTAURANT <- fredr(series_id = "RSFSDP",observation_start = as.Date("2018-01-01"))
RETAIL_GROCERY <- fredr(series_id = "RSDBS",observation_start = as.Date("2018-01-01"))

RETAIL_GROCERY_RESTAURANT_graph <- ggplot() + #Grocery and Restaurant Spending
  geom_line(data=RETAIL_GROCERY, aes(x=date,y= value/1000,color= "Food and Beverage Stores"), size = 1.25) +
  geom_line(data=RETAIL_RESTAURANT, aes(x=date,y= value/1000,color= "Food Service and Drinking Places"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(30,90), breaks = c(30,40,50,60,70,80,90), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("The Big Takeaway") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Americans Spend More on Restaurants Than Grocery Stores Again") +
  theme_apricitas + theme(legend.position = c(.225,.85)) +
  scale_color_manual(name= "Retail Sales",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 30-(.3*60), ymax = 30) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_GROCERY_RESTAURANT_graph, "Retail Stores.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
#Graphing Durable Goods Prices

Appliances <- bls_api("CUSR0000SEHK", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Furniture <- bls_api("CUSR0000SEHJ", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

New_Vehicles <- bls_api("CUSR0000SETA01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Used_Vehicles <- bls_api("CUSR0000SETA02", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Durables_Graph <- ggplot() + #plotting Drink Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=Appliances, aes(x=date,y= pct1,color= "Appliances"), size = 1.25) +
  geom_line(data=Furniture, aes(x=date,y= pct1,color= "Furniture and Bedding"), size = 1.25) +
  geom_line(data=New_Vehicles, aes(x=date,y= pct1,color= "New Vehicles"), size = 1.25) +
  geom_line(data=Used_Vehicles, aes(x=date,y= pct1,color= "Used Cars and Trucks"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,0.5), breaks = c(0,0.1,0.2,0.3,0.4,0.5), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Black Friday?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Price Growth for Durable Goods are Declining") +
  theme_apricitas + theme(legend.position = c(.375,.7)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Used Cars and Trucks","New Vehicles","Furniture and Bedding","Appliances")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.05-(.3*.55), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Durables_Graph, "Durables Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Nondurable Goods

Housekeeping <- bls_api("CUSR0000SEHN", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Apparel <- bls_api("CUSR0000SAA", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Toys <- bls_api("CUSR0000SERE01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Pets <- bls_api("CUSR0000SERB01", startyear = 2017, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  .[order(nrow(.):1),] %>%
  mutate(pct1 = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >as.Date("2017-12-31"))

Nondurables_Graph <- ggplot() + #plotting Nondurable Prices
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=Housekeeping, aes(x=date,y= pct1,color= "Housekeeping Supplies"), size = 1.25) +
  geom_line(data=Apparel, aes(x=date,y= pct1,color= "Apparel"), size = 1.25) +
  geom_line(data=Toys, aes(x=date,y= pct1,color= "Toys"), size = 1.25) +
  geom_line(data=Pets, aes(x=date,y= pct1,color= "Pets and Pet Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.15,0.15), breaks = c(-.15,-.1,-0.05,0,0.05,0.1,0.15), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Black Friday?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Price Growth for Key Nondurable Goods are Still Elevated") +
  theme_apricitas + theme(legend.position = c(.8,.2)) +
  scale_color_manual(name= "CPI Price Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Pets and Pet Products","Housekeeping Supplies","Apparel","Toys")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.15-(.3*.3), ymax = -0.15) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Nondurables_Graph, "Nondurables Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Durable and Nondurable Manufacturing

Nondurable_Goods_IP <- fredr(series_id = "IPNMAN",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = (value/value[1])*100)
Durable_Goods_IP <- fredr(series_id = "IPDMAN",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = (value/value[1])*100)

US_Industrial_Production_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=Nondurable_Goods_IP, aes(x=date,y= value,color= "Nondurable Goods"), size = 1.25) +
  geom_line(data=Durable_Goods_IP, aes(x=date,y= value,color= "Durable Goods"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,110), breaks = c(70,80,90,100,110), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Catching Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "US Manufacturing Industrial Production is Hitting Post-Great-Recession Highs") +
  theme_apricitas + theme(legend.position = c(.25,.35)) +
  scale_color_manual(name= "US Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 70-(.3*40), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Industrial_Production_graph, "US Industrial Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Retail Sales Ratios
Retail_Ex_Motor_Vehicle <- fredr(series_id = "MRTSIR4400AUSS",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = (value/value[1])*100)
Retail_Furniture <- fredr(series_id = "MRTSIR4423XUSS",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = (value/value[1])*100)
Retail_Clothing <- fredr(series_id = "MRTSIR448USS",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = (value/value[1])*100)
Retail_Garden <- fredr(series_id = "MRTSIR444USS",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = (value/value[1])*100)

Retail_Sales_Ratios <- ggplot() + #EU machinery and transport equipment
  geom_line(data=Retail_Ex_Motor_Vehicle, aes(x=date,y= value,color= "Retail Trade Ex Motor Vehicle and Parts Dealers"), size = 1.25) +
  geom_line(data=Retail_Furniture, aes(x=date,y= value,color= "Furniture, Home Furnishings, Electronics, and Appliance Stores"), size = 1.25) +
  geom_line(data=Retail_Clothing, aes(x=date,y= value,color= "Clothing and Clothing Accessory Stores"), size = 1.25) +
  geom_line(data=Retail_Garden, aes(x=date,y= value,color= "Building Materials, Garden Equipment, and Supplies Dealers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(50,115), breaks = c(50,60,70,80,90,100,110), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Stocking the Shelves") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Inventories are Climbing Back to Pre-pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.4,.2)) +
  scale_color_manual(name= "Inventory/Sales Ratios, Indexed",values = c("#FFE98F","#00A99D","#EE6055","#9A348E"), breaks = c("Building Materials, Garden Equipment, and Supplies Dealers","Furniture, Home Furnishings, Electronics, and Appliance Stores","Retail Trade Ex Motor Vehicle and Parts Dealers","Clothing and Clothing Accessory Stores")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 50-(.3*65), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Retail_Sales_Ratios, "Retail Sales Ratio.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
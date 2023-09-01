pacman::p_load(readrba,readabs,bea.R,readxl,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Refugees <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRzvb2ZKLS95aToa_SBYfsZIFhcL_0rvfir5kSUNzl7KNY8UIAVH9AyBZ2I-d5yAZly4l6S15bCVM_d/pub?gid=2043074349&single=true&output=csv") %>%
  .[-1,] %>%
  transmute(date = as.Date(RefugeesDate), value = as.numeric(NoRefugees))
  
Ukraine_Refugees_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=Refugees, aes(x=date,y= value/1000000, color= "Ukrainian Refugees Recorded Across Europe, UNHCR"), size = 1.25) +
  annotate(geom = "text", label = "Note: Refugee Records Lag Early Border Crossings", x = today()-100, y = 1.525, color ="white", size = 4, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(0,10),breaks = c(0,2,4,6,8,10), expand = c(0,0)) +
  ylab("Number, Millions") +
  ggtitle("The Ukrainian Refugee Crisis") +
  labs(caption = "Graph created by @JosephPolitano using UNHCR data",subtitle = "About 1 in 5 Ukrainians Have Fled the Country According to UN Estimates") +
  theme_apricitas + theme(legend.position = c(.65,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-04-25")-(.1861*(today()-as.Date("2022-04-25"))), xmax = as.Date("2022-04-25")-(0.049*(today()-as.Date("2022-04-25"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Ukraine_Refugees_Graph, "Ukrainian Refugees.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UNFAO_INDEX <- read.csv("https://www.fao.org/fileadmin/templates/worldfood/Reports_and_docs/Food_price_indices_data_feb791.csv") %>%
  select(c(1:7)) %>%
  `colnames<-`(.[2,]) %>%
  .[-c(1,2,3),] %>%
  mutate(Date = as.Date(as.yearmon(Date, "%Y-%m")))%>%
  mutate_if(is.character,as.numeric)

UNFAO_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=subset(UNFAO_INDEX, Date >= as.Date("2018-01-01")), aes(x=Date,y= `Oils`/`Oils`[49]*100, color= "Food Oils"), size = 1.25) +
  geom_line(data=subset(UNFAO_INDEX, Date >= as.Date("2018-01-01")), aes(x=Date,y= `Cereals`/`Cereals`[49]*100, color= "Cereals"), size = 1.25) +
  geom_line(data=subset(UNFAO_INDEX, Date >= as.Date("2018-01-01")), aes(x=Date,y= `Food Price Index`/`Food Price Index`[49]*100, color= "All Items"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(0,150),breaks = c(0,50,100,150), expand = c(0,0)) +
  ylab("Index, Jan 2022 = 100") +
  ggtitle("Ukraine and the Global Food Crisis") +
  labs(caption = "Graph created by @JosephPolitano using UNFAO data",subtitle = "Price for Internationally Traded Food Commodities Surged at the Start of the Invasion") +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  scale_color_manual(name= "UNFAO Food Price Index",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*150), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNFAO_Graph, "UNFAO.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BLACK_SEA <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRisnQjodySbp6-XXPGhdsVMp2stg_gyuxw42pP41tuxeic63IARau6bV1TgjLiw_ciAWsTO5LarPqT/pub?gid=0&single=true&output=csv") %>%
  select(Commodity,Tonnage,Departure) %>%
  mutate(Departure = as.Date(Departure,"%d/%m/%Y")) %>%
  group_by(month = lubridate::floor_date(Departure, 'month'), Commodity) %>%
  mutate(Tonnage = sum(as.numeric(gsub(",","",Tonnage)))) %>%
  select(-Departure) %>%
  unique() %>%
  pivot_wider(names_from = Commodity, values_from = Tonnage) %>%
  replace(is.na(.), 0)
  
BLACK_SEA_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=BLACK_SEA, aes(x=month,y= (`Sunflower oil`+`Sunflower meal`+Rapeseed+`Sunflower seed`+`Vegetable oil`+`Canola`+`Rapeseed meal`+`Sunflower pellets`+`Soya oil`+`Soya beans`)/1000000, color= "Sunflower, Canola, Soya, incl. Oils & Related"), size = 1.25) +
  geom_line(data=BLACK_SEA, aes(x=month,y= Corn/1000000, color= "Corn"), size = 1.25) +
  geom_line(data=BLACK_SEA, aes(x=month,y= (Wheat+Barley)/1000000, color= "Wheat and Barley"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "Mt"),limits = c(0,2.9),breaks = c(0,0.5,1,1.5,2,2.5), expand = c(0,0)) +
  ylab("Tons,Mt,Monthly") +
  ggtitle("Ukraine and the Global Food Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Humanitarian Data Exchange data",subtitle = "Ukrainian Exports Via the Black Sea Grain Initiative Have Fallen Significantly") +
  theme_apricitas + theme(legend.position = c(.3,.90), legend.key.height = unit(0, "cm")) +
  scale_color_manual(name= "Ukraine Exports Via Black Sea Grain Initiative",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Corn","Wheat and Barley","Sunflower, Canola, Soya, incl. Oils & Related")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-08-01")-(.1861*(today()-as.Date("2022-07-01"))), xmax = as.Date("2022-08-01")-(0.049*(today()-as.Date("2022-08-01"))), ymin = 0-(.3*2.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BLACK_SEA_Graph, "Black Sea Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IMF_OBSERVE <- data.frame(date = as.Date(c("2021-10-01","2022-01-01","2022-04-01","2022-07-01")), value = c(100,96.27,88.32,79.25))
IMF_FORECAST <- data.frame(date = as.Date(c("2022-07-01","2022-10-01","2023-01-01","2023-04-01","2023-07-01","2023-10-01")), value = c(79.25,67.7,59.5,59.25,60.1,67.01))

IMF_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=IMF_OBSERVE, aes(x=date,y= value, color= "Ukraine Real GDP"), size = 1.25) +
  geom_line(data=IMF_FORECAST, aes(x=date,y= value, color= "IMF Staff Forecast"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(50,110),breaks = c(50,75,100), expand = c(0,0)) +
  ylab("Index, Q4 2021 = 100") +
  ggtitle("Ukraine's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using State Statistics Service of Ukraine and IMF data",subtitle = "the IMF Expects Ukrainian Economic Output to Fall by More than 40% in 2022") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Ukraine Real GDP","IMF Staff Forecast")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-10-01")-(.1861*(as.Date("2023-10-01")-as.Date("2021-10-01"))), as.Date("2021-10-01")-(0.049*(as.Date("2021-10-01")-as.Date("2023-10-01"))), ymin = 50-(.3*60), ymax = 50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IMF_Graph, "IMF Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

test <- read_json("https://bank.gov.ua/NBUStatService/v1/statdirectory/inflation?period=m&date=201501&json")

test1 <- fromJSON("https://bank.gov.ua/NBUStatService/v1/statdirectory/survey?json")

fromJSON("https://bank.gov.ua/NBUStatService/v1/statdirectory/mir?date=yyyymmdd&json")

library("jsonlite")
fromJSON("https://bank.gov.ua/NBUStatService/v1/statdirectory/survey?period=m&date=20190901&json")

NET_AG_EXP <-	data.frame(value = c(3003,3090,	3163,	4318,	3913,	3598,	4232,	4684,	3959,	3274,	3757,	4706,	3311,	3885,	5288,	7536,	4984,	2318,	4441,	5731), date = seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-10-01"), by = "3 months"))

NET_AG_EXPT_Graph <- ggplot() + #plotting US Crude Production
  geom_line(data=NET_AG_EXP, aes(x=date,y= value/1000, color= "Ukraine, Net Agricultural Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,10),breaks = c(0,2.5,5,7.5,10), expand = c(0,0)) +
  ylab("Billions of US Dollars,Quarterly") +
  ggtitle("Ukraine and the Global Food Crisis") +
  labs(caption = "Graph created by @JosephPolitano using National Bank of Ukraine data",subtitle = "The Dollar Value of Ukrainian Agricultural Exports Has Rebounded From the Initial Invasion") +
  theme_apricitas + theme(legend.position = c(.4,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NET_AG_EXPT_Graph, "Net Ag Exp.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BAEI_data <- read.csv("C:/Users/josep/Documents/Ukraine/BAEI.csv") %>%
  mutate(Period = as.Date(Period))

BAEI_Graph <- ggplot() + #plotting BAEI
  geom_line(data=BAEI_data, aes(x=Period,y= Value, color= "Ukraine Business Activity Expectations Index"), size = 1.25) +
  annotate(geom = "hline",y = 50.0,yintercept = 50.0, size = .25,color = "white") +
  annotate("text", label = "50+ = Expansion", x = as.Date("2020-09-01"), y = 51, color = "white", alpha = 0.75, size = 4) +
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = as.Date("2022-02-01"), xmax = as.Date("2022-06-01"), fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Survey Interrupted by War", x = as.Date("2021-08-01"), y = 30, color = "#EE6055", alpha = 0.6, size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(25,60),breaks = c(30,40,50,60), expand = c(0,0)) +
  ylab("Index, 50 + = Expansion") +
  ggtitle("Ukraine's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using National Bank of Ukraine data",subtitle = "Ukrainian Firms Have A Bleak Short-Term Economic Outlook") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-08-01")-(.1861*(today()-as.Date("2019-08-01"))), as.Date("2019-08-01")-(0.049*(today()-as.Date("2019-07-01"))), ymin = 25-(.3*35), ymax = 25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BAEI_Graph, "BAEI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IPMAN_data <- read.csv("C:/Users/josep/Documents/Ukraine/IPMAN.csv") %>%
  mutate(date = as.Date(date))

IPMAN_Graph <- ggplot() + #plotting BAEI
  annotate(geom = "hline",y = 0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=IPMAN_data, aes(x=date,y= (IP-100)/100, color= "Industrial Production"), size = 1.25) +
  geom_line(data=IPMAN_data, aes(x=date,y= (IPMAN-100)/100, color= "Manufacturing Production"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.40,.11),breaks = c(-.40,-.30,-.20,-.10,0,.10), expand = c(0,0)) +
  ylab("Percent Change from Year Ago") +
  ggtitle("Ukraine's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using National Bank of Ukraine data",subtitle = "Ukrainian Industrial Output Has Collapsed Amidst the War") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +
  scale_color_manual(name= "Percent Change From Year Ago",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -.40-(.3*.51), ymax = -.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IPMAN_Graph, "Ukraine IPMAN Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IMPEDE <- read.csv("C:/Users/josep/Documents/Ukraine/IMPEDE.csv") %>%
  mutate(Date = as.Date(Date))

IMPEDE_Graph <- ggplot() + #plotting BAEI
  geom_line(data=IMPEDE, aes(x=Date,y= War/100, color= "War/Political Situation"), size = 1.25) +
  geom_line(data=IMPEDE, aes(x=Date,y= Energy/100, color= "High Energy Prices"), size = 1.25) +
  geom_line(data=IMPEDE, aes(x=Date,y= Materials/100, color= "High Raw Material and Supply Prices"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1),breaks = c(0,.25,.50,.75,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Ukraine's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using National Bank of Ukraine data",subtitle = "Unsurprisingly, the War is Ukraine's Biggest Economic Problem") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= "Factors Impeding Production, Percent of Ukrainian Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-02-01")-(.1861*(today()-as.Date("2019-02-01"))), as.Date("2019-02-01")-(0.049*(today()-as.Date("2019-02-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IMPEDE_Graph, "Ukraine IMPEDE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIGPROBLEM <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Ukraine/BIGPROBLEM.csv") %>%
  mutate(date = as.Date(date)) %>%
  drop_na()
  
BIGPROBLEM_Graph <- ggplot() + #plotting BAEI
  geom_line(data=BIGPROBLEM, aes(x=date,y= SAFE/100, color= "Dangerous to Work"), size = 1.25) +
  geom_line(data=BIGPROBLEM, aes(x=date,y= ELECTRIC/100, color= "Electricity, Water, or Heat Supply Interruptions"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1),breaks = c(0,.25,.50,.75,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Ukraine's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using Institute for Economic Research and Policy Consulting data",subtitle = "Attacks on Infrastrucutre Have Hurt Ukrainian Industry, But Adaptation Has Helped a Recovery") +
  theme_apricitas + theme(legend.position = c(.35,.875)) +
  scale_color_manual(name= "Most Important Problems, Percent of Ukrainian Firms\n(Respondents Could Select Multiple Answers)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Electricity, Water, or Heat Supply Interruptions","Dangerous to Work")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-07-10")-(.1861*(today()-as.Date("2022-07-10"))), as.Date("2022-07-10")-(0.049*(today()-as.Date("2022-07-10"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIGPROBLEM_Graph, "BIGPROBLEM Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IMPEDE_SECTORS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Ukraine/Ukraine_War_Constraint_Sector.csv") %>%
  mutate(date = as.Date(date))

IMPEDE_SECTORS_Graph <- ggplot() + #plotting BAEI
  geom_line(data=IMPEDE_SECTORS, aes(x=date,y= ag/100, color= "Agriculture, Forestry, and Fishing"), size = 1.25) +
  geom_line(data=IMPEDE_SECTORS, aes(x=date,y= mine/100, color= "Mining and Quarrying"), size = 1.25) +
  geom_line(data=IMPEDE_SECTORS, aes(x=date,y= manu/100, color= "Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1),breaks = c(0,.25,.50,.75,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Ukraine's War Economy") +
  labs(caption = "Graph created by @JosephPolitano using National Bank of Ukraine data",subtitle = "Firms Across Sectors Ukrainian Agricultural Firms are Suffering , Even as Mining and Manufacturing Partially Recover") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= "War/Political Situation Impeding Production, Percent of Ukrainian Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-02-01")-(.1861*(today()-as.Date("2019-02-01"))), as.Date("2019-02-01")-(0.049*(today()-as.Date("2019-02-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IMPEDE_SECTORS_Graph, "Impede Sectors Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

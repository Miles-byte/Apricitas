pacman::p_load(estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


TANKAN_GENERAL_PRICES_OUTLOOK <- read.csv("https://www.stat-search.boj.or.jp/ssi/html/nme_R031.17108.20221228061030.02.csv") %>%
  mutate(date = as.Date(as.yearmon(Series.code, "%Y/%m"))) %>%
  drop_na()

TANKAN_GENERAL_PRICES_graph <- ggplot() + #plotting MOVE
  annotate("hline", y = 0.02, yintercept = 0.02, color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "2% Inflation Target", x = as.Date("2019-01-01"), y = 0.0215, color = "white", size = 5) +
  geom_line(data=TANKAN_GENERAL_PRICES_OUTLOOK, aes(x=date,y= CO.TK99F0000204HCQ00000/100,color= "1 Year"), size = 1.25) +
  geom_line(data=TANKAN_GENERAL_PRICES_OUTLOOK, aes(x=date,y= CO.TK99F0000205HCQ00000/100,color= "3 Year"), size = 1.25) +
  geom_line(data=TANKAN_GENERAL_PRICES_OUTLOOK, aes(x=date,y= CO.TK99F0000206HCQ00000/100,color= "5 Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.03), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Anchored Again?") +
  labs(caption = "Graph created by @JosephPolitano using BOJ Data",subtitle = "Japanese Companies Expect Higher Inflation in the Short Term") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name= "Japanese Business General Inflation Expectations, TANKAN Survey",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-03-01")-(.1861*(today()-as.Date("2014-03-01"))), xmax = as.Date("2014-03-01")-(0.049*(today()-as.Date("2014-03-01"))), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TANKAN_GENERAL_PRICES_graph, "TANKAN General Inflation graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#TANKAN Output Prices

TANKAN_OUTPUT_PRICES_OUTLOOK <- read.csv("https://www.stat-search.boj.or.jp/ssi/html/nme_R031.16595.20221228071157.01.csv") %>%
  mutate(date = as.Date(as.yearmon(Series.code, "%Y/%m"))) %>%
  mutate(CO.TK99F0000201HCQ00000 = as.numeric(CO.TK99F0000201HCQ00000)) %>%
  mutate(CO.TK99F0000202HCQ00000 = as.numeric(CO.TK99F0000202HCQ00000)) %>%
  mutate(CO.TK99F0000203HCQ00000 = as.numeric(CO.TK99F0000203HCQ00000)) %>%
  drop_na()

TANKAN_OUTPUT_PRICES_graph <- ggplot() + #plotting
  annotate("hline", y = 0.02, yintercept = 0.02, color = "white", size = 1, linetype = "dashed") +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  annotate("text", label = "2% Inflation Target", x = as.Date("2019-01-01"), y = 0.022, color = "white", size = 5) +
  geom_line(data=TANKAN_OUTPUT_PRICES_OUTLOOK, aes(x=date,y= as.numeric(CO.TK99F0000201HCQ00000)/100,color= "1 Year"), size = 1.25) +
  geom_line(data=TANKAN_OUTPUT_PRICES_OUTLOOK, aes(x=date,y= as.numeric(CO.TK99F0000202HCQ00000)/100,color= "3 Year"), size = 1.25) +
  geom_line(data=TANKAN_OUTPUT_PRICES_OUTLOOK, aes(x=date,y= as.numeric(CO.TK99F0000203HCQ00000)/100,color= "5 Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.005,0.05), breaks = c(-0.01,0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Anchored Again?") +
  labs(caption = "Graph created by @JosephPolitano using BOJ Data",subtitle = "Japanese Companies Own-Output Inflation Expectations Are Above the BOJ's Target") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name= "Japanese Business Own-Output Inflation Expectations, TANKAN Survey",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-03-01")-(.1861*(today()-as.Date("2014-03-01"))), xmax = as.Date("2014-03-01")-(0.049*(today()-as.Date("2014-03-01"))), ymin = -0.005-(.3*.055), ymax = -0.005) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TANKAN_OUTPUT_PRICES_graph, "TANKAN OUTPUT Inflation graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

JAPAN_IP <- read.xlsx("https://www.meti.go.jp/english/statistics/tyo/iip/xls/b2015_gsm1e.xlsx") %>%
  select(-`Seasonally.adjusted.Index.by.Industry.:.Industrial.Production.(2015=100.0)`,-X3) %>%
  transpose() %>%
  select(-V1) %>%
  row_to_names(1) %>%
  clean_names(.) %>%
  mutate(date = as.Date(as.yearmon(item_name,"%Y%m"))) %>%
  mutate_if(is.character,as.numeric)

JAPAN_IP_CARS <- ggplot() + #plotting MOVE
  geom_line(data=subset(JAPAN_IP, date > as.Date("2017-12-01")), aes(x=date,y= motor_vehicles/1.008,color= "Industrial Production of Motor Vehicles, Japan"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,120), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Still Short Semiconductors") +
  labs(caption = "Graph created by @JosephPolitano using METI Data",subtitle = "Japanese Vehicle Production is Still Down Amidst the Chip Shortage") +
  theme_apricitas + theme(legend.position = c(.5,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*120), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = JAPAN_IP_CARS, "Japan IP Cars.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FIN_CON <- read.csv("https://www.stat-search.boj.or.jp/ssi/html/nme_R031.11718.20221228122905.02.csv") %>%
  mutate(date = as.Date(as.yearmon(Series.code, "%Y/%m"))) %>%
  mutate_if(is.character,as.numeric)

FIN_CON_SLOOS <- ggplot() + #plotting SLOOS
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  geom_line(data=subset(FIN_CON, date > as.Date("2000-03-01")), aes(x=date,y= LA05.DLLSLPPB,color= "Large Firms"), size = 1.25) +
  geom_line(data=subset(FIN_CON, date > as.Date("2000-03-01")), aes(x=date,y= LA05.DLLSLPPM,color= "Medium-Sized Firms"), size = 1.25) +
  geom_line(data=subset(FIN_CON, date > as.Date("2000-03-01")), aes(x=date,y= LA05.DLLSLPPH,color= "Households"), size = 1.25) +
  geom_line(data=subset(FIN_CON, date > as.Date("2000-03-01")), aes(x=date,y= LA05.DLLSLPPS,color= "Small Firms"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-5,50), breaks = c(0,20,40,60,80,100,120), expand = c(0,0)) +
  ylab("Diffusion Index, > 0 = Easing") +
  ggtitle("On The Edge") +
  labs(caption = "Graph created by @JosephPolitano using BOJ Data",subtitle = "Japanese Banks Have Stopped Easing Credit Standards") +
  theme_apricitas + theme(legend.position = c(.65,.80)) +
  scale_color_manual(name= "Net Share of Major Japanese Banks Easing Credit Standards to",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-06-01")-(.1861*(today()-as.Date("2000-06-01"))), xmax = as.Date("2000-06-01")-(0.049*(today()-as.Date("2000-06-01"))), ymin = -5-(.3*55), ymax = -5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIN_CON_SLOOS, "Fin Con Sloos.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FIN_CON_TANKAN <- ggplot() + #plotting TANKAN Fin Con
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  geom_line(data=subset(FIN_CON, date > as.Date("2000-03-01")), aes(x=date,y= CO.TK99F0000612GCQ00000,color= "Perceived Lending Attitude of Financial Institutions, TANKAN Survey"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-20,30), breaks = c(-20,-10,0,10,20,30), expand = c(0,0)) +
  ylab("Index, > 0 means Accomodative") +
  ggtitle("On The Edge") +
  labs(caption = "Graph created by @JosephPolitano using BOJ Data",subtitle = "Japanese Firms Perceive Credit Conditions as Worsening") +
  theme_apricitas + theme(legend.position = c(.60,.10)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-06-01")-(.1861*(today()-as.Date("2000-06-01"))), xmax = as.Date("2000-06-01")-(0.049*(today()-as.Date("2000-06-01"))), ymin = -20-(.3*50), ymax = -20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIN_CON_TANKAN, "Fin Con TANKAN.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

usethis::edit_r_environ()

CPI_list <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003427113", #inflation data
  lang = "E", #english language
  cdTab = "3", #change from last year
  cdTimeFrom = "2022001111", #since Jan 1st 2000
  #cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdArea = "00000", #all Japan
)

CPI_flowers <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003427113", #inflation data
  lang = "E", #english language
  cdTab = "3", #change from last year
  #cdTimeFrom = "2022001111", #since Nov 1 2022
  cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdArea = "00000", #all Japan
  cdCat01 = c("9181","9182","9183"),
) %>%
  mutate(TimeMain = as.Date(as.yearmon(Time, "%b. %Y"))) %>% #All dates besides may have been abbreviated, so this creates two columns (1 for may and one for not-may) and then combines them
  mutate(TimeMay = as.Date(as.yearmon(Time, "%b %Y"))) %>%
  mutate(Time = coalesce(TimeMain,TimeMay))

CPI_flowers_graph <- ggplot() + #plotting flowers
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  geom_line(data=CPI_flowers, aes(x=Time,y= value/100,color=`Items(2020-base)`), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.12,0.10), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("WHY WHY WHY WHY") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "JAPAN TRACKS THREE SEPARATE KINDS OF FLOWER PRICES") +
  theme_apricitas + theme(legend.position = c(.85,.15)) +
  scale_color_manual(name= "Year on Year Price Change",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-03-01")-(.1861*(today()-as.Date("2014-03-01"))), xmax = as.Date("2014-03-01")-(0.049*(today()-as.Date("2014-03-01"))), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_flowers_graph, "Cpi Flowers.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_ALL_LFE <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003427113", #inflation data
  lang = "E", #english language
  cdTab = "3", #change from last year
  cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdArea = "00000", #all Japan
  cdCat01 = c("0001","0168") #must be put in quotes otherwise will automaticall round down to 1 
) %>%
  mutate(TimeMain = as.Date(as.yearmon(Time, "%b. %Y"))) %>%
  mutate(TimeMay = as.Date(as.yearmon(Time, "%b %Y"))) %>%
  mutate(Time = coalesce(TimeMain,TimeMay)) %>%
  mutate(`Items(2020-base)` = gsub("All items, less food \\(less alcoholic beverages\\) and energy","All Items Less Food, Alcohol, and Energy",`Items(2020-base)`))

CPI_ALL_LFE_graph <- ggplot() + #plotting MOVE
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  annotate("vline", x = as.Date("2014-04-01"), xintercept = as.Date("2014-04-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("hline", y = 0.02, yintercept = 0.02, color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "2% Inflation Target", x = as.Date("2005-01-01"), y = 0.023, color = "white", size = 5) +
  annotate("text",label = "Consumption Tax Hike", x = as.Date("2017-08-01"), y =0.04, color = "white", size = 5) +
  geom_line(data=CPI_ALL_LFE, aes(x=Time,y= value/100,color=`Items(2020-base)`), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,0.045), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("It's Baaack") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Inflation in Japan is at Multi-Decade Highs") +
  theme_apricitas + theme(legend.position = c(.3,.87)) +
  scale_color_manual(name= "Japan Year on Year CPI Inflation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -.025-(.3*0.07), ymax = -.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_ALL_LFE_graph, "Cpi All and LFE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_ALL_LFE_graph <- ggplot() + #plotting CPILFE
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  annotate("vline", x = as.Date("2014-04-01"), xintercept = as.Date("2014-04-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("hline", y = 0.02, yintercept = 0.02, color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "2% Inflation Target", x = as.Date("2005-01-01"), y = 0.023, color = "white", size = 5) +
  annotate("text",label = "Consumption Tax Hike", x = as.Date("2017-08-01"), y =0.04, color = "white", size = 5) +
  geom_line(data=CPI_ALL_LFE, aes(x=Time,y= value/100,color=`Items(2020-base)`), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,0.045), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("It's Baaack") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Inflation in Japan is at Multi-Decade Highs") +
  theme_apricitas + theme(legend.position = c(.3,.87)) +
  scale_color_manual(name= "Japan Year on Year CPI Inflation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -.025-(.3*0.07), ymax = -.025) +
  coord_cartesian(clip = "off")

FEMALE_REGULAR_EMPLOYMENT <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003074673", #Labor Force Survey Basic Tabulation
  lang = "E", #english language
  #cdTab = "4", #change from last year
  cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdArea = "00000", #all Japan
  cdCat01 = c("00"),
  cdCat02 = c("03","10"),
  cdCat03 = c("2"),#must be put in quotes otherwise will automaticall round down to 1 
) %>%
  mutate(TimeMain = as.Date(as.yearmon(`Time (Monthly)`, "%b. %Y"))) %>%
  mutate(TimeMay = as.Date(as.yearmon(`Time (Monthly)`, "%b %Y"))) %>%
  mutate(`Time (Monthly)` = coalesce(TimeMain,TimeMay))# %>%
  #mutate(`Items(2020-base)` = gsub("All items, less food \\(less alcoholic beverages\\) and energy","All Items Less Food, Alcohol, and Energy",`Items(2020-base)`))

FEMALE_REGULAR_EMPLOYMENT_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=FEMALE_REGULAR_EMPLOYMENT, aes(x=`Time (Monthly)`,y= value/100,color=`Type of employment`), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(9,15.5), expand = c(0,0)) +
  ylab("Number of Workers") +
  ggtitle("Working Women") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Women's Employment Gains Increasingly Come from Regular, not Part-Time/Temporary Work") +
  theme_apricitas + theme(legend.position = c(.3,.40)) +
  scale_color_manual(name= "Japanese Female Employment By Category",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Regular staff","Non-regular staff")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 9-(.3*6.5), ymax = 9) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FEMALE_REGULAR_EMPLOYMENT_graph, "Female Regular vs Non-Regular Employment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_Food_Energy_Durable <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003427113", #inflation data
  lang = "E", #english language
  cdTab = "3", #change from last year
  #cdTimeFrom = "2022001111", #since Jan 1st 2000
  cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdArea = "00000", #all Japan
  cdCat01 = c("0002","0167","0237")
) %>%
  mutate(TimeMain = as.Date(as.yearmon(Time, "%b. %Y"))) %>%
  mutate(TimeMay = as.Date(as.yearmon(Time, "%b %Y"))) %>%
  mutate(Time = coalesce(TimeMain,TimeMay))

CPI_Food_Energy_Durable_graph <- ggplot() + #plotting CPILFE
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  annotate("vline", x = as.Date("2014-04-01"), xintercept = as.Date("2014-04-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "Consumption Tax Hike", x = as.Date("2017-11-01"), y =0.15, color = "white", size = 5) +
  geom_line(data=CPI_Food_Energy_Durable, aes(x=Time,y= value/100,color=`Items(2020-base)`), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.2,0.21), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Food and Energy Inflation") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Food, Energy, and Durable Goods are Driving a Lot of Japanese Inflation") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(name= "Japan Year on Year CPI Inflation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Durable goods","Food","Energy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -.20-(.3*0.41), ymax = -.20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Food_Energy_Durable_graph, "CPI Food Energy Durables.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_Rent_Services <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003427113", #inflation data
  lang = "E", #english language
  cdTab = "3", #change from last year
  #cdTimeFrom = "2022001111", #since Jan 1st 2000
  cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdArea = "00000", #all Japan
  cdCat01 = c("0220","0230")
) %>%
  mutate(TimeMain = as.Date(as.yearmon(Time, "%b. %Y"))) %>%
  mutate(TimeMay = as.Date(as.yearmon(Time, "%b %Y"))) %>%
  mutate(Time = coalesce(TimeMain,TimeMay))

CPI_RENT_SERVICES_graph <- ggplot() + #plotting CPILFE
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  annotate("vline", x = as.Date("2014-04-01"), xintercept = as.Date("2014-04-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "Consumption Tax Hike", x = as.Date("2017-08-01"), y =0.04, color = "white", size = 5) +
  geom_line(data=CPI_Rent_Services, aes(x=Time,y= value/100,color=`Items(2020-base)`), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.0275,0.045), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Still Stuck Stagnant") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Core Cyclical Japanese Inflation Remains Extremely Muted, Especially for Housing") +
  theme_apricitas + theme(legend.position = c(.3,.87)) +
  scale_color_manual(name= "Japan Year on Year CPI Inflation",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Services", "House rent, private")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -.0275-(.3*0.0725), ymax = -.0275) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_RENT_SERVICES_graph, "CPI Rent Services Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Earnings <- read.csv("C:/Users/josep/Documents/Japan/Earnings_Japan_Month_Original.csv") %>%
  mutate(TimeMain = as.Date(as.yearmon(Time, "%b. %Y"))) %>%
  mutate(TimeMay = as.Date(as.yearmon(Time, "%b %Y"))) %>%
  mutate(Time = coalesce(TimeMain,TimeMay))


EARNINGS_graph <- ggplot() + #plotting Wage Growth
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  geom_line(data=Earnings, aes(x=Time,y= X.Changes.from.same.month.of.previous.year......Total.cash.earnings.../100,color= "Average Total Cash Earnings Annual Growth"), size = 1.25) +
  geom_line(data=Earnings, aes(x=Time,y= X.Changes.from.same.month.of.previous.year......Contractual.cash.earnings.../100,color="Average Contractual Cash Earnings (Total Ex Bonuses/Allowances) Annual Growth"), size = 1.25) +
  geom_line(data=Earnings, aes(x=Time,y= X.Changes.from.same.month.of.previous.year......Scheduled.cash.earnings.../100,color="Average Scheduled Cash Earnings (Contractual Ex Overtime Pay) Annual Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.03,0.045), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Japanese Wage Growth Has Picked Up—But Remains Below 3% Soft Target") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Average Total Cash Earnings Annual Growth", "Average Contractual Cash Earnings (Total Ex Bonuses/Allowances) Annual Growth","Average Scheduled Cash Earnings (Contractual Ex Overtime Pay) Annual Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -0.03-(.3*0.08), ymax = -0.03) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EARNINGS_graph, "Earnings Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NGDP_RGDP_INDEX <- read.csv("C:/Users/josep/Documents/Japan/Total Consumption Trend Index_Japan_Month_Original.csv") %>%
  mutate(TimeMain = as.Date(as.yearmon(Time, "%b.%Y"))) %>%
  mutate(TimeMay = as.Date(as.yearmon(Time, "%b %Y"))) %>%
  mutate(Time = coalesce(TimeMain,TimeMay)) %>%
  mutate(Nominal = Total.Consumption.Trend.Index..Nominal..2020.base) %>%
  mutate(Real = Total.Consumption.Trend.Index..Real..2020.base) %>%
  mutate(Nominal = (Nominal-lag(Nominal,12))/lag(Nominal,12)) %>%
  mutate(Real = (Real-lag(Real,12))/lag(Real,12)) %>%
  select(-Annotation,-Annotation.1,-TimeMain,-TimeMay) %>%
  drop_na()
  
NGDP_RGDP_graph <- ggplot() + #plotting Nominal Growth
  annotate("vline", x = as.Date("2014-04-01"), xintercept = as.Date("2014-01-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "Consumption Tax Hike", x = as.Date("2017-03-01"), y =0.05, color = "white", size = 5) +
  annotate("vline", x = as.Date("2011-03-01"), xintercept = as.Date("2011-03-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "2011 Tōhoku Earthquake/Tsunami", x = as.Date("2006-09-01"), y =0.05, color = "white", size = 5) +
  annotate("hline", y = 0.0, yintercept = 0.0, color = "white", size = 0.5) +
  geom_line(data=NGDP_RGDP_INDEX, aes(x=Time,y= Nominal,color= "Nominal Growth"), size = 1.25) +
  geom_line(data=NGDP_RGDP_INDEX, aes(x=Time,y= Real,color="Real Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.15,0.10),breaks = c(-.15,-.1,-0.05,0,0.05,0.10), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Japanese Nominal Growth Has Hit the Highest Levels in Decades") +
  theme_apricitas + theme(legend.position = c(.20,.15)) +
  scale_color_manual(name= "Total Consumption Trend Index",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Nominal Growth","Real Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = -0.15-(.3*0.25), ymax = -0.15) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_RGDP_graph, "NGDP Con Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

JPN_EPOP <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("1990-01-01"), frequency = "m")
 

JPN_EPOP_graph <- ggplot() + #plotting Japanese Growth
  geom_line(data=JPN_EPOP, aes(x=date,y= value/100,color= "Japanese Prime Age (25-54) Employment-Population Ratio"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.75,.875),breaks = c(.75,.80,.85), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "Japanese Employment Levels Continue to Hit New All-Time Highs") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .75-(.3*0.125), ymax = .75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = JPN_EPOP_graph, "JPN EPOP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NON_REGULAR_EMPLOYMENT <- estat_getStatsData(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003082855", #Labor Force Survey Basic Tabulation
  lang = "E", #english language
  cdTimeFrom = "2000000101", #since Jan 1st 2000
  cdCat01 = c("1","2"),
  cdCat02 = c("13"),
  cdCat03 = c("00"),
  cdCat04 = c("07"),
  cdCat05 = c("00"),
) %>%
  select(Sex, value, time_code)%>%
  pivot_wider(names_from = "Sex") %>%
  mutate(time = seq(as.Date("2013-04-01"), by = "3 months", length.out = nrow(.)))
  
NON_REGULAR_EMPLOYMENT_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=NON_REGULAR_EMPLOYMENT, aes(x=time,y= Male/100,color="Male"), size = 1.25) +
  geom_line(data=NON_REGULAR_EMPLOYMENT, aes(x=time,y= Female/100,color="Female"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"),limits = c(0,2), expand = c(0,0)) +
  ylab("Number of Workers") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using E-Stat Japan Data",subtitle = "The Number of Involuntary Part-Time/Irregular Employees in Japan Continues to Shrink") +
  theme_apricitas + theme(legend.position = c(.5,.20)) +
  scale_color_manual(name= "Working a Non-Regular Job, Couldn't Find a Regular Job, Japan",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-04-01")-(.1861*(today()-as.Date("2013-04-01"))), xmax = as.Date("2013-04-01")-(0.049*(today()-as.Date("2013-04-01"))), ymin = 0-(.3*2), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NON_REGULAR_EMPLOYMENT_graph, "JPN Non-Regular.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



estat_getMetaInfo(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  statsDataId = "0003005798",
  lang = "E"
)

test <- estat_getStatsList(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  searchWord = "Monthly Labor Survey",
  lang = "E"
)
esta


CPI_search <- estat_getStatsList(
  appId = "6660597ae9d52f54c4d20dcbb12244c873615203",
  searchWord = "wages and working",
  lang = "E"
)

TANKAN <- get_boj(BOJ_DATASETS$url[BOJ_DATASETS$name == "co"])

?get_boj()

TANKAN_LENDING <- subset(TANKAN, code %in% c("TK99F0000612GCQ01000","TK99F0000612GCQ02000","TK99F0000612GCQ03000"))

#The Inherent Flaws in Inflation Metrics

pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI <- fredr(series_id = c("CPIAUCSL")) #downloading PCEPI data
PCEPI <- fredr(series_id = c("PCEPI")) #downloading PCEPI data
IPD <- fredr(series_id = c("USAGDPDEFQISMEI")) #downloading IPD data
PPI <- fredr(series_id = c("PPIACO")) #downloading PPI data
ECI <- fredr(series_id = c("ECIALLCIV")) #downloading ECI data

PCEPIPCT <- fredr(series_id = c("PCEPI"), units = "pc1") #downloading PCEPI data in percent change format
TRIMPCEPIPCT <- fredr(series_id = c("PCETRIM12M159SFRBDAL")) #downloading Trimmed Mean PCEPI data in percent change format
COREPCEPIPCT <- fredr(series_id = c("JCXFE"), units = "pc1") #downloading Core PCEPI data in percent change format
MEDIANPCE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/Median_PCE.csv") #downloading Median PCE data

MEDIANPCE$Date <- as.Date(MEDIANPCE$Date, "%m/%d/%Y")

Lumber_Futures <- tq_get("LBS=F", from = "2020-01-01")#importing lumber futures market data

CPIURS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/r-cpi-u-rs-allitems.csv")
CPIURSCORE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/r-cpi-u-rs-alllessfe.csv")
BEAPCEDISAGG <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/PCE_Breakdown.csv")
OTC_Securities <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/OTC_Securities.csv")
RMD_Fed_Mfg <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Inherent%20Flaws%20in%20Inflation%20Measurements/rmdfed_mfg_historicaldata.csv")

RMD_Fed_Mfg <- subset(RMD_Fed_Mfg, select = c(date,nsa_mfg_bk_logs_c,nsa_mfg_vend_lead_c,nsa_mfg_pct_chg_prcs_recd_c,nsa_mfg_pct_chg_prcs_pd_c,nsa_mfg_fd_gds_inv_c,nsa_mfg_raw_mats_inv_c)) #taking good columns from the Richmond Fed manufacturing survey
RMD_Fed_Mfg$date <- as.Date(RMD_Fed_Mfg$date, "%m/%d/%Y")
RMD_Fed_Mfg[2:7] <- rollmeanr(RMD_Fed_Mfg[2:7], k = 3, fill = NA)#3 month rolling average to smooth data

OTC_Securities$date <- as.Date(OTC_Securities$date)
BEAPCEDISAGG$Date <- as.Date(BEAPCEDISAGG$Date, "%m/%d/%Y")

CPIURS <- pivot_longer(CPIURS, names_to = "month", values_to = "value", cols = !c(YEAR,AVG,X)) #reorganizing CPIURS data to a real date format in only two columns
CPIURS <- mutate(CPIURS, date = lubridate::dmy(paste("01",month, YEAR, sep = "-")))
CPIURS <- select(CPIURS, -YEAR, -month, - AVG, -X)

CPIURSCORE <- pivot_longer(CPIURSCORE, names_to = "month", values_to = "value", cols = !c(YEAR,AVG)) #reorganizing CPIURS data to a real date format in only two columns
CPIURSCORE <- mutate(CPIURSCORE, date = lubridate::dmy(paste("01",month, YEAR, sep = "-")))
CPIURSCORE <- select(CPIURSCORE, -YEAR, -month, - AVG)


INF_INDEX_GRAPH <- ggplot() + #plotting all inflation indexes
  geom_line(data=PCEPI, aes(x=date,y=value/16.314*100,color= "PCE Price Index"), size = 1.25)+
  geom_line(data=CPI, aes(x=date,y=value/29.37*100,color= "Consumer Price Index"), size = 1.25) +
  geom_line(data=IPD, aes(x=date,y=value/15.81326*100,color= "Implicit Price Deflator"), size = 1.25)+
  geom_line(data=CPIURS, aes(x=date,y=value/(100/2.134)*100,color= "Consumer Price Index - Research Series"), size = 1.25)+
  ylab("Index, January 1960 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,1000), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1950-01-01"),as.Date("2021-09-01"))) +
  ggtitle("The Importance of Indexes") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data", subtitle = "CPI, Even the updated 'Research Series', Tends to Overstate Inflation") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#A7ACD9","#EE6055","#00A99D","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26176), xmax = as.Date("1950-01-01")-(0.049*26176), ymin = 0-(.3*1000), ymax = 0) +
  coord_cartesian(clip = "off")

ALT_INF_INDEX_GRAPH <- ggplot() + #plotting all inflation indexes
  geom_line(data=PCEPI, aes(x=date,y=value/16.314*100,color= "PCE Price Index"), size = 1.25)+
  geom_line(data=ECI, aes(x=date,y=value/(100/5.72)*100,color= "Employment Cost Index"), size = 1.25) +
  geom_line(data=PPI, aes(x=date,y=value/31.6*100,color= "Producer Price Index"), size = 1.25)+
  ylab("Index, January 1960 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,1000), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1950-01-01"),as.Date("2021-09-01"))) +
  ggtitle("The Importance of Indexes") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data", subtitle = "Alternative Price Indexes Diverge Substantially From PCE") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26176), xmax = as.Date("1950-01-01")-(0.049*26176), ymin = 0-(.3*1000), ymax = 0) +
  coord_cartesian(clip = "off")

CORE_INF_INDEX_GRAPH <- ggplot() + #plotting all inflation indexes
  geom_line(data=PCEPIPCT, aes(x=date,y=value/100,color= "PCE Price Index"), size = 1.25)+
  geom_line(data=TRIMPCEPIPCT, aes(x=date,y=value/100,color= "Trimmed Mean PCE Price Index"), size = 1.25) +
  geom_line(data=COREPCEPIPCT, aes(x=date,y=value/100,color= "Core PCE Price Index"), size = 1.25)+
  geom_line(data=MEDIANPCE, aes(x=Date,y=Median.PCE.Inflation..year.over.year.percent.change/100,color= "Median PCE Price Index"), size = 1.25)+
  ylab("Index, Percent Change From Year Ago") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,0.05), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2000-01-01"),as.Date("2021-09-01"))) +
  ggtitle("Straight to the Core") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data", subtitle = "Even 'Core' Inflation Does Not Exlude All Volatility") +
  theme_apricitas + theme(legend.position = c(.75,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7914), xmax = as.Date("2000-01-01")-(0.049*7914), ymin = -0.025-(.3*0.075), ymax = -0.025) +
  coord_cartesian(clip = "off")


Commissions_PCE_graph <- ggplot() + #plotting all commission fees
  geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Securities.commissions,color= "Security Commissions"), size = 1.25)+
  geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Other.direct.commissions,color= "Other Direct Commissions"), size = 1.25)+
  geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Indirect.commissions,color= "Indirect Commissions"), size = 1.25)+
  geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Other.imputed.commissions,color= "Other Imputed Commissions"), size = 1.25)+
  ylab("Index, January 2012 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(50,200), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1999-01-01"),as.Date("2021-09-01"))) +
  ggtitle("Out Of Commission") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data", subtitle = "Financial Commissions are Among the Most Volatile Price Indexes") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1999-01-01")-(.1861*8279), xmax = as.Date("1999-01-01")-(0.049*8279), ymin = 50-(.3*150), ymax = 50) +
  coord_cartesian(clip = "off")

OTC_Securities_graph <- ggplot() + #plotting otc securities price index
  geom_line(data=OTC_Securities, aes(x=date,y=Over.the.counter.equity.securities,color= "Over The Counter Equity Securities PCE Price Index"), size = 1.25)+
  ylab("Index, January 2012 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,3500), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1997-01-01"),as.Date("2021-09-01"))) +
  ggtitle("Priced Out: OTC Securities Price Index") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data", subtitle = "Sometimes Price Indexes Collapse Alongside the Product or Cost") +
  theme_apricitas + theme(legend.position = c(.50,.73)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*9009), xmax = as.Date("1997-01-01")-(0.049*9009), ymin = 0-(.3*3500), ymax = 0) +
  coord_cartesian(clip = "off")

RMD_Fed_graph <- ggplot() + #plotting otc securities price index
  geom_line(data=RMD_Fed_Mfg, aes(x=date,y=nsa_mfg_bk_logs_c,color= "Order Backlogs"), size = 1.25)+
  geom_line(data=RMD_Fed_Mfg, aes(x=date,y=nsa_mfg_vend_lead_c,color= "Vendor Lead Time"), size = 1.25)+
  geom_line(data=RMD_Fed_Mfg, aes(x=date,y=nsa_mfg_fd_gds_inv_c,color= "Inventories - Finished Goods"), size = 1.25)+
  geom_line(data=RMD_Fed_Mfg, aes(x=date,y=nsa_mfg_raw_mats_inv_c,color= "Inventories - Raw Materials"), size = 1.25)+
  ylab("Diffusion Index, 3 Month Moving Average") +
  xlab("Date") +
  scale_y_continuous(limits = c(-50,75), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
  ggtitle("I'm Willing to Wait For it") +
  labs(caption = "Graph created by @JosephPolitano using Richmond Fed Survey of Manufacturing Activity Data", subtitle = "Manufacturers Respond to Demand Pressure By Depleting Inventories and Increasing Lead Times") +
  theme_apricitas + theme(legend.position = c(.65,.83)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotate("vline", x = as.Date("2020-03-01"), xintercept = as.Date("2020-03-01"), color = "white", linetype = "dashed", size = 1.25) +
  annotate("text", label = "Shutdowns Begin", x = as.Date("2019-12-01"), y = 50, color = "white") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*974), xmax = as.Date("2019-01-01")-(0.049*974), ymin = -50-(.3*125), ymax = -50) +
  coord_cartesian(clip = "off")

Price_Controls_Graph <- ggplot() + #plotting all commission fees
  #geom_line(data=BEAPCEDISAGG, aes(x=Date,y=First.class.postal.service..by.U.S..Postal.Service.,color= "USPS First Class"), size = 1.25)+
  geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Physician.services..44.,color= "Physician Services"), size = 1.25)+
  #geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Food.supplied.to.military,color= "Food Supplied to Military"), size = 1.25)+
  #geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Elementary.and.secondary.school.lunches,color= "Food at Employee Sites and Schools"), size = 1.25)+
  ylab("PCE Price Index, January 2012 = 100") +
  xlab("Date") +
  scale_y_continuous(limits = c(90,110), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2012-01-01"),as.Date("2021-09-01"))) +
  ggtitle("Complete Control") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data", subtitle = "Governments Have Direct or Indirect Price-Setting Power In Many Sectors") +
  theme_apricitas + theme(legend.position = c(.52,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  #annotate("vline", x = as.Date("2020-04-01"), xintercept = as.Date("2020-04-01"), color = "#FFE98F", linetype = "dashed", size = 1.25) +
  #annotate("vline", x = as.Date("2021-04-01"), xintercept = as.Date("2021-07-01"), color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate("vline", x = as.Date("2014-12-01"), xintercept = as.Date("2014-12-01"), color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate("vline", x = as.Date("2020-12-01"), xintercept = as.Date("2020-12-01"), color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate("text", label = "ACA 100% Federal Funding For", x = as.Date("2013-5-01"), y = 97, color = "#FFE98F") +
  annotate("text", label = "Primary Care Fees End", x = as.Date("2013-5-01"), y = 96, color = "#FFE98F") +
  annotate("text", label = "CMS Physician Fee Schedule", x = as.Date("2019-6-01"), y = 97, color = "#FFE98F") +
  annotate("text", label = "Bump Takes Effect", x = as.Date("2019-6-01"), y = 96, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*3531), xmax = as.Date("2012-01-01")-(0.049*3531), ymin = 90-(.3*20), ymax = 90) +
  coord_cartesian(clip = "off")

Lunches_Graph <- ggplot() + #plotting elementary and secondary school lunches
  geom_line(data=BEAPCEDISAGG, aes(x=Date,y=Elementary.and.secondary.school.lunches,color= "Food at Employee Sites and Schools"), size = 1.25)+
  ylab("CPI Price Index, January 2012 = 100") +#technically these BEA data points are derived from CPI
  xlab("Date") +
  scale_y_continuous(limits = c(60,130), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2018-01-01"),as.Date("2021-09-01"))) +
  ggtitle("School Success Story") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data", subtitle = "Government Programs Have Slashed School Meal Costs During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.40,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotate("vline", x = as.Date("2020-04-01"), xintercept = as.Date("2020-04-01"), color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate("vline", x = as.Date("2021-04-01"), xintercept = as.Date("2021-07-01"), color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate("text", label = "USDA Reimburses Universal", x = as.Date("2019-9-01"), y = 72, color = "#FFE98F") +
  annotate("text", label = "Free Meals to Students", x = as.Date("2019-9-01"), y = 68, color = "#FFE98F") +
  annotate("text", label = "California Passes Free School", x = as.Date("2020-12-01"), y = 72, color = "#FFE98F") +
  annotate("text", label = "Meals For All Act", x = as.Date("2020-12-01"), y = 68, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1339), xmax = as.Date("2018-01-01")-(0.049*1339), ymin = 60-(.3*70), ymax = 60) +
  coord_cartesian(clip = "off")

Lumber_Graph <- ggplot() + #plotting lumber Futures Prices
  geom_line(data=Lumber_Futures, aes(x=date,y=adjusted,color= "Lumber Futures Price"), size = 1.25)+
  ylab("US Dollars") +#technically these BEA data points are derived from CPI
  xlab("Date") +
  scale_y_continuous(limits = c(200,1800), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-10-01"))) +
  ggtitle("That's Bullwhip!") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance Data", subtitle = "Prices Can Shift in a Volatile Self Reinforcing Cycle Called a 'Bullwhip'") +
  theme_apricitas + theme(legend.position = c(.40,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))+ 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*639), xmax = as.Date("2020-01-01")-(0.049*639), ymin = 200-(.3*1600), ymax = 200) +
  coord_cartesian(clip = "off")


#postal,meals at school, military uniforms, and Physician services alongside some other healthcare stuff for "price controls" index
#maybe talk about phone service hedonic adjustment
#whiplash effect

ggsave(dpi = "retina",plot = INF_INDEX_GRAPH, "The Importance of Indexes.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Commissions_PCE_graph, "Commissions Indexes.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = OTC_Securities_graph, "OTC Index.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = RMD_Fed_graph, "Richmond Fed Survey of Manufacturing Activity.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Price_Controls_Graph, "Price Controls.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Lunches_Graph, "Lunches.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = ALT_INF_INDEX_GRAPH, "Alternative Inflation Indexes.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = CORE_INF_INDEX_GRAPH, "Core Inflation.png", type = "cairo-png") 
ggsave(dpi = "retina",plot = Lumber_Graph, "Lumber Futures.png", type = "cairo-png") 



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

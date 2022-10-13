pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), plot.title = element_text(size = 28, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#downloading pre and post revisions GDI and GDP
GDP_PRE <-  fredr(series_id = "GDP", realtime_start = as.Date("2022-09-28"), observation_start = as.Date("2018-01-01")) %>% subset(realtime_start == as.Date("2022-09-28"))
GDP_POS <- fredr(series_id = "GDP", realtime_start = as.Date("2022-09-30"), observation_start = as.Date("2018-01-01"))

GDI_PRE <- fredr(series_id = "GDI", realtime_start = as.Date("2022-09-28"), observation_start = as.Date("2018-01-01")) %>% subset(realtime_start == as.Date("2022-09-28"))
GDI_POS <- fredr(series_id = "GDI", realtime_start = as.Date("2022-09-30"), observation_start = as.Date("2018-01-01"))

BEA_PRIV_WAG_PRE <- fredr(series_id = "A132RC1", realtime_start = as.Date("2022-09-28"), observation_start = as.Date("2018-01-01")) %>% subset(realtime_start == as.Date("2022-09-28"))
BEA_PRIV_WAG_POS <- fredr(series_id = "A132RC1", realtime_start = as.Date("2022-10-08"), observation_start = as.Date("2018-01-01"))

BLS_PRIV_WAG <- fredr(series_id = "CES0500000017", realtime_start = as.Date("2022-09-30"), observation_start = as.Date("2018-01-01"))

#adding wages for manufacturing
BEA_MAN_WAG_PRE <- fredr(series_id = "N552RC1M027SBEA", realtime_start = as.Date("2022-09-28"), observation_start = as.Date("2018-01-01")) %>% subset(realtime_start == as.Date("2022-09-28"))
BEA_MAN_WAG_POS <- fredr(series_id = "N552RC1M027SBEA", realtime_start = as.Date("2022-10-08"), observation_start = as.Date("2018-01-01"))

BLS_MAN_WAG <- fredr(series_id = "CES3000000017", realtime_start = as.Date("2022-09-30"), observation_start = as.Date("2018-01-01"))

#Trade, Transportation, Utilities Wages
BEA_TTU_WAG_PRE <- fredr(series_id = "N234RC1M027SBEA", realtime_start = as.Date("2022-09-28"), observation_start = as.Date("2018-01-01")) %>% subset(realtime_start == as.Date("2022-09-28"))
BEA_TTU_WAG_POS <- fredr(series_id = "N234RC1M027SBEA", realtime_start = as.Date("2022-10-08"), observation_start = as.Date("2018-01-01"))

BLS_TTU_WAG <- fredr(series_id = "CES4000000017", realtime_start = as.Date("2022-09-30"), observation_start = as.Date("2018-01-01"))

EMP_PARTS <- bls_api("CES3133630001", startyear = 2019, endyear = 2022, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))#cpi used cars data

REAL_GDP_QUARTERLY <- fredr(series_id = "GDPC1", observation_start = as.Date("1948-01-01"), units = "pch")
NONFARM_GROWTH_QUARTERLY <- fredr(series_id = "PAYEMS", observation_start = as.Date("1948-01-01"), frequency = "q", aggregation_method = "eop", units = "pch")

OKUN_MERGE <- merge(REAL_GDP_QUARTERLY, NONFARM_GROWTH_QUARTERLY, by = "date") %>%
  select(date, value.x, value.y)

FIXED_EQUIP_PRE <- fredr(series_id = "Y033RX1Q020SBEA", realtime_start = as.Date("2022-09-28"), observation_start = as.Date("2018-01-01")) %>% subset(realtime_start == as.Date("2022-09-28"))
FIXED_EQUIP_POST <- fredr(series_id = "Y033RX1Q020SBEA", realtime_start = as.Date("2022-10-08"), observation_start = as.Date("2018-01-01"))

RGDP <- fredr(series_id = "GDPC1",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP
RGDI <- fredr(series_id = "A261RX1Q020SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDI
RGDO <- fredr(series_id = "LB0000091Q020SBEA",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDI


GDP_GDI_PRE_POST_Graph <- ggplot() + #plotting GDP Pre and Post Revisions
  geom_line(data = GDP_PRE, aes(x=date, y = value/1000, color = "GDP: Pre-Revisions"), size = 1.25, linetype = "dashed") + 
  geom_line(data = GDI_PRE, aes(x=date, y = value/1000, color = "GDI: Pre-Revisions"), size = 1.25, linetype = "dashed") + 
  geom_line(data = GDP_POS, aes(x=date, y = value/1000, color = "GDP: Post-Revisions"), size = 1.25) + 
  geom_line(data = GDI_POS, aes(x=date, y = value/1000 , color = "GDI: Post-Revisions"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(19,26), breaks = c(19,20,21,22,23,24,25,26), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The New Economic Picture") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Downward Revisions to GDI and Upward Revisions to GDP Helped Close the Discrepancy") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("GDP: Pre-Revisions","GDI: Pre-Revisions","GDP: Post-Revisions","GDI: Post-Revisions"),guide=guide_legend(override.aes=list(linetype=c(2,2,1,1), lwd = c(.75,.75,1.25,1.25)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 19-(.3*7), ymax = 19) +
  coord_cartesian(clip = "off")

BEA_BLS_PRE_POST_Graph <- ggplot() + #plotting GDP Pre and Post Revisions
  geom_line(data = BEA_PRIV_WAG_PRE, aes(x=date, y = value/77.473, color = "BEA Private Wages and Salaries: Pre-Revisions"), size = 1.25, linetype = "dashed") + 
  geom_line(data = BEA_PRIV_WAG_POS, aes(x=date, y = value/77.473, color = "BEA Private Wages and Salaries: Post-Revisions"), size = 1.25) + 
  geom_line(data = BLS_PRIV_WAG, aes(x=date, y = value/1.458, color = "BLS Index of Private Aggregate Payrolls"), size = 1.25) + 
  xlab("Date") +
  ylab("Index, Jan 2019 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120,130), limits = c(90,130), expand = c(0,0)) +
  ggtitle("The New Economic Picture") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Revisions Helped Narrow the Wage Discrepancy") +
  theme_apricitas + theme(legend.position = c(.37,.69)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("BEA Private Wages and Salaries: Pre-Revisions","BEA Private Wages and Salaries: Post-Revisions","BLS Index of Private Aggregate Payrolls"),guide=guide_legend(override.aes=list(linetype=c(2,1,1), lwd = c(.75,1.25,1.25)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*40), ymax = 90) +
  coord_cartesian(clip = "off")

OKUN_Graph <- ggplot() + #plotting Okun's Law
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("vline", x = 0, xintercept = 0, color = "white", size = 0.5) +
  geom_point(data = subset(OKUN_MERGE, date < as.Date("2022-01-01")), aes(x=value.x/100, y = value.y/100, color = "1948-2022"), size = 1.5) + 
  geom_point(data = subset(OKUN_MERGE, date >= as.Date("2022-01-01")), aes(x=value.x/100, y = value.y/100, color = "2022"), size = 1.5) + 
  xlab("Real GDP Growth") +
  ylab("Nonfarm Payrolls Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.03,-0.02,-0.01,0,0.01,0.02,0.031), limits = c(-0.03,0.031), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05), limits = c(-.03,0.05), expand = c(0,0)) +
  ggtitle("Eat Your Heart Out, Art Okun!") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "The US Saw Strong Employment Growth With Negative GDP Growth-A Historical Rarity") +
  theme_apricitas + theme(legend.position = c(.15,.85), legend.title = element_text(size = 14)) +
  scale_color_manual(name= "Job vs GDP Growth",values = c("#FFE98F","#EE6055"), breaks = c("1948-2022","2022")) +
  annotation_custom(apricitas_logo_rast, xmin = -.03, xmax = -.027-(.1861*.08), ymin = -.030-(.3*0.06), ymax = -.030) +
  coord_cartesian(clip = "off")

FIXED_EQUP_PRE_POST_Graph <- ggplot() + #plotting fixed equipment
  geom_line(data = FIXED_EQUIP_PRE, aes(x=date, y = value/1000, color = "Real Fixed Investment: Equipment: Pre-Revision"), size = 1.25, linetype = "dashed") + 
  geom_line(data = FIXED_EQUIP_POST, aes(x=date, y = value/1000, color = "Real Fixed Investment: Equipment: Post-Revision"), size = 1.25) + 
  xlab("Date") +
  ylab("Trillions of 2012 Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = .1, suffix = "T"), breaks = c(1,1.1,1.2,1.3,1.4), limits = c(1,1.4), expand = c(0,0)) +
  ggtitle("The New Economic Picture") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Fixed Investment in Equipment Was Revised Down Significantly") +
  theme_apricitas + theme(legend.position = c(.42,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("Real Fixed Investment: Equipment: Pre-Revision","Real Fixed Investment: Equipment: Post-Revision"),guide=guide_legend(override.aes=list(linetype=c(2,1), lwd = c(.75,1.25)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 1-(.3*.4), ymax = 1) +
  coord_cartesian(clip = "off")

RGDO_Graph <- ggplot() +
  geom_line(data = RGDP, aes(x=date, y = value/1000, color = "Real GDP"), size = 1.25) + 
  geom_line(data = RGDI, aes(x=date, y = value/1000, color = "Real GDI"), size = 1.25) + 
  geom_line(data = RGDO, aes(x=date, y = value/1000, color = "Real GDO (Average of GDP and GDI)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(17,21), breaks = c(17,18,19,20,21), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Updated Data Shows Worse Economic Growth in 2022") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055"), breaks = c("Real GDP","Real GDI","Real GDO (Average of GDP and GDI)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 17-(.3*4), ymax = 17) +
  coord_cartesian(clip = "off")

BEA_BLS_PRE_POST_MANU_Graph <- ggplot() + #plotting GDP Pre and Post Revisions for Manufacturing
  geom_line(data = BEA_MAN_WAG_PRE, aes(x=date, y = value/9.047, color = "BEA Manufacturing Wages and Salaries: Pre-Revisions"), size = 1.25, linetype = "dashed") + 
  geom_line(data = BEA_MAN_WAG_POS, aes(x=date, y = value/9.047, color = "BEA Manufacturing Wages and Salaries: Post-Revisions"), size = 1.25) + 
  geom_line(data = BLS_MAN_WAG, aes(x=date, y = value/1.196, color = "BLS Index of Private Aggregate Payrolls, Manufacturing"), size = 1.25) + 
  xlab("Date") +
  ylab("Index, Jan 2019 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120), limits = c(85,120), expand = c(0,0)) +
  ggtitle("Understanding the Wage Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Manufacturing Data's Wage Discrepancy Has Shrunk Dramatically and is Now Small") +
  theme_apricitas + theme(legend.position = c(.37,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("BEA Manufacturing Wages and Salaries: Pre-Revisions","BEA Manufacturing Wages and Salaries: Post-Revisions","BLS Index of Private Aggregate Payrolls, Manufacturing"),guide=guide_legend(override.aes=list(linetype=c(2,1,1), lwd = c(.75,1.25,1.25)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*35), ymax = 85) +
  coord_cartesian(clip = "off")

BEA_BLS_PRE_POST_TTU_Graph <- ggplot() + #plotting GDP Pre and Post Revisions for Manufacturing
  geom_line(data = BEA_TTU_WAG_PRE, aes(x=date, y = value/13.950, color = "BEA Trade, Transportation, and Utilities Wages and Salaries: Pre-Revisions"), size = 1.25, linetype = "dashed") + 
  geom_line(data = BEA_TTU_WAG_POS, aes(x=date, y = value/13.950, color = "BEA Trade, Transportation, and Utilities Wages and Salaries: Post-Revisions"), size = 1.25) + 
  geom_line(data = BLS_TTU_WAG, aes(x=date, y = value/1.332, color = "BLS Index of Private Aggregate Payrolls, Trade, Transportation, and Utilities"), size = 1.25) + 
  xlab("Date") +
  ylab("Index, Jan 2019 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120,130,140), limits = c(90,140), expand = c(0,0)) +
  ggtitle("Understanding the Wage Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Trade, Transportation, and Utilities' Wage Discrepancy Still Remains") +
  theme_apricitas + theme(legend.position = c(.49,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),breaks = c("BEA Trade, Transportation, and Utilities Wages and Salaries: Pre-Revisions","BEA Trade, Transportation, and Utilities Wages and Salaries: Post-Revisions","BLS Index of Private Aggregate Payrolls, Trade, Transportation, and Utilities"),guide=guide_legend(override.aes=list(linetype=c(2,1,1), lwd = c(.75,1.25,1.25)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*50), ymax = 90) +
  coord_cartesian(clip = "off")

CES_PAYROLL_PRIVATE_ANNUAL <- fredr(series_id = "CEU0500000017",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "sum") %>%
  mutate(value = value/17.794)

QCEW_PAYROLL_PRIVATE_ANNUAL <- bls_api("ENUUS00030510", startyear = 2012,endyear = 2021, Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  group_by(year) %>%
  mutate(value = AVERAGE(value)) %>%
  filter(duplicated(year) == FALSE) %>%
  ungroup() %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year")) %>%
  mutate(value= value/18701665.24)


QCEW_CES_PRIVATE_MERGE <- merge(QCEW_PAYROLL_PRIVATE_ANNUAL,CES_PAYROLL_PRIVATE_ANNUAL, by = "date") %>%
  select(value.x,value.y,date) %>%
  mutate(diff = value.x-value.y) %>%
  mutate(sector = "Total Private Sector")
  
CES_PAYROLL_FINANCE_ANNUAL <- fredr(series_id = "CES5500000017",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "sum") %>%
  mutate(value = value/18.147)

QCEW_PAYROLL_FINANCE_ANNUAL <- bls_api("ENUUS0003051023", startyear = 2012,endyear = 2021, Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  group_by(year) %>%
  mutate(value = AVERAGE(value)) %>%
  filter(duplicated(year) == FALSE) %>%
  ungroup() %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year")) %>%
  mutate(value= value/2049095.35)

QCEW_CES_FINANCE_MERGE <- merge(QCEW_PAYROLL_FINANCE_ANNUAL,CES_PAYROLL_FINANCE_ANNUAL, by = "date") %>%
  select(value.x,value.y,date) %>%
  mutate(diff = value.x-value.y) %>%
  mutate(sector = "Finance")


#Information
CES_PAYROLL_INFORMATION_ANNUAL <- fredr(series_id = "CES5000000017",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "sum") %>%
  mutate(value = value/17.119)

QCEW_PAYROLL_INFORMATION_ANNUAL <- bls_api("ENUUS0003051022", startyear = 2012,endyear = 2021, Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  group_by(year) %>%
  mutate(value = AVERAGE(value)) %>%
  filter(duplicated(year) == FALSE) %>%
  ungroup() %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year")) %>%
  mutate(value= value/851940.01)

QCEW_CES_INFORMATION_MERGE <- merge(QCEW_PAYROLL_INFORMATION_ANNUAL,CES_PAYROLL_INFORMATION_ANNUAL, by = "date") %>%
  select(value.x,value.y,date) %>%
  mutate(diff = value.x-value.y) %>%
  mutate(sector = "Information")


#Business
CES_PAYROLL_BUSINESS_ANNUAL <- fredr(series_id = "CES6000000017",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "sum") %>%
  mutate(value = value/19.752)

QCEW_PAYROLL_BUSINESS_ANNUAL <- bls_api("ENUUS0003051024", startyear = 2012,endyear = 2021, Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  group_by(year) %>%
  mutate(value = AVERAGE(value)) %>%
  filter(duplicated(year) == FALSE) %>%
  ungroup() %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year")) %>%
  mutate(value= value/4161046.60)

QCEW_CES_BUSINESS_MERGE <- merge(QCEW_PAYROLL_BUSINESS_ANNUAL,CES_PAYROLL_BUSINESS_ANNUAL, by = "date") %>%
  select(value.x,value.y,date) %>%
  mutate(diff = value.x-value.y) %>%
  mutate(sector = "Professional and Business Services")



CES_QCEW_PRIVATE_PAYROLL_Graph <- ggplot() + #plotting CES private payrolls
  geom_line(data=CES_PAYROLL_PRIVATE_ANNUAL, aes(x=date,y= value,color= "CES Aggregate Private Sector Wages"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_PRIVATE_ANNUAL, aes(x=date,y= value,color= "QCEW Aggregate Private Sector Wages"), size = 1.25)+ 
  geom_line(data=CES_PAYROLL_INFORMATION_ANNUAL, aes(x=date,y= value,color= "CES Aggregate Private Sector Wages: Finance"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_INFORMATION_ANNUAL, aes(x=date,y= value,color= "QCEW Aggregate Private Sector Wages: Finance"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Total, Index, 2012 = 100") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("Aggregate Wages Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Why is There a Discrepancy Between QCEW and CES Data?") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

CES_QCEW_PRIVATE_PAYROLL_Graph <- ggplot() + #plotting CES private payrolls
  geom_bar(data=QCEW_CES_FINANCE_MERGE, aes(x=date,y= diff,color= "FINANCE"), size = 1.25)+ 
  geom_bar(data=QCEW_CES_PRIVATE_MERGE, aes(x=date,y= diff,color= "AGGREGATE PRIVATE"), size = 1.25)+ 
  geom_line(data=QCEW_CES_INFORMATION_MERGE, aes(x=date,y= diff,color= "INFORMATION"), size = 1.25)+ 
  geom_line(data=QCEW_CES_BUSINESS_MERGE, aes(x=date,y= diff,color= "BUSINESS"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Total, Index, 2012 = 100") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("Aggregate Wages Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Why is There a Discrepancy Between QCEW and CES Data?") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

QCEW_CES_COMPARISON_RBIND <- rbind(QCEW_CES_BUSINESS_MERGE,QCEW_CES_INFORMATION_MERGE,QCEW_CES_FINANCE_MERGE,QCEW_CES_PRIVATE_MERGE) %>%
  subset(., date > as.Date("2019-02-01"))

CES_QCEW_GROWTH_GAP_Graph <- ggplot(data = QCEW_CES_COMPARISON_RBIND, aes(x = date, y = diff, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab("Year") +
  ylab("Gap Between CES and QCEW Growth Since 2019") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,5,10,15,20), limits = c(0,22.5), expand = c(0,0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggtitle("The CES-QCEW Growth Gap Since 2019") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Information Sector Has the Biggest QCEW-CES Growth Gap Gap") +
  theme_apricitas + theme(legend.position = c(.25,.79)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-9-15")-(.1861*(today()-as.Date("2019-09-15"))), xmax = as.Date("2019-09-15")-(0.049*(today()-as.Date("2019-09-15"))), ymin = 0-(.3*22.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_GDI_PRE_POST_Graph, "GDP GDI PRE POST.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = BEA_BLS_PRE_POST_Graph, "BEA BLS PRE POST.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = OKUN_Graph, "OKUN Graph.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = FIXED_EQUP_PRE_POST_Graph, "Fixed Equipment Pre Post.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = RGDO_Graph, "RGDO.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = BEA_BLS_PRE_POST_MANU_Graph, "BEA BLS PRE POST MANU.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = BEA_BLS_PRE_POST_TTU_Graph, "BEA BLS PRE POST TTU.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = CES_QCEW_GROWTH_GAP_Graph, "CES QCEW Growth Gap.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
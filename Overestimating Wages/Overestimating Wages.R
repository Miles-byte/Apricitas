pacman::p_load(svglite,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


BEA_PAYROLL_INDEX <- fredr(series_id = "NA000275Q",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

QCEW_PAYROLL_INDEX <- bls_api("ENUUS00030010", startyear = 2019) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2019-01-01"), as.Date("2022-01-01"), "3 months")) %>%
  mutate(value= value/22451792.77)

CES_PAYROLL_PRIVATE_INDEX <- fredr(series_id = "CEU0500000017",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

QCEW_PAYROLL_PRIVATE_INDEX <- bls_api("ENUUS00030510", startyear = 2019) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2019-01-01"), as.Date("2022-01-01"), "3 months")) %>%
  mutate(value= value/19299130.62)

CES_PAYROLL_PRIVATE_SA <- fredr(series_id = "CES0500000017",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
BEA_PAYROLL_PRVATE_SA <- fredr(series_id = "A132RC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

BEA_CES_PRIVATE_PAYROLL_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CES_PAYROLL_PRIVATE_SA, aes(x=date,y= value/1.458,color= "CES Index of Aggregate Weekly Payrolls of All Employees, Total Private"), size = 1.25)+ 
  geom_line(data=BEA_PAYROLL_PRVATE_SA, aes(x=date,y= value/77.473,color= "BEA Wage and Salary Disbursements, Private Industries"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2019 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120,130,140), limits = c(90,140), expand = c(0,0)) +
  ggtitle("The $665B Wage Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "There is a Widening Gap Between CES and BEA's Estimates of Aggregate Wages") +
  theme_apricitas + theme(legend.position = c(.47,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 90-(.3*50), ymax = 90) +
  coord_cartesian(clip = "off")

BEA_QCEW_Graph <- ggplot() + #plotting permanent and temporary job losers
  #geom_line(data=CES_PAYROLL_INDEX, aes(x=date,y= value/1.42,color= "CES Private Payroll (NSA)"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_INDEX, aes(x=date,y= value,color= "QCEW Payrolls (NSA)"), size = 1.25)+ 
  geom_line(data=BEA_PAYROLL_INDEX, aes(x=date,y= value/23898.83,color= "BEA Payrolls (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan/Q1 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("NSA Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Why is there a Discrepancy Between NSA BEA/QCEW data for 2021?") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

QCEW_CES_PrivateGraph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CES_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value/1.42,color= "CES Private Payroll (NSA)"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value,color= "QCEW Private Payrolls (NSA)"), size = 1.25)+ 
  annotate("vline", x = as.Date("2019-01-01"), xintercept = as.Date("2019-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2021-01-01"), xintercept = as.Date("2021-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2022-01-01"), xintercept = as.Date("2022-01-01"), color = "white", size = .5, linetype = "dashed") +
  xlab("Date") +
  ylab("Index, Jan/Q1 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("CES/QCEW Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "No Major Discrepancy Between CES/QCEW Data") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

BEA_QCEW_CES_PrivateGraph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CES_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value/1.42,color= "CES Private Payroll (NSA)"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value,color= "QCEW Private Payrolls (NSA)"), size = 1.25)+ 
  geom_line(data=BEA_PAYROLL_INDEX, aes(x=date,y= value/23898.83,color= "BEA Payrolls (NSA)"), size = 1.25)+ 
  annotate("vline", x = as.Date("2019-01-01"), xintercept = as.Date("2019-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2021-01-01"), xintercept = as.Date("2021-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2022-01-01"), xintercept = as.Date("2022-01-01"), color = "white", size = .5, linetype = "dashed") +
  xlab("Date") +
  ylab("Index, Jan/Q1 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("BEA/CES/QCEW Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "No Major Discrepancy Between CES/QCEW Data") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")


#Comparing QCEW to CES data
CES_PAYROLL_PRIVATE_ANNUAL <- fredr(series_id = "CEU0500000017",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "sum") %>%
  mutate(value = value/13.08)
BEA_PAYROLL_PRIVATE_ANNUAL <- fredr(series_id = "A132RC1",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "sum") %>%
  mutate(value = value/687.58)

QCEW_PAYROLL_PRIVATE_ANNUAL <- bls_api("ENUUS00030510", startyear = 2012, calculations = TRUE, annualaverage = TRUE, catalog = TRUE) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  group_by(year) %>%
  mutate(value = AVERAGE(value)) %>%
  filter(duplicated(year) == FALSE) %>%
  ungroup() %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year")) %>%
  mutate(value= value/13609515.96)

CES_QCEW_PRIVATE_PAYROLL_Graph <- ggplot() + #plotting CES private payrolls
  geom_line(data=CES_PAYROLL_PRIVATE_ANNUAL, aes(x=date,y= value,color= "CES Aggregate Private Sector Wages"), size = 1.25)+ 
  geom_line(data=BEA_PAYROLL_PRIVATE_ANNUAL, aes(x=date,y= value,color= "BEA Aggregate Private Sector Wages"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_PRIVATE_ANNUAL, aes(x=date,y= value,color= "QCEW Aggregate Private Sector Wages"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Total, Index, 2012 = 100") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("Aggregate Wages Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Why is There a Discrepancy Between QCEW and CES Data?") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

QCEW_EMPLOYMENT <- bls_api("ENUUS00010010", startyear = 2012, registrationKey = Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2022-03-01"), "1 month"))

CES_EMPLOYMENT <- fredr(series_id = "PAYNSA",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL)

CES_PAYROLL_Graph <- ggplot() + #plotting CES private payrolls
  geom_line(data=CES_EMPLOYMENT, aes(x=date,y= value/1310.95,color= "CES Nonfarm Payrolls (NSA, Quarterly)"), size = 1.25)+ 
  geom_line(data=QCEW_EMPLOYMENT, aes(x=date,y= value/1285146.90,color= "QCEW Total Employment (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Total, Index, 2012 = 100") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("Indexed Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Why is There a Discrepancy Between QCEW and CES Data?") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

AWE_PRIVATE_NSA <- fredr(series_id = "CEU0500000011",observation_start = as.Date("2012-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(annualpct = (value-lag(value, 12))/lag(value, 12))

QCEW_WEEKLY_WAGES <- bls_api("ENUUS00040510", startyear = 2012, registrationKey = Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2022-03-01"), "3 months")) %>%
  mutate(annualpct = (value-lag(value, 4))/lag(value, 4))

QCEW_ANNUAL_WAGES <- bls_api("ENUUS00050510", startyear = 2012, registrationKey = Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year")) %>%
  

CES_QCEW_PRIVATE_WAGES_Graph <- ggplot() + #plotting CES private payrolls
  geom_line(data=AWE_PRIVATE_NSA, aes(x=date,y= value/8.1248,color= "CES Average Weekly Earnings, Total Private, NSA"), size = 1.25)+ 
  geom_line(data=QCEW_WEEKLY_WAGES, aes(x=date,y= value/9.90,color= "QCEW Average Weekly Wages, Total Private, NSA"), size = 1.25)+ 
  geom_line(data=QCEW_ANNUAL_WAGES, aes(x=date+180,y= value/492.00,color= "QCEW Average Annual Wages, Total Private, NSA"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Total, Index, 2012 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120,130,140), limits = c(89,146), expand = c(0,0)) +
  ggtitle("The Wage Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "There a Widening Discrepancy Between QCEW and CES Data") +
  theme_apricitas + theme(legend.position = c(.34,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 89-(.3*57), ymax = 89) +
  coord_cartesian(clip = "off")

CES_QCEW_PRIVATE_WAGES_Graph <- ggplot() + #plotting CES private payrolls
  geom_line(data=AWE_PRIVATE_NSA, aes(x=date,y= annualpct,color= "CES Average Weekly Earnings, Total Private"), size = 1.25)+ 
  geom_line(data=QCEW_WEEKLY_WAGES, aes(x=date,y= annualpct,color= "QCEW Average Weekly Wages, Total Private"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Percent Growth") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(90,100,110,120,130,140), limits = c(0,0.15), expand = c(0,0)) +
  ggtitle("The Wage Growth Gap") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "For CES and QCEW to Converge Again, CES Growth Will Have to Outpace QCEW Growth") +
  theme_apricitas + theme(legend.position = c(.34,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 89-(.3*57), ymax = 89) +
  coord_cartesian(clip = "off")

NGDI <- fredr(series_id = "GDI",observation_start = as.Date("2019-01-01"), units = "pca", realtime_start = NULL, realtime_end = NULL)
NGDP <- fredr(series_id = "GDP",observation_start = as.Date("2019-01-01"), units = "pca", realtime_start = NULL, realtime_end = NULL)

GDP_GDI_GROWTH <- ggplot() + #plotting CES private payrolls
  geom_line(data=NGDI, aes(x=date,y= value/100,color= "Nominal Gross Domestic Income"), size = 1.25)+ 
  geom_line(data=NGDP, aes(x=date,y= value/100,color= "Nominal Gross Domestic Product"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annualized Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.4,-.2,0,.2,.4), limits = c(-.40,.40), expand = c(0,0)) +
  ggtitle("The GDP-GDI Gap") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "When QCEW data is Incorporated into 2022 Growth Estimates, GDP and GDI Could Converge") +
  theme_apricitas + theme(legend.position = c(.70,.39)) +
  scale_color_manual(name= "Quarterly Percent Growth, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -.40-(.3*.80), ymax = -.40) +
  coord_cartesian(clip = "off")

CES_AWE_SA <- fredr(series_id = "CES0500000011",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
ECI_WAGES_SA <- fredr(series_id = "ECIWAG",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
CPS_WAGES_SA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Overestimating%20Wages/EPI_MEDIAN_AVERAGE_WAGES.csv") %>%
  mutate(Median = gsub("\\$","",.$Median)) %>%
  mutate(Average = gsub("\\$","",.$Average)) %>%
  mutate(Date = seq(as.Date("2021-03-01"), as.Date("1973-03-01"), "-1 year")) %>%
  subset(.,Date > as.Date("2017-01-01")) %>%
  mutate(Average = as.numeric(Average))
QCEW_ANNUAL_WAGES_2017 <- bls_api("ENUUS00050510", startyear = 2017, registrationKey = Sys.getenv("BLS_KEY")) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2017-01-01"), as.Date("2021-01-01"), "1 year"))

CES_ECI_QCEW_CPS_WAGES_Graph <- ggplot() + #plotting CES private payrolls
  geom_line(data=CES_AWE_SA, aes(x=date,y= value/8.95,color= "CES Average Weekly Earnings, Total Private"), size = 1.25)+ 
  geom_line(data=CPS_WAGES_SA, aes(x=Date,y= Average/.271,color= "CPS Average Hourly Earnings"), size = 1.25)+ 
  geom_line(data=ECI_WAGES_SA, aes(x=date,y= value/1.28,color= "ECI Private Sector Wages and Salaries"), size = 1.25)+ 
  geom_line(data=QCEW_ANNUAL_WAGES_2017, aes(x=date+180,y= value/546.8,color= "QCEW Average Annual Wages, Total Private"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Total, Index, 2017 = 100, QCEW Re-indexed to CES") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(90,100,110,120,130,140), limits = c(99,130), expand = c(0,0)) +
  ggtitle("The Wage Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS and EPI data", subtitle = "QCEW Data is Overshooting All Other Official Wage Estimates") +
  theme_apricitas + theme(legend.position = c(.32,.79)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 99-(.3*30), ymax = 99) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = BEA_QCEW_Graph, "BEA QCEW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = QCEW_CES_PrivateGraph, "QCEW CES Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_QCEW_CES_PrivateGraph, "BEA QCEW CES Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CES_QCEW_PRIVATE_WAGES_Graph, "QCEW CES PRIVATE PAYROLL.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CES_PAYROLL_Graph, "QCEW CES EMPLOYMENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CES_QCEW_PRIVATE_PAYROLL_Graph, "QCEW CES Private Payrolls.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_CES_PRIVATE_PAYROLL_Graph, "BEA CES Private Payrolls.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GDP_GDI_GROWTH, "GDP GDI GAP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CES_ECI_QCEW_CPS_WAGES_Graph, "CES ECI QCEW CPS WAGES.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
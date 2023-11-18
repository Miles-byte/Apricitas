pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

REAL_HOURLY_COMPENSATION <- fredr("COMPRNFB", observation_start = as.Date("2015-01-01")) 

REAL_HOURLY_COMPENSATION_GROWTH <- REAL_HOURLY_COMPENSATION %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

REAL_HOURLY_COMPENSATION <- REAL_HOURLY_COMPENSATION %>%
  mutate(trend = if_else(row_number() > 20, 
                         value[20] * (1 + REAL_HOURLY_COMPENSATION_GROWTH$growth_rate[20]/100)^(row_number()-20), 
                         value))

REAL_HOURLY_COMPENSATION_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_HOURLY_COMPENSATION, aes(x=date, y = value/value[20]*100, color = "Nonfarm Business Sector: Real Hourly Compensation for All Workers"), size = 1.25) + 
  geom_line(data = filter(REAL_HOURLY_COMPENSATION, date >= as.Date("2019-10-01")), aes(x=date, y = trend/trend[1]*100, color = "Q1 2015-Q4 2019 Trend Growth Rate"), size = 1.25, linetype = "dashed") + 
  annotate("text", label ="Average Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-02-01"), y = 102.5, color = "white", size = 4)+
  xlab("Date") +
  scale_y_continuous(limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("Average Real Wages are Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Real Hourly Compensation is Rising, Although it Remains Below the 2015-2019 Trend Line") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 92.5-(.3*17.5), ymax = 92.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_HOURLY_COMPENSATION_GRAPH, "Real Hourly Compensation Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_MEDIAN_USUAL_EARNINGS <- fredr("LES1252881600Q", observation_start = as.Date("2015-01-01"))
  
REAL_MEDIAN_USUAL_EARNINGS_GROWTH <- REAL_MEDIAN_USUAL_EARNINGS %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

REAL_MEDIAN_USUAL_EARNINGS <- REAL_MEDIAN_USUAL_EARNINGS %>%
  mutate(trend = if_else(row_number() > 20, 
                         value[20] * (1 + REAL_MEDIAN_USUAL_EARNINGS_GROWTH$growth_rate[20]/100)^(row_number()-20), 
                         value))

NOMINAL_MEDIAN_USUAL_EARNINGS <- fredr("LES1252881500Q", observation_start = as.Date("2015-01-01"))

PCEPI_QUARTERLY <- fredr("PCEPI", observation_start = as.Date("2015-01-01"), frequency = "q", aggregation_method = "avg")

REAL_PCEPI_MEDIAN_USUAL_EARNINGS <- merge(NOMINAL_MEDIAN_USUAL_EARNINGS,PCEPI_QUARTERLY, by = "date") %>%
  transmute(date, value = value.x/value.y)

REAL_PCEPI_MEDIAN_USUAL_EARNINGS_GROWTH <- REAL_PCEPI_MEDIAN_USUAL_EARNINGS %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

REAL_PCEPI_MEDIAN_USUAL_EARNINGS <- REAL_PCEPI_MEDIAN_USUAL_EARNINGS %>%
  mutate(trend = if_else(row_number() > 20, 
                         value[20] * (1 + REAL_PCEPI_MEDIAN_USUAL_EARNINGS_GROWTH$growth_rate[20]/100)^(row_number()-20), 
                         value))


REAL_MEDIAN_USUAL_EARNINGS_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_MEDIAN_USUAL_EARNINGS, aes(x=date, y = value/value[20]*100, color = "Median Usual Weekly Earnings, Full-Time, Deflated by CPI"), size = 1.25) + 
  geom_line(data = filter(REAL_MEDIAN_USUAL_EARNINGS, date >= as.Date("2019-10-01")), aes(x=date, y = trend/trend[1]*100, color = "Q1 2015-Q4 2019 Trend Growth Rate"), size = 1.25, linetype = "dashed") + 
  geom_line(data = REAL_PCEPI_MEDIAN_USUAL_EARNINGS, aes(x=date, y = value/value[20]*100, color = "Median Usual Weekly Earnings, Full-Time, Deflated by PCEPI"), size = 1.25) + 
  geom_line(data = filter(REAL_PCEPI_MEDIAN_USUAL_EARNINGS, date >= as.Date("2019-10-01")), aes(x=date, y = trend/trend[1]*100, color = "Q1 2015-Q4 2019 Trend Growth Rate "), size = 1.25, linetype = "dashed") + 
  annotate("text", label ="Median Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-03-01"), y = 102.5, color = "white", size = 4)+
  xlab("Date") +
  scale_y_continuous(limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("Median Real Wages are Up But Below-Trend") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Median Real Wages Have Risen Since 2019 But Remain Below the Late-2010s Trend") +
  theme_apricitas + theme(legend.position = c(.70,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Median Usual Weekly Earnings, Full-Time, Deflated by CPI","Q1 2015-Q4 2019 Trend Growth Rate","Median Usual Weekly Earnings, Full-Time, Deflated by PCEPI","Q1 2015-Q4 2019 Trend Growth Rate "),guide=guide_legend(override.aes=list(linetype=c(1,2,1,2), lwd = c(1.25,.75,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm"), plot.title = element_text(size =27),legend.key.height = unit(0,"cm"), legend.spacing.y = unit(0,"cm"), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 92.5-(.3*17.5), ymax = 92.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_MEDIAN_USUAL_EARNINGS_GRAPH, "Real Median Usual Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

AHE_ALL_PRIVATE <- fredr("CES0500000003", observation_start = as.Date("2015-01-01"))
AHE_PROD_NONSUPER <- fredr("AHETPI", observation_start = as.Date("2015-01-01"))
CPI_2015 <- fredr("CPIAUCSL", observation_start = as.Date("2014-12-01"))

AHE_ALL_PRIVATE_REAL <- merge(AHE_ALL_PRIVATE,CPI_2015, by = "date") %>%
  transmute(date, value = value.x/value.y)

AHE_ALL_PRIVATE_REAL_GROWTH <- AHE_ALL_PRIVATE_REAL %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

AHE_ALL_PRIVATE_REAL <- AHE_ALL_PRIVATE_REAL %>%
  mutate(trend = if_else(row_number() > 60, 
                         value[60] * (1 + AHE_ALL_PRIVATE_REAL_GROWTH$growth_rate[60]/100)^(row_number()-60), 
                         value))

AHE_PROD_NONSUPER_REAL <- merge(AHE_PROD_NONSUPER,CPI_2015, by = "date") %>%
  transmute(date, value = value.x/value.y)

REAL_AHE_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = AHE_PROD_NONSUPER_REAL, aes(x=date, y = value/value[60]*100, color = "Real Average Hourly Earnings of Production & Nonsupervisory Employees: Total Private"), size = 1.25) + 
  geom_line(data = AHE_ALL_PRIVATE_REAL, aes(x=date, y = value/value[60]*100, color = "Real Average Hourly Earnings of All Employees: Total Private"), size = 1.25) + 
  geom_line(data = filter(AHE_ALL_PRIVATE_REAL, date >= as.Date("2019-12-01")), aes(x=date, y = trend/trend[1]*100, color = "Jan 2015-Dec 2019 Trend Growth Rate"), size = 1.25, linetype = "dashed") + 
  annotate("text", label ="Average Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-05-01"), y = 102.5, color = "white", size = 4)+
  xlab("Date") +
  scale_y_continuous(limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Dec 2019 = 100") +
  ggtitle("Average Real Wages are Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Real Average Earnings are Up, But Only Above-Trend for Production & Nonsupervisory Workers") +
  theme_apricitas + theme(legend.position = c(.525,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Real Average Hourly Earnings of All Employees: Total Private","Real Average Hourly Earnings of Production & Nonsupervisory Employees: Total Private","Jan 2015-Dec 2019 Trend Growth Rate"),guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 92.5-(.3*17.5), ymax = 92.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_AHE_GRAPH, "Real AHE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ECI_WAG <- fredr("ECIWAG", observation_start = as.Date("2014-10-01")) %>%
  mutate(date = date %m+% months(2))

REAL_ECI_WAG <- merge(ECI_WAG,CPI_2015, by = "date") %>%
  transmute(date, value = value.x/value.y)

REAL_ECI_WAG_GROWTH <- REAL_ECI_WAG %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

REAL_ECI_WAG <- REAL_ECI_WAG_GROWTH %>%
  mutate(trend = if_else(row_number() > 21, 
                         value[21] * (1 + REAL_ECI_WAG_GROWTH$growth_rate[21]/100)^(row_number()-21), 
                         value))


REAL_ECI_WAG_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_ECI_WAG, aes(x=date, y = value/value[21]*100, color = "Employment Cost Index: Real Wages & Salaries for Private Industry Workers"), size = 1.25) + 
  geom_line(data = filter(REAL_ECI_WAG, date >= as.Date("2019-12-01")), aes(x=date, y = trend/trend[1]*100, color = "Dec 2014-Dec 2019 Trend Growth Rate"), size = 1.25, linetype = "dashed") + 
  annotate("text", label ="The Employment Cost Index Adjusts for Changes in Job Composition\nSo it Doesn't Spike Aritificially When Low Wage Workers are Laid Off\nBut Also Doesn't Account for Pay Changes Caused By Switching Industry/Occupation", x = as.Date("2018-05-01"), y = 104, color = "white", size = 4)+
  xlab("Date") +
  scale_y_continuous(limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Dec 2019 = 100") +
  ggtitle("Job-Composition-Adjusted Real Wages Have Fallen") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Average Real Wages Rose Due to Job-Switching, But Adjusting for Composition Shifts They Fell") +
  theme_apricitas + theme(legend.position = c(.525,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Employment Cost Index: Real Wages & Salaries for Private Industry Workers","Dec 2014-Dec 2019 Trend Growth Rate"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm"), plot.title = (element_text(size = 22))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-12-01")-(.1861*(today()-as.Date("2014-12-01"))), xmax = as.Date("2014-12-01")-(0.049*(today()-as.Date("2014-12-01"))), ymin = 92.5-(.3*17.5), ymax = 92.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_ECI_WAG_GRAPH, "Real ECI WAG Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ATLANTA_FED_WAGE_TRACKER <- fredr("FRBATLWGT3MMAWMHWGO", observation_start = as.Date("2015-01-01"))
CPI_2015_GROWTH <- fredr("CPIAUCNS", observation_start = as.Date("2014-11-01"), units = "pc1") %>%
  mutate(value = c(NA,NA,rollmean(value,3))) %>%
  drop_na()

REAL_ATLANTA_FED_WAGE_TRACKER <- merge(ATLANTA_FED_WAGE_TRACKER,CPI_2015_GROWTH, by = "date") %>%
  transmute(date, value = (1+(value.x/100))/(1+value.y/100)-1)

REAL_ATLANTA_FED_WAGE_TRACKER_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=REAL_ATLANTA_FED_WAGE_TRACKER, aes(x=date,y= value,color= "Year-on-Year Real Median Hourly Wage Growth, Atlanta Fed Wage Tracker, 3MMA"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.02,.04),breaks = c(-0.02,0,.02,0.04), expand = c(0,0)) +
  ggtitle("Median Real Wage Growth is Positive") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "The Median Worker is Now Seeing Real Wage Increases, a Bounce-Back From 2022") +
  theme_apricitas + theme(legend.position = c(.5,.98)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.02-(.3*.06), ymax = -0.02) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_ATLANTA_FED_WAGE_TRACKER_Graph, "Real Atlanta Fed Wage Tracker Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

AVG_HOURLY_COMPENSATION <- fredr("COMPNFB", observation_start = as.Date("2015-01-01"))
PCEPI_2015 <- fredr("PCEPI", observation_start = as.Date("2015-01-01"), frequency = "q", aggregation_method = "avg")

REAL_PCEPI_HOURLY_COMPENSATION <- merge(AVG_HOURLY_COMPENSATION,PCEPI_2015, by = "date") %>%
  transmute(date, value = value.x/value.y)

REAL_PCEPI_HOURLY_COMPENSATION_GROWTH <- REAL_PCEPI_HOURLY_COMPENSATION %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

REAL_PCEPI_HOURLY_COMPENSATION <- REAL_PCEPI_HOURLY_COMPENSATION %>%
  mutate(trend = if_else(row_number() > 20, 
                         value[20] * (1 + REAL_PCEPI_HOURLY_COMPENSATION_GROWTH$growth_rate[20]/100)^(row_number()-20), 
                         value))

REAL_PCEPI_HOURLY_COMPENSATION_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_HOURLY_COMPENSATION, aes(x=date, y = value/value[20]*100, color = "Hourly Compensation for All Workers Deflated by CPI"), size = 1.25) + 
  geom_line(data = filter(REAL_HOURLY_COMPENSATION, date >= as.Date("2019-10-01")), aes(x=date, y = trend/trend[1]*100, color = "Q1 2015-Q4 2019 Trend Growth Rate"), size = 1.25, linetype = "dashed") + 
  geom_line(data = REAL_PCEPI_HOURLY_COMPENSATION, aes(x=date, y = value/value[20]*100, color = "Hourly Compensation for All Workers Deflated by PCEPI"), size = 1.25) + 
  geom_line(data = filter(REAL_PCEPI_HOURLY_COMPENSATION, date >= as.Date("2019-10-01")), aes(x=date, y = trend/trend[1]*100, color = "Q1 2015-Q4 2019 Trend Growth Rate "), size = 1.25, linetype = "dashed") + 
  annotate("text", label ="Average Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-03-01"), y = 102.5, color = "white", size = 4)+
  xlab("Date") +
  scale_y_continuous(limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("Average Real Wages are Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Choice of Inflation Adjustment Matters a Lot—and PCEPI Adjusted Compensation is On-Trend") +
  theme_apricitas + theme(legend.position = c(.725,.125)) +
  scale_color_manual(name= "Nonfarm Business Sector",values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Hourly Compensation for All Workers Deflated by CPI","Q1 2015-Q4 2019 Trend Growth Rate","Hourly Compensation for All Workers Deflated by PCEPI","Q1 2015-Q4 2019 Trend Growth Rate "), guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm"), legend.key.height = unit(0,"cm"), legend.spacing.y = unit(0,"cm"), legend.title = element_text(size = 14), legend.text = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 92.5-(.3*17.5), ymax = 92.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_PCEPI_HOURLY_COMPENSATION_GRAPH, "Real PCEPI Compensation Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

BEA_AGG_WAGES <- fredr("A576RC1", observation_start = as.Date("2015-01-01")) %>%
  mutate(value = value/value[61]*100)
BEA_AGG_NONWAGE_COMP <- fredr("B040RC1M027SBEA", observation_start = as.Date("2015-01-01")) %>%
  mutate(value = value/value[61]*100)

AGG_WAGE_VS_NONWAGE_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=BEA_AGG_WAGES, aes(x=date,y= value ,color= "Nominal Aggregate Wage and Salary Disbursements (Including Bonuses, Tips, etc)"), size = 1.25) + 
  geom_line(data=BEA_AGG_NONWAGE_COMP, aes(x=date,y= value ,color= "Nominal Aggregate Employer Contributions for Private Employee Pension & Insurance Funds"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(80,135),breaks = c(80,90,100,110,120,130), expand = c(0,0)) +
  ggtitle("Cutting up Compensation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Nominal Wages Have Risen Much Faster Than Employer Health Insurance/Retirement Spending") +
  theme_apricitas + theme(legend.position = c(.51,.90), legend.text = element_text(size = 12), legend.title = element_text(size = 13), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Index, Jan 2020 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Nominal Aggregate Wage and Salary Disbursements (Including Bonuses, Tips, etc)","Nominal Aggregate Employer Contributions for Private Employee Pension & Insurance Funds")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 80-(.3*55), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AGG_WAGE_VS_NONWAGE_GRAPH, "AGG_WAGE_VS_NONWAGE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CPI_NYC <- bls_api("CUURS12ASA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 12)) / lag(value, 12)) %>%
  drop_na()
  
CPI_LAX <- bls_api("CUURS49ASA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 12)) / lag(value, 12)) %>%
  drop_na()

CPI_CHI <- bls_api("CUURS23ASA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 12)) / lag(value, 12)) %>%
  drop_na()

CPI_DAL <- bls_api("CUURS37ASA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 6)) / lag(value, 6)) %>%
  drop_na()

CPI_HOU <- bls_api("CUURS37BSA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 6)) / lag(value, 6)) %>%
  drop_na()

CPI_WAS <- bls_api("CUURS35ASA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 6)) / lag(value, 6)) %>%
  drop_na()

CPI_MIA <- bls_api("CUURS35BSA0", startyear = 2014, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(Category = "All") %>%
  select(date, value, Category) %>%
  mutate(value = (value - lag(value, 6)) / lag(value, 6)) %>%
  drop_na()

CPI_CBSA_MSA_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=CPI_NYC, aes(x=date,y= value,color= "New York-Newark-Jersey City, NY-NJ-PA"), size = 1.25) + 
  geom_line(data=CPI_LAX, aes(x=date,y= value,color= "Los Angeles-Long Beach-Anaheim, CA"), size = 1.25)+ 
  geom_line(data=CPI_CHI, aes(x=date,y= value,color= "Chicago-Naperville-Elgin, IL-IN-WI"), size = 1.25)+ 
  geom_line(data=CPI_DAL, aes(x=date,y= value,color= "Dallas-Fort Worth-Arlington, TX"), size = 1.25)+ 
  geom_line(data=CPI_HOU, aes(x=date,y= value,color= "Houston-The Woodlands-Sugar Land, TX"), size = 1.25)+ 
  geom_line(data=CPI_WAS, aes(x=date,y= value,color= "Washington-Arlington-Alexandria, DC-VA-MD-WV"), size = 1.25)+ 
  geom_line(data=CPI_MIA, aes(x=date,y= value,color= "Miami-Fort Lauderdale-West Palm Beach, FL"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,.11),breaks = c(0,0.02,0.04,0.06,0.08,0.10), expand = c(0,0)) +
  ggtitle("Inflation Isn't Geographically Even") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Inflation Is Different Across The Country—Driven In Large Part by Rent Movements") +
  theme_apricitas + theme(legend.position = c(.35,.72)) +
  scale_color_manual(name= "CPI Inflation by Metropolitan Statistical Area",values = rev(c("#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F","#FF8E72")), breaks = c("New York-Newark-Jersey City, NY-NJ-PA","Los Angeles-Long Beach-Anaheim, CA","Chicago-Naperville-Elgin, IL-IN-WI","Dallas-Fort Worth-Arlington, TX","Houston-The Woodlands-Sugar Land, TX","Washington-Arlington-Alexandria, DC-VA-MD-WV","Miami-Fort Lauderdale-West Palm Beach, FL")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.01-(.3*.11), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CBSA_MSA_Graph, "CPI CBSA PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CPI_2015_GROWTH_12 <- fredr("CPIAUCNS", observation_start = as.Date("2014-01-01"), units = "pc1") %>%
  mutate(value = c(rep(NA,11),rollmean(value,12))) %>%
  drop_na()

ATL_WAG_QUART_1 <- fredr("FRBATLWGT12MMUMHWGWD1WP", observation_start = as.Date("2015-01-01")) %>%
  merge(.,CPI_2015_GROWTH_12, by = "date") %>%
  transmute(date, value = (1+(value.x/100))/(1+value.y/100)-1)
ATL_WAG_QUART_2 <- fredr("FRBATLWGT12MMUMHWGWD26WP", observation_start = as.Date("2015-01-01")) %>%
  merge(.,CPI_2015_GROWTH_12, by = "date") %>%
  transmute(date, value = (1+(value.x/100))/(1+value.y/100)-1)
ATL_WAG_QUART_3 <- fredr("FRBATLWGT12MMUMHWGWD51WP", observation_start = as.Date("2015-01-01")) %>%
  merge(.,CPI_2015_GROWTH_12, by = "date") %>%
  transmute(date, value = (1+(value.x/100))/(1+value.y/100)-1)
ATL_WAG_QUART_4 <- fredr("FRBATLWGT12MMUMHWGWD76WP", observation_start = as.Date("2015-01-01")) %>%
  merge(.,CPI_2015_GROWTH_12, by = "date") %>%
  transmute(date, value = (1+(value.x/100))/(1+value.y/100)-1)

REAL_ATLANTA_FED_WAGE_TRACKER_DISTRIB_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=ATL_WAG_QUART_1, aes(x=date,y= value,color= "First (Lowest) Wage Quartile"), size = 1.25)+ 
  geom_line(data=ATL_WAG_QUART_2, aes(x=date,y= value,color= "Second Wage Quartile"), size = 1.25)+ 
  geom_line(data=ATL_WAG_QUART_3, aes(x=date,y= value,color= "Third Wage Quartile"), size = 1.25)+ 
  geom_line(data=ATL_WAG_QUART_4, aes(x=date,y= value,color= "Fourth (Highest) Wage Quartile"), size = 1.25)+ 
  annotate("text",label = "NOTE: Data Lags Significantly Due to Being a 12M Moving Average of Annual Growth", x = as.Date("2019-04-01"), y =0.0375, color = "white", size = 5) +
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,.04),breaks = c(-0.04,-0.02,0,.02,0.04), expand = c(0,0)) +
  ggtitle("Real Wage Growth is Fastest at the Bottom") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data Deflated Using CPI", subtitle = "The Median Worker is Now Seeing Real Wage Increases, and the Wage Distribution has Compressed") +
  theme_apricitas + theme(legend.position = c(.3,.225)) +
  scale_color_manual(name= "Median Real Wage Growth by Wage Quartile\n12MMA of Median 12M Wage Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("First (Lowest) Wage Quartile","Second Wage Quartile","Third Wage Quartile","Fourth (Highest) Wage Quartile")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.04-(.3*.08), ymax = -0.04) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_ATLANTA_FED_WAGE_TRACKER_DISTRIB_Graph, "Real Atlanta Fed Wage Tracker Distrib Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ECIWAG <- fredr("ECIWAG", observation_start = as.Date("2002-01-01"), units = "pc1")
PCEPI <- fredr("PCEPI", observation_start = as.Date("2002-01-01"), units = "pc1")
PCEPILFE <- fredr("PCEPILFE", observation_start = as.Date("2002-01-01"), units = "pc1")

ECI_VS_PCEPI_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=PCEPILFE, aes(x=date,y= value/100,color= "Core PCE Inflation (i.e. Excluding Food and Energy)"), size = 1.25) + 
  geom_line(data=PCEPI, aes(x=date,y= value/100,color= "Headline PCE Inflation"), size = 1.25)+ 
  geom_line(data=ECIWAG, aes(x=date,y= value/100,color= "Employment Cost Index: Wages & Salaries: Private Industry Workers"), size = 2.25) + 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-0.025,.075),breaks = c(-0.025,0,.025,0.05,0.075), expand = c(0,0)) +
  ggtitle("Price Volatility Exceeds Wage Volatility") +
  labs(caption = "Graph created by @JosephPolitano using BEA and BLS", subtitle = "Inflation, Especially in Food/Energy, is Volatile and Thus Drives Short-Run Real Wage Dynamics") +
  theme_apricitas + theme(legend.position = c(.45,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Employment Cost Index: Wages & Salaries: Private Industry Workers","Headline PCE Inflation","Core PCE Inflation (i.e. Excluding Food and Energy)"), guide=guide_legend(override.aes=list(lwd = c(2.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = -0.025-(.3*.1), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECI_VS_PCEPI_Graph, "ECI vs PCEPI Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

INF_DEMO_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/1ef794ec727fa2b5f851ef3dbd889eb7f35660b8/Real%20Wages/Excess_Inflation_Demo.csv") %>%
  mutate(Date = as.Date(Date))

INF_DEMO_DATA_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=INF_DEMO_DATA, aes(x=Date,y= White/100,color= "White"), size = 1.25) + 
  geom_line(data=INF_DEMO_DATA, aes(x=Date,y= Black/100,color= "Black"), size = 1.25)+ 
  geom_line(data=INF_DEMO_DATA, aes(x=Date,y= Hispanic/100,color= "Hispanic"), size = 1.25) + 
  geom_line(data=INF_DEMO_DATA, aes(x=Date,y= AAPI/100,color= "AAPI"), size = 1.25) + 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.015,.02),breaks = c(-0.01,0,0.01,0.02), expand = c(0,0)) +
  ggtitle("Excess CPI Inflation By Demographic") +
  labs(caption = "Graph created by @JosephPolitano using NY Fed Equitable Growth Indicators", subtitle = "Inflation Has Been Higher for Black/Hispanic Households Since the Start of COVID") +
  theme_apricitas + theme(legend.position = c(.4,.8)) +
  scale_color_manual(name= "1-Year Demographic Inflation Rates, Percent Above/Below CPI\nNY Fed's Equitable Growth Indicators",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("White","Black","AAPI","Hispanic")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.015-(.3*.035), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INF_DEMO_DATA_Graph, "Inf Demo Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


HOU_1_20 <- bls_api("CXUHOUSINGLB0102M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 1)
HOU_2_20 <- bls_api("CXUHOUSINGLB0103M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 2)
HOU_3_20 <- bls_api("CXUHOUSINGLB0104M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 3)
HOU_4_20 <- bls_api("CXUHOUSINGLB0105M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 4)
HOU_5_20 <- bls_api("CXUHOUSINGLB0106M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 5)

HOU_RBIND <- rbind(HOU_1_20,HOU_2_20,HOU_3_20,HOU_4_20,HOU_5_20) %>%
  mutate(category = "Housing")

FOOD_1_20 <- bls_api("CXUFOODTOTLLB0102M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 1)
FOOD_2_20 <- bls_api("CXUFOODTOTLLB0103M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 2)
FOOD_3_20 <- bls_api("CXUFOODTOTLLB0104M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 3)
FOOD_4_20 <- bls_api("CXUFOODTOTLLB0105M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 4)
FOOD_5_20 <- bls_api("CXUFOODTOTLLB0106M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 5)

FOOD_RBIND <- rbind(FOOD_1_20,FOOD_2_20,FOOD_3_20,FOOD_4_20,FOOD_5_20) %>%
  mutate(category = "Food")


HEALTH_1_20 <- bls_api("CXUHEALTHLB0102M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 1)
HEALTH_2_20 <- bls_api("CXUHEALTHLB0103M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 2)
HEALTH_3_20 <- bls_api("CXUHEALTHLB0104M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 3)
HEALTH_4_20 <- bls_api("CXUHEALTHLB0105M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 4)
HEALTH_5_20 <- bls_api("CXUHEALTHLB0106M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 5)

HEALTH_RBIND <- rbind(HEALTH_1_20,HEALTH_2_20,HEALTH_3_20,HEALTH_4_20,HEALTH_5_20) %>%
  mutate(category = "Healthcare")


EXPEND_1_20 <- bls_api("CXUTOTALEXPLB0102M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 1)
EXPEND_2_20 <- bls_api("CXUTOTALEXPLB0103M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 2)
EXPEND_3_20 <- bls_api("CXUTOTALEXPLB0104M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 3)
EXPEND_4_20 <- bls_api("CXUTOTALEXPLB0105M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 4)
EXPEND_5_20 <- bls_api("CXUTOTALEXPLB0106M", startyear = 2021, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  slice(1) %>%
  mutate(quintile = 5)

EXPEND_RBIND <- rbind(EXPEND_1_20,EXPEND_2_20,EXPEND_3_20,EXPEND_4_20,EXPEND_5_20) %>%
  mutate(category = "Expend")

CEX_AGG_RBIND <- rbind(HOU_RBIND,FOOD_RBIND,HEALTH_RBIND,EXPEND_RBIND) %>%
  select(quintile, category, value) %>%
  pivot_wider(names_from = category) %>%
  transmute(quintile, `Housing (Excluding Mortgage Principal)` = Housing/Expend, Food = Food/Expend, Healthcare = Healthcare/Expend) %>%
  pivot_longer(-quintile) %>%
  mutate(name = factor(name, levels = rev(c("Housing (Excluding Mortgage Principal)","Food","Healthcare"))))

CEX_AGG_RBIND_Graph <- ggplot(CEX_AGG_RBIND, aes(fill=name, x=quintile, y=value)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Quintiles of Pre-Tax Income") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,.25,.50,.75,1),expand = c(0,0)) +
  ylab("Percent of Consumer Expenditures") +
  ggtitle("Selected Expenditure Shares by Income Quintile, 2022") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Consumption Patterns Differ—And Low-Income Households Spend More on Rent/Food/Healthcare") +
  theme_apricitas + theme(legend.position = c(.40,.875), plot.title = element_text(size = 22)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = (c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"))) +
  annotation_custom(apricitas_logo_rast, xmin = 1-(.1861*(5)), xmax = 1-(0.049*(5)), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CEX_AGG_RBIND_Graph, "CEX AGG RBind Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_Income_Deciles <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Real%20Wages/CPI_QUINTILES.csv")

CPI_Income_Deciles_pct <- CPI_Income_Deciles %>%
  mutate(across(
    .cols = setdiff(names(.), c("Date", "Year", "Month")),
    .fns = ~(. - lag(., 12)) / lag(., 12) * 100
  )) %>%
  filter(Date >= as.Date("2015-01-01")) %>%
  mutate(Date = as.Date(Date))

CPI_Income_Deciles_Lin <- CPI_Income_Deciles %>%
  filter(Date >= as.Date("2019-01-01")) %>%
  mutate(Date = as.Date(Date)) %>%
  select(-Year,-Month) %>%
  mutate_if(is.numeric, ~ . / .[13] * 100)
  
CPI_Income_Quintiles_Line_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=CPI_Income_Deciles_Lin, aes(x=Date,y= (R.CPI.I1-100)/100,color= "First (Lowest) Income Quintile"), size = 1.25) + 
  geom_line(data=CPI_Income_Deciles_Lin, aes(x=Date,y= (R.CPI.I2-100)/100,color= "Second Income Quintile"), size = 1.25)+ 
  geom_line(data=CPI_Income_Deciles_Lin, aes(x=Date,y= (R.CPI.I3-100)/100,color= "Third Income Quintile"), size = 1.25)+ 
  geom_line(data=CPI_Income_Deciles_Lin, aes(x=Date,y= (R.CPI.I4-100)/100,color= "Fourth Income Quintile"), size = 1.25)+ 
  geom_line(data=CPI_Income_Deciles_Lin, aes(x=Date,y= (R.CPI.I5-100)/100,color= "Fifth (Highest) Income Quintile"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.03,.20),breaks = c(0,0.05,0.10,0.15,0.20), expand = c(0,0)) +
  ggtitle("Inflation Isn't (Quite) Evenly Distributed") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Higher Income Workers Tend to See More Inflation") +
  theme_apricitas + theme(legend.position = c(.4,.7)) +
  scale_color_manual(name= "Experimental CPI By Equivalized Household Income Quintiles\nKlick and Stockburger 2021\nCumulative Inflation Since Jan 2020",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("First (Lowest) Income Quintile","Second Income Quintile","Third Income Quintile","Fourth Income Quintile","Fifth (Highest) Income Quintile")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.03-(.3*.23), ymax = -0.03) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Income_Quintiles_Line_Graph, "CPI Income Quintiles Line Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CPI_Income_Quintiles_PCT_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=CPI_Income_Deciles_pct, aes(x=Date,y= R.CPI.I1/100,color= "First (Lowest) Income Quintile"), size = 1.25) + 
  geom_line(data=CPI_Income_Deciles_pct, aes(x=Date,y= R.CPI.I2/100,color= "Second Income Quintile"), size = 1.25)+ 
  geom_line(data=CPI_Income_Deciles_pct, aes(x=Date,y= R.CPI.I3/100,color= "Third Income Quintile"), size = 1.25)+ 
  geom_line(data=CPI_Income_Deciles_pct, aes(x=Date,y= R.CPI.I4/100,color= "Fourth Income Quintile"), size = 1.25)+ 
  geom_line(data=CPI_Income_Deciles_pct, aes(x=Date,y= R.CPI.I5/100,color= "Fifth (Highest) Income Quintile"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,.10),breaks = c(0,0.02,0.04,0.06,0.08,0.10), expand = c(0,0)) +
  ggtitle("Inflation Isn't (Quite) Evenly Distributed") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Lower Income Households Have Seen More Inflation Since the Start of 2020") +
  theme_apricitas + theme(legend.position = c(.4,.78)) +
  scale_color_manual(name= "Experimental CPI By Equivalized Household Income Quintiles\nKlick and Stockburger 2021",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("First (Lowest) Income Quintile","Second Income Quintile","Third Income Quintile","Fourth Income Quintile","Fifth (Highest) Income Quintile")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.01-(.3*.11), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Income_Quintiles_PCT_Graph, "CPI Income Quintiles Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PI_DISTRIBUTION_BULK <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Real%20Wages/Distributional_Personal_Income.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(Total = as.numeric(gsub("\\$","",(gsub("\\,","",Total))))) %>%
  mutate(across(everything(), ~gsub("%", "", .))) %>%
  mutate(across(tail(names(.), 11), as.numeric)) %>%
  setNames(c("date","categories","income","First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth")) %>%
  mutate(across(tail(names(.), 10), ~ . / 100 * .data$income)) %>%
  transmute(date = as.Date(date), categories, income = First + Second + Third + Fourth + Fifth) %>%
  pivot_wider(names_from = categories, values_from = income) %>%
  transmute(date, `Wages & Other Compensation of Employees` = `Compensation of employees`, `Net Government Transfers & Benefits` = `Government social benefits` - `Less: Contributions for government social insurance, domestic` , `Other (Taxes, Asset/Business Income, Nonprofit Transfers, etc)`= `Proprietors' income with inventory valuation`+`Rental income of households with capital consumption adjustment`+`Household income receipts on assets`+`From business (net)`+`From nonprofit institutions`-`Less: Taxes`)
  
PCEPI_ANNUAL <- fredr("PCEPI", observation_start = as.Date("2000-01-01"), frequency = "a")
HHCOUNT_ANNUAL <- fredr("TTLHH", observation_start = as.Date("2000-01-01"), frequency = "a")

PI_DISTRIBUTION_BULK <- merge(PI_DISTRIBUTION_BULK,PCEPI_ANNUAL, by = "date") %>%
  mutate_if(is.numeric, ~ .*1000000000 / (value/100)) %>%
  select(-series_id,-value,-realtime_start,-realtime_end) %>%
  merge(.,HHCOUNT_ANNUAL, by = "date") %>%
  mutate_if(is.numeric, ~ . / (value*500)) %>%
  select(-series_id,-value,-realtime_start,-realtime_end) %>%
  mutate_if(is.numeric, ~ .-.[1]) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Wages & Other Compensation of Employees","Net Government Transfers & Benefits","Other (Taxes, Asset/Business Income, Nonprofit Transfers, etc)"))))


PI_DISTRIBUTION_BULK_Graph <- ggplot(PI_DISTRIBUTION_BULK, aes(fill=name, x=date, y=value/1000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Year") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"),limits = c(-2.75,22.5), breaks = c(0,5,10,15,20),expand = c(0,0)) +
  ylab("2017 Dollars (Adjusted by PCEPI)") +
  ggtitle("Cumuluative Change in Average Real Post-tax Income\nBottom 50% of Households, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BEA data on Distribution of Personal Income by Equivalized Household Income",subtitle = "Government Spending, Not Wages, Drove Rising Living Standards for America's Low-Income Half") +
  theme_apricitas + theme(legend.position = c(.40,.875), plot.title = element_text(size = 22)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = (c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")), breaks = c(c("Wages & Other Compensation of Employees","Net Government Transfers & Benefits","Other (Taxes, Asset/Business Income, Nonprofit Transfers, etc)"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -2.75-(.3*25.25), ymax = -2.75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PI_DISTRIBUTION_BULK_Graph, "PI Distribution Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



MIDDLE_20_GROCERIES <- bls_api("CXUFOODHOMELB0104M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUFOODHOMELB0104M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

MIDDLE_20_FUEL <- bls_api("CXUGASOILLB0104M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUGASOILLB0104M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

MIDDLE_20_INCOME <- bls_api("CXUINCAFTTXLB0104M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXUINCAFTTXLB0104M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

MIDDLE_20_GROCERY_SHARE <- merge(MIDDLE_20_GROCERIES,MIDDLE_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

MIDDLE_20_FUEL_SHARE <- merge(MIDDLE_20_FUEL,MIDDLE_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

MIDDLE_20_UTILS_SHARE <- merge(MIDDLE_20_UTILS,MIDDLE_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

BOTTOM_20_GROCERIES <- bls_api("CXUFOODHOMELB0102M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXUFOODHOMELB0102M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

BOTTOM_20_FUEL <- bls_api("CXUGASOILLB0102M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXUGASOILLB0102M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

BOTTOM_20_INCOME <- bls_api("CXUINCAFTTXLB0102M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUINCAFTTXLB0102M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

BOTTOM_20_GROCERY_SHARE <- merge(BOTTOM_20_GROCERIES,BOTTOM_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

BOTTOM_20_FUEL_SHARE <- merge(BOTTOM_20_FUEL,BOTTOM_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

SHARES_BOTTOM_Graph <- ggplot() + 
  geom_line(data=BOTTOM_20_FUEL_SHARE, aes(x=date,y= value,color= "Bottom 20%: Gasoline and Other Fuels as a Share of Post-tax Income"), size = 1.25)+ 
  geom_line(data=BOTTOM_20_GROCERY_SHARE, aes(x=date,y= value,color= "Bottom 20%: Food at Home as a Share of Post-tax Income"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.42),breaks = c(0,.10,.20,.30,.40), expand = c(0,0)) +
  ggtitle("Back in Business") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Unemployment Rate is Near Historic Lows") +
  theme_apricitas + theme(legend.position = c(.6,.88)) +
  scale_color_manual(name= "Lowest 20% of Consumer Units by Pretax Income",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26420), xmax = as.Date("1950-01-01")-(0.049*26420), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SHARES_BOTTOM_Graph, "Share of Expenditures Bottom 20.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

SHARES_MIDDLE_Graph <- ggplot() +
  geom_line(data=MIDDLE_20_FUEL_SHARE, aes(x=date,y= value,color= "Gasoline, Other Fuels, and Motor Oil as a Share of Post-tax Income"), size = 1.25)+ 
  geom_line(data=MIDDLE_20_GROCERY_SHARE, aes(x=date,y= value,color= "Food at Home as a Share of Post-tax Income"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.12),breaks = c(0,.05,.10), expand = c(0,0)) +
  ggtitle("The Cost of Gas and Grocery Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Relative Spending on Groceries and Gasoline Increased Significantly in 2022") +
  theme_apricitas + theme(legend.position = c(.5,.15)) +
  scale_color_manual(name= "Middle 20% of Consumer Units by Pretax Income",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1984-01-01")-(.1861*(today()-as.Date("1984-01-01"))), xmax = as.Date("1984-01-01")-(0.049*(today()-as.Date("1984-01-01"))), ymin = 0-(.3*.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SHARES_MIDDLE_Graph, "Share of Expenditures Middle 20.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RENT_EXPEND <- bls_api("CXURNTDWELLLB1705M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXURNTDWELLLB1705M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

RENT_INCOME <- bls_api("CXUINCAFTTXLB1705M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUINCAFTTXLB1705M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

RENT_INCOME_EXPEND_SHARE <- merge(RENT_EXPEND,RENT_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

SHARES_MIDDLE_Graph <- ggplot() +
  geom_line(data=RENT_INCOME_EXPEND_SHARE, aes(x=date,y= value,color= "Rent as a Share of Post-tax Income, Renter Households"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.3),breaks = c(0,.05,.10), expand = c(0,0)) +
  ggtitle("The Cost of Gas and Grocery Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Relative Spending on Groceries and Gasoline Increased Significantly in 2022") +
  theme_apricitas + theme(legend.position = c(.5,.15)) +
  scale_color_manual(name= "Middle 20% of Consumer Units by Pretax Income",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1984-01-01")-(.1861*(today()-as.Date("1984-01-01"))), xmax = as.Date("1984-01-01")-(0.049*(today()-as.Date("1984-01-01"))), ymin = 0-(.3*.13), ymax = 0) +
  coord_cartesian(clip = "off")

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
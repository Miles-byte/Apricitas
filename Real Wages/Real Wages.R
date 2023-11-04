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
  annotate("text", label ="Average Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-05-01"), y = 102.5, color = "white")+
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

REAL_MEDIAN_USUAL_EARNINGS <- fredr("LES1252882800Q", observation_start = as.Date("2015-01-01"))
  
REAL_MEDIAN_USUAL_EARNINGS_GROWTH <- REAL_MEDIAN_USUAL_EARNINGS %>%
  mutate(growth_rate = ((value / first(value))^(1/row_number()) - 1) * 100)

REAL_MEDIAN_USUAL_EARNINGS <- REAL_MEDIAN_USUAL_EARNINGS %>%
  mutate(trend = if_else(row_number() > 20, 
                         value[20] * (1 + REAL_MEDIAN_USUAL_EARNINGS_GROWTH$growth_rate[20]/100)^(row_number()-20), 
                         value))

REAL_MEDIAN_USUAL_EARNINGS_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = REAL_MEDIAN_USUAL_EARNINGS, aes(x=date, y = value/value[20]*100, color = "Median Usual Weekly Real Earnings, Full-Time"), size = 1.25) + 
  geom_line(data = filter(REAL_MEDIAN_USUAL_EARNINGS, date >= as.Date("2019-10-01")), aes(x=date, y = trend/trend[1]*100, color = "Q1 2015-Q4 2019 Trend Growth Rate"), size = 1.25, linetype = "dashed") + 
  annotate("text", label ="Median Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-05-01"), y = 102.5, color = "white")+
  xlab("Date") +
  scale_y_continuous(limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("Median Real Wages are Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Real Hourly Compensation is Rising, Although it Remains Below the 2015-2019 Trend Line") +
  theme_apricitas + theme(legend.position = c(.30,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
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
  annotate("text", label ="Average Real Wages Spike in 2020\nWhen Low-Wage Workers are Laid Off\nand Fall When They're Rehired", x = as.Date("2018-05-01"), y = 102.5, color = "white")+
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
  annotate("text", label ="The Employment Cost Index Adjusts for Changes in Job Composition\nSo it Doesn't Spike Aritificially When Low Wage Workers are Laid Off\nBut Also Doesn't Account for Pay Changes Caused By Switching Industry/Occupation", x = as.Date("2018-05-01"), y = 104, color = "white")+
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
  transmute(date, value = value.x-value.y)

REAL_ATLANTA_FED_WAGE_TRACKER_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=REAL_ATLANTA_FED_WAGE_TRACKER, aes(x=date,y= value/100,color= "Year-on-Year Real Median Hourly Wage Growth, Atlanta Fed Wage Tracker, 3MMA"), size = 1.25)+ 
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
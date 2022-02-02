pacman::p_load(cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Wage_Growth_Quartile <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americans%20Are%20Quitting%20Their%20Jobs/wage_growth_quartile.csv")

GLI_BEA <- fredr(series_id = "W209RC1",observation_start = as.Date("2018-01-01")) #downloading "Compensation of Employees, Received" data from Fred to calculate Gross Labor Income
ECIPRIVWAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2018-01-01")) #downloading "Wages and Salaries: Private Industry Workers" data from Fred to calculate Gross Labor Income using a third method
ELEV <- fredr(series_id = "LNS12000060",observation_start = as.Date("2018-01-01"))#downloading "Employment Level - 25-54 Yrs" data from Fred to calculate Gross Labor Income using a second method
NFP <- fredr(series_id = "PAYEMS",observation_start = as.Date("2018-01-01")) #downloading "All Employees, Total Nonfarm" data from Fred to calculate Gross Labor Income using a third method
HOURS <- fredr(series_id = "AWHAETP",observation_start = as.Date("2018-01-01")) #downloading "Average Weekly Hours of All Employees, Total Private" data from Fred to calculate Gross Labor Income using a third method
WAGE <- fredr(series_id = "CES0500000003",observation_start = as.Date("2018-01-01")) #downloading "Average Hourly Earnings of All Employees, Total Private" data from Fred to calculate Gross Labor Income using a third method

GLI_CPS_NCS <- merge(ECIPRIVWAG,ELEV, by = "date") #merging ECI and EPOP data for the second GLI calculation method
GLI_CPS_NCS <- subset(GLI_CPS_NCS, select = c("date","value.x","value.y")) #cleaning up data frame
colnames(GLI_CPS_NCS) <- c("date","ECI","ELEV") #renaming columns for ease of use
GLI_BLS <- Reduce(function(x,y) merge(x,y,all=TRUE), list(NFP,HOURS,WAGE)) #merging NFP, average hours, and average wage data
GLI_BLS <- pivot_wider(GLI_BLS, names_from = series_id, values_from = value) #converting the data to a wider format for ease of use
GLI_BLS <- subset(GLI_BLS, select = c("date","AWHAETP","CES0500000003","PAYEMS")) #cleaning up data frame
colnames(GLI_BLS) <- c("date","hours","wage","NFP") #renaming columns for ease of use 

GLITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-12-01"), "months")), trend = 100*1.003274^(0:23)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 

AHE <- fredr(series_id = "CES0500000003",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading "Average Hourly Earnings of All Employees, Total Private" data from Fred to calculate Gross Labor Income using a third method
ECIPRIVWAGPCT18 <- fredr(series_id = "ECIWAG",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading "Wages and Salaries: Private Industry Workers" data from Fred to calculate Gross Labor Income using a third method

ECIPRIVWAGPCT <- fredr(series_id = "ECIWAG", units = "pc1") #downloading "Wages and Salaries: Private Industry Workers" data from Fred to calculate Gross Labor Income using a third method

ECIEXINCENTIVE <-  fredr(series_id = "CIU2020000000710I", units = "pc1") #downloading "Wages and Salaries: Private Industry Workers" data from Fred to calculate Gross Labor Income using a third method

ECIRetailTrade <- fredr(series_id = "CIS2024120000000I",observation_start = as.Date("2018-01-01"), units = "pc1") 
ECIAccomFood <- fredr(series_id = "CIS2027200000000I",observation_start = as.Date("2018-01-01"), units = "pc1") 
ECITransWare <- fredr(series_id = "CIS2014300000000I",observation_start = as.Date("2018-01-01"), units = "pc1") 

ECIProduction <- fredr(series_id = "CIU2020000510710I",observation_start = as.Date("2018-01-01"), units = "pc1") 
ECIService <- fredr(series_id = "ECISRVWAG",observation_start = as.Date("2018-01-01"), units = "pc1") 
ECIManagement <- fredr(series_id = "CIU2020000110710I",observation_start = as.Date("2018-01-01"), units = "pc1") 

Wage_Growth_Quartile <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americans%20Are%20Quitting%20Their%20Jobs/wage_growth_quartile.csv")

Wage_Growth_Quartile$date <- as.Date(Wage_Growth_Quartile$date, "%m/%d/%Y")

Wage_Growth_Quartile[2:5] <- lapply(Wage_Growth_Quartile[2:5], as.numeric)

GLI_Graph <- ggplot() +
  geom_line(data = GLI_BEA, aes(x=date, y = value/11790.9*100, color = "Nominal Gross Labor Income: BEA Method"), size = 1.25) + 
  geom_line(data = GLI_CPS_NCS, aes(x=date, y = ECI*ELEV/14138366.2*100, color = "Nominal Gross Labor Income: ECI Method"), size = 1.25) + 
  geom_line(data = GLI_BLS, aes(x=date, y = NFP*wage*hours/148450832.866*100, color = "Nominal Gross Labor Income: NFP Method"), size = 1.25) +
  geom_line(data = GLITrend, aes(x=date, y = trend, color = "Pre-Covid 4% Annual GLI Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,110), breaks = c(85,90,95,100,105,110), expand = c(0,0)) +
  ylab("Index, January 2020 = 100") +
  ggtitle("Falling Short?") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Gross Labor Income Looks on Trend, But Using ECI Reveals Possible Room for Improvement") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,1,2), lwd = c(1.25,1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 85-(.3*25), ymax = 85) +
  coord_cartesian(clip = "off")

WAGE_GROWTH_GRAPH <- ggplot() +
  geom_line(data = AHE, aes(x=date, y = value/100, color = "Average Hourly Earnings of All Employees, Total Private"), size = 1.25) + 
  geom_line(data = ECIPRIVWAGPCT18, aes(x=date, y = value/100, color = "ECI: Wages and Salaries: Private Industry Workers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.09), breaks = c(0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Index, January 2020 = 100") +
  ggtitle("What's Going On With Wages?") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Composition Bias Ruins Average Hourly Earnings as an Analytical Tool") +
  theme_apricitas + theme(legend.position = c(.30,.92),legend.text = element_text(size = 13, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 0-(.3*0.09), ymax = 0) +
  coord_cartesian(clip = "off")

WAGE_COMPENSATION_GROWTH_GRAPH <- ggplot() +
  geom_line(data = drop_na(ECIPRIVWAGPCT), aes(x=date, y = value/100, color = "ECI: Wages and Salaries: Private Industry Workers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Year-on-Year Growth, Percent") +
  ggtitle("What's Going On With Wages?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Private Sector Wage Growth is at Multi-Decade Highs") +
  theme_apricitas + theme(legend.position = c(.46,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*7305), xmax = as.Date("2002-01-01")-(0.049*7305), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

WAGE_EXINCENTIVE_COMPENSATION_GROWTH_GRAPH <- ggplot() +
  geom_line(data = drop_na(ECIPRIVWAGPCT), aes(x=date, y = value/100, color = "ECI: Wages and Salaries: Private Industry Workers"), size = 1.25) + 
  geom_line(data = drop_na(ECIEXINCENTIVE), aes(x=date, y = value/100, color = "ECI: Wages and Salaries Excluding Incentive Paid: Private Industry Workers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Year-on-Year Growth, Percent") +
  ggtitle("What's Going On With Wages?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Private Sector Wage Growth is at Multi-Decade Highs") +
  theme_apricitas + theme(legend.position = c(.46,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*7305), xmax = as.Date("2002-01-01")-(0.049*7305), ymin = 0-(.3*0.05), ymax = 0) +
  coord_cartesian(clip = "off")

INDUSTRY_GRAPH <- ggplot() +
  geom_line(data = drop_na(ECIProduction), aes(x=date, y = value/100, color = "Production Occupations"), size = 1.25) + 
  geom_line(data = drop_na(ECIAccomFood), aes(x=date, y = value/100, color = "Service Occupations"), size = 1.25) + 
  geom_line(data = drop_na(ECITransWare), aes(x=date, y = value/100, color = "Transportation and Warehousing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.10), breaks = c(0,0.02,0.04,0.06,0.08,0.1), expand = c(0,0)) +
  ylab("Year-on-Year Growth, Percent") +
  ggtitle("Essential Workers") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Wages are Skyrocketing in Front-Facing Service Sector Industries") +
  theme_apricitas + theme(legend.position = c(.40,.75)) +
  scale_color_manual(name= "ECI Wages and Salaries for Private Industry Workers",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 0-(.3*0.1), ymax = 0) +
  coord_cartesian(clip = "off")

OCCUPATION_GRAPH <- ggplot() +
  geom_line(data = drop_na(ECIProduction), aes(x=date, y = value/100, color = "Production Occupations (ex-Incentive Pay)"), size = 1.25) + 
  geom_line(data = drop_na(ECIService), aes(x=date, y = value/100, color = "Service Occupations"), size = 1.25) + 
  geom_line(data = drop_na(ECIManagement), aes(x=date, y = value/100, color = "Management, Business, and Financial Occupations (ex-Incentive Pay)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.10), breaks = c(0,0.02,0.04,0.06,0.08,0.1), expand = c(0,0)) +
  ylab("Year-on-Year Growth, Percent") +
  ggtitle("Essential Workers") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Wages are Growing Faster For Lower-Pay Occupations") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= "ECI Wages and Salaries for Private Industry Workers",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 0-(.3*0.1), ymax = 0) +
  coord_cartesian(clip = "off")

Wage_Growth_Quartile_Graph <- ggplot() + #plotting wage growth by quartile 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X1st/100,color= "1st"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X2nd/100,color= "2nd"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X3rd/100,color= "3rd"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X4th/100,color= "4th"), size = 1.25)+ 
  xlab("Date") +
  ylab("Median Wage Growth Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.02,.04,.06), limits = c(0,.07), expand = c(0,0)) +
  ggtitle("Show Me the Money!") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Wage Growth is Highest for Low-Income Workers, as is Often the Case in Tight Labor Markets") +
  theme_apricitas + theme(legend.position = c(.7,.8),legend.title=element_text(size=14)) +
  scale_color_manual(name= "Wage Growth by Quartile",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*8700), xmax = as.Date("1997-01-01")-(0.049*8700), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")


ECITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-12-01"), "months")), trend = 100*1.002466^(0:23)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 

ECI_Trend <- ggplot() +
  geom_line(data = ECIPRIVWAG, aes(x=date, y = value/1.402, color = "ECI: Wages and Salaries: Private Industry Workers"), size = 1.25) + 
  geom_line(data = ECITrend, aes(x=date, y = trend, color = "Pre-Covid 3% Annual Wage Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(92.5,110), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, January 2020 = 100") +
  ggtitle("Moving With the Trends") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Wages are Marginally Above Their Pre-COVID Trend") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 92.5-(.3*17.5), ymax = 92.5) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = ECI_Trend, "ECI Trend.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Wage_Growth_Quartile_Graph, "Wage Growth Quartile.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = OCCUPATION_GRAPH, "Occupation Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = INDUSTRY_GRAPH, "Industry Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = WAGE_EXINCENTIVE_COMPENSATION_GROWTH_GRAPH, "Wage Compensation Growth Ex Incentive.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = WAGE_GROWTH_GRAPH, "Wage Compensation Growth.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = GLI_Graph, "GLI Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

ECI <- bls_api("CIU2010000000000I", startyear = 2018, endyear = 2021, Sys.getenv("BLS_KEY"))
ECI=ECI[order(nrow(ECI):1),]
ECI$date <- seq(as.Date("2018-01-01"), as.Date("2021-11-01"), "3 months")

GLI_BEA <- fredr(series_id = "W209RC1",observation_start = as.Date("2018-01-01")) #downloading "Compensation of Employees, Received" data from Fred to calculate Gross Labor Income
ECI <- fredr(series_id = "CIU2010000000000I",observation_start = as.Date("2018-01-01")) #downloading "Employment Cost Index - Total Compensation for Private Industry workers" data from Fred to calculate Gross Labor Income using a second method
ELEV <- fredr(series_id = "LNS12000060",observation_start = as.Date("2018-01-01"))#downloading "Employment Level - 25-54 Yrs" data from Fred to calculate Gross Labor Income using a second method
NFP <- fredr(series_id = "PAYEMS",observation_start = as.Date("2018-01-01")) #downloading "All Employees, Total Nonfarm" data from Fred to calculate Gross Labor Income using a third method
HOURS <- fredr(series_id = "AWHAETP",observation_start = as.Date("2018-01-01")) #downloading "Average Weekly Hours of All Employees, Total Private" data from Fred to calculate Gross Labor Income using a third method
WAGE <- fredr(series_id = "CES0500000003",observation_start = as.Date("2018-01-01")) #downloading "Average Hourly Earnings of All Employees, Total Private" data from Fred to calculate Gross Labor Income using a third method

GLI_CPS_NCS <- merge(ECI,ELEV, by = "date") #merging ECI and EPOP data for the second GLI calculation method
GLI_CPS_NCS <- subset(GLI_CPS_NCS, select = c("date","value.x","value.y")) #cleaning up data frame
colnames(GLI_CPS_NCS) <- c("date","ECI","ELEV") #renaming columns for ease of use
GLI_BLS <- Reduce(function(x,y) merge(x,y,all=TRUE), list(NFP,HOURS,WAGE)) #merging NFP, average hours, and average wage data
GLI_BLS <- pivot_wider(GLI_BLS, names_from = series_id, values_from = value) #converting the data to a wider format for ease of use
GLI_BLS <- subset(GLI_BLS, select = c("date","AWHAETP","CES0500000003","PAYEMS")) #cleaning up data frame
colnames(GLI_BLS) <- c("date","hours","wage","NFP") #renaming columns for ease of use 

GLITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-12-01"), "months")), trend = 100*1.003274^(0:23))

Wage_Growth_Quartile <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americans%20Are%20Quitting%20Their%20Jobs/wage_growth_quartile.csv")

DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-10-01"), "months")), trend = 16622.8*1.003274^(0:21)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2021-10-01"), "months")), trend = 15328.8*1.003274^(0:21))

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

GLI_Graph <- ggplot() +
  geom_line(data = GLI_BEA, aes(x=date, y = value/11790.9*100, color = "Nominal Gross Labor Income: BEA Method"), size = 1.25) + 
  geom_line(data = GLI_CPS_NCS, aes(x=date, y = ECI*ELEV/14138366.2*100, color = "Nominal Gross Labor Income: ECI Method"), size = 1.25) + 
  geom_line(data = GLI_BLS, aes(x=date, y = NFP*wage*hours/148450832.866*100, color = "Nominal Gross Labor Income: NFP Method"), size = 1.25) +
  geom_line(data = GLITrend, aes(x=date, y = trend, color = "Pre-Covid 4% Annual GLI Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,115), breaks = c(85,90,95,100,105,110,115), expand = c(0,0)) +
  ylab("Index, January 2020 = 100") +
  ggtitle("Falling Short?") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Gross Labor Income Looks on Trend, But Using ECI Reveals Possible Room for Improvement") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,1,2), lwd = c(1.25,1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 85-(.3*25), ymax = 85) +
  coord_cartesian(clip = "off")

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Personal Income and Outlays are on Trend, But Consumers Have Significant Excess Savings") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

ECI_Graph <- ggplot() + #plotting ECI growth
  geom_line(data=ECI, aes(x=date,y= value/100,color= "Employment Cost Index"), size = 1.25)+ 
  xlab("Date") +
  ylab("Annual Percentage Change, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.01,0.04), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2001-01-01"),as.Date("2021-04-01"))) +
  ggtitle("Nominal Compensation Growth is at Pre-Great Recession Levels") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.75,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*7295), xmax = as.Date("2001-01-01")-(0.049*7295), ymin = 0.01-(.3*0.03), ymax = 0.01) +
  coord_cartesian(clip = "off")

Wage_Growth_Quartile_Graph <- ggplot() + #plotting wage growth by quartile 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X1st/100,color= "1st"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X2nd/100,color= "2nd"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X3rd/100,color= "3rd"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X4th/100,color= "4th"), size = 1.25)+ 
  xlab("Date") +
  ylab("Wage Growth Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.02,.04,.06), limits = c(0,.07), expand = c(0,0)) +
  ggtitle("Show Me the Money!") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Wage Growth is Highest for Low-Income Workers, as is Often the Case in Tight Labor Markets") +
  theme_apricitas + theme(legend.position = c(.7,.8),legend.title=element_text(size=14)) +
  scale_color_manual(name= "Wage Growth by Quartile",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*8700), xmax = as.Date("1997-01-01")-(0.049*8700), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = TRNSPT_Graph, "Transport.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
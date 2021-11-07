pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

ECISERV <- fredr(series_id = "CIS201S000000000I",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI services
ECIGOOD <- fredr(series_id = "CIU201G000000000I",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI goods
PCESERV <- fredr(series_id = "DSERRG3M086SBEA",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI goods
PCEGOOD <- fredr(series_id = "DGDSRG3M086SBEA",observation_start = as.Date("2018-01-01"), units = "pc1") #downloading ECI goods

#add obersvation_ends before sending data for application

Wage_Price_Merge <- do.call("rbind", list(ECISERV,ECIGOOD,PCESERV,PCEGOOD))
Wage_Price_Merge$series_id <- gsub("CIS201S000000000I","Services Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("CIU201G000000000I","Goods Compensation (ECI)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DSERRG3M086SBEA","Services Prices (PCE)",Wage_Price_Merge$series_id)
Wage_Price_Merge$series_id <- gsub("DGDSRG3M086SBEA","Goods Prices (PCE)",Wage_Price_Merge$series_id)

FEDDEFICIT_SURPLUS <- fredr(series_id = "MTSDS133FMS",observation_start = as.Date("2017-09-01")) #downloading ECI goods
FEDDEFICIT_SURPLUS$value <- c(NA,NA,rollmean(FEDDEFICIT_SURPLUS$value, k = 3))
FEDDEFICIT_SURPLUS <- FEDDEFICIT_SURPLUS[-1:-2,]

GDP <- fredr(series_id = "GDP",observation_start = as.Date("2018-01-01")) #downloading ECI goods
GDPTrend <- data.frame(date = c(as.Date("2019-10-01"),as.Date("2021-07-01")),trend = c(21694.46,23267.40)) #creating 4% NGDP growth trend

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

GLITrend <- data.frame(date = c(as.Date("2020-01-01"),as.Date("2021-09-01")),trend = c(100,107.25))

DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposal Income
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays

DSPITrend <- data.frame(date = c(as.Date("2020-01-01"),as.Date("2021-09-01")),trend = c(16622.8,17828.02))
POUTTrend <- data.frame(date = c(as.Date("2020-01-01"),as.Date("2021-09-01")),trend = c(15328.8,16440.20))


theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Wage_Price_Graph <- ggplot() +
  geom_line(data = Wage_Price_Merge, aes(x=date, y = value/100, color = series_id, alpha = series_id), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-.025,0.065), breaks = c(-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Spiral? Not so Fast!") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "Price Growth is in Goods, but Wage Growth is in Services") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  scale_alpha_manual(name= NULL,values = c(1,.5,1,.5)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = -.025-(.3*.09), ymax = -0.025) +
  coord_cartesian(clip = "off")

Feddeficit_Surplus_Graph <- ggplot() +
  geom_line(data = FEDDEFICIT_SURPLUS, aes(x=date, y = -value/1000, color = "Monthly Federal Budget Deficit, 3 Month Rolling Average"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(0,700), breaks = c(0,200,400,600), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Shrinking Stimulus") +
  labs(caption = "Graph created by @JosephPolitano using Treasury data",subtitle = "The Federal Budget Deficit is Elevated, but Nowhere Near Pandemic Highs") +
  theme_apricitas + theme(legend.position = c(.30,.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-11-01")-(.1861*1461), xmax = as.Date("2017-11-01")-(0.049*1461), ymin = 0-(.3*700), ymax = 0) +
  coord_cartesian(clip = "off")

GDP_Graph <- ggplot() +
  geom_line(data = GDP, aes(x=date, y = value/1000, color = "Nominal Gross Domestic Product"), size = 1.25) + 
  geom_line(data = GDPTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Annual NGDP Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"),limits = c(19,24), breaks = c(19,20,21,22,23,24), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("On Trend") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Nominal Gross Domestic Product is Returning to its Pre-COVID Trend") +
  theme_apricitas + theme(legend.position = c(.30,.7)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 19-(.3*5), ymax = 19) +
  coord_cartesian(clip = "off")

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
  theme_apricitas + theme(legend.position = c(.30,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,1,2), lwd = c(1.25,1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 85-(.3*25), ymax = 85) +
  coord_cartesian(clip = "off")

Personal_Income_Graph <- ggplot() +
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "Personal Income and Outlays are on Trend, But Consumers Have Significant Excess Savings") +
  theme_apricitas + theme(legend.position = c(.30,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1400), xmax = as.Date("2018-01-01")-(0.049*1400), ymin = 85-(.3*25), ymax = 85) +
  coord_cartesian(clip = "off")




ggsave(dpi = "retina",plot = Wage_Price_Graph, "Spiral.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Feddeficit_Surplus_Graph, "Fed Deficit Surplus.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = GDP_Graph, "GDP_Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = GLI_Graph, "GLI_Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
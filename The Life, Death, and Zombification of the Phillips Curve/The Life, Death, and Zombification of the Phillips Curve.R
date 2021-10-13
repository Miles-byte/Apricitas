pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Be sure to set your FRED API key in the environment if you have not already. If not, insert it as code before importing data
UNRATE <- fredr(series_id = c("UNRATE"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) #downloading unemployment data from FRED
U6RATE <- fredr(series_id = c("U6RATE"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) #downloading U6 unemployment rate data
EPOP <- fredr(series_id = c("LNS12300060"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) #downloading prime age employment-population ratio data
PCE <- fredr(series_id = c("PCEPI"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading PCE Inflation data
PCELFE <- fredr(series_id = c("PCEPILFE"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading PCE Inflation less food and energy data
PCETRIM <- fredr(series_id = c("PCETRIM12M159SFRBDAL"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) #downloading trimmed mean PCE Inflation data
ECIALLCIV <- fredr(series_id = c("ECIALLCIV"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading Employment Cost Index - Total Compensation: All Civilian data
ECIPRIVWS <- fredr(series_id = c("ECIWAG"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading Employment Cost Index data
ULC <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading Unit Labor Cost Data

#Pulling Unemployment Rate, Inflation, Prime Age Employment-Population Ratio, Total Employment, and Total Compensation for currency sovereigns Japan, the UK, and Canada
#Unemployment Rate
JPNUNRATE <- fredr(series_id = c("LRHUTTTTJPM156S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) 
UKUNRATE <- fredr(series_id = c("LRHUTTTTGBM156S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) 
CANUNRATE <- fredr(series_id = c("LRHUTTTTCAM156S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) 
#Inflation
JPNCPI <- fredr(series_id = c("CPALTT01JPM661S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
UKCPI <- fredr(series_id = c("GBRCPIALLMINMEI"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
CANCPI <- fredr(series_id = c("CANCPALTT01IXOBSAM"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"),units = "pc1") 
#Core Inflation
#Prime Age Employment-Population Ratio
JPNEPOP <- fredr(series_id = c("LREM25TTJPM156S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) 
UKEPOP <- fredr(series_id = c("LREM25TTGBQ156S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) 
CANEPOP <- fredr(series_id = c("LREM25TTCAM156S"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01")) 
#Total Employment - these start in 199 because annual growth rates starting in 2000 have to be calculated later
#JPNEMP <- fredr(series_id = c("LFEMTTTTJPM647S"), observation_start = as.Date("1999-01-01"), observation_end = as.Date("2020-01-01")) 
#UKEMP <- fredr(series_id = c("LFEMTTTTGBQ647S"), observation_start = as.Date("1999-01-01"), observation_end = as.Date("2020-01-01")) 
#CANEMP <- fredr(series_id = c("LFEMTTTTCAM647S"), observation_start = as.Date("1999-01-01"), observation_end = as.Date("2020-01-01")) 
#Total Compensation - these start in 199 because annual growth rates starting in 2000 have to be calculated later
#JPNCOMP <- fredr(series_id = c("JPNCOMPQDSNAQ"), observation_start = as.Date("1999-01-01"), observation_end = as.Date("2020-01-01")) 
#UKCOMP <- fredr(series_id = c("GBRCOMPQDSNAQ"), observation_start = as.Date("1999-01-01"), observation_end = as.Date("2020-01-01")) 
#CANCOMP <- fredr(series_id = c("CANCOMPQDSNAQ"), observation_start = as.Date("1999-01-01"), observation_end = as.Date("2020-01-01")) 


#Pulling Unemployment Rate, Inflation, and Prime Age Employment-Population Ratio for dollarized economies Hong Kong, Ecuador, and Saudi Arabia
HKUNRATE <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
ECUUNRATE <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
SAUDUNRATE <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
HKCPI <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
ECUCPI <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
SAUDCPI <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
HKEPOP <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
ECUEPOP <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 
SAUDEPOP <- fredr(series_id = c("ULCNFB"), observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") 



EPOP_ECIALLCIV <- merge(EPOP, ECIALLCIV, by = "date") #merging EPOP and ECI data for the ECI/EPOP and PCE/EPOP chart
EPOP_PCE <- merge(EPOP, PCE, by = "date") #merging EPOP and PCE data for the ECI/EPOP and PCE/EPOP chart

summary(lm(value.x~value.y, data = EPOP_ECIALLCIV))#creating summary stats for the regression of Prime Age Employment-Population Ratio and ECI Total Compensation: All Civilians
summary(lm(value.x~value.y, data = EPOP_PCE))#creating summary stats for the regression of Prime Age Employment-Population Ratio and PCE Inflation

EPOP_ECI_PCE_GRAPH <- ggplot() + #plotting EPOP/ECI All Civilian Regression and EPOP/PCE Regression
  geom_point(data=EPOP_ECIALLCIV, aes(x=value.x/100,y=value.y/100, color= "ECI: Total Compensation: All Civilian"), size = 1.25)+
  geom_point(data=EPOP_PCE, aes(x=value.x/100,y=value.y/100, color= "Personal Consumption Expenditures Price Index"), size = 1.25)+
  stat_smooth(data=EPOP_ECIALLCIV,method = "lm", aes(x=value.x/100,y=value.y/100, color= "ECI: Total Compensation: All Civilian"), size = 1.25) +
  stat_smooth(data=EPOP_PCE,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Personal Consumption Expenditures Price Index"), size = 1.25) +
  ylab("ECI/PCE, Percent Change from Year Ago, %") +
  xlab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.017,0.042), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.745,.82)) +
  ggtitle("The Wage Curve vs the Price Curve, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Prime-Age Employment Correlates with Nominal Wage Growth More Than Inflation") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  annotate("text", label = "Coefficient: 0.765", x = 0.812, y = 0.019, color = "#00A99D") +
  annotate("text", label = "R-squared: 0.151", x = 0.812, y = 0.016, color = "#00A99D") +
  annotate("text", label = "Coefficient: 2.023", x = 0.812, y = 0.04, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.657", x = 0.812, y = 0.037, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = .745-(.1861*0.075), xmax = .745-(0.049*0.075), ymin = -0.017-(.3*0.059), ymax = -0.017) +
  coord_cartesian(clip = "off")

EPOP_ECIPRIVWS <- merge(EPOP, ECIPRIVWS, by = "date") #merging EPOP and ECI Private Wages and Salaries data for the ECI Wages & Salaries/EPOP and Trimmed Mean PCE/EPOP chart
EPOP_PCETRIM <- merge(EPOP, PCETRIM, by = "date") #merging EPOP and Trimmed Mean PCE data for the ECI Wages & Salaries/EPOP and Trimmed Mean PCE/EPOP chart

summary(lm(value.x~value.y, data = EPOP_ECIPRIVWS)) #creating summary stats for the regression of Prime Age Employment-Population Ratio and ECI Private Wages and Salaries
summary(lm(value.x~value.y, data = EPOP_PCETRIM)) #creating summary stats for the regression of Prime Age Employment-Population Ratio and Trimmed Mean PCE Inflation

EPOP_ECIPRIVWS_PCETRIM_GRAPH <- ggplot() + #plotting EPOP/ECI Private Wages and Salaries and EPOP/Trimmed Mean PCE Regression
  geom_point(data=EPOP_ECIPRIVWS, aes(x=value.x/100,y=value.y/100, color= "ECI: Wages and Salaries: Private Industry Workers"), size = 1.25)+
  geom_point(data=EPOP_PCETRIM, aes(x=value.x/100,y=value.y/100, color= "Trimmed Mean Personal Consumption Expenditures Price Index"), size = 1.25)+
  stat_smooth(data=EPOP_ECIPRIVWS,method = "lm", aes(x=value.x/100,y=value.y/100, color= "ECI: Wages and Salaries: Private Industry Workers"), size = 1.25) +
  stat_smooth(data=EPOP_PCETRIM,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Trimmed Mean Personal Consumption Expenditures Price Index"), size = 1.25) +
  ylab("ECI/PCE, Percent Change from Year Ago, %") +
  xlab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.017,0.042), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.745,.82)) +
  ggtitle("The Wage Curve vs the Price Curve, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Prime-Age Employment Correlates More with Private Sector Wages and Trimmed Mean Inflation") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  annotate("text", label = "Coefficient: 3.260", x = 0.812, y = 0.019, color = "#00A99D") +
  annotate("text", label = "R-squared: 0.564", x = 0.812, y = 0.016, color = "#00A99D") +
  annotate("text", label = "Coefficient: 2.652", x = 0.812, y = 0.04, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.834", x = 0.812, y = 0.037, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = .745-(.1861*0.075), xmax = .745-(0.049*0.075), ymin = -0.017-(.3*0.059), ymax = -0.017) +
  coord_cartesian(clip = "off")

UNRATE_PCE <- merge(UNRATE, PCE, by = "date") #merging Unemployment Rate and PCE Inflation data for the Unemployment/PCE chart

summary(lm(value.x~value.y, data = UNRATE_PCE)) #creating summary stats for the regression of Unemployment Rate and PCE Inflation

PHILLIPS_CURVE_GRAPH <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  geom_point(data=UNRATE_PCE, aes(x=value.x/100,y=value.y/100, color= "Personal Consumption Expenditures Price Index"), size = 1.25)+
  stat_smooth(data=UNRATE_PCE,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Personal Consumption Expenditures Price Index"), size = 1.25) +
  ylab("PCE, Percent Change from Year Ago, %") +
  xlab("Unemployment Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.017,0.042), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.033,.105)) +
  ggtitle("The Flat Phillips Curve, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "The Correlation Between Unemployment and Inflation Has Broken Down") +
  theme_apricitas + theme(legend.position = c(.30,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  annotate("text", label = "Coefficient: -0.496", x = 0.085, y = 0.01, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.070", x = 0.085, y = 0.007, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = .033-(.1861*0.072), xmax = .033-(0.049*0.072), ymin = -0.017-(.3*0.059), ymax = -0.017) +
  coord_cartesian(clip = "off")

JPN_UNRATE_CPI <- merge(JPNUNRATE, JPNCPI, by = "date") #merging Japanese Unemployment Rate and Inflation data for the Phillips Curve Ex-US Chart
UK_UNRATE_CPI <- merge(UKUNRATE, UKCPI, by = "date") #merging UK Unemployment Rate and Inflation data for the Phillips Curve Ex-US Chart
CAN_UNRATE_CPI <- merge(CANUNRATE, CANCPI, by = "date") #merging Canadian Unemployment Rate and Inflation data for the Phillips Curve Ex-US Chart

summary(lm(value.x~value.y, data = JPN_UNRATE_CPI)) #creating summary stats for the regression of Unemployment Rate and CPI Inflation
summary(lm(value.x~value.y, data = UK_UNRATE_CPI)) #creating summary stats for the regression of Unemployment Rate and CPI Inflation
summary(lm(value.x~value.y, data = CAN_UNRATE_CPI)) #creating summary stats for the regression of Unemployment Rate and CPI Inflation

XUS_PHILLIPS_CURVE_GRAPH <- ggplot() + #plotting Unemployment Inflation Phillips curve for non-US countries
  geom_point(data=JPN_UNRATE_CPI, aes(x=value.x/100,y=value.y/100, color= "Japan"), size = 1.25)+
  stat_smooth(data=JPN_UNRATE_CPI,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Japan"), size = 1.25) +
  geom_point(data=UK_UNRATE_CPI, aes(x=value.x/100,y=value.y/100, color= "United Kingdom"), size = 1.25)+
  stat_smooth(data=UK_UNRATE_CPI,method = "lm", aes(x=value.x/100,y=value.y/100, color= "United Kingdom"), size = 1.25) +
  geom_point(data=CAN_UNRATE_CPI, aes(x=value.x/100,y=value.y/100, color= "Canada"), size = 1.25)+
  stat_smooth(data=CAN_UNRATE_CPI,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Canada"), size = 1.25) +
  ylab("CPI, Percent Change from Year Ago, %") +
  xlab("Unemployment Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,0.05),breaks = c(-0.02,0.00,0.02,0.04), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.02,.09)) +
  ggtitle("The Phillips Curve Abroad, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using OECD data", subtitle = "The Correlation Between Unemployment and Inflation Varies Dramatically by Country") +
  theme_apricitas + theme(legend.position = c(.12,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D"))+
  annotate("text", label = "Coefficient: -0.204", x = 0.075, y = 0.00, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.060", x = 0.075, y = -0.003, color = "#FFE98F") +
  annotate("text", label = "Coefficient: 0.610", x = 0.045, y = 0.04, color = "#00A99D") +
  annotate("text", label = "R-squared: 0.158", x = 0.045, y = 0.037, color = "#00A99D") +
  annotate("text", label = "Coefficient: -0.500", x = 0.035, y = -0.012, color = "#EE6055") +
  annotate("text", label = "R-squared: 0.309", x = 0.035, y = -0.015, color = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = .02-(.1861*0.07), xmax = .02-(0.049*0.07), ymin = -0.025-(.3*0.075), ymax = -0.025) +
  coord_cartesian(clip = "off")

JPN_EPOP_CPI <- merge(JPNEPOP, JPNCPI, by = "date") #merging Japanese EPOP and Inflation data for the EPOP Phillips Curve Ex-US Chart
UK_EPOP_CPI <- merge(UKEPOP, UKCPI, by = "date") #merging UK EPOP and Inflation data for the EPOP Phillips Curve Ex-US Chart
CAN_EPOP_CPI <- merge(CANEPOP, CANCPI, by = "date") #merging Canadian EPOP and Inflation data for the EPOP Phillips Curve Ex-US Chart

summary(lm(value.x~value.y, data = JPN_EPOP_CPI)) #creating summary stats for the regression of Unemployment Rate and CPI Inflation
summary(lm(value.x~value.y, data = UK_EPOP_CPI)) #creating summary stats for the regression of Unemployment Rate and CPI Inflation
summary(lm(value.x~value.y, data = CAN_EPOP_CPI)) #creating summary stats for the regression of Unemployment Rate and CPI Inflation


XUS_EPOP_PHILLIPS_CURVE_GRAPH <- ggplot() + #plotting Prime Age Epop-Inflation Phillips curve for non-US countries
  geom_point(data=JPN_EPOP_CPI, aes(x=value.x/100,y=value.y/100, color= "Japan"), size = 1.25)+
  stat_smooth(data=JPN_EPOP_CPI,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Japan"), size = 1.25) +
  geom_point(data=UK_EPOP_CPI, aes(x=value.x/100,y=value.y/100, color= "United Kingdom"), size = 1.25)+
  stat_smooth(data=UK_EPOP_CPI,method = "lm", aes(x=value.x/100,y=value.y/100, color= "United Kingdom"), size = 1.25) +
  geom_point(data=CAN_EPOP_CPI, aes(x=value.x/100,y=value.y/100, color= "Canada"), size = 1.25)+
  stat_smooth(data=CAN_EPOP_CPI,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Canada"), size = 1.25) +
  ylab("CPI, Percent Change from Year Ago, %") +
  xlab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,0.05),breaks = c(-0.02,0.00,0.02,0.04), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.78,.86)) +
  ggtitle("The Prime Age Employment Phillips Curve Abroad, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using OECD data", subtitle = "The Correlation Between Prime Age Employment and Inflation is Even Weaker") +
  theme_apricitas + theme(legend.position = c(.12,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D"))+
  annotate("text", label = "Relationship Not Significant", x = 0.83, y = 0.04, color = "#FFE98F") +
  annotate("text", label = "Relationship Not Significant", x = 0.84, y = 0.032, color = "#00A99D") +
  annotate("text", label = "Coefficient: 1.061", x = 0.82, y = -0.012, color = "#EE6055") +
  annotate("text", label = "R-squared: 0.221", x = 0.82, y = -0.015, color = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = .78-(.1861*0.08), xmax = .78-(0.049*0.08), ymin = -0.025-(.3*0.075), ymax = -0.025) +
  coord_cartesian(clip = "off")



#JPN_COMP_EMP <- merge(JPNCOMP, JPNEMP, by = "date") #merging Japanese compensation and employment data to calculate compensation per employee
#UK_COMP_EMP <- merge(UKCOMP, UKEMP, by = "date") #merging UK compensation and employment data to calculate compensation per employee
#CAN_COMP_EMP <- merge(CANCOMP, CANEMP, by = "date") #merging Canadian compensation and employment data to calculate compensation per employee

#JPN_COMP_EMP <- mutate(JPN_COMP_EMP, COMP_PER_EMP_GROWTH = (value.x/value.y) / lag(value.x/value.y, 4) - 1) #mutating a Japanese annualized growth rate column for compensation/employee
#UK_COMP_EMP <- mutate(UK_COMP_EMP, COMP_PER_EMP_GROWTH = (value.x/value.y) / lag(value.x/value.y, 4) - 1) #mutating a Japanese annualized growth rate column for compensation/employee
#CAN_COMP_EMP <- mutate(CAN_COMP_EMP, COMP_PER_EMP_GROWTH = (value.x/value.y) / lag(value.x/value.y, 4) - 1) #mutating a Japanese annualized growth rate column for compensation/employee

#JPN_EPOP_COMP_EMP <- merge(JPN_COMP_EMP, JPNEPOP, by = "date") #merging Japanese compensation/employee and prime age employment-population ratio data for EPOP/compensation per employee chart
#UK_EPOP_COMP_EMP <- merge(UK_COMP_EMP, UKEPOP, by = "date") #merging UK compensation/employee and prime age employment-population ratio data for EPOP/compensation per employee chart
#CAN_EPOP_COMP_EMP <- merge(CAN_COMP_EMP, CANEPOP, by = "date") #merging Canadian compensation/employee and prime age employment-population ratio data for EPOP/compensation per employee chart 

#JPN_EPOP_COMP_EMP <- data.frame(JPN_EPOP_COMP_EMP$date,JPN_EPOP_COMP_EMP$COMP_PER_EMP_GROWTH,JPN_EPOP_COMP_EMP$value) #condensing Japanese Compensation/employee and prime age employment-population ratio to one data frame
#UK_EPOP_COMP_EMP <- data.frame(UK_EPOP_COMP_EMP$date,UK_EPOP_COMP_EMP$COMP_PER_EMP_GROWTH,UK_EPOP_COMP_EMP$value) #condensing UK Compensation/employee and prime age employment-population ratio to one data frame
#CAN_EPOP_COMP_EMP <- data.frame(CAN_EPOP_COMP_EMP$date,CAN_EPOP_COMP_EMP$COMP_PER_EMP_GROWTH,CAN_EPOP_COMP_EMP$value) #condensing Canadian Compensation/employee and prime age employment-population ratio to one data frame

#colnames(JPN_EPOP_COMP_EMP) <- c("date","COMP_PER_EMP","EPOP") #renaming columns in Japanese compensation/employee and prime age employment-population ratio data frame for simplicity
#colnames(UK_EPOP_COMP_EMP) <- c("date","COMP_PER_EMP","EPOP") #renaming columns in UK compensation/employee and prime age employment-population ratio data frame for simplicity
#colnames(CAN_EPOP_COMP_EMP) <- c("date","COMP_PER_EMP","EPOP") #renaming columns in Canadian compensation/employee and prime age employment-population ratio data frame for simplicity

#XUS_EPOP_COMP_EMP_GRAPH <- ggplot() + #plotting Prime Age Epop-Compensation Per Employee Phillips curve for non-US countries
  #geom_point(data=JPN_EPOP_COMP_EMP, aes(y=COMP_PER_EMP,x=EPOP/100, color= "Japan"), size = 1.25)+
  #stat_smooth(data=JPN_EPOP_COMP_EMP,method = "lm", aes(y=COMP_PER_EMP,x=EPOP/100, color= "Japan"), size = 1.25) +
  #geom_point(data=UK_EPOP_COMP_EMP, aes(y=COMP_PER_EMP,x=EPOP/100, color= "United Kingdom"), size = 1.25)+
  #stat_smooth(data=UK_EPOP_COMP_EMP,method = "lm", aes(y=COMP_PER_EMP,x=EPOP/100, color= "United Kingdom"), size = 1.25) +
  #geom_point(data=CAN_EPOP_COMP_EMP, aes(y=COMP_PER_EMP,x=EPOP/100, color= "Canada"), size = 1.25)+
  #stat_smooth(data=CAN_EPOP_COMP_EMP,method = "lm", aes(y=COMP_PER_EMP,x=EPOP/100, color= "Canada"), size = 1.25) +
  #ylab("CPI, Percent Change from Year Ago, %") +
  #xlab("Prime Age (25-54) Employment-Population Ratio, %") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,0.05),breaks = c(-0.02,0.00,0.02,0.04), expand = c(0,0)) +
  #scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.78,.86)) +
  #ggtitle("The Prime Age Employment Wage Phillips Curve Abroad, 2000-2020") +
  #labs(caption = "Graph created by @JosephPolitano using OECD data", subtitle = "The Correlation Between Prime Age Employment and Inflation Varies is Even Weaker") +
  #theme_apricitas + theme(legend.position = c(.12,.80)) +
  #scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D"))#+
  #annotate("text", label = "Relationship Not Significant", x = 0.83, y = 0.04, color = "#FFE98F") +
  #annotate("text", label = "Relationship Not Significant", x = 0.84, y = 0.032, color = "#00A99D") +
  #annotate("text", label = "Coefficient: 1.061", x = 0.82, y = -0.012, color = "#EE6055") +
  #annotate("text", label = "R-squared: 0.221", x = 0.82, y = -0.015, color = "#EE6055") +
  #annotation_custom(apricitas_logo_rast, xmin = .78-(.1861*0.08), xmax = .78-(0.049*0.08), ymin = -0.025-(.3*0.075), ymax = -0.025) +
  #coord_cartesian(clip = "off")


#PCE Autocorrelation Graph
PCE <- fredr(series_id = c("PCEPI"), observation_start = as.Date("1960-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading new PCE Inflation data with expanded time series

PCEACF <- acf(PCE$value,lag.max = 125,plot = F)
PCEPACF <- pacf(PCE$value,lag.max = 121,plot = F)
MELT_PCEACFS<- data.frame(lag = PCEACF$lag,PCEACF=PCEACF$acf)
MELT_PCEACFS<- melt(as.data.table(MELT_PCEACFS),id="lag")


PCE_AUTOCORRELATION_GRAPH <- ggplot(data= MELT_PCEACFS, aes(x = lag, y = value, fill = variable)) + 
  geom_area(position = "dodge", color = NA) +
  theme_apricitas + theme(legend.position = c(.60,.70)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,125), expand = c(0,0)) +
  ylab("Autocorrelation") +
  xlab("Lag, Months") +
  ggtitle("PCE Inflation Autocorrelation, 1960-2020") +
  scale_fill_manual(name = NULL, values = c("#FFE98F","#00A99D"), labels = c("PCE Inflation Autocorrelation","PCE Inflation Partial Autocorrelation"))+
  scale_color_manual(values = c("#FFE98F","#00A99D")) +
  annotate("hline", yintercept = 0.140, y = 0.140, linetype = "dashed", color = "white", size = 1.25) +
  annotate("text", label = "5% Significance", y = 0.170, x = 115, color = "white") +
  annotation_custom(apricitas_logo_rast, xmin = 5-(.1861*125), xmax = 7-(0.049*125), ymin = .11-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")


25-54 EPOP/Inflation (trimmed mean inflation?) and EPOP Wage Growth for other countries 
UNRATE/INFLATION FOR OTHER COUNTRIES TOO
SHOW AUTOCORRELATION
Endogenous to mon pol
DO REAL WAGE CURVE VS EMPLOYMENT GRAPH
REDO WAGE CURVE GRAPH WITH PRIVATE ECI AND TRIMMED MEAN PCE
ANIMATED GRAPH ANCHORED WITH TIME FIXED EFFECT OR LAGGED PCE INFLATION? WILL PROBABLY HAVE TO ALLOW MOVEMENT ON THE Y AXIS
LOOK AT OTHER COUNTRIES TOO

ggsave(dpi = "retina",plot = EPOP_ECI_PCE_GRAPH, "EPOP ECI PCE Graph.png", type = "cairo-png") #saving ECI All Civilian and PCE Inflation Graph
ggsave(dpi = "retina",plot = EPOP_ECIPRIVWS_PCETRIM_GRAPH, "EPOP Private Sector ECI Trimmed Mean PCE Graph.png", type = "cairo-png")  #saving ECI Private Wages and Salaries and Trimmed Mean PCE Inflation
ggsave(dpi = "retina",plot = PHILLIPS_CURVE_GRAPH, "The Flat Phillips Curve.png", type = "cairo-png")  #saving ECI Private Wages and Salaries and Trimmed Mean PCE Inflation
ggsave(dpi = "retina",plot = XUS_PHILLIPS_CURVE_GRAPH, "The Phillips Curve Abroad.png", type = "cairo-png")  #saving Unemployment/CPI Phillips Curve for Japan, Canada, and the United Kingdom
ggsave(dpi = "retina",plot = XUS_EPOP_PHILLIPS_CURVE_GRAPH, "The EPOP Phillips Curve Abroad.png", type = "cairo-png")  #saving EPOP/CPI Phillips Curve for Japan, Canada, and the United Kingdom
ggsave(dpi = "retina",plot = PCE_AUTOCORRELATION_GRAPH, "PCE Autocorrelation Function.png", type = "cairo-png")  #saving a graph of PCE's Autocorrelation Function


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
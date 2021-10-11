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
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.745,.82), expand = c(0,0)) +
  ggtitle("The Wage Curve vs the Price Curve, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Prime-Age Employment Correlates with Nominal Wage Growth More Than Inflation") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  annotate("text", label = "Coefficient: 0.765", x = 0.812, y = 0.019, color = "#00A99D") +
  annotate("text", label = "R-squared: 0.151", x = 0.812, y = 0.016, color = "#00A99D") +
  annotate("text", label = "Coefficient: 2.023", x = 0.812, y = 0.04, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.657", x = 0.812, y = 0.037, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = .7495-(.1861*0.075), xmax = .749-(0.049*0.075), ymin = -0.017-(.3*0.059), ymax = -0.017) +
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
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.745,.82), expand = c(0,0)) +
  ggtitle("The Wage Curve vs the Price Curve, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "Prime-Age Employment Correlates More with Private Sector Wages and Trimmed Mean Inflation") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  annotate("text", label = "Coefficient: 3.260", x = 0.812, y = 0.019, color = "#00A99D") +
  annotate("text", label = "R-squared: 0.564", x = 0.812, y = 0.016, color = "#00A99D") +
  annotate("text", label = "Coefficient: 2.652", x = 0.812, y = 0.04, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.834", x = 0.812, y = 0.037, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = .7495-(.1861*0.075), xmax = .749-(0.049*0.075), ymin = -0.017-(.3*0.059), ymax = -0.017) +
  coord_cartesian(clip = "off")

UNRATE_PCE <- merge(UNRATE, PCE, by = "date") #merging Unemployment Rate and ECI PCE Inflation data for the Unemployment/PCE and U6 Unemployment/Trimmed Mean PCE chart

summary(lm(value.x~value.y, data = UNRATE_PCE)) #creating summary stats for the regression of Unemployment Rate and PCE Inflation

PHILLIPS_CURVE_GRAPH <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  geom_point(data=UNRATE_PCE, aes(x=value.x/100,y=value.y/100, color= "Personal Consumption Expenditures Price Index"), size = 1.25)+
  stat_smooth(data=UNRATE_PCE,method = "lm", aes(x=value.x/100,y=value.y/100, color= "Personal Consumption Expenditures Price Index"), size = 1.25) +
  ylab("PCE, Percent Change from Year Ago, %") +
  xlab("Unemployment Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.017,0.042), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.033,.105), expand = c(0,0)) +
  ggtitle("The Flat Phillips Curve, 2000-2020") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data", subtitle = "The Correlation Between Unemployment and Inflation Has Broken Down") +
  theme_apricitas + theme(legend.position = c(.30,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  annotate("text", label = "Coefficient: -0.4955", x = 0.085, y = 0.01, color = "#FFE98F") +
  annotate("text", label = "R-squared: 0.07", x = 0.085, y = 0.007, color = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = .0375-(.1861*0.072), xmax = .037-(0.049*0.072), ymin = -0.017-(.3*0.059), ymax = -0.017) +
  coord_cartesian(clip = "off")

PCE <- fredr(series_id = c("PCEPI"), observation_start = as.Date("1960-01-01"), observation_end = as.Date("2020-01-01"), units = "pc1") #downloading PCE Inflation data

PCEACF <- acf(PCE$value,lag.max = 125,plot = F)
PCEPACF <- pacf(PCE$value,lag.max = 121,plot = F)
MELT_PCEACFS<- data.frame(lag = PCEACF$lag,PCEACF=PCEACF$acf)#PCEPACF=PCEPACF$acf)
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
  annotation_custom(apricitas_logo_rast, xmin = .0375-(.1861*0.072), xmax = .037-(0.049*0.072), ymin = -0.017-(.3*0.059), ymax = -0.017) +
  coord_cartesian(clip = "off")

SHOW AUTOCORRELATION
DO REAL WAGE CURVE VS EMPLOYMENT GRAPH
REDO WAGE CURVE GRAPH WITH PRIVATE ECI AND TRIMMED MEAN PCE
ANIMATED GRAPH ANCHORED WITH TIME FIXED EFFECT OR LAGGED PCE INFLATION? WILL PROBABLY HAVE TO ALLOW MOVEMENT ON THE Y AXIS
LOOK AT OTHER COUNTRIES TOO

ggsave(dpi = "retina",plot = EPOP_ECI_PCE_GRAPH, "EPOP ECI PCE Graph.png", type = "cairo-png") #saving ECI All Civilian and PCE Inflation Graph
ggsave(dpi = "retina",plot = EPOP_ECIPRIVWS_PCETRIM_GRAPH, "EPOP Private Sector ECI Trimmed Mean PCE Graph.png", type = "cairo-png")  #saving ECI Private Wages and Salaries and Trimmed Mean PCE Inflation
ggsave(dpi = "retina",plot = PHILLIPS_CURVE_GRAPH, "The Flat Phillips Curve.png", type = "cairo-png")  #saving ECI Private Wages and Salaries and Trimmed Mean PCE Inflation
ggsave(dpi = "retina",plot = PCE_AUTOCORRELATION_GRAPH, "PCE Autocorrelation Function.png", type = "cairo-png")  #saving a graph of PCE's Autocorrelation Function


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
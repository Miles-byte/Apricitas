pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)

ECI_PCE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/ECI_PCE.csv")
GDP_Comp <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/GDP_Comp.csv")
PCE_Inflation_Rates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/PCE_Inflation_Rates.csv")
CPI_New_Used_Car_Vehicles <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/CPI_New_Used_Vehicles.csv")

PCE_Inflation_Rates$DATE <- as.Date(PCE_Inflation_Rates$DATE, "%m/%d/%Y")
CPI_New_Used_Car_Vehicles$DATE <- as.Date(CPI_New_Used_Car_Vehicles$DATE, "%m/%d/%Y")
GDP_Comp$DATE <- as.Date(GDP_Comp$DATE, "%m/%d/%Y")
ECI_PCE$DATE <- as.Date(ECI_PCE$DATE, "%Y-%m-%d")

colnames(GDP_Comp) <- c("DATE","Nominal Gross Compensation","Nominal GDP","Real GDP")
GDP_Comp <- pivot_longer(GDP_Comp, cols = `Nominal Gross Compensation`:`Real GDP`, names_to = "Measure", values_to = "Value")


PCE_Inflation_Rates$PCEPI_Trimmed_Mean <- as.numeric(PCE_Inflation_Rates$PCEPI_Trimmed_Mean)

PCE_Inflation_Graphrates <- pivot_longer(PCE_Inflation_Rates, cols = `PCEPI_Trimmed_Mean`:`PCEPI_Core`, names_to = "Index", values_to = "Inflation")

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

PCE_Inflation_Rates_Graph <- ggplot() + #plotting PCE Inflation Rates
  geom_line(data=PCE_Inflation_Graphrates, aes(x=DATE,y= Inflation/100 ,color= Index), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.05), breaks = c(0,.02,.04), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2010-01-01"),as.Date("2021-08-01"))) +
  ylab("Percent Change from One Year Ago") +
  ggtitle("Is Inflation Transitory?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Traditional 'Core' Inflation Metric Does Not Eliminate Pandemic Volatility") +
  theme_apricitas + theme(legend.position = c(.70,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E"), labels = c("PCE Price Index","Core PCE Price Index", "Trimmed Mean PCE Price Index"))+
  annotate(geom = "hline",y = 0.02,yintercept = 0.02, size = 1.25,linetype = "dashed",color = "white") +#annotating 2% target inflation
  annotate(geom = "text", label = as.character("2% Inflation Target"),x = as.Date("2015-01-01"),y = 0.022,color = "white")

CPI_New_Used_Car_Vehicles_Graph <- ggplot() + #plotting Used Cars and Truck Prices
  geom_line(data=CPI_New_Used_Car_Vehicles, aes(x=DATE,y= CPI_Used_Vehicles ,color= "Used Cars and Trucks"), size = 1.25) +
  geom_line(data=CPI_New_Used_Car_Vehicles, aes(x=DATE,y= CPI_New_Vehicles ,color= "New Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,150), breaks = c(90,110,130,150), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2010-01-01"),as.Date("2021-08-01"))) +
  ylab("Index, January 2020 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Used Cars and Trucks Experienced Unprecedented Price Increases") +
  theme_apricitas + theme(legend.position = c(.80,.90)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))
  
ECI_PCE_Graph <- ggplot() + #ECI/PCE Graph 
  geom_line(data=ECI_PCE, aes(x=DATE,y= ECIALLCIV_PCEPI/1.2366*100 ,color= "Real Compensation - ECI/PCE"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,110), breaks = c(90,100,110), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2021-08-01"))) +
  ylab("Index, Q4 2019 = 100") +
  ggtitle("What's Really Happening With Compensation?") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA data",subtitle = "Real Worker Compensation Has Shrunk Since The Pandemic Started - But Mostly During Last Quarter") +
  theme_apricitas + theme(legend.position = c(.80,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9","#9A348E"))

GDP_Comp_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=GDP_Comp[GDP_Comp$Measure %in% c("Nominal GDP","Real GDP"),], aes(x=DATE,y= Value ,color= Measure), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(10000,25000), breaks = c(10000,15000,20000,25000), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2010-01-01"),as.Date("2021-08-01"))) +
  ylab("Percent Change from One Year Ago") +
  ggtitle("Is Inflation Transitory?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Traditional 'Core' Inflation Metric Does Not Eliminate Pandemic Volatility") +
  theme_apricitas + theme(legend.position = c(.70,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E"))#, labels = c("PCE Price Index","Core PCE Price Index", "Trimmed Mean PCE Price Index"))+
  


ggsave(dpi = "retina",plot = PCE_Inflation_Rates_Graph, "PCE_Inflation_Rates.png", type = "cairo-png") #Saving Image of PCE Inflation Rates Chart
ggsave(dpi = "retina",plot = CPI_New_Used_Car_Vehicles_Graph, "CPI_New_Used_Car_Vehicles.png", type = "cairo-png") #Saving Image of Car Price Indexes
ggsave(dpi = "retina",plot = ECI_PCE_Graph, "ECI_PCE.png", type = "cairo-png") #Saving Image of Real Compensation


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

Total_Quits <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2019-01-01")) #downloading quits data
Total_Discharges <- fredr(series_id = c("JTSLDL"), observation_start = as.Date("2019-01-01")) #discharges data

Retail_Quits <- fredr(series_id = c("JTS4400QUR"), observation_start = as.Date("2007-01-01")) #discharges data
LAH_Quits <- fredr(series_id = c("JTS7000QUR"), observation_start = as.Date("2007-01-01")) #discharges data
Manufacturing_Quits <- fredr(series_id = c("JTS3000QUR"), observation_start = as.Date("2007-01-01")) #discharges data
Professional_Quits <- fredr(series_id = c("JTS540099QUR"), observation_start = as.Date("2007-01-01")) #discharges data

Wage_Growth_Quartile <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americans%20Are%20Quitting%20Their%20Jobs/wage_growth_quartile.csv")
Wage_Growth_Edu <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americans%20Are%20Quitting%20Their%20Jobs/wage_growth_education.csv")

Wage_Growth_Quartile$date <- as.Date(Wage_Growth_Quartile$date, "%m/%d/%Y")
Wage_Growth_Edu$date <- as.Date(Wage_Growth_Edu$date, "%m/%d/%Y")

Wage_Growth_Edu[2:5] <- lapply(Wage_Growth_Edu[2:5], as.numeric)
Wage_Growth_Quartile[2:5] <- lapply(Wage_Growth_Quartile[2:5], as.numeric)

Employer_to_Employer_Transitions <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Americans%20Are%20Quitting%20Their%20Jobs/employer-to-employer_quits.csv")
Employer_to_Employer_Transitions$date <- as.Date(Employer_to_Employer_Transitions$date, "%m/%d/%Y")


theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Total_Quits_Graph <- ggplot() + #plotting total quits
  geom_line(data=Total_Quits, aes(x=date,y= value/1000,color= "Quits, Total Nonfarm"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(2,3,4), limits = c(2,4.5), expand = c(0,0)) +
  ggtitle("You Can't Fire Me, I Quit!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "A Record Number of Americans are Quitting Their Jobs") +
  theme_apricitas + theme(legend.position = c(.65,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*960), xmax = as.Date("2019-01-01")-(0.049*960), ymin = 2-(.3*2.5), ymax = 2) +
  coord_cartesian(clip = "off")

Total_Discharges_Graph <- ggplot() + #plotting total discharges
  geom_line(data=Total_Discharges, aes(x=date,y= value/1000,color= "Layoffs and Discharges, Total Nonfarm"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,4,8,12), limits = c(0,13.5), expand = c(0,0)) +
  ggtitle("Don't Leave Me, Okay?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Layoffs and Discharges Have Hit Record Lows") +
  theme_apricitas + theme(legend.position = c(.75,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*960), xmax = as.Date("2019-01-01")-(0.049*960), ymin = 0-(.3*13.5), ymax = 0) +
  coord_cartesian(clip = "off")

Industry_Quits_Graph <- ggplot() + #plotting total quits by industry
  geom_line(data=LAH_Quits, aes(x=date,y= value/100,color= "Leisure and Hospitality"), size = 1.25)+ 
  geom_line(data=Manufacturing_Quits, aes(x=date,y= value/100,color= "Manufacturing"), size = 1.25)+ 
  geom_line(data=Professional_Quits, aes(x=date,y= value/100,color= "Professional and Business Services"), size = 1.25)+ 
  geom_line(data=Retail_Quits, aes(x=date,y= value/100,color= "Retail Trade"), size = 1.25)+ 
  xlab("Date") +
  ylab("Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.02,.04,.06), limits = c(0,.07), expand = c(0,0)) +
  ggtitle("There's my Flair!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Workers in Retail, Leisure, and Hospitality-Not Professionals-Are Causing the Jump in Quits") +
  theme_apricitas + theme(legend.position = c(.4,.8),legend.title=element_text(size=14)) +
  scale_color_manual(name= "Quit Rates",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*5418), xmax = as.Date("2007-01-01")-(0.049*5418), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

Wage_Growth_Quartile_Graph <- ggplot() + #plotting wage growth by quartile 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X1st/100,color= "1st"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X2nd/100,color= "2nd"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X3rd/100,color= "3rd"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Quartile, aes(x=date,y= X4th/100,color= "4th"), size = 1.25)+ 
  xlab("Date") +
  ylab("Median Annual Wage Growth, 3 Month Moving Average %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.02,.04,.06), limits = c(0,.07), expand = c(0,0)) +
  ggtitle("The Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Wage Growth is Highest for Low-Income Workers, as is Often the Case in Tight Labor Markets") +
  theme_apricitas + theme(legend.position = c(.7,.8),legend.title=element_text(size=14)) +
  scale_color_manual(name= "Wage Growth by Quartile",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*8700), xmax = as.Date("1997-01-01")-(0.049*8700), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

Wage_Growth_Edu_Graph <- ggplot() + #plotting wage growth by education
  geom_line(data=Wage_Growth_Edu, aes(x=date,y= High.school.of.less/100,color= "High School or Less"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Edu, aes(x=date,y= Associates.degree/100,color= "Associates Degree"), size = 1.25)+ 
  geom_line(data=Wage_Growth_Edu, aes(x=date,y= Bachelors.degree.or.higher/100,color= "Bachelor's Degree or Higher"), size = 1.25)+ 
  xlab("Date") +
  ylab("Wage Growth Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.02,.04,.06), limits = c(0,.07), expand = c(0,0)) +
  ggtitle("Show Me the Money!") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "For the First Time, Wage Growth is Highest Among High School Graduates") +
  theme_apricitas + theme(legend.position = c(.7,.8),legend.title=element_text(size=14)) +
  scale_color_manual(name= "Wage Growth by Education",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("High School or Less","Associates Degree","Bachelor's Degree or Higher")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*8700), xmax = as.Date("1997-01-01")-(0.049*8700), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

Employer_to_Employer_Transitions_Graph <- ggplot() + #plotting employer to employer transition rates
  geom_line(data=Employer_to_Employer_Transitions, aes(x=date,y= c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(FMP, k = 12)) ,color= "Employer-to-Employer Transition Rate, 1 Year Rolling Average"), size = 1.25)+ 
  xlab("Date") +
  ylab("Wage Growth Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25), breaks = c(0,.02,.0225,.025,.0275,.03), limits = c(0.02,.03), expand = c(0,0)) +
  ggtitle("There's Change in the Air") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta data from Fujita, Moscarini, and Postel-Vinay", subtitle = "Workers are Changing Jobs at Pre-Pandemic Rates") +
  theme_apricitas + theme(legend.position = c(.6,.8),legend.text=element_text(size=13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*8700), xmax = as.Date("1997-01-01")-(0.049*8700), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Total_Quits_Graph, "Total Quits Chart.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Total_Discharges_Graph, "Total Discharges Chart.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Industry_Quits_Graph, "Industry Quits.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Wage_Growth_Quartile_Graph, "Wage Growth Quartile.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Wage_Growth_Edu_Graph, "Wage Growth Edu.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Employer_to_Employer_Transitions_Graph, "Employer to Employer Transitions.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()


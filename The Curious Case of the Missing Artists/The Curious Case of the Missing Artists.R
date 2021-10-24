pacman::p_load(plm,transformr,stringi,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Museum_Employees <- fredr(series_id = c("CES7071200001"), observation_start = as.Date("1990-01-01")) #downloading employment in museums and other cultural institutions from FRED
Prime_Age_Population <- fredr(series_id = c("LFWA25TTUSM647N"), observation_start = as.Date("1990-01-01")) #downloading prime age employment data from FRED
Museum_Prime_Age_Merge <- merge(Museum_Employees, Prime_Age_Population, by = "date")

Broadway_Stats <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Curious%20Case%20of%20the%20Missing%20Artists/Broadway_Stats.csv")
Broadway_Stats$Week.Ending <- as.Date(Broadway_Stats$Week.Ending)
Broadway_Stats <- mutate(Broadway_Stats[Broadway_Stats$Week.Ending > "1997-11-23", ], Capacity_MA = rollmean(Broadway_Stats$Capacity,k = 26), Gross_Potential_MA = rollmean(Broadway_Stats$Gross_Potential,k = 26)) #creating a moving average for attendance capacity

Arts_Rec_Employees <- fredr(series_id = c("CES7071000001"), observation_start = as.Date("1990-01-01")) #downloading Unit Labor Cost Data
Arts_Rec_Prime_Age_Merge <- merge(Arts_Rec_Employees, Prime_Age_Population, by = "date")
 
Museum_Employees_Graph <- ggplot() + 
  geom_line(data = Museum_Prime_Age_Merge, aes(x=date, y =value.x*1000/value.y, color = "Museums, Historical Sites, and Similar Institutions Prime Age Employment-Population Ratio"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.025), limits = c(0.0005,.0015), breaks = c(0,.0005,.001,.0015), expand = c(0,0)) +
  scale_x_date() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("That Belongs in a Museum!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment Growth at Museums and Historical Institutions is Extremely Sensitive to Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11566), xmax = as.Date("1990-01-01")-(0.049*11566), ymin = 0.0005-(.3*0.001), ymax = 0.0005) +
  coord_cartesian(clip = "off")

Broadway_Util_Graph <- ggplot() + 
  geom_line(data = Broadway_Stats, aes(x=Week.Ending, y = Capacity_MA, color = "Attendance, Percent of Total Capacity, 6 Month Rolling Average"), size = 1.25) +
  geom_line(data = Broadway_Stats, aes(x=Week.Ending, y = Gross_Potential_MA, color = "Gross Revenue, Percent of Potential, 6 Month Rolling Average"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.5,1), breaks = c(.5,.6,.7,.8,.9,1), expand = c(0,0)) +
  scale_x_date() +
  ylab("Percent") +
  ggtitle("It's Hard to be the Bard") +
  labs(caption = "Graph created by @JosephPolitano using Broadway League data",subtitle = "Broadway Shows Cut Prices, Preserved Attendance, but Lost Revenue During the Recession") +
  theme_apricitas + theme(legend.position = c(.50,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-11-30")-(.1861*11566), xmax = as.Date("1997-11-30")-(0.049*11566), ymin = 0.0005-(.3*0.001), ymax = 0.0005) +
  coord_cartesian(clip = "off")

Arts_Rec_Employees_Graph <- ggplot() + 
  geom_line(data = Arts_Rec_Prime_Age_Merge, aes(x=date, y =value.x*1000/value.y, color = "Arts, Entertainment, and Recreation Prime Age Employment-Population Ratio"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.025), limits = c(0.009,.021), breaks = c(0.01,.015,.02), expand = c(0,0)) +
  scale_x_date() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("Seize The Day") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Unions Tend to Take Strong Actions When Labor Markets are Tight") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11566), xmax = as.Date("1990-01-01")-(0.049*11566), ymin = 0.009-(.3*0.012), ymax = 0.009) +
  coord_cartesian(clip = "off") +
  annotate("rect", xmin = as.Date("2000-05-01"), xmax = as.Date("2000-10-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "SAG-AFTRA", x = as.Date("1997-01-01"), y = 0.011, color = "#EE6055") +
  annotate("text", label = "Commercial Actors", x = as.Date("1997-01-01"), y = 0.0105, color = "#EE6055") +
  annotate("text", label = "Strike", x = as.Date("1997-01-01"), y = 0.01, color = "#EE6055") +
  annotate("rect", xmin = as.Date("2003-03-07"), xmax = as.Date("2003-03-15"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Broadway", x = as.Date("2002-01-01"), y = 0.011, color = "#EE6055", size = 2.7) +
  annotate("text", label = "Musicians", x = as.Date("2002-01-01"), y = 0.0105, color = "#EE6055", size = 2.7) +
  annotate("text", label = "Strike", x = as.Date("2002-01-01"), y = 0.01, color = "#EE6055", size = 2.7) +
  annotate("rect", xmin = as.Date("2007-11-05"), xmax = as.Date("2008-02-12"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "WGA", x = as.Date("2006-09-01"), y = 0.011, color = "#EE6055") +
  annotate("text", label = "Writers", x = as.Date("2006-09-01"), y = 0.0105, color = "#EE6055") +
  annotate("text", label = "Strike", x = as.Date("2006-09-01"), y = 0.01, color = "#EE6055") +
  annotate("text", label = "Broadway", x = as.Date("2005-12-01"), y = 0.013, color = "#EE6055") +
  annotate("text", label = "Stagehands", x = as.Date("2005-12-01"), y = 0.0125, color = "#EE6055") +
  annotate("text", label = "Strike", x = as.Date("2005-12-01"), y = 0.012, color = "#EE6055") +
  annotate("rect", xmin = as.Date("2007-11-10"), xmax = as.Date("2007-11-28"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2016-10-21"), xmax = as.Date("2017-09-23"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "SAG-AFTRA", x = as.Date("2014-09-01"), y = 0.0115, color = "#EE6055") +
  annotate("text", label = "Video Game", x = as.Date("2014-09-01"), y = 0.011, color = "#EE6055") +
  annotate("text", label = "Voice Actors", x = as.Date("2014-09-01"), y = 0.0105, color = "#EE6055") +
  annotate("text", label = "Strike", x = as.Date("2014-09-01"), y = 0.01, color = "#EE6055")


ggsave(dpi = "retina",plot = Museum_Employees_Graph, "Museum Employees 1990.png", type = "cairo-png")  #saving a graph of employment-population ratio in museums
ggsave(dpi = "retina",plot = Broadway_Util_Graph, "Broadway Attendance and Revenue pct.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Arts_Rec_Employees_Graph, "Arts Employment and Strikes.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
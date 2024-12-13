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

OEWS_Data <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Curious%20Case%20of%20the%20Missing%20Artists/OEWS_DATA.csv")
OEWS_Data$TOT_EMP <- gsub(",","", OEWS_Data$TOT_EMP) 
OEWS_Data$A_MEDIAN <- gsub(",","", OEWS_Data$A_MEDIAN)
OEWS_Data$A_PCT10 <- gsub(",","", OEWS_Data$A_PCT10)
OEWS_Data$A_PCT25 <- gsub(",","", OEWS_Data$A_PCT25)
OEWS_Data$A_PCT75 <- gsub(",","", OEWS_Data$A_PCT75)
OEWS_Data$A_PCT90 <- gsub(",","", OEWS_Data$A_PCT90)

OEWS_Data$OCC_TITLE <- gsub("Musicians and singers","Musicians and Singers", OEWS_Data$OCC_TITLE) 
OEWS_Data$OCC_TITLE <- gsub("Writers and authors","Writers and Authors", OEWS_Data$OCC_TITLE) 
OEWS_Data$OCC_TITLE <- gsub("Artists and related workers","Artists and Related Workers", OEWS_Data$OCC_TITLE) 
OEWS_Data$OCC_TITLE <- gsub("Producers and directors","Producers and Directors", OEWS_Data$OCC_TITLE) 
OEWS_Data$OCC_TITLE <- gsub("Film and video editors","Film and Video Editors", OEWS_Data$OCC_TITLE) 

OEWS_Data$TOT_EMP <- as.numeric(OEWS_Data$TOT_EMP)
OEWS_Data$H_MEDIAN <- as.numeric(OEWS_Data$H_MEDIAN)
OEWS_Data$H_PCT10 <- as.numeric(OEWS_Data$H_PCT10)
OEWS_Data$H_PCT25 <- as.numeric(OEWS_Data$H_PCT25)
OEWS_Data$H_PCT75 <- as.numeric(OEWS_Data$H_PCT75)
OEWS_Data$H_PCT90 <- as.numeric(OEWS_Data$H_PCT90)


OEWS_Data$A_PCT10 <- as.numeric(OEWS_Data$A_PCT10)
OEWS_Data$A_PCT25 <- as.numeric(OEWS_Data$A_PCT25)
OEWS_Data$A_PCT75 <- as.numeric(OEWS_Data$A_PCT75)
OEWS_Data$A_PCT90 <- as.numeric(OEWS_Data$A_PCT90)
OEWS_Data$A_MEDIAN <- as.numeric(OEWS_Data$A_MEDIAN)

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

Artists_Employees_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Actors", ], aes(x=Year, y = TOT_EMP , color = "Actors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = TOT_EMP , color = "Musicians and Singers"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Writers and Authors", ], aes(x=Year, y = TOT_EMP , color = "Writers and Authors"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(30000,75000), breaks = c(30000,40000,50000,60000,70000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Total Employees") +
  ggtitle("Hey, The Big Artiste! Aren't You Working On Your Masterpiece?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment in Creative Occupations Is Unstable And Scarce") +
  theme_apricitas + theme(legend.position = c(.80,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 30000-(.3*45000), ymax = 30000) +
  coord_cartesian(clip = "off")

Artists_Pay_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Actors", ], aes(x=Year, y = H_MEDIAN , color = "Actors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_MEDIAN , color = "Musicians and Singers"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Writers and Authors", ], aes(x=Year, y = H_MEDIAN , color = "Writers and Authors"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(10,35), breaks = c(10,15,20,25,30,25), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Median Hourly Wage, $") +
  ggtitle("The Wage is Nothing and the Work is Hard") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Median Wage Growth Stagnated for Writers and Musicians, and Varied Considerably for Actors") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 10-(.3*25), ymax = 10) +
  coord_cartesian(clip = "off")

Artists_Pay_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Actors", ], aes(x=Year, y = H_PCT10 , color = "10"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Actors", ], aes(x=Year, y = H_PCT25 , color = "25"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Actors", ], aes(x=Year, y = H_MEDIAN , color = "50"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Actors", ], aes(x=Year, y = H_PCT75 , color = "75"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(limits = c(30000,75000), breaks = c(30000,40000,50000,60000,70000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("That Belongs in a Museum!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment Growth at Museums and Historical Institutions is Extremely Sensitive to Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 30000-(.3*45000), ymax = 30000) +
  coord_cartesian(clip = "off")


Artists_Pay_Graph <- ggplot() +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_PCT10 , color = "10"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_PCT25 , color = "25"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_MEDIAN , color = "50"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_PCT75 , color = "50"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_PCT90 , color = "50"), size = 1.25) +
  
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = (H_PCT10-lag(H_PCT10))/H_PCT10 , color = "10"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = (H_PCT25-lead(H_PCT25))/H_PCT25 , color = "25"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = (H_MEDIAN-lead(H_MEDIAN))/H_MEDIAN , color = "50"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = (H_PCT75-lead(H_PCT75))/H_PCT75 , color = "75"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Musicians and Singers", ], aes(x=Year, y = H_PCT90 , color = "75"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(limits = c(30000,75000), breaks = c(30000,40000,50000,60000,70000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("That Belongs in a Museum!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment Growth at Museums and Historical Institutions is Extremely Sensitive to Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 30000-(.3*45000), ymax = 30000) +
  coord_cartesian(clip = "off")

Writers_Pay_Graph <- ggplot() +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Writers and Authors", ], aes(x=Year, y = H_MEDIAN , color = "Median Annual Income: Writers and Authors"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(limits = c(30000,75000), breaks = c(30000,40000,50000,60000,70000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("That Belongs in a Museum!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment Growth at Museums and Historical Institutions is Extremely Sensitive to Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 30000-(.3*45000), ymax = 30000) +
  coord_cartesian(clip = "off")




Producers_Employees_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Producers and Directors", ], aes(x=Year, y = TOT_EMP , color = "Producers and Directors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Editors", ], aes(x=Year, y = TOT_EMP , color = "Editors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Film and Video Editors", ], aes(x=Year, y = TOT_EMP , color = "Film and Video Editors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "27-1014", ], aes(x=Year, y = TOT_EMP , color = "Special Effects Artists, Multimedia Artists, and Animators"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(limits = c(30000,75000), breaks = c(30000,40000,50000,60000,70000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("That Belongs in a Museum!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment Growth at Museums and Historical Institutions is Extremely Sensitive to Recessions") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 30000-(.3*45000), ymax = 30000) +
  coord_cartesian(clip = "off")

Producers_Wages_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Producers and Directors", ], aes(x=Year, y = A_MEDIAN , color = "Producers and Directors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Editors", ], aes(x=Year, y = A_MEDIAN , color = "Editors (Writing)"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_TITLE == "Film and Video Editors", ], aes(x=Year, y = A_MEDIAN , color = "Film and Video Editors"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "27-1014", ], aes(x=Year, y = A_MEDIAN , color = "Special Effects Artists, Multimedia Artists, and Animators"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(40000,90000), breaks = c(50000,70000,90000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Annual Wages, Dollars") +
  ggtitle("I Wanna Be a Producer") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Wages in Editing, Directorial, and Production Roles Grew More Robustly, But Still Suffered") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 30000-(.3*40000), ymax = 50000) +
  coord_cartesian(clip = "off")


Attendants_Employees_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3000", ], aes(x=Year, y = TOT_EMP , color = "Total Employees: Amusement and Recreation Attendants"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(limits = c(225000,350000), expand = c(0,0)) +
  scale_x_continuous(limits = c(2004,2019)) +
  ylab("Total Employees") +
  ggtitle("Will They Tell Your Story?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "'Attendant' is Far and Away the Largest Occupation in the Arts") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 225000-(.3*125000), ymax = 225000) +
  coord_cartesian(clip = "off")

Waiters_Employees_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "35-3031", ], aes(x=Year, y = TOT_EMP , color = "Total Empoyees: Waiters and Waitresses"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3031", ], aes(x=Year, y = TOT_EMP , color = "Ushers, Lobby Attendants, and Ticket Takers"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3091", ], aes(x=Year, y = TOT_EMP , color = "Amusement and Recreation Attendants"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(2200000,2600000), expand = c(0,0)) +
  scale_x_continuous(limits = c(2004,2019)) +
  ylab("Total Employees") +
  ggtitle("Waiters and Waitresses: Employees") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Number Of Waiters and Waitresses Peaked in 2017 and Started Declining in 2018") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*15), xmax = 2004-(0.049*15), ymin = 2200000-(.3*400000), ymax = 2200000) +
  coord_cartesian(clip = "off")

Cooks_Employees_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "35-2014", ], aes(x=Year, y = TOT_EMP , color = "Total Empoyees: Cooks, Restaurant"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3031", ], aes(x=Year, y = TOT_EMP , color = "Ushers, Lobby Attendants, and Ticket Takers"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3091", ], aes(x=Year, y = TOT_EMP , color = "Amusement and Recreation Attendants"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(700000,1500000), expand = c(0,0)) +
  scale_x_continuous(limits = c(2004,2019)) +
  ylab("Total Employees") +
  ggtitle("Cooks: Employees") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Number Of Restaurant Cooks Has Been Consistently Growing") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*15), xmax = 2004-(0.049*15), ymin = 700000-(.3*800000), ymax = 700000) +
  coord_cartesian(clip = "off")

Waiters_Wages_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "35-3031", ], aes(x=Year, y = A_MEDIAN , color = "Median Annual Wages: Waiters and Waitresses"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3031", ], aes(x=Year, y = TOT_EMP , color = "Ushers, Lobby Attendants, and Ticket Takers"), size = 1.25) +
  #geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3091", ], aes(x=Year, y = TOT_EMP , color = "Amusement and Recreation Attendants"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(14000,24000), expand = c(0,0)) +
  scale_x_continuous(limits = c(2004,2019)) +
  ylab("Median Annual Wages, Dollars") +
  ggtitle("Waiters and Waitresses: Wages") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Wages For Waiters and Waitresses Stalled Until 2014 But Have Been Climbing Since") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*15), xmax = 2004-(0.049*15), ymin = 14000-(.3*10000), ymax = 14000) +
  coord_cartesian(clip = "off")


Attendants_Wages_Graph <- ggplot() + 
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3031", ], aes(x=Year, y = A_MEDIAN , color = "Ushers, Lobby Attendants, and Ticket Takers"), size = 1.25) +
  geom_line(data = OEWS_Data[OEWS_Data$OCC_CODE == "39-3091", ], aes(x=Year, y = A_MEDIAN , color = "Amusement and Recreation Attendants"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::dollar_format(), limits = c(15000,25500), breaks = c(15000,17500,20000,22500,25000), expand = c(0,0)) +
  scale_x_continuous() +
  ylab("Annual Wages, Dollars") +
  ggtitle("Opening Up") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Great Recession Stopped Attendants' Wage Growth for Five Years") +
  theme_apricitas + theme(legend.position = c(.50,.66)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2004-(.1861*16), xmax = 2004-(0.049*16), ymin = 15000-(.3*11000), ymax = 15000) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = Museum_Employees_Graph, "Museum Employees 1990.png", type = "cairo-png")  #saving a graph of employment-population ratio in museums
ggsave(dpi = "retina",plot = Broadway_Util_Graph, "Broadway Attendance and Revenue pct.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Arts_Rec_Employees_Graph, "Arts Employment and Strikes.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Attendants_Wages_Graph, "Attendants Wages.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Producers_Wages_Graph, "Producers Wages.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Waiters_Employees_Graph, "Waiters Employees.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Waiters_Wages_Graph, "Waiters Wages.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Cooks_Employees_Graph, "Cooks Employees.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Artists_Employees_Graph, "Artists Employees.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Artists_Pay_Graph, "Artists Pay.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League
ggsave(dpi = "retina",plot = Attendants_Employees_Graph, "Attendants Employees.png", type = "cairo-png")  #saving a graph of attendance and gross revenue percents from Broadway League


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
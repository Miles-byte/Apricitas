pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Share_Reading_For_Pleasure <- bls_api("TUU30105AA01006315", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU30105AA01006315", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))


Share_Reading_For_Pleasure_Graph <- ggplot() + 
  geom_line(data= Share_Reading_For_Pleasure, aes(x=date,y=value/100,color= "% of Americans Age 15+\nReading for Personal Interest\nOn an Average Day"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.30),breaks = c(0,0.1,0.2,0.3), expand = c(0,0)) +
  ylab("% of Americans Age 15+") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Americans Are Reading Less") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "The Share of Americans Reading on an Average Day is at Record Lows") +
  theme_apricitas + theme(legend.position = c(.55,.88), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(.30)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Share_Reading_For_Pleasure_Graph, "Reading for Pleasure Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Food_Prep_Women <- bls_api("TUU10101AA01001260", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01001260", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Food_Prep_Men <- bls_api("TUU10101AA01001181", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01001181", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Food_Prep_Men_Women_Graph <- ggplot() +
  geom_line(data= Food_Prep_Women, aes(x=date,y=value*60,color= "Women"), size = 1.25) +
  geom_line(data= Food_Prep_Men, aes(x=date,y=value*60,color= "Men"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "m"),limits = c(0,60),breaks = c(0,20,40,60), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Americans Cook More—Especially Men") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Americans are Spending More Time Than Ever Cooking, With Men Seeing the Largest Increase") +
  theme_apricitas + theme(legend.position = c(.45,.48), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Food Prep, Cooking, & Cleaning\nAvg. Mins. Per Day, Americans 15+",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Women","Men")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(60)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Food_Prep_Men_Women_Graph, "Food Prep Men Women Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Computer_Games <- bls_api("TUU10101AA01005910", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01005910", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))
Computer_Other <- bls_api("TUU10101AA01006114", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01006114", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Computer_Games_Other_Graph <- ggplot() +
  geom_line(data= Computer_Other, aes(x=date,y=value*60,color= "Leisure Computer Time (non-Gaming)"), size = 1.25) +
  geom_line(data= Computer_Games, aes(x=date,y=value*60,color= "Gaming Time"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "m"),limits = c(0,30),breaks = c(0,10,20,30), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Video Game Use Continues Rising") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "The Share of Americans Reading on an Average Day is at Record Lows") +
  theme_apricitas + theme(legend.position = c(.35,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Avg. Mins. Per Day, Americans 15+",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Gaming Time","Leisure Computer Time (non-Gaming)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(30)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Computer_Games_Other_Graph, "Computer Games Other Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Socializing_Per_Day <- bls_api("TUU10101AA01013951", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01013951", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Socializing_Per_Day_Graph <- ggplot() + 
  geom_line(data= Socializing_Per_Day, aes(x=date,y=value*60,color= "Socializing & Communicating\nAvg. Mins. Per Day, Americans 15+"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "m"),limits = c(0,60),breaks = c(0,20,40,60), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("The Slow Rebound in Socialization") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Socialization Continues its Post-COVID Rebound, but Remains Below 2019 Levels") +
  theme_apricitas + theme(legend.position = c(.35,.90), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(60)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Socializing_Per_Day_Graph, "Socializing Per Day Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Pet_Care_Per_Day <- bls_api("TUU10101AA01001720", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01001720", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Pet_Care_Per_Day_Graph <- ggplot() + 
  geom_line(data= Pet_Care_by_Date, aes(x=date,y=value*60,color= "Pet & Animal Care\nAvg. Mins. Per Day, Americans 15+"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "m"),limits = c(0,15),breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Time Spent With Pets Remains High") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Time Spent Caring & Playing With Pets Remains at a Record High") +
  theme_apricitas + theme(legend.position = c(.35,.90), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(15)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Pet_Care_Per_Day_Graph, "Pet Care Per Day Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



Caring_Kids_Women <- bls_api("TUU10101AA01042928", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01042928", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Caring_Kids_Men <- bls_api("TUU10101AA01042913", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01042913", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Caring_Kids_Men_Women_Graph <- ggplot() +
  geom_line(data= Caring_Kids_Women, aes(x=date,y=value*60,color= "Mothers"), size = 1.25) +
  geom_line(data= Caring_Kids_Men, aes(x=date,y=value*60,color= "Fathers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "m"),limits = c(0,110),breaks = c(0,20,40,60,80,100), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Dads Are Caring For Their Kids More") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Dads Spend More Time With Their Kids Than Ever, Though Still Less Than Moms") +
  theme_apricitas + theme(legend.position = c(.35,.25), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Caring for Children\nAvg. Mins. Per Day, Americans 15+",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Mothers","Fathers")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(110)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Caring_Kids_Men_Women_Graph, "Caring Kids Men Women Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Sleep_Per_Day <- bls_api("TUU10101AA01000247", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01000247", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Sleep_Per_Day_Graph <- ggplot() + 
  geom_line(data= Sleep_Per_Day, aes(x=date,y=value,color= "Sleep, Avg. Mins. Per Day, Americans 15+"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .1, suffix = "hr"),limits = c(8.5,9.1),breaks = c(8.5,8.6,8.7,8.8,8.9,9,9.1), expand = c(0,0)) +
  ylab("Average Hours Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Americans are Sleeping More") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Americans Have Gained a Half-Hour of Sleep Over the Last Twenty Years") +
  theme_apricitas + theme(legend.position = c(.40,.90), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 8.5-(.3*(0.6)), ymax = 8.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Sleep_Per_Day_Graph, "Sleep Per Day Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Housework_Women <- bls_api("TUU10101AA01001023", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01001023", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Housework_Men <- bls_api("TUU10101AA01000944", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01000944", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Housework_Men_Women_Graph <- ggplot() +
  geom_line(data= Housework_Women, aes(x=date,y=value*60,color= "Women"), size = 1.25) +
  geom_line(data= Housework_Men, aes(x=date,y=value*60,color= "Men"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "m"),limits = c(0,60),breaks = c(0,20,40,60), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Men Are Doing More Housework") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Men Are Spending More time on Housework, Though Still Much Less than Women") +
  theme_apricitas + theme(legend.position = c(.35,.5), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Cleaning, Laundry, & Other Housework\nAvg. Mins. Per Day, Americans 15+",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Women","Men")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*(60)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Housework_Men_Women_Graph, "Housework Men Women Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Work_Women <- bls_api("TUU10101AA01012226", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01012226", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Work_Men <- bls_api("TUU10101AA01012110", startyear = 2000, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("TUU10101AA01012110", startyear = 2020, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

Work_Men_Women_Graph <- ggplot() +
  geom_line(data= Work_Women, aes(x=date,y=value,color= "Women"), size = 1.25) +
  geom_line(data= Work_Men, aes(x=date,y=value,color= "Men"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "hr"),limits = c(4.5,6.5),breaks = c(4.5,5,5.5,6,6.5), expand = c(0,0)) +
  ylab("Average Minutes Per Day") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Men Are Doing Less Formal Work") +
  labs(caption = "Graph created by @JosephPolitano using BLS ATUS data",subtitle = "Men Are Spending Less Time at Formal Jobs, Though Still Much More than Women") +
  theme_apricitas + theme(legend.position = c(.35,.5), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Work in Formal Jobs, Including Commutes\nAvg. Hrs. Per Day, Employed Americans 15+",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Women","Men")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 4.5-(.3*(2)), ymax = 4.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Work_Men_Women_Graph, "Work Men Women Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

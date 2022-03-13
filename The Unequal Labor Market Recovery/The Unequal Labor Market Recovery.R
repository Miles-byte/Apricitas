pacman::p_load(tidyr,Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

install.packages("rlang")
install.packages("cli")
install.packages("tidyr")
library(tidyr)
library(cli)
devtools::install_github("economic/epiextractr")
library(epiextractr)

download_cps("Basic", "C:/Users/Joseph/Documents")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Race <- load_cps("basic",2019:2022,year,month, basicwgt, age, wbhaom, emp) %>% #categorizing prime age epop by race
  filter(age >= 25 & age<= 54 & basicwgt > 0) %>%
  mutate(date = make_date(year, month, 01)) %>%
  group_by(date, wbhaom) %>%
  summarize(value = weighted.mean(emp, w = basicwgt)) %>%
  mutate(wbhaom = str_to_lower(as.character(haven::as_factor(wbhaom)))) %>%
  pivot_wider(date, names_from = wbhaom)

RaceExtended <- load_cps("basic",1990:2022,year,month, basicwgt, age, wbhao, emp) %>% #categorizing prime age epop by race
  filter(age >= 25 & age<= 54 & basicwgt > 0) %>%
  mutate(date = make_date(year, month, 01)) %>%
  group_by(date, wbhao) %>%
  summarize(value = weighted.mean(emp, w = basicwgt)) %>%
  mutate(wbhao = str_to_lower(as.character(haven::as_factor(wbhao)))) %>%
  pivot_wider(date, names_from = wbhao)

Gender <- load_cps("basic",2019:2022,year,month, basicwgt, age, female, emp) %>% #categorizing prime age epop by gender
  filter(age >= 25 & age<= 54 & basicwgt > 0) %>%
  mutate(date = make_date(year, month, 01)) %>%
  group_by(date, female) %>%
  summarize(value = weighted.mean(emp, w = basicwgt)) %>%
  mutate(female = str_to_lower(as.character(haven::as_factor(female)))) %>%
  pivot_wider(date, names_from = female)

RaceGender <- load_cps("basic",2019:2022,year,month,female, basicwgt, age, wbhaom, emp) %>% #prime age epop by race and gender
  filter(age >= 25 & age<= 54 & basicwgt > 0) %>%
  mutate(date = make_date(year, month, 01)) %>%
  group_by(date,female, wbhaom) %>%
  summarize(value = weighted.mean(emp, w = basicwgt)) %>%
  mutate(wbhaom = str_to_lower(as.character(haven::as_factor(wbhaom)))) %>%
  mutate(female = str_to_lower(as.character(haven::as_factor(female)))) %>%
  pivot_wider(date, names_from = c(wbhaom,female))

Education <- load_cps("basic",2019:2022,year,month, basicwgt, age, educ, emp) %>% #categorizing prime age epop by education
  filter(age >= 25 & age<= 54 & basicwgt > 0) %>%
  mutate(date = make_date(year, month, 01)) %>%
  group_by(date, educ) %>%
  summarize(value = weighted.mean(emp, w = basicwgt)) %>%
  mutate(educ = str_to_lower(as.character(haven::as_factor(educ)))) %>%
  pivot_wider(date, names_from = educ)

Children <- load_cps("basic",2019:2022,year,month, basicwgt, age, female, ownchild, emp) %>% #categorizing prime age epop by marital and child status
  filter(age >= 25 & age<= 54 & basicwgt > 0) %>%
  mutate(date = make_date(year, month, 01)) %>%
  mutate(ownchild = factor(ifelse(ownchild<1,"Without Children","With Children"))) %>%
  group_by(date, female, ownchild) %>%
  summarize(value = weighted.mean(emp, w = basicwgt)) %>%
  mutate(ownchild = str_to_lower(as.character(haven::as_factor(ownchild)))) %>%
  mutate(female = str_to_lower(as.character(haven::as_factor(female)))) %>%
  pivot_wider(date, names_from = c(female, ownchild))

RaceEpop_Graph <- ggplot() + 
  geom_line(data = Race, aes(x = date, y = white, color = "White"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = black, color = "Black"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = asian, color = "Asian"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = hispanic, color = "Hispanic (of Any Race)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.63,.9), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.70,.76)) +
  scale_color_manual(name= "Prime Age (25-54) Employment Population Ratios",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("White", "Asian","Hispanic (of Any Race)","Black")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .63-(.3*.27), ymax = .63) +
  coord_cartesian(clip = "off")

RaceExtended$blackgap <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(RaceExtended$white-RaceExtended$black,12))
RaceExtended$asiangap <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(RaceExtended$white-RaceExtended$asian,12))
RaceExtended$hispanicgap <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(RaceExtended$white-RaceExtended$hispanic,12))

RaceEpopExtended_Graph <- ggplot() + 
  geom_line(data = RaceExtended, aes(x = date, y = blackgap, color = "Black"), size = 1.25) +
  geom_line(data = RaceExtended, aes(x = date, y = asiangap, color = "Asian"), size = 1.25) +
  geom_line(data = RaceExtended, aes(x = date, y = hispanicgap, color = "Hispanic (of Any Race)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.15), breaks = c(0,.05,.10,.15), expand = c(0,0)) +
  ylab("Rolling 12 Month Moving Average, %") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.65,.80)) +
  scale_color_manual(name= "Difference Between White 25-54 Employment Population Ratio",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11000), xmax = as.Date("1990-01-01")-(0.049*11000), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

GenderEpop_Graph <- ggplot() + 
  geom_line(data = Gender, aes(x = date, y = male, color = "Male"), size = 1.25) +
  geom_line(data = Gender, aes(x = date, y = female, color = "Female"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.63,.9), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.70,.86)) +
  scale_color_manual(name= "Prime Age (25-54) Employment Population Ratios",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Male","Female")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .63-(.3*.27), ymax = .63) +
  coord_cartesian(clip = "off")

EducEpop_Graph <- ggplot() + 
  geom_line(data = Education, aes(x = date, y = `less than high school`, color = "Less Than High School"), size = 1.25) +
  geom_line(data = Education, aes(x = date, y = `high school`, color = "High School"), size = 1.25) +
  geom_line(data = Education, aes(x = date, y = `some college`, color = "Some College"), size = 1.25) +
  geom_line(data = Education, aes(x = date, y = `college`, color = "College"), size = 1.25) +
  geom_line(data = Education, aes(x = date, y = `advanced`, color = "Advanced"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.45,.92), breaks = c(.50,.60,.70,.80,.90), expand = c(0,0)) +
  ylab("Prime Age Employment-Population Ratio, %") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.3,.19)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Advanced","College","Some College","High School","Less Than High School")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .45-(.3*.47), ymax = .45) +
  coord_cartesian(clip = "off")

RaceEpopExpanded_Graph <- ggplot() + 
  geom_line(data = Race, aes(x = date, y = white, color = "White"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = black, color = "Black"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = asian, color = "Asian"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = hispanic, color = "Hispanic (of Any Race)"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = `multiple races`, color = "Multiple Races"), size = 1.25) +
  geom_line(data = Race, aes(x = date, y = `native american`, color = "Native American"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.57,.95), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.70,.84), legend.text = element_text(size = 13, color = "white"),legend.title =element_text(size = 13)) +
  scale_color_manual(name= "Prime Age (25-54) Employment Population Ratios",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("White", "Asian","Hispanic (of Any Race)","Black","Multiple Races","Native American")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .57-(.3*.38), ymax = .57) +
  coord_cartesian(clip = "off")

BlackGender_Graph <- ggplot() + 
  geom_line(data = RaceGender, aes(x = date, y = black_male, color = "Black Male"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = black_female, color = "Black Female"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = white_male, color = "White Male"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = white_female, color = "White Female"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.53,.9), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("Prime Age Employment-Population Ratio, %") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.77,.24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("White Male","White Female","Black Male","Black Female")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .53-(.3*.37), ymax = .53) +
  coord_cartesian(clip = "off")

HispanicGender_Graph <- ggplot() + 
  geom_line(data = RaceGender, aes(x = date, y = hispanic_male, color = "Hispanic Male"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = hispanic_female, color = "Hispanic Female"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = white_male, color = "White Male"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = white_female, color = "White Female"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.53,.9), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("Prime Age Employment-Population Ratio, %") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.73,.45), legend.text = element_text(size = 13, color = "white"),legend.title =element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("White Male","White Female","Hispanic Male","Hispanic Female")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .53-(.3*.37), ymax = .53) +
  coord_cartesian(clip = "off")

AsianGender_Graph <- ggplot() + 
  geom_line(data = RaceGender, aes(x = date, y = asian_male, color = "Asian Male"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = asian_female, color = "Asian Female"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = white_male, color = "White Male"), size = 1.25) +
  geom_line(data = RaceGender, aes(x = date, y = white_female, color = "White Female"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.53,.9), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("Prime Age Employment-Population Ratio, %") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.85,.25), legend.text = element_text(size = 14, color = "white"),legend.title =element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("White Male","White Female","Asian Male","Asian Female")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .53-(.3*.37), ymax = .53) +
  coord_cartesian(clip = "off")

Children_Graph <- ggplot() + 
  geom_line(data = Children, aes(x = date, y = `male_without children`, color = "Male Without Children"), size = 1.25) +
  geom_line(data = Children, aes(x = date, y = `female_without children`, color = "Female Without Children"), size = 1.25) +
  geom_line(data = Children, aes(x = date, y = `male_with children`, color = "Male With Children"), size = 1.25) +
  geom_line(data = Children, aes(x = date, y = `female_with children`, color = "Female With Children"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.53,.95), breaks = c(.60,.70,.80,.90), expand = c(0,0)) +
  ylab("Prime Age Employment-Population Ratio, %") +
  ggtitle("The Unequal Labor Market Recovery") +
  labs(caption = "Graph created by @JosephPolitano using CPS data",subtitle = "Gaps Persist in the Employment Recovery") +
  theme_apricitas + theme(legend.position = c(.25,.25), legend.text = element_text(size = 14, color = "white"),legend.title =element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Male Without Children","Female Without Children","Male With Children","Female With Children")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = .53-(.3*.42), ymax = .53) +
  coord_cartesian(clip = "off")

#DONT FORGET: DIFFERENT WEIGHT FOR TEENAGERS
#Don't forget map
#DO GLI ESTIMATES
#Do Motherhood by age and by age of child and by number of kids.stacked bar chart with bins for age and bars for number of kids. Same for single v married

ggsave(dpi = "retina",plot = RaceEpop_Graph, "RaceEpop.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RaceEpopExpanded_Graph, "RaceEpopExpanded.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BlackGender_Graph, "Black Gender.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AsianGender_Graph, "Asian Gender.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = HispanicGender_Graph, "Hispanic Gender.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GenderEpop_Graph, "Gender.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EducEpop_Graph, "Educ.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Children_Graph, "Children.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RaceEpopExtended_Graph, "Race Epop Extended.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload("all")

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
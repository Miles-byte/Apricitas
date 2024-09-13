pacman::p_load(rsdmx,bea.R,cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

MANUFACTURING_PRODUCTIVITY <- bls_api("PRS30006093", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("PRS30006093", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(
    period = as.integer(sub("Q", "", period)),
    date = yq(paste(year, period, sep = " Q"))
  ) %>%
  arrange(date) %>%
  filter(date >= as.Date("2004-01-01"))

NONFARM_PRODUCTIVITY <- bls_api("PRS85006093", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("PRS85006093", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(
  period = as.integer(sub("Q", "", period)),
  date = yq(paste(year, period, sep = " Q"))
) %>%
  arrange(date) %>%
  filter(date >= as.Date("2004-01-01"))


US_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY <- ggplot() +
  geom_line(data=filter(MANUFACTURING_PRODUCTIVITY, date>= as.Date("2005-01-01")), aes(x=date,y= value/value[1]*100,color= "Manufacturing Sector Labor Productivity"), size = 1.25) +
  geom_line(data=filter(NONFARM_PRODUCTIVITY, date>= as.Date("2005-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,135), breaks = c(95,100,105,110,115,120,125,130,135), expand = c(0,0)) +
  ylab("Index Q1 2005 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS dat. NOTE: Labor Productivity Defined as Output per Hour Worked",subtitle = "US Labor Productivity Has Grown Significantly—Outside of The Manufacturing Sector") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Overall US Labor Productivity","Manufacturing Sector Labor Productivity")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 95-(.3*40), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY, "US Overall vs Manufacturing Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_OVERALL_LABOR_PRODUCTIVITY <- ggplot() + 
  geom_line(data=filter(NONFARM_PRODUCTIVITY, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity\n(Real Output Per Hour Worked)"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2015-01-01"), xend = as.Date("2019-10-01"), y = 100, yend = NONFARM_PRODUCTIVITY$value[64]/NONFARM_PRODUCTIVITY$value[45]*100, color = "#00A99D",linetype = "dashed", size = 1) +
  annotate("text", label = paste0("Q1 2015-Q4 2019:\n+",round(((NONFARM_PRODUCTIVITY$value[64]/NONFARM_PRODUCTIVITY$value[45])^(4 /(64-45)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2017-02-01"), y = 106, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate(geom = "segment", x = as.Date("2019-10-01"), xend = max(NONFARM_PRODUCTIVITY$date), y = NONFARM_PRODUCTIVITY$value[64]/NONFARM_PRODUCTIVITY$value[45]*100, yend = NONFARM_PRODUCTIVITY$value[nrow(NONFARM_PRODUCTIVITY)]/NONFARM_PRODUCTIVITY$value[45]*100, color = "#EE6055",linetype = "dashed", size = 1) +
  annotate("text", label = paste0("Q4 2019-",paste0("Q", lubridate::quarter(max(as.Date(NONFARM_PRODUCTIVITY$date))), " ", lubridate::year(max(as.Date(NONFARM_PRODUCTIVITY$date)))),"\n+",round(((NONFARM_PRODUCTIVITY$value[nrow(NONFARM_PRODUCTIVITY)]/NONFARM_PRODUCTIVITY$value[64])^(4 /(nrow(NONFARM_PRODUCTIVITY)-64)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2023-02-01"), y = 116, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = "Productivity Spikes\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-12-01"), hjust = 0.5, y = 112, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Productivity Stalls/Falls\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2020-12-01"), hjust = 0.5, y = 116, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,117.5), breaks = c(95,100,105,110,115), expand = c(0,0)) +
  ylab("Index Q1 2015 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Cumulative US Labor Productivity Growth Has Matched Pre-COVID Levels Since 2020") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_OVERALL_LABOR_PRODUCTIVITY, "US Overall Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#TWO DIGIT LABOR PRODUCTIVITY

LAB_PROD_WHOLESALE_TRADE <- bls_api("IPUGN42____L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("IPUGN42____L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  arrange(date) %>%
  filter(date >= as.Date("2004-01-01"))

LAB_PROD_RETAIL_TRADE <- bls_api("IPUHN44_45_L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("IPUHN44_45_L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(
    period = as.integer(sub("Q", "", period)),
    date = yq(paste(year, period, sep = " Q"))
  ) %>%
  arrange(date) %>%
  filter(date >= as.Date("2004-01-01"))

#Detailed Industry Subcategory Here:
#https://www.bls.gov/productivity/tables/
DETAILED_PRODUCTIVITY_BULK <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/US%20Service%20Sector%20Productivity/DETAILED_PRODUCTIVITY_DATA.csv")

DETAILED_LABOR_PRODUCTIVITY_2_DIGIT <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)" & Digit == "2-Digit") %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)

DETAILED_LABOR_PRODUCTIVITY_3_DIGIT <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)" & Digit == "3-Digit") %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)

DETAILED_LABOR_PRODUCTIVITY_4_DIGIT <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)" & Digit == "4-Digit") %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)

DETAILED_LABOR_PRODUCTIVITY_5_DIGIT <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)" & Digit == "5-Digit") %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)

DETAILED_LABOR_PRODUCTIVITY_6_DIGIT <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)") %>%
  filter(Digit %in% c("6-Digit","Custom")) %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)

US_BOWLING_ALLEY_PRODUCTIVITY <- ggplot() + 
  geom_line(data=DETAILED_LABOR_PRODUCTIVITY_6_DIGIT, aes(x=Year,y= `Bowling centers`,color= "Bowling Alley Labor Productivity\n(Output Per Hour Worked)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,110), breaks = c(70,80,90,100,110), expand = c(0,0)) +
  ylab("Index 2019 = 100") +
  ggtitle("US Bowling Alley Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Isn't It Funny How Detailed the US Labor Market Data is?") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*(today()-as.Date("1987-01-01"))), ymin = 65-(.3*45), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_BOWLING_ALLEY_PRODUCTIVITY, "US Bowling Alley Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#LOW-END PRODUCTIVITY
US_RESTAURANT_PRODUCTIVITY <- ggplot() + 
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_6_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Full-service restaurants`,color= "Full Service Sit-Down Restaurants"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_6_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Limited-service eating places`,color= "Limited Service Fast-Food Restaurants"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_4_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Drinking places (alcoholic beverages)`,color= "Bars & Drinking Places"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_4_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Special food services`,color= "Catering & Special Food Services"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Food services and drinking places`,color= "Total Food Service & Drinking Places"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(80,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index 2019 = 100") +
  ggtitle("US Food Service Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Food Service Labor Productivity Has Boomed Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.25,.83), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  scale_color_manual(name= "Labor Productivity Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Total Food Service & Drinking Places","Full Service Sit-Down Restaurants","Limited Service Fast-Food Restaurants", "Bars & Drinking Places", "Catering & Special Food Services"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 80-(.3*50), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_RESTAURANT_PRODUCTIVITY, "US Restaurant Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#
US_RETAIL_PRODUCTIVITY <- ggplot() + 
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Nonstore retailers`,color= "Online & Nonstore Retailers"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Clothing and clothing accessories stores`,color= "Clothing & Accessory Stores"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Electronics and appliance stores`,color= "Electronics & Appliances Stores"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Sporting goods, hobby, book, and music stores`,color= "Sporting Goods, Hobby, Book, & Music Stores"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Health and personal care stores`,color= "Health and Personal Care Stores"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Retail trade`,color= "Total Retail Trade"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,180), breaks = c(0,40,80,120,160), expand = c(0,0)) +
  ylab("Index 2019 = 100") +
  ggtitle("US Retail Trade Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Food Service Labor Productivity Has Boomed Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.3,.81), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  scale_color_manual(name= "Labor Productivity Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Total Retail Trade","Online & Nonstore Retailers","Clothing & Accessory Stores", "Electronics & Appliances Stores", "Sporting Goods, Hobby, Book, & Music Stores", "Health and Personal Care Stores"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 0-(.3*180), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_RETAIL_PRODUCTIVITY, "US Retail Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CAPITAL_INTENSITY_LABOR_PRODUCTIVITY <- bls_api("MPU4910152", startyear = 2015, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  arrange(year)
  
CAPITAL_INTENSITY_LABOR_PRODUCTIVITY_GRAPH <- ggplot() + 
  geom_line(data=filter(CAPITAL_INTENSITY_LABOR_PRODUCTIVITY, year >= as.Date("2015-01-01")), aes(x=year,y= value/value[1]*100,color= "US Capital Intensity\nContribution to Labor Productivity"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2015-01-01"), xend = as.Date("2019-01-01"), y = 100, yend = CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[5]/CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[1]*100, color = "#00A99D",linetype = "dashed", size = 1) +
  annotate("text", label = paste0("2015-2019:\n+",round(((CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[5]/CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[1])^(1/4) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2017-02-01"), y = 102.5, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate(geom = "segment", x = as.Date("2019-01-01"), xend = max(CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$year), y = CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[5]/CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[1]*100, yend = CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[nrow(CAPITAL_INTENSITY_LABOR_PRODUCTIVITY)]/CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[1]*100, color = "#EE6055",linetype = "dashed", size = 1) +
  annotate("text", label = paste0("2019-",paste0(lubridate::year(max(as.Date(CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$year)))),"\n+",round(((CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[nrow(CAPITAL_INTENSITY_LABOR_PRODUCTIVITY)]/CAPITAL_INTENSITY_LABOR_PRODUCTIVITY$value[5])^(1 /(nrow(CAPITAL_INTENSITY_LABOR_PRODUCTIVITY)-5)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2022-02-01"), y = 104, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = "Contribution Spikes\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-05-15"), hjust = 0.5, y = 105, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Contribution Stalls/Falls\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2021-04-01"), hjust = 0.5, y = 107, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(98,109), breaks = c(95,100,105,110,115), expand = c(0,0)) +
  ylab("Index 2015 = 100") +
  ggtitle("US Capital Intensity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Cumulative US Labor Productivity Growth Has Matched Pre-COVID Levels Since 2020") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-365-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-365-as.Date("2015-01-01"))), ymin = 98-(.3*11), ymax = 98) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAPITAL_INTENSITY_LABOR_PRODUCTIVITY_GRAPH, "US Capital Intensity Labor Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


  

DETAILED_OUTPUT_PER_HOUR <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure %in% c("Sectoral output","Hours worked","Sectoral output price deflator")) %>%
  filter(Units != "% Change from previous year") %>%
  filter(Measure != "Hours worked" | (Measure == "Hours worked" & Units == "Millions of hours")) %>%
  filter(Measure != "Sectoral output" | (Measure == "Sectoral output" & Units == "Millions of current dollars")) %>%
  select(-Units) %>%
  mutate(Value = as.numeric(gsub(",","",Value))) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(`Nominal output per hour worked` = `Sectoral output`/`Hours worked`, `Real output per hour worked` = `Nominal output per hour worked`/`Sectoral output price deflator`*100)



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

p_unload(all)
pacman::p_load(purrr,estatapi,statcanR,readabs,rsdmx,bea.R,cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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

OVERALL_PRODUCTIVITY <- bls_api("PRS85006093", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("PRS85006093", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(
  period = as.integer(sub("Q", "", period)),
  date = yq(paste(year, period, sep = " Q"))
) %>%
  arrange(date) %>%
  filter(date >= as.Date("2004-01-01"))


US_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY <- ggplot() +
  geom_line(data=filter(MANUFACTURING_PRODUCTIVITY, date>= as.Date("2005-01-01")), aes(x=date,y= value/value[1]*100,color= "Manufacturing Sector Labor Productivity"), size = 1.25) +
  geom_line(data=filter(OVERALL_PRODUCTIVITY, date>= as.Date("2005-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,140), breaks = c(95,100,105,110,115,120,125,130,135,140), expand = c(0,0)) +
  ylab("Index Q1 2005 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data. NOTE: Labor Productivity Defined as Output per Hour Worked",subtitle = "US Labor Productivity Has Grown Significantly—Outside of The Manufacturing Sector") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Overall US Labor Productivity","Manufacturing Sector Labor Productivity")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 95-(.3*45), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY, "US Overall vs Manufacturing Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_OVERALL_LABOR_PRODUCTIVITY <- ggplot() + 
  geom_line(data=filter(OVERALL_PRODUCTIVITY, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity\n(Real Output Per Hour Worked)"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2015-01-01"), xend = as.Date("2019-10-01"), y = 100, yend = OVERALL_PRODUCTIVITY$value[64]/OVERALL_PRODUCTIVITY$value[45]*100, color = "#00A99D",linetype = "dashed", size = 1) +
  annotate("text", label = paste0("Q1 2015-Q4 2019:\n+",round(((OVERALL_PRODUCTIVITY$value[64]/OVERALL_PRODUCTIVITY$value[45])^(4 /(64-45)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2017-02-01"), y = 106, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate(geom = "segment", x = as.Date("2019-10-01"), xend = max(OVERALL_PRODUCTIVITY$date), y = OVERALL_PRODUCTIVITY$value[64]/OVERALL_PRODUCTIVITY$value[45]*100, yend = OVERALL_PRODUCTIVITY$value[nrow(OVERALL_PRODUCTIVITY)]/OVERALL_PRODUCTIVITY$value[45]*100, color = "#EE6055",linetype = "dashed", size = 1) +
  annotate("text", label = paste0("Q4 2019-",paste0("Q", lubridate::quarter(max(as.Date(OVERALL_PRODUCTIVITY$date))), " ", lubridate::year(max(as.Date(OVERALL_PRODUCTIVITY$date)))),"\n+",round(((OVERALL_PRODUCTIVITY$value[nrow(OVERALL_PRODUCTIVITY)]/OVERALL_PRODUCTIVITY$value[64])^(4 /(nrow(OVERALL_PRODUCTIVITY)-64)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2023-02-01"), y = 119, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = "Productivity Spikes\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-12-01"), hjust = 0.5, y = 112, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Productivity Stalls/Falls\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2020-12-01"), hjust = 0.5, y = 116, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,120), breaks = c(95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index Q1 2015 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Cumulative US Labor Productivity Growth Has Matched Pre-COVID Levels Since 2020") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_OVERALL_LABOR_PRODUCTIVITY, "US Overall Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_OVERALL_LABOR_PRODUCTIVITY_RAW_GROWTH <- ggplot() + 
  geom_line(data=filter(OVERALL_PRODUCTIVITY, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity\n(Real Output Per Hour Worked)"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2015-01-01"), xend = as.Date("2019-10-01"), y = 100, yend = OVERALL_PRODUCTIVITY$value[64]/OVERALL_PRODUCTIVITY$value[45]*100, color = "#00A99D",linetype = "dashed", size = 1) +
  #annotate("text", label = paste0("Q1 2015-Q4 2019:\n+",round(((PROD_NOWCASTED$value[64]/PROD_NOWCASTED$value[45])^(4 /(64-45)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2017-02-01"), y = 106, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = paste0("Q1 2015-Q4 2019:\n+", round(((OVERALL_PRODUCTIVITY$value[64] / OVERALL_PRODUCTIVITY$value[45]) - 1) * 100, 1), "% Growth"), x = as.Date("2017-02-01"), y = 106, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate(geom = "segment", x = as.Date("2019-10-01"), xend = max(OVERALL_PRODUCTIVITY$date), y = OVERALL_PRODUCTIVITY$value[64]/OVERALL_PRODUCTIVITY$value[45]*100, yend = OVERALL_PRODUCTIVITY$value[nrow(OVERALL_PRODUCTIVITY)]/OVERALL_PRODUCTIVITY$value[45]*100, color = "#EE6055",linetype = "dashed", size = 1) +
  #annotate("text", label = paste0("Q4 2019-",paste0("Q", lubridate::quarter(max(as.Date(PROD_NOWCASTED$date))), " ", lubridate::year(max(as.Date(PROD_NOWCASTED$date)))),"\n+",round(((PROD_NOWCASTED$value[nrow(PROD_NOWCASTED)]/PROD_NOWCASTED$value[64])^(4 /(nrow(PROD_NOWCASTED)-64)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2023-02-01"), y = 116, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = paste0("Q4 2019-", paste0("Q", lubridate::quarter(max(as.Date(OVERALL_PRODUCTIVITY$date))), " ", lubridate::year(max(as.Date(OVERALL_PRODUCTIVITY$date)))), "\n+", round(((OVERALL_PRODUCTIVITY$value[nrow(OVERALL_PRODUCTIVITY)] / OVERALL_PRODUCTIVITY$value[64]) - 1) * 100, 1), "% Growth"), x = as.Date("2023-02-01"), y = 117, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = "Productivity Spikes\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-12-01"), hjust = 0.5, y = 112, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Productivity Stalls/Falls\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2020-12-01"), hjust = 0.5, y = 116, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,120), breaks = c(95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index Q1 2015 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Productivity data Nowcasted With Updated BEA GDP Data.",subtitle = "Cumulative US Labor Productivity Growth Has Exceeded Pre-COVID Levels Since 2020") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_OVERALL_LABOR_PRODUCTIVITY_RAW_GROWTH, "US Overall Labor Productivity Raw Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


NONFARM_PRODUCTIVITY <- fredr("OPHNFB", observation_start = as.Date("2015-01-01"))
AVERAGE_COMPENSATION_NOMINAL <- fredr("COMPNFB", observation_start = as.Date("2015-01-01"))
MEDIAN_WAGE_NOMINAL <- fredr("LES1252881500Q", observation_start = as.Date("2015-01-01"))

PCEPI <- fredr("PCEPI", observation_start = as.Date("2015-01-01"))

AVERAGE_COMPENSATION_REAL <- merge(AVERAGE_COMPENSATION_NOMINAL,PCEPI, by = "date") %>%
  transmute(date, value = value.x/value.y)
MEDIAN_WAGE_REAL <- merge(MEDIAN_WAGE_NOMINAL,PCEPI, by = "date") %>%
  transmute(date, value = value.x/value.y)

US_PRODUCTIVITY_WAGES <- ggplot() + 
  geom_line(data=filter(OVERALL_PRODUCTIVITY, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "US Labor Productivity"), size = 1.25) + 
  geom_line(data=filter(AVERAGE_COMPENSATION_REAL, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "US Real Average Compensation"), size = 1.25) + 
  geom_line(data=filter(MEDIAN_WAGE_REAL, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "US Real Median Wage"), size = 1.25) + 
  annotate("text", label = "Productivity & Wages Spike\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-10-01"), hjust = 0.5, y = 112, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Productivity & Wages Stall/Fall\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2021-06-01"), hjust = 0.5, y = 118, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,120), breaks = c(95,100,105,110,115), expand = c(0,0)) +
  ylab("Index Q1 2015 = 100") +
  ggtitle("US Labor Productivity and Wages ") +
  labs(caption = "Graph created by @JosephPolitano using BLS data\nWages Deflated by PCEPI. Compensation and Productivity Hourly for Nonfarm Business. Median Wage Weekly",subtitle = "Increased Productivity Has Translated into Rising Wages & Compensation for American Workers") +
  theme_apricitas + theme(legend.position = c(.23,.915)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_PRODUCTIVITY_WAGES, "US Productivity Wages.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

Total_Quits <- fredr(series_id = c("JTSQUL"), observation_start = as.Date("2015-01-01")) #downloading quits data
Total_Layoffs <- bls_api("JTS000000000000000LDL", startyear = 2015, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


Total_Quits_Layoffs_Graph <- ggplot() + #plotting total quits and layoffs
  geom_line(data=Total_Quits, aes(x=date,y= value/1000,color= "Quits, Total Nonfarm"), size = 1.25)+ 
  geom_line(data=Total_Layoffs, aes(x=date,y= value/1000,color= "Layoffs and Discharges, Total Nonfarm"), size = 1.25)+
  annotate(geom = "text", label = "Note: Discontinuity at March 2020, When Layoffs hit 13M", x = as.Date("2020-01-01"), y = 1.15, color ="white", size = 4, alpha = 1) +
  xlab("Date") +
  ylab("Millions of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3,4,5), limits = c(0,5), expand = c(0,0)) +
  ggtitle("The End of The Great Reshuffling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Number of Quits is Coming Down From Record Highs, as Layoffs Have Risen a Bit") +
  theme_apricitas + theme(legend.position = c(.26,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Quits, Total Nonfarm","Layoffs and Discharges, Total Nonfarm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Total_Quits_Layoffs_Graph, "Total Quits and Layoffs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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

#Major Sector Industry Productivity Here:
#https://www.bls.gov/productivity/tables/
MAJOR_INDUSTRIES_TFP_BULK <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/US%20Service%20Sector%20Productivity/MAJOR_INDUSTRY_TFP.csv")

DETAILED_INDUSTRIES_TFP_2_DIGIT <- MAJOR_INDUSTRIES_TFP_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)" & (nchar(as.character(NAICS)) == 2 | Industry == "Professional and business services")) %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)

DETAILED_INDUSTRIES_TFP_2_DIGIT_GRAPH <- ggplot() + 
  #geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("2015-01-01")), aes(x=Year,y= `Finance and insurance`,color= "Finance & Insurance"), size = 1.25) +
  #geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("2013-01-01")), aes(x=Year,y= `Professional, scientific, and technical services`,color= "Professional, Scientific, & Technical Services"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Professional and business services`,color= "Professional and Business Services"), size = 1.25) +
  #geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("2013-01-01")), aes(x=Year,y= `Management of companies and enterprises`,color= "Management of Companies"), size = 1.25) +
  #geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("2013-01-01")), aes(x=Year,y= `Administrative and waste management services`,color= "Administrative & Waste Management Services"), size = 1.25) +
  #geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("2015-01-01")), aes(x=Year,y= `Educational services`,color= "Educational Services"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Health care and social assistance`,color= "Healthcare & Social Assistance"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Arts, entertainment, and recreation`,color= "Arts, Entertainment, & Recreation"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Accommodation and food services`,color= "Accomodation & Food Services"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Information`,color= "Information"), size = 1.25) +
  annotate("text", label = "NOTE: Y-axis truncated\nfor subcategories with\nfast long-run growth\nto help legibility", x = as.Date("2018-12-01"), hjust = 0.5, y = 70, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,120), breaks = c(60,70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index 2019 = 100") +
  ggtitle("The US Service-Sector Productivity Boom") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Productivity Growth has Boomed in Industries Like Information, Food Service, Business, & More") +
  theme_apricitas + theme(legend.position = c(.3,.83), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  scale_color_manual(name= "Labor Productivity Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Information","Accomodation & Food Services", "Professional and Business Services", "Arts, Entertainment, & Recreation","Healthcare & Social Assistance")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-800-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-800-as.Date("1993-01-01"))), ymin = 60-(.3*60), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DETAILED_INDUSTRIES_TFP_2_DIGIT_GRAPH, "Detailed Industries 2-Digit Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

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
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Retail Trade Labor Productivity Has Accelerated Since the Start of the Pandemic, Especially in Online Areas") +
  theme_apricitas + theme(legend.position = c(.3,.81), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  scale_color_manual(name= "Labor Productivity Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Total Retail Trade","Online & Nonstore Retailers","Clothing & Accessory Stores", "Electronics & Appliances Stores", "Sporting Goods, Hobby, Book, & Music Stores", "Health and Personal Care Stores"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 0-(.3*180), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_RETAIL_PRODUCTIVITY, "US Retail Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DETAILED_INDUSTRIES_TFP_ALL <- MAJOR_INDUSTRIES_TFP_BULK %>%
  filter(Measure == "Labor productivity" & Units == "Index (2017=100)") %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)


US_BIZ_LABOR_PRODUCTIVITY <- ggplot() + 
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_ALL, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Administrative and support services`,color= "Administrative & Support Services"), size = 1.25) +
  geom_line(data=filter(DETAILED_LABOR_PRODUCTIVITY_3_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Publishing`,color= "Information Publishing (incl. Software)"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Management of companies and enterprises`,color= "Management of Companies and Enterprises"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_2_DIGIT, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Professional, scientific, and technical services`,color= "Professional, Scientific, & Technical Services"), size = 1.25) +
  geom_line(data=filter(DETAILED_INDUSTRIES_TFP_ALL, Year >= as.Date("1993-01-01")), aes(x=Year,y= `Data processing, internet publishing, and other information services`,color= "Computing Infrastructure, Data Processing, Web Search, & Related"), size = 1.25) +
  annotate("text", label = "NOTE: Y-axis truncated\nfor subcategories with\nfast long-run growth\nto help legibility", x = as.Date("2018-12-01"), hjust = 0.5, y = 70, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,130), breaks = c(60,70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index 2019 = 100") +
  ggtitle("US Business Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Labor Productivity Grew Quickly in Computing Infra, Management, Information, & Professional Services") +
  theme_apricitas + theme(legend.position = c(.42,.84), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  scale_color_manual(name= "Labor Productivity Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Computing Infrastructure, Data Processing, Web Search, & Related","Management of Companies and Enterprises","Information Publishing (incl. Software)", "Professional, Scientific, & Technical Services", "Administrative & Support Services")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 60-(.3*70), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_BIZ_LABOR_PRODUCTIVITY, "US Biz Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_BIZ_CAPITAL_INTENSITY_ALL <- MAJOR_INDUSTRIES_TFP_BULK %>%
  filter(Measure == "Contribution of capital intensity to labor productivity" & Units == "Index (2017=100)") %>%
  group_by(NAICS) %>%
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(Industry,Value,Year) %>%
  pivot_wider(names_from = Industry, values_from = Value) %>%
  mutate(Year = as.Date(paste0(Year, "-01-01"))) %>%
  mutate(across(where(is.numeric), ~ . /.[33])*100)  




  
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

DETAILED_WAGE_PER_HOUR <- DETAILED_PRODUCTIVITY_BULK %>%
  filter(Measure %in% c("Sectoral output","Hours worked","Sectoral output price deflator")) %>%
  filter(Units != "% Change from previous year") %>%
  filter(Measure != "Hours worked" | (Measure == "Hours worked" & Units == "Millions of hours")) %>%
  filter(Measure != "Sectoral output" | (Measure == "Sectoral output" & Units == "Millions of current dollars")) %>%
  select(-Units) %>%
  mutate(Value = as.numeric(gsub(",","",Value))) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(`Nominal output per hour worked` = `Sectoral output`/`Hours worked`, `Real output per hour worked` = `Nominal output per hour worked`/`Sectoral output price deflator`*100)

#Compare to nominal compensation
#compare to wage levels

#Business Sector
CAN_PRODUCTIVITY <- statcan_data("36-10-0206-01", "eng") %>%
  filter(VECTOR == "v1409153" & REF_DATE >= as.Date("2015-01-01")) %>%
  transmute(date = REF_DATE, value = VALUE)

#Market Sector
AUS_PRODUCTIVITY <- read_abs(series_id = "A3606058X") %>%
  mutate(date = date %m+% months(1)) %>%
  filter(date >= as.Date("2015-01-01")) %>%
  select(date,value)

#Market Sector
UK_PRODUCTIVITY <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/labourproductivity/timeseries/gyy7/prdy") %>%
  setNames(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2015-01-01"))

EU_PRODUCTIVITY_BULK <- get_eurostat("namq_10_lp_ulc",legacy_bulk_download = FALSE)

FRA_PRODUCTIVITY <- EU_PRODUCTIVITY_BULK %>%
  filter(geo == "FR", s_adj == "SCA", na_item == "RLPR_HW", unit == "I20", TIME_PERIOD >= as.Date("2015-01-01")) %>%
  transmute(date = TIME_PERIOD, value = values)

GER_PRODUCTIVITY <- EU_PRODUCTIVITY_BULK %>%
  filter(geo == "DE", s_adj == "SCA", na_item == "RLPR_HW", unit == "I20", TIME_PERIOD >= as.Date("2015-01-01")) %>%
  transmute(date = TIME_PERIOD, value = values)

ITA_PRODUCTIVITY <- EU_PRODUCTIVITY_BULK %>%
  filter(geo == "IT", s_adj == "SCA", na_item == "RLPR_HW", unit == "I20", TIME_PERIOD >= as.Date("2015-01-01")) %>%
  transmute(date = TIME_PERIOD, value = values)

JPN_GDP <- read.csv("https://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2024/qe242/tables/gaku-jk2421.csv",fileEncoding="latin1") %>%
  slice(-1:-6) %>%
  select(X) %>%
  transmute(date = seq.Date(from = as.Date("1994-01-01"), by = "quarter", length.out = nrow(.)), value = as.numeric(gsub(",","",X))) %>%
  drop_na() %>%
  filter(date >= as.Date("2015-01-01")) %>%
  mutate(value = value/value[7]*100)

#DOWNLOADED FROM HERE: https://dashboard.e-stat.go.jp/en/dataSearch
#SELECT TOTAL HOURS WORKED AND EMPLOYMENT LEVEL
JPN_AVG_HOURS_WORKED <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/US%20Service%20Sector%20Productivity/JPN_HOURS_WORKED.csv") %>%
  mutate(date = floor_date(as.Date(date), "quarter")) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarize(value = mean(value, na.rm = TRUE))

JPN_EMP_LEVEL <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/US%20Service%20Sector%20Productivity/JPN_EMPLOYMENT_LEVEL.csv") %>%
  mutate(date = floor_date(as.Date(date), "quarter")) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarize(value = mean(value, na.rm = TRUE))

JPN_HOURS_WORKED <- merge(JPN_EMP_LEVEL,JPN_AVG_HOURS_WORKED, by = "date") %>%
  transmute(date, value = value.x*value.y)

JPN_PRODUCTIVITY <- merge(JPN_GDP,JPN_HOURS_WORKED, by = "date") %>%
  transmute(date, value = value.x/value.y)

PRODUCTIVITY_2015_Graph <- ggplot() + #RGDP Index
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 0.5) +
  geom_line(data=UK_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Canada"), size = 1.25) +
  geom_line(data=GER_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "France"), size = 1.25) +
  geom_line(data=AUS_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Australia"), size = 1.25) +
  geom_line(data=JPN_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Japan"), size = 1.25) +
  geom_line(data=filter(OVERALL_PRODUCTIVITY, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "United States"), size = 1.25) +
  #annotate(geom = "text", label = "USE FIGURES WITH CAUTION:\n Ukrainian Refugees Boosted Pop Growth Significantly, Especially in Germany (~1.2%),\n But Also in Canada (~0.5%), Italy (~0.3%), the UK (~0.2%), and France (~0.2%)", x = as.Date("2020-03-15"), y = 107.5, color ="white", size = 4, alpha = 0.75,lineheight = 0.9) +
  #annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,120), breaks = c(90,100,110,120), expand = c(0,0)) +
  ylab("Index, 2015 Q1 = 100") +
  ggtitle("Comparing Labor Productivity Growth") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED & National Databases",subtitle = "America Has Radically Outshined Its Peers In Terms of Productivity Growth") +
  theme_apricitas + theme(legend.position = c(.19,.69), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Labor Productivity\n(Real Output Per Hour Worked)\n2015 Q1 = 100",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93","#B30089"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-90-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-90-as.Date("2015-01-01"))), ymin = 95-(.3*25), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PRODUCTIVITY_2015_Graph, "Productivity Comparison 2015 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PRODUCTIVITY_2019_Graph <- ggplot() + #RGDP Index
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  geom_line(data=UK_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Canada"), size = 1.25) +
  geom_line(data=GER_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "France"), size = 1.25) +
  geom_line(data=AUS_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Australia"), size = 1.25) +
  geom_line(data=JPN_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Japan"), size = 1.25) +
  geom_line(data=filter(OVERALL_PRODUCTIVITY, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[19]*100,color= "United States"), size = 1.25) +
  annotate("text",label = "Pre-COVID Labor Productivity", x = as.Date("2016-03-01"), y =100.75, color = "white", size = 4) +
  #annotate(geom = "text", label = "USE FIGURES WITH CAUTION:\n Ukrainian Refugees Boosted Pop Growth Significantly, Especially in Germany (~1.2%),\n But Also in Canada (~0.5%), Italy (~0.3%), the UK (~0.2%), and France (~0.2%)", x = as.Date("2020-03-15"), y = 107.5, color ="white", size = 4, alpha = 0.75,lineheight = 0.9) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(92.5,117.5), breaks = c(90,95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("Comparing Labor Productivity Growth") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED & National Databases",subtitle = "America Has Radically Outshined Its Peers In Terms of Productivity Growth") +
  theme_apricitas + theme(legend.position = c(.19,.70), legend.key.height = unit(0,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Labor Productivity\n(Real Output Per Hour Worked)\n2019 Q3 = 100",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93","#B30089"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-90-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-90-as.Date("2015-01-01"))), ymin = 92.5-(.3*25), ymax = 92.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PRODUCTIVITY_2019_Graph, "Productivity Comparison 2019 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIZ_APPS <- fredr("BABATOTALSAUS", observation_start = as.Date("2016-01-01"))
HPA_BIZ_APPS <- fredr("BAHBATOTALSAUS", observation_start = as.Date("2016-01-01"))
PLW_BIZ_APPS <- fredr("BAWBATOTALSAUS", observation_start = as.Date("2016-01-01"))

BIZ_APPS_GRAPH <- ggplot() + #Graphing Business Applications Data
  geom_line(data=BIZ_APPS, aes(x=date,y= value/1000, color= "Business Applications"), size = 1.25) +
  geom_line(data=HPA_BIZ_APPS, aes(x=date,y= value/1000, color= "Business Applications with High Propensity of Hiring"), size = 1.25) +
  geom_line(data=PLW_BIZ_APPS, aes(x=date,y= value/1000, color= "Business Applications with Planned Wages"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,700), expand = c(0,0)) +
  ylab("Number of Business Applications, Monthly") +
  ggtitle("America's New Business Boom") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Americans Continue to Found New Businesses at Near-Record Rates") +
  theme_apricitas + theme(legend.position = c(.325,.91)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Business Applications","Business Applications with High Propensity of Hiring","Business Applications with Planned Wages")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*700), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIZ_APPS_GRAPH, "Biz Apps.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



FIXED_IP_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T50306',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2014, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_IP_INVEST <- beaGet(FIXED_IP_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2014-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na() %>%
  mutate(RandD_Growth = t50306_y006rx_18_research_and_development_chained_dollars_level_6/lag(t50306_y006rx_18_research_and_development_chained_dollars_level_6,4)-1,Software_Growth = t50306_b985rx_17_software_chained_dollars_level_6/lag(t50306_b985rx_17_software_chained_dollars_level_6,4)-1) %>%
  drop_na()

FIXED_EQUIP_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50506',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2014, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_EQUIP_INVEST <- beaGet(FIXED_EQUIP_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2014-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na() %>%
  mutate(Compu_Growth = u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6/lag(u50506_b935rx_4_computers_and_peripheral_equipment_chained_dollars_level_6,4)-1) %>%
  drop_na()


FIXED_INVEST_GROWTH_GRAPH <- ggplot() + #growth in IP investment
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = FIXED_EQUIP_INVEST, aes(x=date, y = Compu_Growth, color = "Computers & Peripherals"), size = 1.25) + 
  geom_line(data = FIXED_IP_INVEST, aes(x=date, y = RandD_Growth, color = "Research and Development"), size = 1.25) + 
  geom_line(data = FIXED_IP_INVEST, aes(x=date, y = Software_Growth, color = "Software"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.15,0.35), breaks = c(-.1,0,0.1,0.2,.3), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Real Investment, Year-on-Year Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Investment in Software, R&D, and Computers Was Strong During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.225,.925)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Software","Research and Development","Computers & Peripherals")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -.15-(.3*.5), ymax = -.15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVEST_GROWTH_GRAPH, "Fixed Invest Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

p_unload(all)
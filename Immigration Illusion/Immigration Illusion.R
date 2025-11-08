pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EMP_FOREIGN_BORN <- bls_api("LNU02073395", startyear = 2007, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  drop_na() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(n() == 12) %>%
  summarise(value = sum(value))

EMP_TOTAL <- bls_api("LNU02000000", startyear = 2007, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  drop_na() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(n() == 12) %>%
  summarise(value = sum(value))

EMP_TOTAL_FOREIGN_BORN_SHARE <- merge(EMP_FOREIGN_BORN,EMP_TOTAL, by = "year") %>%
  transmute(date = as.Date(paste0(year,"-01-01")),foreign_born_share = value.x/value.y)


EMP_TOTAL_FOREIGN_BORN_SHARE_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=EMP_TOTAL_FOREIGN_BORN_SHARE, aes(x=date,y= foreign_born_share,color= "Foreign-born Share of US Employment, 2007-2024"), size = 1.25) +
  geom_point(data=EMP_TOTAL_FOREIGN_BORN_SHARE, aes(x=date,y= foreign_born_share,color= "Foreign-born Share of US Employment, 2007-2024"), size = 3) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.15,.20), breaks = c(0.15,0.16,0.17,0.18,0.19,0.2), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Immigrants are Nearly 1/5 of US Workers") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Immigrants are a Record Share of the US Workforce, Rising Rapidly Before Trump's 2nd Term") +
  theme_apricitas + theme(legend.position = c(.4,.72)) +
  scale_color_manual(name=NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = .15-(.3*.05), ymax = .15) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_TOTAL_FOREIGN_BORN_SHARE_Graph, "Emp Total Foreign Born Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

UNLEVEL_FOREIGN_BORN <- bls_api("LNU03073395", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
LFLEVEL_FOREIGN_BORN <- bls_api("LNU01073395", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

UNRATE_FOREIGN_BORN <- merge(UNLEVEL_FOREIGN_BORN,LFLEVEL_FOREIGN_BORN, by = "date") %>%
  transmute(date, UNLEVEL = value.x, LFLEVEL = value.y, UNRATE = UNLEVEL/LFLEVEL, ROLLUNRATE = c(0,0,0,0,0,0,0,0,0,0,0,roll_mean(UNRATE,12)))

UNLEVEL_NATIVE_BORN <- bls_api("LNU03073413", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
LFLEVEL_NATIVE_BORN <- bls_api("LNU01073413", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

UNRATE_NATIVE_BORN <- merge(UNLEVEL_NATIVE_BORN,LFLEVEL_NATIVE_BORN, by = "date") %>%
  transmute(date, UNLEVEL = value.x, LFLEVEL = value.y, UNRATE = UNLEVEL/LFLEVEL, ROLLUNRATE = c(0,0,0,0,0,0,0,0,0,0,0,roll_mean(UNRATE,12)))


UNRATE_TOTAL_FOREIGN_BORN_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=filter(UNRATE_FOREIGN_BORN, date >= as.Date("2023-01-01")), aes(x=date,y= UNRATE,color= "Foreign-Born"), linetype = "dashed", size = 0.75, alpha = 0.5) +
  geom_line(data=filter(UNRATE_NATIVE_BORN, date >= as.Date("2023-01-01")), aes(x=date,y= UNRATE,color= "Native-Born"), linetype = "dashed", size = 0.75, alpha = 0.5) +
  geom_line(data=filter(UNRATE_FOREIGN_BORN, date >= as.Date("2023-01-01")), aes(x=date,y= ROLLUNRATE,color= "Foreign-Born"), size = 1.25) +
  geom_line(data=filter(UNRATE_NATIVE_BORN, date >= as.Date("2023-01-01")), aes(x=date,y= ROLLUNRATE,color= "Native-Born"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.03,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Unemployment Rate by Immigration Status") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Unemployment Rates for Native-Born Workers Are Inching Up Even Amidst Trump's  Crackdown") +
  theme_apricitas + theme(legend.position = c(.2,.82)) +
  scale_color_manual(name="Solid = 12M Moving Average,\nDashed = Monthly, NSA",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Native-Born","Foreign-Born")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-02-01")-(.1861*(today()-as.Date("2023-02-01"))), xmax = as.Date("2023-02-01")-(0.049*(today()-as.Date("2023-02-01"))), ymin = 0.03-(.3*.02), ymax = 0.03) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNRATE_TOTAL_FOREIGN_BORN_Graph, "UNRATE Total Foreign Born Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



EMLEVEL_FOREIGN_25_34 <- bls_api("LNU02073399", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EMLEVEL_FOREIGN_35_44 <- bls_api("LNU02073400", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EMLEVEL_FOREIGN_45_54 <- bls_api("LNU02073401", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

EMLEVEL_FOREIGN_25_54 <- rbind(EMLEVEL_FOREIGN_25_34,EMLEVEL_FOREIGN_35_44,EMLEVEL_FOREIGN_45_54) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup()

EMLEVEL_NATIVE_25_34 <- bls_api("LNU02073417", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EMLEVEL_NATIVE_35_44 <- bls_api("LNU02073418", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EMLEVEL_NATIVE_45_54 <- bls_api("LNU02073419", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

EMLEVEL_NATIVE_25_54 <- rbind(EMLEVEL_NATIVE_25_34,EMLEVEL_NATIVE_35_44,EMLEVEL_NATIVE_45_54) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup()

  
POPLEVEL_FOREIGN_25_34 <- bls_api("LNU00073399", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
POPLEVEL_FOREIGN_35_44 <- bls_api("LNU00073400", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
POPLEVEL_FOREIGN_45_54 <- bls_api("LNU00073401", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

POPLEVEL_FOREIGN_25_54 <- rbind(POPLEVEL_FOREIGN_25_34,POPLEVEL_FOREIGN_35_44,POPLEVEL_FOREIGN_45_54) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup()


POPLEVEL_NATIVE_25_34 <- bls_api("LNU00073417", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
POPLEVEL_NATIVE_35_44 <- bls_api("LNU00073418", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
POPLEVEL_NATIVE_45_54 <- bls_api("LNU00073419", startyear = 2015, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

POPLEVEL_NATIVE_25_54 <- rbind(POPLEVEL_NATIVE_25_34,POPLEVEL_NATIVE_35_44,POPLEVEL_NATIVE_45_54) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup()


EMPRATE_FOREIGN_25_54 <- merge(EMLEVEL_FOREIGN_25_54,POPLEVEL_FOREIGN_25_54, by = "date") %>%
  transmute(date, value = value.x/value.y, rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))

EMPRATE_NATIVE_25_54 <- merge(EMLEVEL_NATIVE_25_54,POPLEVEL_NATIVE_25_54, by = "date") %>%
  transmute(date, value = value.x/value.y, rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))

EMRATE_TOTAL_FOREIGN_BORN_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=filter(EMPRATE_FOREIGN_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= value,color= "Foreign-Born"), linetype = "dashed", size = 0.75, alpha = 0.5) +
  geom_line(data=filter(EMPRATE_NATIVE_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= value,color= "Native-Born"), linetype = "dashed", size = 0.75, alpha = 0.5) +
  geom_line(data=filter(EMPRATE_FOREIGN_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= rollmean,color= "Foreign-Born"), size = 1.25) +
  geom_line(data=filter(EMPRATE_NATIVE_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= rollmean,color= "Native-Born"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.7,0.85), breaks = c(0.7,0.75,0.8,0.85), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Prime Age (25-54) Employment Rates") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Employment Rates for Native Born & Immigrants are Flat Despite Trump's Immigration Crackdown") +
  theme_apricitas + theme(legend.position = c(.2,.85)) +
  scale_color_manual(name="Solid = 12M Moving Average,\nDashed = Monthly, NSA",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Native-Born","Foreign-Born")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0.7-(.3*0.15), ymax = 0.7) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMRATE_TOTAL_FOREIGN_BORN_Graph, "EMRATE Total Foreign Born Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

POPLEVEL_NATIVE_16_24 <- bls_api("LNU00073416", startyear = 2016, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

POPLEVEL_NATIVE_16_54 <- rbind(POPLEVEL_NATIVE_16_24,POPLEVEL_NATIVE_25_34,POPLEVEL_NATIVE_35_44,POPLEVEL_NATIVE_45_54) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup()


TOTAL_NATIVE_BORN_POP_DATA <- ggplot() + #plotting Wage Growth
  #geom_line(data=filter(POPLEVEL_NATIVE_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= value/1000,color= "Official Native-Born Population Level, BLS"), size = 1.25) +
  geom_line(data=filter(POPLEVEL_NATIVE_16_54, date >= as.Date("2016-01-01")), aes(x=date,y= value/1000,color= "Official Native-Born Population Level, Age 16 to 54\nBLS data from Current Population Survey"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(99.5,103.5), breaks = c(100,101,102,103), expand = c(0,0)) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(212.5,227.5), breaks = c(210,215,220,225,230), expand = c(0,0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(134,140), breaks = c(134,135,136,137,138,139,140), expand = c(0,0)) +
  ylab("Millions") +
  ggtitle("Native-Born Population Data is Misleading") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Official Counts of Native Born Populations are Skyrocketing, Something That's Physically Impossible") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name=NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 134-(.3*6), ymax = 134) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TOTAL_NATIVE_BORN_POP_DATA, "Total Native Born Pop Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HISPANIC_POP_COUNTS <- bls_api("LNU00000009", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

HISPANIC_FOREIGN_BORN <- bls_api("LNU00073407", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_SHARE_HISPANIC <- merge(HISPANIC_FOREIGN_BORN,HISPANIC_POP_COUNTS, by = "date") %>%
  transmute(date, value = value.x/value.y, rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))

ASIAN_NATIVE_BORN <- bls_api("LNU00073424", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

ASIAN_FOREIGN_BORN <- bls_api("LNU00073406", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_SHARE_ASIAN <- merge(ASIAN_FOREIGN_BORN,ASIAN_NATIVE_BORN, by = "date") %>%
  transmute(date, value = value.x/(value.y+value.x), rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))


BLACK_NATIVE_BORN <- bls_api("LNU00073423", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

BLACK_FOREIGN_BORN <- bls_api("LNU00073405", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_SHARE_BLACK <- merge(BLACK_FOREIGN_BORN,BLACK_NATIVE_BORN, by = "date") %>%
  transmute(date, value = value.x/(value.y+value.x), rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))


WHITE_NATIVE_BORN <- bls_api("LNU00073422", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

WHITE_FOREIGN_BORN <- bls_api("LNU00073404", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_SHARE_WHITE <- merge(WHITE_FOREIGN_BORN,WHITE_NATIVE_BORN, by = "date") %>%
  transmute(date, value = value.x/(value.y+value.x), rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))



NATIVE_BORN <- bls_api("LNU00073413", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_BORN <- bls_api("LNU00073395", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_SHARE <- merge(FOREIGN_BORN,NATIVE_BORN, by = "date") %>%
  transmute(date, value = value.x/(value.y+value.x), rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))


FOREIGN_SHARE_HISPANIC_Graph <- ggplot() + #plotting Wage Growth
  #geom_line(data=filter(POPLEVEL_NATIVE_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= value/1000,color= "Official Native-Born Population Level, BLS"), size = 1.25) +
  geom_line(data=filter(FOREIGN_SHARE_HISPANIC, date >= as.Date("2020-01-01")), aes(x=date,y= value,color= "% of Hispanic/Latino Respondents Identifying as Foreign-Born\nBLS data from Current Population Survey"), size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump's\nInauguration", x = as.Date("2025-02-01"), y = .47, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  
  #geom_line(data=filter(FOREIGN_SHARE_ASIAN, date >= as.Date("2006-01-01")), aes(x=date,y= value,color= "Non-Hispanic Asian"), size = 1.25) +
  #geom_line(data=filter(FOREIGN_SHARE_BLACK, date >= as.Date("2018-01-01")), aes(x=date,y= value,color= "Black"), size = 1.25) +
  #geom_line(data=filter(FOREIGN_SHARE_WHITE, date >= as.Date("2018-01-01")), aes(x=date,y= value,color= "White"), size = 1.25) +
  #geom_line(data=filter(FOREIGN_SHARE, date >= as.Date("2018-01-01")), aes(x=date,y= value,color= "Foreign Share"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(99.5,103.5), breaks = c(100,101,102,103), expand = c(0,0)) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(212.5,227.5), breaks = c(210,215,220,225,230), expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.43,.48), breaks = c(.43,.44,.45,.46,.47,.48,.49,.50), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Hispanics are Less Likely to ID as Immigrants") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Since Trump's Inauguration, Hispanic CPS Respondents are Less Likely to Say They're Immigrants") +
  theme_apricitas + theme(legend.position = c(.4,.9)) +
  scale_color_manual(name=NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 134-(.3*6), ymax = 134) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = FOREIGN_SHARE_HISPANIC_Graph, "Foreign Share Hispanic Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

POP_FOREIGN_BORN <- bls_api("LNU00073395", startyear = 2025, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

TOTAL_FOREIGN_BORN_POPULATION_Graph <- ggplot() + #plotting Foreign Population
  geom_line(data=POP_FOREIGN_BORN, aes(x=date,y= value/1000,color= "Foreign-born Population, 2025\nBLS data from Current Population Survey"), size = 1.25) +
  geom_point(data=POP_FOREIGN_BORN, aes(x=date,y= value/1000,color= "Foreign-born Population, 2025\nBLS data from Current Population Survey"), size = 3) +
  annotate("text", label = "NOTE: DATA IS INACCURATE\nONLY SHOWN FOR DEMONSTRATION PURPOSES", x = as.Date("2025-01-01"), y = 49.15, color = "#EE6055", size = 5, hjust = 0, alpha = 0.75,lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(48,51), breaks = c(48,49,50,51), expand = c(0,0)) +
  ylab("Millions") +
  ggtitle("Official Immigrant Counts are Dropping") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Official Immigrant Counts are Dropping, Though Are Unrepresentative of Actual Immigrant Populations") +
  theme_apricitas + theme(legend.position = c(.6,.82)) +
  scale_color_manual(name=NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-10")-(.1861*(today()-as.Date("2025-01-10"))), xmax = as.Date("2025-01-10")-(0.049*(today()-as.Date("2025-01-10"))), ymin = 48-(.3*3), ymax = 48) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TOTAL_FOREIGN_BORN_POPULATION_Graph, "Total Foreign Born Population Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

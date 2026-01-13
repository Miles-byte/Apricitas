pacman::p_load(roll,sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EMP_FOREIGN_BORN <- bls_api("LNU02073395", startyear = 2007, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  select(date, value, seriesID) %>%
  arrange(date) %>%
  drop_na() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(n() == 12) %>%
  summarise(value = sum(value))

EMP_TOTAL <- bls_api("LNU02000000", startyear = 2007, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
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
  geom_line(data=EMP_TOTAL_FOREIGN_BORN_SHARE, aes(x=date,y= foreign_born_share,color= "Foreign-born Share of US Employment, 2007-2025"), size = 1.25) +
  geom_point(data=EMP_TOTAL_FOREIGN_BORN_SHARE, aes(x=date,y= foreign_born_share,color= "Foreign-born Share of US Employment, 2007-2025"), size = 3) +
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
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  transmute(date, UNLEVEL = value.x, LFLEVEL = value.y, UNRATE = UNLEVEL/LFLEVEL, ROLLUNRATE = c(roll_mean(UNRATE,12)))

UNLEVEL_NATIVE_BORN <- bls_api("LNU03073413", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
LFLEVEL_NATIVE_BORN <- bls_api("LNU01073413", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

UNRATE_NATIVE_BORN <- merge(UNLEVEL_NATIVE_BORN,LFLEVEL_NATIVE_BORN, by = "date") %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  transmute(date, UNLEVEL = value.x, LFLEVEL = value.y, UNRATE = UNLEVEL/LFLEVEL, ROLLUNRATE = c(roll_mean(UNRATE,12)))


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
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  transmute(date, value = value.x/value.y, rollmean = c(0,0,0,0,0,0,0,0,0,0,0,rollmean(value.x,12)/rollmean(value.y,12)))

EMPRATE_NATIVE_25_54 <- merge(EMLEVEL_NATIVE_25_54,POPLEVEL_NATIVE_25_54, by = "date") %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
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
  ungroup() %>%
  drop_na()


TOTAL_NATIVE_BORN_POP_DATA <- ggplot() + #plotting Wage Growth
  #geom_line(data=filter(POPLEVEL_NATIVE_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= value/1000,color= "Official Native-Born Population Level, BLS"), size = 1.25) +
  geom_line(data=filter(POPLEVEL_NATIVE_16_54, date >= as.Date("2016-01-01")), aes(x=date,y= value/1000,color= "Official Native-Born Population Level, Age 16 to 54\nBLS data from Current Population Survey"), size = 1.25) +
  annotate("text", label = "NOTE: DATA IS INACCURATE\nONLY SHOWN FOR\nDEMONSTRATION PURPOSES", x = as.Date("2016-01-01"), y = 137, color = "#EE6055", size = 5, hjust = 0, alpha = 0.75,lineheight = 0.8) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(99.5,103.5), breaks = c(100,101,102,103), expand = c(0,0)) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(212.5,227.5), breaks = c(210,215,220,225,230), expand = c(0,0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(134,141), breaks = c(134,135,136,137,138,139,140,141), expand = c(0,0)) +
  ylab("Millions") +
  ggtitle("Native-Born Population Data is Misleading") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Official Counts of Native Born Populations are Skyrocketing, Something That's Physically Impossible") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name=NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 134-(.3*7), ymax = 134) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TOTAL_NATIVE_BORN_POP_DATA, "Total Native Born Pop Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HISPANIC_POP_COUNTS <- bls_api("LNU00000009", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

HISPANIC_FOREIGN_BORN <- bls_api("LNU00073407", startyear = 2006, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

FOREIGN_SHARE_HISPANIC <- merge(HISPANIC_FOREIGN_BORN,HISPANIC_POP_COUNTS, by = "date") %>%
  #select(-latest.x,-latest.y) %>%
  drop_na() %>%
  transmute(date, value = value.x/value.y)

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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.42,.48), breaks = c(.42,.43,.44,.45,.46,.47,.48,.49,.50), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Hispanics are Less Likely to ID as Immigrants") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Since Trump's Inauguration, Hispanic CPS Respondents are Less Likely to Say They're Immigrants") +
  theme_apricitas + theme(legend.position = c(.4,.93)) +
  scale_color_manual(name=NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 134-(.3*6), ymax = 134) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = FOREIGN_SHARE_HISPANIC_Graph, "Foreign Share Hispanic Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

POP_FOREIGN_BORN <- bls_api("LNU00073395", startyear = 2025, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  #select(-latest) %>%
  drop_na()

TOTAL_FOREIGN_BORN_POPULATION_Graph <- ggplot() + #plotting Foreign Population
  geom_line(data=POP_FOREIGN_BORN, aes(x=date,y= value/1000,color= "Foreign-born Population, 2025\nBLS data from Current Population Survey"), size = 1.25) +
  geom_point(data=POP_FOREIGN_BORN, aes(x=date,y= value/1000,color= "Foreign-born Population, 2025\nBLS data from Current Population Survey"), size = 3) +
  annotate("text", label = "NOTE: DATA IS INACCURATE\nONLY SHOWN FOR\nDEMONSTRATION PURPOSES", x = as.Date("2025-01-01"), y = 49.15, color = "#EE6055", size = 5, hjust = 0, alpha = 0.75,lineheight = 0.8) +
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


BREAKEVEN_JOB_GROWTH <- data.frame(
  Names = c("Morgan Stanley", "Wells Fargo", "Deutsche Bank", "Goldman Sachs", "Bick (Stl Fed)", "Cheremukhin (Dal Fed)","Kolko (PIIE)", "Bostic (Atl Fed Prez)", "Musalem (Stl Fed Prez)", "Goolsbee (Chi Fed Prez)"),
  Breakeven = c(50,55,50,80,32,34,90,60,50,50)
) %>%
  mutate(Names = factor(Names, levels = rev(c("Morgan Stanley", "Wells Fargo", "Deutsche Bank", "Goldman Sachs", "Bick (Stl Fed)", "Cheremukhin (Dal Fed)","Kolko (PIIE)", "Bostic (Atl Fed Prez)", "Musalem (Stl Fed Prez)", "Goolsbee (Chi Fed Prez)"))))


BREAKEVEN_JOB_GROWTH_GRAPH <- ggplot(data = BREAKEVEN_JOB_GROWTH, aes(x = Names, y = Breakeven)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("Payroll Growth Required to Keep Unemployment Stable") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,100), expand = c(0,0)) +
  ggtitle("Forecasts of Breakeven Employment Growth\nAmidst Trump's Immigration Crackdown") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed's Cheremukhin Data") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip() +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = BREAKEVEN_JOB_GROWTH_GRAPH, "Breakeven Job Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



imm_vintages <- data.frame(
  vintage = c("2024 Estimates", "2023 Estimates", "2022 Estimates", "2021 Estimates"),
  `2020` = c(19885, 19886, 19893, 12247),
  `2021` = c(376004, 376008, 376029, 244622),
  `2022` = c(1693535, 999267, 1010923, NA),
  `2023` = c(2294299, 1138989, NA, NA),
  `2024` = c(2786119, NA, NA, NA),
  check.names = FALSE
) %>%
  pivot_longer(-vintage, names_to = "year", values_to = "value") %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  drop_na()


US_NET_IMMIGRATION_VINTAGES <- ggplot() + #plotting Wage Growth
  #geom_line(data=filter(POPLEVEL_NATIVE_25_54, date >= as.Date("2016-01-01")), aes(x=date,y= value/1000,color= "Official Native-Born Population Level, BLS"), size = 1.25) +
  geom_line(data=filter(imm_vintages, vintage == "2021 Estimates"), aes(x=date,y= value/1000000, color = "2021 Estimates"), size = 1.25) +
  geom_line(data=filter(imm_vintages, vintage == "2022 Estimates"), aes(x=date,y= value/1000000, color = "2022 Estimates"), size = 1.25) +
  geom_line(data=filter(imm_vintages, vintage == "2023 Estimates"), aes(x=date,y= value/1000000, color = "2023 Estimates"), size = 1.25) +
  geom_line(data=filter(imm_vintages, vintage == "2024 Estimates"), aes(x=date,y= value/1000000, color = "2024 Estimates"), size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(99.5,103.5), breaks = c(100,101,102,103), expand = c(0,0)) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(212.5,227.5), breaks = c(210,215,220,225,230), expand = c(0,0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,3), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Millions") +
  ggtitle("US Repeatedly Revised Up Migration Estimates") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Amidst Surging Immigration, the Census Bureau Struggled to Keep Up and Had to Revise Up Estimates") +
  theme_apricitas + theme(legend.position = c(.3,.65)) +
  scale_color_manual(name="US Net International Migration Vintages",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#9A348E"),breaks = c("2024 Estimates","2023 Estimates","2022 Estimates","2021 Estimates")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-02-01")-(.1861*(today()-600-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-600-as.Date("2020-01-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot", plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = US_NET_IMMIGRATION_VINTAGES, "US Net Immigration Vintages.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

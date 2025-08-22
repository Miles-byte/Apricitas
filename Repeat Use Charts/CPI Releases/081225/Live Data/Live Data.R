pacman::p_load(openxlsx,readxl,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("quantmod")
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


CPIPCT <- bls_api("CUUR0000SA0", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data
CPILFEPCT <- bls_api("CUUR0000SA0L1E", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12)) %>%
  subset(date >= as.Date("2019-01-01"))


CPIPCT_MOM <- bls_api("CUSR0000SA0", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data
CPILFEPCT_MOM <- bls_api("CUSR0000SA0L1E", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1)) %>%
  subset(date >= as.Date("2019-01-01"))

sprintf(
  "NEW: CPI Inflation rose to %s%% year-on-year, increasing %s%% month-on-month\n\nCore CPI inflation rose %s%% year-on-year, growing %s%% month-on-month",
  formatC(CPIPCT %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1),
  formatC(CPIPCT_MOM %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1),
  formatC(CPILFEPCT %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1),
  formatC(CPILFEPCT_MOM %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1)
)

CPIPCT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=CPILFEPCT, aes(x=date,y= value,color= "Core CPI"), size = 1.25) +
  geom_line(data=CPIPCT, aes(x=date,y= value,color= "CPI"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.091), breaks = c(0,0.03,0.06,0.09), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("The Inflation Situation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation Is Coming Down Off a 40 Year High") +
  theme_apricitas + theme(legend.position = c(.20,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI","Core CPI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*0.091), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPIPCT_Graph, "CPI PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



CPIPCT_COMMODITIES <- bls_api("CUUR0000SAC", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data

CPIPCT_DURABLES <- bls_api("CUUR0000SAD", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data

CPIPCT_NONDURABLES <- bls_api("CUUR0000SAN", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data


CPIPCT_GOODS_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CPIPCT_DURABLES, aes(x=date,y= value,color= "Nondurables"), size = 1.25) +
  geom_line(data=CPIPCT_NONDURABLES, aes(x=date,y= value,color= "Durables"), size = 1.25) +
  geom_line(data=CPIPCT_COMMODITIES, aes(x=date,y= value,color= "All Goods"), size = 2.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,0.20), breaks = c(-0.05,0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("The Goods Inflation Situation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Goods Inflation is Rebounding Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.20,.50)) +
  scale_color_manual(name= "CPI Growth, Year-on-Year",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("All Goods","Durables","Nondurables")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.05-(.3*0.25), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPIPCT_GOODS_Graph, "CPI PCT GOODS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



COFFEE_PRICES <- bls_api("CUUR0000SEFP01", startyear = 2016, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data


CPI_COFFEE_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=COFFEE_PRICES, aes(x=date,y= value,color= "CPI: Coffee Prices,\nYear-on-Year Growth"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,0.23), breaks = c(-0.05,0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("US Coffee Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Coffee Prices are Up ",round(COFFEE_PRICES$value[1]*100),"% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.20,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.05-(.3*0.28), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_COFFEE_Graph, "CPI COFFEE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

COFFEE_PRICES_INDEX <- bls_api("CUSR0000SEFP01", startyear = 2019, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value)/value[nrow(.)]-1)  %>%
  subset(date >= as.Date("2019-01-01")) 

CPI_COFFEE_INDEX_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=COFFEE_PRICES_INDEX, aes(x=date,y= value,color= "CPI: Coffee Prices,\nChange Since Jan 2019"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.05,0.40), breaks = c(-0.05,0,0.05,0.1,0.15,0.2,.25,.3,.35,.4), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("US Coffee Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Coffee Prices are Up ",round(COFFEE_PRICES$value[1]*100),"% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.05-(.3*0.45), ymax = -0.05) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_COFFEE_INDEX_Graph, "CPI COFFEE Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



ELECTRICITY_PRICES <- bls_api("CUUR0000SEHF01", startyear = 2016, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data


CPI_ELECTRICITY_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=ELECTRICITY_PRICES, aes(x=date,y= value,color= "CPI: Electicity Prices,\nYear-on-Year Growth"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.025,0.175), breaks = c(-0.05,0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("US Electricity Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Electricity Prices are Up ",round(ELECTRICITY_PRICES$value[1]*100),"% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.20,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.025-(.3*0.2), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_ELECTRICITY_Graph, "CPI ELECTRICITY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



USED_CAR_PRICES <- bls_api("CUUR0000SETA02", startyear = 2016, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data


CPI_USED_CAR_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=USED_CAR_PRICES, aes(x=date,y= value,color= "CPI: Used Car & Truck Prices,\nYear-on-Year Growth"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.15,0.5), breaks = c(-.1,0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("US Used Vehicle Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Used Vehicle Prices are Up ",round(USED_CAR_PRICES$value[1]*100),"% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.75,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.15-(.3*0.65), ymax = -0.15) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_USED_CAR_Graph, "CPI USED CAR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


AUDIO_EQUIPMENT_PRICES <- bls_api("CUUR0000SERA05", startyear = 2016, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data

CPI_AUDIO_EQUIPMENT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=AUDIO_EQUIPMENT_PRICES, aes(x=date,y= value,color= "CPI: Audio Equipment Prices,\nYear-on-Year Growth"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1,0.2), breaks = c(-.1,0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("US Audio Equipment Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Audio Equipment Prices are Up ",round(AUDIO_EQUIPMENT_PRICES$value[1]*100),"% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.30,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.1-(.3*0.3), ymax = -0.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_AUDIO_EQUIPMENT_Graph, "CPI AUDIO EQUIPMENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


APPLIANCE_PRICES <- bls_api("CUUR0000SEHK", startyear = 2016, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2017-01-01")) #cpi rent data

CPI_APPLIANCE_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=APPLIANCE_PRICES, aes(x=date,y= value,color= "CPI: Appliance Prices,\nYear-on-Year Growth"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1,0.1), breaks = c(-.1,0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("US Appliance Price Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Appliance Prices are Up ",round(APPLIANCE_PRICES$value[1]*100),"% Over the Last Year")) +
  theme_apricitas + theme(legend.position = c(.30,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -0.1-(.3*0.2), ymax = -0.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_APPLIANCE_Graph, "CPI APPLIANCE GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



Relative_Importance <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/RelativeImportance.csv") %>%
  `colnames<-`(c("Category","2018-01-01","2020-01-01","2022-01-01","2023-01-01")) %>%
  #select(-`2023-01-01`) %>% #DELETE THIS BEFORE JAN CPI
  pivot_longer(cols=c(-Category),names_to="Original_Vars")%>%
  pivot_wider(names_from=c(Category)) %>%
  mutate(Original_Vars = as.Date(Original_Vars)) %>%
  select(Original_Vars, `All items`,Food,Energy,`Commodities less food and energy commodities`,`Services less energy services`)%>%
  `colnames<-`(c("date","All","Food","Energy","Goods_LFE","Services_LE")) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  `colnames<-`(c("date","Category","value","Indicator")) %>%
  mutate(Indicator = "Relative_Importance")

CPI_ALL <- bls_api("CUUR0000SA0", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "All") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_FOOD <- bls_api("CUUR0000SAF1", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Food") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_ENERGY <- bls_api("CUUR0000SA0E", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Energy") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_COM_LFE <- bls_api("CUUR0000SACL1E", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Goods_LFE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_SERV_LE <- bls_api("CUUR0000SASLE", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Services_LE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

# CPI_ALL <- fredr(series_id = "CPIAUCNS",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "All") %>%
#   select(date,value,Category)
# CPI_FOOD <- fredr(series_id = "CPIUFDNS",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "Food")%>%
#   select(date,value,Category)
# CPI_ENERGY <- fredr(series_id = "CPIENGNS",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Energy")%>%
#   select(date,value,Category)
# CPI_COM_LFE <- fredr(series_id = "CUUR0000SACL1E",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Goods_LFE")%>%
#   select(date,value,Category)
# CPI_SERV_LE <- fredr(series_id = "CUUR0000SASLE",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Services_LE")%>%
#   select(date,value,Category)

CPI_Indices <- rbind(CPI_ALL,CPI_FOOD,CPI_ENERGY,CPI_COM_LFE,CPI_SERV_LE) %>%
  mutate(Indicator = "Index_NSA")

CPI_CONTRIBUTION <- rbind(CPI_Indices,Relative_Importance) %>%
  pivot_wider(names_from = "Indicator") %>%
  mutate_cond(Category == "All" & date < as.Date("2020-01-01") & date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/246.524*100) %>%
  mutate_cond(Category == "All" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/256.974*100) %>%
  mutate_cond(Category == "All" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/278.802*100) %>%
  mutate_cond(Category == "Food" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/251.238*13.38400) %>%
  mutate_cond(Category == "Food" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/259.823*13.771) %>%
  mutate_cond(Category == "Food" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/286.966*13.37) %>%
  mutate_cond(Category == "Energy" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/206.598*7.513) %>%
  mutate_cond(Category == "Energy" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/212.982*6.706) %>%
  mutate_cond(Category == "Energy" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/256.207*7.348) %>%
  mutate_cond(Category == "Goods_LFE" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/142.647*19.849000) %>%
  mutate_cond(Category == "Goods_LFE" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/142.920*20.137000) %>%
  mutate_cond(Category == "Goods_LFE" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/160.850*21.699000) %>%
  mutate_cond(Category == "Services_LE" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/322.250*59.254000) %>%
  mutate_cond(Category == "Services_LE" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/341.347*59.387) %>%
  mutate_cond(Category == "Services_LE" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/359.559*57.583)

CPI_RI_ANNUAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index_NSA), names_from = "Category", values_from = Relative_Importance) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  arrange(match(name, c("All","Food","Energy","Goods_LFE","Services_LE")))

CPI_CONTRIBUTION_ANNUAL <- CPI_CONTRIBUTION
CPI_CONTRIBUTION_ANNUAL$Relative_Importance <- CPI_RI_ANNUAL_CALCULATIONS$value
CPI_CONTRIBUTION_ANNUAL <- drop_na(CPI_CONTRIBUTION_ANNUAL)


# write.csv(CPI_CONTRIBUTION, "RI and Contrib.csv")
#making updated relative importance calculations
CPI_RI_FINAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index_NSA), names_from = "Category", values_from = Relative_Importance) %>%
  mutate(Food = Food/All*100) %>%
  mutate(Energy = Energy/All*100) %>%
  mutate(Goods_LFE = Goods_LFE/All*100) %>%
  mutate(Services_LE = Services_LE/All*100) %>%
  mutate(All = All/All*100) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  arrange(match(name, c("All","Food","Energy","Goods_LFE","Services_LE")))

CPI_CONTRIBUTION$Relative_Importance <- CPI_RI_FINAL_CALCULATIONS$value
CPI_CONTRIBUTION <- drop_na(CPI_CONTRIBUTION)
#CPI_CONTRIBUTION$January <- JANUARY$value

#adding seasonally adjusted data for seasonally adjusted monthly charts

CPI_ALL_SA <- bls_api("CUSR0000SA0", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "All") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_FOOD_SA <- bls_api("CUSR0000SAF1", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Food") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_ENERGY_SA <- bls_api("CUSR0000SA0E", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Energy") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_COM_LFE_SA <- bls_api("CUSR0000SACL1E", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Goods_LFE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_SERV_LE_SA <- bls_api("CUSR0000SASLE", startyear = 2017, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Services_LE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

# 
# CPI_ALL_SA <- fredr(series_id = "CUSR0000SA0",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "All") %>%
#   select(date,value,Category)
# CPI_FOOD_SA <- fredr(series_id = "CPIUFDSL",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "Food")%>%
#   select(date,value,Category)
# CPI_ENERGY_SA <- fredr(series_id = "CPIENGSL",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Energy")%>%
#   select(date,value,Category)
# CPI_COM_LFE_SA <- fredr(series_id = "CUSR0000SACL1E",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Goods_LFE")%>%
#   select(date,value,Category)
# CPI_SERV_LE_SA <- fredr(series_id = "CUSR0000SASLE",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Services_LE")%>%
#   select(date,value,Category)

CPI_Indices_SA <- rbind(CPI_ALL_SA,CPI_FOOD_SA,CPI_ENERGY_SA,CPI_COM_LFE_SA,CPI_SERV_LE_SA)

# JANUARY <- CPI_Indices_SA %>% #sometimes YOY calculations cross a weight update so I am creating a column for just the January relative importance data 
#   filter(month(ymd(date)) %in% c(1)) %>%
#   rowwise() %>%
#   mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
#   unnest(cols = c(date)) %>%
#   subset(date <= as.Date("2023-01-01"))

CPI_Indices_SA <- subset(CPI_Indices_SA, date > as.Date("2017-12-01"))

CPI_CONTRIBUTION_FINAL <- CPI_CONTRIBUTION %>%
  subset(., date > as.Date("2017-12-01")) %>%
  mutate(Index_SA = CPI_Indices_SA$value) %>%
  #mutate(January = JANUARY$value) %>%
  mutate(Monthly_Contribution_NSA = (Index_NSA/lead(Index_NSA))*lead(Relative_Importance)-lead(Relative_Importance)) %>%
  mutate(Monthly_Contribution_SA = (Index_SA/lead(Index_SA))*lead(Relative_Importance)-lead(Relative_Importance)) %>%
  #mutate(January_Contribution_NSA = lag(Relative_Importance,13)*(January-lag(Index_SA,12))/lag(Index_SA,12) + lag(Relative_Importance)*((Index_SA-January)/January)) %>%
  #mutate(January_Contribution_NSA = lag(Relative_Importance,12)*(January-lag(Index_SA,12))/lag(Index_SA,12) + lag(Relative_Importance)*((Index_SA-January)/January)) %>%
  mutate(Yearly_Contribution = (Index_NSA/lead(Index_NSA,12))*lead(Relative_Importance,12)-lead(Relative_Importance,12)) %>%
  drop_na() %>%
  subset(date >= as.Date("2019-01-01"))
  
# ALL <- subset(CPI_CONTRIBUTION_ANNUAL, Category == "All") %>%
#   mutate(All = Relative_Importance) %>%
#   select(date, All) %>%
#   bind_rows(replicate(3, ., simplify = FALSE))
# 
# JANUARY_ALL <- ALL %>% #sometimes YOY calculations cross a weight update so I am creating a column for just the January relative importance data 
#   filter(month(ymd(date)) %in% c(1)) %>%
#   rowwise() %>%
#   mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
#   unnest(cols = c(date)) 
# 
# JANUARY_RI <- CPI_CONTRIBUTION_ANNUAL %>% #sometimes YOY calculations cross a weight update so I am creating a column for just the January relative importance data 
#   subset(Category != "All") %>%
#   select(date, Category, Relative_Importance) %>%
#   filter(month(ymd(date)) %in% c(1)) %>%
#   rowwise() %>%
#   mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
#   unnest(cols = c(date)) 

#CPI_CONTRIBUTION_JANUARY <- CPI_CONTRIBUTION_ANNUAL %>%
  #mutate(Index_SA = CPI_Indices_SA$value) %>%
  #subset(Category != "All") %>%
  #mutate(JanuaryALL = JANUARY_ALL$All) %>%
  #mutate(JanuaryRI = as.numeric(JANUARY_RI$Relative_Importance)) %>%
  #mutate(All = ALL$All) %>%
  #mutate(January_Contribution_NSA = lag(Relative_Importance,13)*((January-lag(Index_SA,12))/lag(Index_SA,12)) + lag(Relative_Importance)*((Index_SA-January)/January)) %>%
  #mutate(All_January_Contribution_NSA = JANUARY_RI*lag(All,12)) %>%
  #mutate(All_January_Contribution_NSA = (JANUARY_RI/lag(All,12)-(lag(Relative_Importance,12)/lag(All,12)))+(Relative_Importance/JANUARY_ALL - JANUARY_RI/JANUARY_ALL)) %>%
  #select(date,Category,All_January_Contribution_NSA) %>%
  #subset(Category != "All") %>%
  #subset(date >= as.Date("2019-02-01")) %>%
  #pivot_wider(names_from = Category, values_from = January_Contribution_NSA) %>%
  #mutate()
  
#write.csv(CPI_CONTRIBUTION_FINAL,"CPICONTRIBFINAL.csv")


# CPI_CONTRIBUTION_JANUARY <- CPI_CONTRIBUTION_FINAL %>%
#   select(date,Category,January_Contribution_NSA) %>%
#   subset(Category != "All") %>%
#   subset(date >= as.Date("2019-02-01")) %>%
#   pivot_wider(names_from = Category, values_from = January_Contribution_NSA) #%>%
#   #mutate(Total = CPI_YOY_GROWTH$value) %>%
#   #mutate(test = Food + Energy + Goods_LFE + Services_LE - Total) %>%
#   #mutate(Food = Food *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
#   #mutate(Energy = Energy *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
#   #mutate(Goods_LFE = Goods_LFE *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
#   #mutate(Services_LE = Services_LE *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
  #mutate(test = Food + Energy + Goods_LFE + Services_LE - Total)
  
# CPI_CONTRIBUTION_JANUARY_GRAPH <- ggplot() + #plotting components of annual inflation
#   geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = January_Contribution_NSA/100, fill = Category), color = NA, size = 0, stat= "identity") +
#   #geom_line(data=CPI_YOY_GROWTH, aes(x=date, y=value/100, color="Annual Inflation"), size = 1.25) +
#   annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
#   xlab("Date") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.1), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
#   ylab("Annual Inflation, Percent") +
#   ggtitle("Pandemic Prices") +
#   labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is Now Mostly a Story about Food and Core Services Prices") +
#   theme_apricitas + theme(legend.position = c(.25,.80)) +
#   scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
#   #scale_color_manual(name = NULL,values = c("#A7ACD9")) +
#   annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.025-(.3*.125), ymax = -0.025) +
#   coord_cartesian(clip = "off")

# ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_JANUARY_GRAPH, "CPI January.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CPI_CONTRIBUTION_ANNUAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = Yearly_Contribution/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.1), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation Now Mostly Comes From Increases in Food and Core Services Prices") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.025-(.3*.125), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_ANNUAL_GRAPH, "CPI Annual.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CPI_CONTRIBUTION_MONTHLY_NSA_GRAPH <- ggplot() + #plotting components of monthly inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = Monthly_Contribution_NSA/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.01,.0175), breaks = c(-.01,-0.005,0,0.005,.01,.015), expand = c(0,0)) +
  ylab("Monthly Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Dropping Energy Prices Pulled Inflation Down In August") +
  theme_apricitas + theme(legend.position = c(.275,.80)) +
  scale_fill_manual(name= "Contributions to Monthly CPI Inflation (NSA)",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.01-(.3*.025), ymax = -0.01) +
  coord_cartesian(clip = "off")

CPI_CONTRIBUTION_ANNUAL %>% subset(CPI_CONTRIBUTION_ANNUAL, date>= as.Date("2019-01-01"))

CPI_CONTRIBUTION_MONTHLY_SA_GRAPH <- ggplot() + #plotting components of monthly inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All" & date >= as.Date("2019-01-01")), aes(x = date, y = Monthly_Contribution_SA/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.01,.015), breaks = c(-.01,-0.005,0,0.005,.01,.015), expand = c(0,0)) +
  ylab("Monthly Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Monthly Inflation is Now Mostly a Core Services Story") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Monthly CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.01-(.3*.025), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_MONTHLY_SA_GRAPH, "CPI RENT Contrib SA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ZORI <- read.csv("https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_month.csv?t=1665666510") %>%
  select(-RegionID, -SizeRank, - RegionType, - StateName) %>%
  subset(RegionName == "United States") %>%
  #transpose() %>%
  gather(key = "date", value = "value", -1) %>%
  #`colnames<-`(.[1, ]) %>%
  mutate(date = c(seq(as.Date("2015-01-01"), length = nrow(.), by = "months"))) %>%
  #.[-1, ] %>%
  mutate(value = (value-lag(value,12))/lag(value,12))

CPIRENT <- bls_api("CUSR0000SEHA", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data
CPIORENT <- bls_api("CUSR0000SEHC", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12)) %>%
  subset(date >= as.Date("2019-01-01"))
#cpi owners equivalent rent

ApartmentList <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/091322/apartmentlist.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

NTRR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/NTRR/NTRR.csv") %>%
  mutate(date = as.Date(date))

CPI_Rent_Zillow <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CPIRENT, aes(x=date,y= (value) ,color= "CPI Rent"), size = 1.25) +
  geom_line(data=CPIORENT, aes(x=date,y= (value) ,color= "CPI Owner's Equivalent Rent"), size = 1.25) +
  geom_line(data=subset(ZORI, date >= as.Date("2018-01-01")), aes(x=date+365,y= (value) ,color= "Zillow Observed Rent Index, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(ApartmentList, date >= as.Date("2018-01-01")), aes(x=date+365,y= annualpct ,color= "ApartmentList Median New Lease, Lagged 1 Year"), size = 1.25) +
  #geom_line(data=subset(NTRR,date > as.Date("2018-01-01")), aes(x=date+365,y= NTRR/100,color= "New Tenant Repeat Rent Index, Lagged 1 Year"), size = 1.25)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,.20), breaks = c(0,.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Leading Indicators Show Rent Deceleration") +
  labs(caption = "Graph created by @JosephPolitano using BLS,Zillow, and ApartmentList data",subtitle = "Zillow and ApartmentList Data Show Rent Growth Decelerating Significantly Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.32,.90)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent","CPI Owner's Equivalent Rent","Zillow Observed Rent Index, Lagged 1 Year","ApartmentList Median New Lease, Lagged 1 Year","New Tenant Repeat Rent Index, Lagged 1 Year")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+365-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+365-as.Date("2019-01-01"))), ymin = -0.025-(.3*0.225), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Rent_Zillow, "CPI RENT ZILLOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Manheim_Bulk <- read.xlsx("https://site.manheim.com/wp-content/uploads/sites/2/2025/06/Jun-2025-Manheim-Used-Vehicle-Value-Index.xlsx") %>%
  mutate(date = seq.Date(from = as.Date("1997-01-01"), by = "month", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-11-01"))

CPIUSEDCARS <- bls_api("CUSR0000SETA02", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  subset(date >= as.Date("2019-01-01"))

CPI_Manheim_Used_Car_Vehicles_Graph <- ggplot() + #plotting "Used Cars and Trucks" and "Mannheim" price Indexes
  geom_line(data=CPIUSEDCARS, aes(x=date,y= (value/value[nrow(CPIUSEDCARS)])*100 ,color= "CPI: Used Cars and Trucks"), size = 1.25) +
  geom_line(data=subset(Manheim_Bulk, date > as.Date("2018-12-31")), aes(x=date,y= (`Index.(1/97.=.100)`/`Index.(1/97.=.100)`[1])*100 ,color= "Manheim Used Vehicles Value Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,180), breaks = c(90,120,150,180), expand = c(0,0)) +
  ylab("Index, January 2019 = 100") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Manheim data",subtitle = "Manheim Wholesale Data Could Be Giving Early Indications of Movements in Used Car Prices") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(name= "January 2019 = 100",values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 90-(.3*90), ymax = 90) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Manheim_Used_Car_Vehicles_Graph, "CPI Manheim Used Vehicles.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CPIUSEDCARS_GROWTH <- bls_api("CUUR0000SETA02", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))/lag(value,12)) %>%
  subset(date >= as.Date("2019-01-01"))
  
CPI_Manheim_Used_Car_Vehicles_Growth_Graph <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=CPIUSEDCARS_GROWTH, aes(x=date,y= (value) ,color= "CPI: Used Cars and Trucks, Annual Growth"), size = 1.25) +
  geom_line(data=Manheim_Bulk, aes(x=date+60,y= `NSA.Price.%.YoY`,color= "Manheim Used Vehicles Value Index, Annual Growth, Lagged 2 Months"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.25,.60), breaks = c(-0.2,0,0.2,0.4,0.6), expand = c(0,0)) +
  ylab("Year-on-year Percent Growth, %") +
  ggtitle("Used Vehicle Prices are Dropping") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Manheim Wholesale Data Suggests Official Used Car Prices Should Decelerate") +
  theme_apricitas + theme(legend.position = c(.46,.085)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Manheim Used Vehicles Value Index, Annual Growth, Lagged 2 Months","CPI: Used Cars and Trucks, Annual Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -.25-(.3*0.85), ymax = -.25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Manheim_Used_Car_Vehicles_Growth_Graph, "CPI Manheim Used Vehicles Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPIRENTmonth <- bls_api("CUSR0000SEHA", startyear = 2018, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1)) %>%
  subset(date >= as.Date("2019-01-01"))

CPIOERmonth <- bls_api("CUSR0000SEHC", startyear = 2018, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1)) %>%
  subset(date >= as.Date("2019-01-01"))

CPI_Rent_Month <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  geom_line(data=CPIRENTmonth, aes(x=date,y= (value) ,color= "CPI Rent: Monthly Percentage Growth"), size = 1.25) +
  geom_line(data=CPIOERmonth, aes(x=date,y= (value) ,color= "CPI Owner's Equivalent Rent: Monthly Percentage Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .25),limits = c(0,.010), breaks = c(0,.0025,0.005,.0075), expand = c(0,0)) +
  ylab("Monthly Percent Growth, %") +
  ggtitle("Housing Inflation is Cooling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Official Housing Price Growth is Decelerating, Though Remains Elevated") +
  theme_apricitas + theme(legend.position = c(.36,.95)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("CPI Rent: Monthly Percentage Growth","CPI Owner's Equivalent Rent: Monthly Percentage Growth")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*0.010), ymax = 0.00) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Rent_Month, "CPI Rent Month.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
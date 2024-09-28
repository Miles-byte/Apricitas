pacman::p_load(ggpubr,ggarrange,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("quantmod")
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


FIVEYEARBREAKEVEN2018 <- fredr(series_id = "T5YIE", observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) %>% #5 Year Inflation Breakevens data
  drop_na()
FIVEYEARFWDBREAKEVEN2018 <- fredr(series_id = "T5YIFR",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) %>% # 5 year 5 year forward breakevens data
  drop_na()

T5YIE2018_GRAPH <- ggplot() + #plotting inflation breakevens
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.0225, ymax = 0.0275, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=FIVEYEARFWDBREAKEVEN2018, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARBREAKEVEN2018, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  annotate("text", label = "Breakevens Approximately Consistent\nWith 2% Inflation Target", x = as.Date("2018-01-01"), y = 0.0295, color = "#EE6055", alpha = 0.6, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Inflation Expectations Were\nBelow Target in the Low-Demand\n2010s Macroeconomic Environment", x = as.Date("2017-11-01"), hjust = 0, y = 0.0115, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "5 Year Inflation Expectations\nSurged Above Target in 2022", x = as.Date("2022-06-01"), hjust = 0, y = 0.035, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Tighter Monetary Policy Brought\nInflation Expectation Back Down", x = as.Date("2023-01-01"), hjust = 0, y = 0.017, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Long-Run Market Inflation Expectations") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Market-Based Inflation Expectations Surged in 2022 and Normalized After Monetary Policy Tightened") +
  theme_apricitas + theme(legend.position = c(.30,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.038), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = T5YIE2018_GRAPH, "Inflation Expectations.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

test <- beaParams(Sys.getenv("BEA_KEY"),"NIPA")

test2 <- beaParamVals(Sys.getenv("BEA_KEY"),"NIPA","TableName")

BEA_NGDP_NSA_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "NIPA", # Specify dataset
  "TableName" = "T80105", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "Year" =  paste(seq(from = 2010, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_NGDP_NSA_YOY <- beaGet(BEA_NGDP_NSA_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2010-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  select(date,`T80105 NA000334 1 Gross domestic product Current Dollars (Period Rate) Level 6`) %>%
  setNames(c("date","NGDP")) %>%
  mutate(NGDP = (NGDP-lag(NGDP,4))/lag(NGDP,4))
  
BEA_NGDP_INDEX_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "NIPA", # Specify dataset
  "TableName" = "T10105", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "Year" =  paste(seq(from = 2010, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_NGDP_INDEX <- beaGet(BEA_NGDP_INDEX_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2010-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  select(date,`T10105 A191RC 1 Gross domestic product Current Dollars Level 6`) %>%
  setNames(c("date","NGDP")) %>%
  mutate(
    growth_rate = (log(NGDP[date == as.Date("2019-10-01")] / NGDP[date == as.Date("2016-01-01")])) / 15
  ) %>%
  arrange(date) %>%
  #mutate(Trend = c(rep(NA,39),NGDP[40],rep(NGDP[40]*(1 + growth_rate[40])^(nrow(.)-40),nrow(.)-40))) %>%
  mutate(Trend = c(
    rep(NA, 39), 
    NGDP[40], 
    sapply(41:n(), function(i) NGDP[40] * (1 + growth_rate[40])^(i - 40))
  ))
  
NGDP_GRAPH <- ggplot() + #plotting inflation breakevens
  geom_line(data=filter(BEA_NGDP_INDEX, date>=as.Date("2016-01-01")), aes(x=date,y= Trend/1000000,color= "2016-2020 Trend"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(BEA_NGDP_INDEX, date>=as.Date("2016-01-01")), aes(x=date,y= NGDP/1000000,color= "Nominal GDP"), size = 1.25) +
  annotate("rect", xmin = as.Date("2019-10-01"), xmax = as.Date("2020-04-01"), ymin = -Inf, ymax = 29, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2021-04-01"), ymin = -Inf, ymax = 29, fill = "#00A99D", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2023-01-01"), ymin = -Inf, ymax = 29, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2023-01-01"), xmax = as.Date(Inf), ymin = -Inf, ymax = 29, fill = "#00A99D", color = NA, alpha = 0.4) +
  annotate("text", label = "COVID\nRecession", x = as.Date("2019-04-01"), y = 29.75, color = "#EE6055", alpha = 0.7, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Recovery\nto Trend", x = as.Date("2020-05-01"), y = 29.75, color = "#00A99D", alpha = 0.85, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Trend Overshoot\n& Excess Inflation", x = as.Date("2021-05-15"), y = 29.75, color = "#EE6055", alpha = 0.7, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Disinflation\n& Renormalization", x = as.Date("2023-02-01"), y = 29.75, color = "#00A99D", alpha = 0.85, size = 4, hjust = 0, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), limits = c(18,30.5), breaks = c(19,23,27), expand = c(0,0)) +
  ylab("Nominal GDP, Dollars") +
  ggtitle("Nominal Gross Domestic Product") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "NGDP Recovered, Exceeded Trend, and then Renormalized Post-COVID") +
  theme_apricitas + theme(legend.position = c(.2,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("Nominal GDP","2016-2020 Trend"), guide=guide_legend(override.aes=list(linetype=c(1,2),lwd = c(1.25,0.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 18-(.3*12.5), ymax = 18) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_GRAPH, "NGDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

NGDP_YOY_Graph <- ggplot() + #plotting inflation breakevens
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(BEA_NGDP_NSA_YOY,date>=as.Date("2016-01-01")), aes(x=date,y= NGDP ,color= "NGDP Year-on-Year Growth"), size = 1.25) +
  geom_line(data=filter(BEA_NGDP_INDEX, date>=as.Date("2016-01-01")), aes(x=date,y= (1+growth_rate)^4-1 ,color= "2016-2020 Average Growth Rate"), size = 0.75, linetype = "dashed") +
  annotate("rect", xmin = as.Date("2019-10-01"), xmax = as.Date("2020-04-01"), ymin = -Inf, ymax = .18, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2021-04-01"), ymin = -Inf, ymax = .18, fill = "#00A99D", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2023-01-01"), ymin = -Inf, ymax = .18, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2023-01-01"), xmax = as.Date(Inf), ymin = -Inf, ymax = .18, fill = "#00A99D", color = NA, alpha = 0.4) +
  annotate("text", label = "COVID\nRecession", x = as.Date("2019-04-01"), y = .195, color = "#EE6055", alpha = 0.7, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Recovery\nto Trend", x = as.Date("2020-05-01"), y = .195, color = "#00A99D", alpha = 0.85, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Trend Overshoot\n& Excess Inflation", x = as.Date("2021-05-15"), y = .195, color = "#EE6055", alpha = 0.7, size = 4, hjust = 0, lineheight = 0.8) +
  annotate("text", label = "Disinflation\n& Renormalization", x = as.Date("2023-02-01"), y = .195, color = "#00A99D", alpha = 0.85, size = 4, hjust = 0, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.08,.21), breaks = c(-.18,-.12,-.06,.0,.06,.12,.18), expand = c(0,0)) +
  ylab("Nominal GDP, Year-on-Year Growth") +
  ggtitle("Nominal Gross Domestic Product Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "NGDP Recovered, Exceeded Trend, and then Renormalized Post-COVID") +
  theme_apricitas + theme(legend.position = c(.23,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("NGDP Year-on-Year Growth","2016-2020 Average Growth Rate"), guide=guide_legend(override.aes=list(linetype=c(1,2),lwd = c(1.25,0.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -0.08-(.3*.29), ymax = -0.08) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_YOY_Graph, "NGDP YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 


QSPC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/Compiled_QSPC.csv") %>%
  mutate(date = paste(Year,"Q",Quarter)) %>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q %q"))) %>%
  mutate(Value = as.numeric(Value))

QSPC_Supply_Chains_Graph <- ggplot() + #Share of Manufacturing Constrained
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Insufficient Supply of Materials"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Logistics/transportation constraints"), aes(x=date,y= Value/100,color= "Logistics/Transportation Constraints"), size = 1.25) +
  #geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(0.1,0.2,0.3,0.4,0.5), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Supply Constraints Facing US Factories") +
  labs(caption = "Graph created by @JosephPolitano using US Census QPC data",subtitle = "Manufacturers Are Citing Materials Shortages & Logistics Constraints Less than During 2022 Peaks") +
  theme_apricitas + theme(legend.position = c(.375,.875)) +
  scale_color_manual(name= "US Manufacturers, Reasons for Not Running at Full Capacity, %",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-04-01")))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QSPC_Supply_Chains_Graph, "QSPC Supply Chains Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UNRATE <- fredr(series_id = "UNRATE",observation_start = as.Date("2018-01-01"), frequency = "q")

SPF_UNRATE_BULK <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/meanlevel.xlsx?la=en&hash=0E19F14E5BC0E464774A5E796456FB69", sheet = "UNEMP")
  
SPF_UNRATE <- SPF_UNRATE_BULK %>%
  mutate(date = as.Date(as.yearqtr(paste0(YEAR,"-",QUARTER)))) %>%
  filter(date >= as.Date("2022-07-01")) %>%
  select(UNEMP1:UNEMP6,date) %>%
  mutate(date = date %m-% months(3)) %>%
  mutate(across(starts_with("UNEMP"), as.numeric)) %>%
  left_join(UNRATE, by = "date") %>%
  mutate(UNEMP1 = value) %>%
  select(-value,-series_id,-realtime_start,-realtime_end) %>%   
  pivot_longer(cols = starts_with("UNEMP"), names_to = "forecast_horizon", values_to = "forecast_value") %>%
  mutate(forecast_horizon = as.numeric(gsub("UNEMP", "", forecast_horizon))) %>%
  mutate(forecast_date = case_when(
    forecast_horizon == 1 ~ date,
    forecast_horizon == 2 ~ date %m+% months(3),
    forecast_horizon == 3 ~ date %m+% months(6),
    forecast_horizon == 4 ~ date %m+% months(9),
    forecast_horizon == 5 ~ date %m+% months(12),
    forecast_horizon == 6 ~ date %m+% months(15)
  )) #%>%
  #select(-forecast_horizon) %>%
  #pivot_wider(names_from = date, values_from = forecast_value)
  
UNRATE_Projections_Graph <- ggplot() + #Share of Manufacturing Constrained
  geom_line(data=SPF_UNRATE, aes(x=forecast_date,y= forecast_value/100,group= as.character(date), color = "Average Professional Forecast"), size = 1.25, alpha = 0.6) +
  geom_line(data=filter(UNRATE, date >= as.Date("2021-10-01")), aes(x=date,y= value/100,color= "Actual Unemployment Rate"), size = 2.25) +
  #geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(.034,.046), breaks = c(0.035,0.04,0.045), expand = c(0,0)) +
  ylab("Unemployment Rate, %") +
  ggtitle("Unemployment Beat 2022/2023 Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using BLS CPS data & Philadelphia Fed SPF data",subtitle = "The Unemployment Rate Increased Much Slower Than Forecasters Expected it Would in 2022/2023") +
  theme_apricitas + theme(legend.position = c(.21,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1),lwd = c(2.25,1.25), alpha = c(1,0.6)))) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-10-01")-(.1861*(max(SPF_UNRATE$forecast_date)-as.Date("2021-10-01"))), xmax = as.Date("2021-10-01")-(0.049*(.1861*(max(SPF_UNRATE$forecast_date)-as.Date("2021-10-01")))), ymin = .034-(.3*.012), ymax = .034) +
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNRATE_Projections_Graph, "UNRATE Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CORE_PCEPI <- fredr(series_id = "BPCCRO1Q156NBEA", observation_start = as.Date("2018-01-01"))
  
SPF_COREPCE_BULK <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/meanlevel.xlsx?la=en&hash=0E19F14E5BC0E464774A5E796456FB69", sheet = "COREPCE")

SPF_COREPCE <- SPF_COREPCE_BULK %>%
  mutate(date = as.Date(as.yearqtr(paste0(YEAR,"-",QUARTER)))) %>%
  filter(date >= as.Date("2022-07-01")) %>%
  select(COREPCE1:COREPCE6,date) %>%
  mutate(date = date %m-% months(3)) %>%
  mutate(across(starts_with("COREPCE"), as.numeric)) %>%
  left_join(CORE_PCEPI, by = "date") %>%
  mutate(COREPCE1 = value) %>%
  select(-value,-series_id,-realtime_start,-realtime_end) %>%   
  pivot_longer(cols = starts_with("COREPCE"), names_to = "forecast_horizon", values_to = "forecast_value") %>%
  mutate(forecast_horizon = as.numeric(gsub("COREPCE", "", forecast_horizon))) %>%
  mutate(forecast_date = case_when(
    forecast_horizon == 1 ~ date,
    forecast_horizon == 2 ~ date %m+% months(3),
    forecast_horizon == 3 ~ date %m+% months(6),
    forecast_horizon == 4 ~ date %m+% months(9),
    forecast_horizon == 5 ~ date %m+% months(12),
    forecast_horizon == 6 ~ date %m+% months(15)
  )) #%>%
#select(-forecast_horizon) %>%
#pivot_wider(names_from = date, values_from = forecast_value)

COREPCE_Projections_Graph <- ggplot() + #Share of Manufacturing Constrained
  geom_line(data=SPF_COREPCE, aes(x=forecast_date,y= forecast_value/100,group= as.character(date), color = "Average Professional Forecast"), size = 1.25, alpha = 0.6) +
  geom_line(data=filter(CORE_PCEPI, date >= as.Date("2021-10-01")), aes(x=date,y= value/100,color= "Actual Core PCE Inflation"), size = 2.25) +
  #geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(.0175,.0575), breaks = c(0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Core PCE Inflation, %") +
  ggtitle("Inflation Exceeded 2022/2023 Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using BLS CPS data & Philadelphia Fed SPF data",subtitle = "Core Inflation Fell Much Slower Than Forecasters Expected it Would in 2022/2023") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1),lwd = c(2.25,1.25), alpha = c(1,0.6)))) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-10-01")-(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01"))), xmax = as.Date("2021-10-01")-(0.049*(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01")))), ymin = .0175-(.3*.04), ymax = .0175) +
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COREPCE_Projections_Graph, "Core PCE Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PCEPI <- fredr(series_id = "PCECTPI", observation_start = as.Date("2018-01-01"), units = "pc1")

SPF_PCE_BULK <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/meanlevel.xlsx?la=en&hash=0E19F14E5BC0E464774A5E796456FB69", sheet = "PCE")

SPF_PCE <- SPF_PCE_BULK %>%
  mutate(date = as.Date(as.yearqtr(paste0(YEAR,"-",QUARTER)))) %>%
  filter(date >= as.Date("2022-07-01")) %>%
  select(PCE1:PCE6,date) %>%
  mutate(date = date %m-% months(3)) %>%
  mutate(across(starts_with("PCE"), as.numeric)) %>%
  left_join(PCEPI, by = "date") %>%
  mutate(PCE1 = value) %>%
  select(-value,-series_id,-realtime_start,-realtime_end) %>%   
  pivot_longer(cols = starts_with("PCE"), names_to = "forecast_horizon", values_to = "forecast_value") %>%
  mutate(forecast_horizon = as.numeric(gsub("PCE", "", forecast_horizon))) %>%
  mutate(forecast_date = case_when(
    forecast_horizon == 1 ~ date,
    forecast_horizon == 2 ~ date %m+% months(3),
    forecast_horizon == 3 ~ date %m+% months(6),
    forecast_horizon == 4 ~ date %m+% months(9),
    forecast_horizon == 5 ~ date %m+% months(12),
    forecast_horizon == 6 ~ date %m+% months(15)
  )) #%>%
#select(-forecast_horizon) %>%
#pivot_wider(names_from = date, values_from = forecast_value)

PCE_Projections_Graph <- ggplot() + #Share of Manufacturing Constrained
  geom_line(data=SPF_PCE, aes(x=forecast_date,y= forecast_value/100,group= as.character(date), color = "Average Professional Forecast"), size = 1.25, alpha = 0.6) +
  geom_line(data=filter(PCEPI, date >= as.Date("2021-10-01")), aes(x=date,y= value/100,color= "Actual PCE Inflation"), size = 2.25) +
  #geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(.0175,.07), breaks = c(0.02,0.03,0.04,0.05,0.06,0.07), expand = c(0,0)) +
  ylab("Core PCE Inflation, %") +
  ggtitle("Inflation Exceeded 2022/2023 Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using BLS CPS data & Philadelphia Fed SPF data",subtitle = "Headline Inflation Fell Much Slower Than Forecasters Expected it Would in 2022/2023") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1),lwd = c(2.25,1.25), alpha = c(1,0.6)))) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-10-01")-(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01"))), xmax = as.Date("2021-10-01")-(0.049*(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01")))), ymin = .0175-(.3*.04), ymax = .0175) +
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_Projections_Graph, "PCE Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

REAL_GDP <- fredr(series_id = "GDPC1", observation_start = as.Date("2018-01-01"))

SPF_RGDP_BULK <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/meanlevel.xlsx?la=en&hash=0E19F14E5BC0E464774A5E796456FB69", sheet = "RGDP")

SPF_RGDP <- SPF_RGDP_BULK %>%
  mutate(date = as.Date(as.yearqtr(paste0(YEAR,"-",QUARTER)))) %>%
  filter(date >= as.Date("2022-07-01")) %>%
  select(RGDP1:RGDP6,date) %>%
  mutate(date = date %m-% months(3)) %>%
  mutate(across(starts_with("RGDP"), as.numeric)) %>%
  mutate(across(where(is.numeric), ~./RGDP1)) %>%
  left_join(REAL_GDP, by = "date") %>%
  mutate(RGDP1 = value) %>%
  mutate(across(RGDP2:RGDP6, ~.*RGDP1)) %>%
  select(-value,-series_id,-realtime_start,-realtime_end) %>%   
  pivot_longer(cols = starts_with("RGDP"), names_to = "forecast_horizon", values_to = "forecast_value") %>%
  mutate(forecast_horizon = as.numeric(gsub("RGDP", "", forecast_horizon))) %>%
  mutate(forecast_date = case_when(
    forecast_horizon == 1 ~ date,
    forecast_horizon == 2 ~ date %m+% months(3),
    forecast_horizon == 3 ~ date %m+% months(6),
    forecast_horizon == 4 ~ date %m+% months(9),
    forecast_horizon == 5 ~ date %m+% months(12),
    forecast_horizon == 6 ~ date %m+% months(15)
  )) #%>%
#select(-forecast_horizon) %>%
#pivot_wider(names_from = date, values_from = forecast_value)

REAL_GDP_Projections_Graph <- ggplot() + #Share of Manufacturing Constrained
  geom_line(data=SPF_RGDP, aes(x=forecast_date,y= forecast_value/1000,group= as.character(date), color = "Average Professional Forecast"), size = 1.25, alpha = 0.6) +
  geom_line(data=filter(REAL_GDP, date >= as.Date("2021-10-01")), aes(x=date,y= value/1000,color= "Actual Real GDP"), size = 2.25) +
  #geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "T"),limits = c(21.5,23.5), breaks = c(21.5,22,22.5,23,23.5), expand = c(0,0)) +
  ylab("Real GDP, 2017 Dollars") +
  ggtitle("Real GDP Beat 2022/2023 Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using BLS CPS data & Philadelphia Fed SPF data",subtitle = "Real GDP Beat Forecasters Expected it Would in 2022/2023") +
  theme_apricitas + theme(legend.position = c(.21,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1),lwd = c(2.25,1.25), alpha = c(1,0.6)))) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-10-01")-(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01"))), xmax = as.Date("2021-10-01")-(0.049*(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01")))), ymin = 21.5-(.3*2), ymax = 21.5) +
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_GDP_Projections_Graph, "Real GDP Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#DO SURVEY OF PROFESSIONAL FORECASTERS  
  
TBILL_3M <- fredr(series_id = "TB3MS", observation_start = as.Date("2018-01-01"))
  
SPF_TBILL_3M_BULK <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/meanlevel.xlsx?la=en&hash=0E19F14E5BC0E464774A5E796456FB69", sheet = "TBILL")

SPF_TBILL_3M <- SPF_TBILL_3M_BULK %>%
  mutate(date = as.Date(as.yearqtr(paste0(YEAR,"-",QUARTER)))) %>%
  filter(date >= as.Date("2022-07-01")) %>%
  select(TBILL1:TBILL6,date) %>%
  mutate(date = date %m-% months(3)) %>%
  mutate(across(starts_with("TBILL"), as.numeric)) %>%
  left_join(TBILL_3M, by = "date") %>%
  mutate(TBILL1 = value) %>%
  select(-value,-series_id,-realtime_start,-realtime_end) %>%   
  pivot_longer(cols = starts_with("TBILL"), names_to = "forecast_horizon", values_to = "forecast_value") %>%
  mutate(forecast_horizon = as.numeric(gsub("TBILL", "", forecast_horizon))) %>%
  mutate(forecast_date = case_when(
    forecast_horizon == 1 ~ date,
    forecast_horizon == 2 ~ date %m+% months(3),
    forecast_horizon == 3 ~ date %m+% months(6),
    forecast_horizon == 4 ~ date %m+% months(9),
    forecast_horizon == 5 ~ date %m+% months(12),
    forecast_horizon == 6 ~ date %m+% months(15)
  )) #%>%
#select(-forecast_horizon) %>%
#pivot_wider(names_from = date, values_from = forecast_value)

SPF_TBILL_3M_Graph <- ggplot() + #Share of Manufacturing Constrained
  geom_line(data=SPF_TBILL_3M, aes(x=forecast_date,y= forecast_value/100,group= as.character(date), color = "Average Professional Forecast"), size = 1.25, alpha = 0.6) +
  geom_line(data=filter(TBILL_3M, date >= as.Date("2021-10-01")), aes(x=date,y= value/100,color= "Actual 3M T-Bill Interest Rate"), size = 2.25) +
  #geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(0,0.06), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06), expand = c(0,0)) +
  ylab("3M T-Bill Rates, %") +
  ggtitle("Interest Rates Exceeded 2022/2023 Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using BLS CPS data & Philadelphia Fed SPF data",subtitle = "Peak Interest Rates Were Much Higher Than Forecasters Expected They Would be in 2022/2023") +
  theme_apricitas + theme(legend.position = c(.21,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1),lwd = c(2.25,1.25), alpha = c(1,0.6)))) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-10-01")-(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01"))), xmax = as.Date("2021-10-01")-(0.049*(.1861*(max(SPF_COREPCE$forecast_date)-as.Date("2021-10-01")))), ymin = 0-(.3*.06), ymax = 0) +
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SPF_TBILL_3M_Graph, "3M TBILL Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ARRANGE_PROJECTIONS_Graph <- ggarrange(UNRATE_Projections_Graph + ggtitle("Unemployment Rate") + labs(caption = NULL, subtitle = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)) + scale_color_manual(name = NULL,labels = c("Actual", "Forecasts"), values = c("#FFE98F","#00A99D"),guide=guide_legend(override.aes=list(linetype=c(1,1),lwd = c(2.25,1.25), alpha = c(1,0.6)))),
                                  REAL_GDP_Projections_Graph + ggtitle("Real GDP") + labs(caption = NULL, subtitle = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)),
                                  COREPCE_Projections_Graph + ggtitle("Core PCE Inflation") + labs(caption = NULL, subtitle = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)),
                                  SPF_TBILL_3M_Graph + ggtitle("3M T-Bill Rates") + labs(caption = NULL, subtitle = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)), 
                                  common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

tgrob <- text_grob(expression(bold("Professional Macro Forecasts vs Actual Results")),size = 27, color = "white", hjust = 0.5, lineheight = 0.8) 
tgrob2 <- text_grob(expression(bold("                                                        Graph created by @JosephPolitano using BLS CPS data, BEA NIPA data & Avg Philadelphia Fed SPF data")),size = 9, color = "#929299", vjust = 1, hjust = 1, face = "plain") 

# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0.3,0,-0.3,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(0,-0.15,-0.15,-0.15),"cm"))  
plot_1 <- as_ggplot(tgrob2) + theme_apricitas + theme(plot.margin = margin(0.3,0,-0.3,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "gray45"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(0.15,-15,-0.04,-0.15),"cm"))  


FINAL_ARRANGE_PROJECTIONS_Graph <- ggarrange(plot_0,ARRANGE_PROJECTIONS_Graph,plot_1, nrow = 3, heights = c(3.5,20,1.75), widths = 10)

ggsave(dpi = "retina",plot = FINAL_ARRANGE_PROJECTIONS_Graph, "Final Arrange Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)

ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index\nOption-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.11), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  annotate(geom = "segment", x = as.Date("2022-03-16"), xend = as.Date("2022-03-16"), y = 0, yend = 0.075, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "First\nRate\nHike", x = as.Date("2021-11-01"), y = 0.07, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  ylab("Spread, %") +
  ggtitle("US Credit Conditions") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "High Yield Credit Spreads Have Slid Back Down to Early-2022 Lows") +
  theme_apricitas + theme(legend.position = c(.60,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.11), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "US Credit Conditions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIE_1YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ATL_BIE_1YR.csv") %>%
  mutate(date = as.Date(date))

BIE_Graph <- ggplot() + #plotting total quits
  geom_line(data=BIE_1YR, aes(x=date,y= bie,color= "Business Unit Cost Inflation Expectations: Next Year"), size = 1.25) + 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ggtitle("Inflation Expectations are Falling") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Business Unit Cost Inflation Expectations are Falling Significantly") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Business Unit Cost Inflation Expectations: Next Year","Business Unit Cost Inflation Expectations: Next 5-10 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-10-21")-(.1861*(today()-as.Date("2011-10-21"))), xmax = as.Date("2011-10-21")-(0.049*(today()-as.Date("2011-10-21"))), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_Graph, "BIE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

BIE_LAGGED <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Inflation%20Retrospective/BIE_LAGGED_DATA.csv") %>%
  mutate(date = as.Date(date))

BIE_5YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Understanding%20Inflation%20Expectations/ATL_BIE_5YR.csv") %>%
  mutate(date = as.Date(date))

BIE_LAGGED_Graph <- ggplot() + #plotting total quits
  geom_line(data=BIE_LAGGED, aes(x=date,y= bie_lagged,color= "Business Unit Cost Inflation Over the Last Year"), size = 1.25) + 
  geom_line(data=BIE_1YR, aes(x=date,y= bie,color= "Business Unit Cost Inflation Expectations For the Next Year"), size = 1.25) +  
  geom_line(data=BIE_5YR, aes(x=date,y= bie_5yr,color= "Business Unit Cost Inflation Expectations For the Next 5 Years"), size = 1.25) +  
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ggtitle("Business Inflation Expectations are Falling") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Business Unit Cost Inflation Expectations are Falling Significantly") +
  theme_apricitas + theme(legend.position = c(.38,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Business Unit Cost Inflation Over the Last Year","Business Unit Cost Inflation Expectations For the Next Year","Business Unit Cost Inflation Expectations For the Next 5 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-10-21")-(.1861*(today()-as.Date("2011-10-21"))), xmax = as.Date("2011-10-21")-(0.049*(today()-as.Date("2011-10-21"))), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_LAGGED_Graph, "BIE Lagged.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

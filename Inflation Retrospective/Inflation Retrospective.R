pacman::p_load(tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
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
  
T5YIE2018_GRAPH <- ggplot() + #plotting inflation breakevens
  geom_line(data=BEA_NGDP_INDEX, aes(x=date,y= NGDP/100 ,color= "Nominal Gross Domestic Product"), size = 1.25) +
  geom_line(data=BEA_NGDP_INDEX, aes(x=date,y= Trend/100 ,color= "Trend"), size = 1.25) +
  
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


UNRATE <- fredr(series_id = "UNRATE",observation_start = as.Date("2018-01-01"), units = "pc1")

CORE_PCEPI <- 
  
#DO SURVEY OF PROFESSIONAL FORECASTERS  
  
UNRATE_FCAST <-
  
CORE_PCEPI_FCAST <-
  
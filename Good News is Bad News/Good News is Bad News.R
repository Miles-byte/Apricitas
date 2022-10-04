pacman::p_load(stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EFFR <- fredr(series_id = "EFFR",observation_start = as.Date("2019-01-01"), frequency = "m", aggregation_method = "avg")

#for most recent data - 7/14/2022
FFR_FUTURES_MEGA_MERGE_7_14 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/30-day-fed-funds-prices-intraday-07-15-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

FFR_FUTURES_MEGA_MERGE_9_16 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Good%20News%20is%20Bad%20News/30-day-fed-funds-prices-intraday-09-17-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

FFR_FUTURES_MEGA_MERGE_9_27 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Good%20News%20is%20Bad%20News/30-day-fed-funds-prices-intraday-09-27-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

CES_AGGREGATE_PAYROLLS <- fredr(series_id = "CES0500000017",observation_start = as.Date("2018-01-01"), frequency = "q", aggregation_method = "avg", units = "pca")
ECIEPOP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Good%20News%20is%20Bad%20News/ECIEPOP.csv") %>%
  mutate(DATE = as.Date(DATE))

RETAIL_LESS_GAS_CARS <- fredr(series_id = "MARTSSM44W72USS",observation_start = as.Date("2018-01-01"), frequency = "sa", aggregation_method = "avg", units = "pca")


FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  geom_line(data=FFR_FUTURES_MEGA_MERGE_7_14, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path July 14th"), size = 1.25) +
  #geom_line(data=FFR_FUTURES_MEGA_MERGE_9_16, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path September 16th"), size = 1.25) +
  geom_line(data=FFR_FUTURES_MEGA_MERGE_9_27, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path September 27th"), size = 1.25) +
  geom_line(data=EFFR, aes(x=date,y= value/100,color= "Effective Federal Funds Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(0,0.0575), breaks = c(0.01,0.02,0,.03,.04,.05), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Higher For Longer") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and CME data",subtitle = "Federal Funds Futures Price In Higher Interest Rates-And a Longer Period of Tight Policy") +
  theme_apricitas + theme(legend.position = c(.40,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Effective Federal Funds Rate","Futures Implied Federal Funds Rate Path July 14th","Futures Implied Federal Funds Rate Path September 27th")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+1825-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+1825-as.Date("2019-01-01"))), ymin = 0-(.3*.0575), ymax = 0) +
  coord_cartesian(clip = "off")

AGGREGATE_WAGE_GROWTH_GRAPH <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=CES_AGGREGATE_PAYROLLS, aes(x=date,y=value/100,color= "Index of Aggregate Weekly Payrolls of All Employees, Total Private"), size = 1.25) +
  geom_line(data=ECIEPOP, aes(x=DATE,y=EPOP_ECI/100,color= "Employment Level, Private Industry * ECI: Private Wages and Salaries"), size = 1.25) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", x = as.Date("2019-01-01"), y = .095, label = "Pre-COVID 5% Average", color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.46,0.45), breaks = c(-.4,-.3,-.2,-.1,0,.1,.2,.3,.4), expand = c(0,0)) +
  ylab("Annualized Percent Growth, Quarterly") +
  ggtitle("Good News is Bad News") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Aggregate Income Growth is Above Pre-Pandemic Averages") +
  theme_apricitas + theme(legend.position = c(.45,.96)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.46-(.3*.91), ymax = -.46) +
  coord_cartesian(clip = "off")

RETAIL_LESS_GAS_CARS_GRAPH <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=RETAIL_LESS_GAS_CARS, aes(x=date,y=value/100,color= "Retail and Food Services Sales Ex Motor Vehicle and Gasoline"), size = 1.25) +
  annotate("hline", y = 0.04, yintercept = 0.04, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text", x = as.Date("2019-01-01"), y = .065, label = "Pre-COVID 5% Average", color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.10,0.25), breaks = c(-.1,0,.1,.2), expand = c(0,0)) +
  ylab("Annualized Percent Growth, Semiannual") +
  ggtitle("Good News is Bad News") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Retail Sales Growth is Above Pre-Pandemic Averages") +
  theme_apricitas + theme(legend.position = c(.45,.96)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.1-(.3*.35), ymax = -.1) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph, "FFR Futures Mega Merge.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AGGREGATE_WAGE_GROWTH_GRAPH, "AGGREGATE WAGE GROWTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RETAIL_LESS_GAS_CARS_GRAPH, "RETAIL SALES GROWTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


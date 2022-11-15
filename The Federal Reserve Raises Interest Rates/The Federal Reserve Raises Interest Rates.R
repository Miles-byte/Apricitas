pacman::p_load(stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

NFCI <- fredr(series_id = "NFCI",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 
NFCICREDIT <- fredr(series_id = "NFCICREDIT",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

ICECORPORATE <- fredr(series_id = "BAMLC0A0CM",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECORPORATE <- drop_na(ICECORPORATE)

ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)

Corp_Issuance <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/CORP_ISSUANCE.csv")
Corp_Issuance$Date <- as.Date(Corp_Issuance$Date)

Yield_Curve <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/YIELD_CURVE.csv")

WORKINGAGEPOP <- fredr(series_id = "LNU00000060",realtime_start = NULL, realtime_end = NULL, frequency = "a", aggregation_method = "avg")

Top1pct <- fredr(series_id = "WFRBST01108",observation_start = as.Date("1990-01-01"),realtime_start = NULL, realtime_end = NULL)

T5 <- fredr(series_id = "DGS5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 year nominal interest rates
T5IE <- fredr(series_id = "T5YIE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 year inflation expectations
T5RL <- fredr(series_id = "DFII5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates

T5Bind <- rbind(T5IE,T5RL) #binding real yields and inflation expectations
T5Bind <- drop_na(T5Bind)
T5Bind$series_id <- gsub("T5YIE","5-Year Breakeven Inflation Expectations",T5Bind$series_id)
T5Bind$series_id <- gsub("DFII5","5-Year Real Bond Yield",T5Bind$series_id)
T5 <- drop_na(T5)

IORB <- fredr(series_id = "IORB",realtime_start = NULL, realtime_end = NULL)
IOER <- fredr(series_id = "IOER",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL)

TENYR <- fredr(series_id = "DGS10",observation_start = as.Date("1980-01-01"),realtime_start = NULL, realtime_end = NULL)
TENYRREAL <- fredr(series_id = "DFII10",realtime_start = NULL, realtime_end = NULL)
TENYR <- drop_na(TENYR)
TENYRREAL <- drop_na(TENYRREAL)

FIVEYEARTIPS <- fredr(series_id = "DFII5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
FIVEYEARTIPS <- drop_na(FIVEYEARTIPS)

SEVENYEARTIPS <- fredr(series_id = "DFII7",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
SEVENYEARTIPS <- drop_na(SEVENYEARTIPS)

TENYEARTIPS <- fredr(series_id = "DFII10",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
TENYEARTIPS <- drop_na(TENYEARTIPS)

TWENTYYEARTIPS <- fredr(series_id = "DFII20",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
TWENTYYEARTIPS <- drop_na(TWENTYYEARTIPS)

THIRTYYEARTIPS <- fredr(series_id = "DFII30",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
THIRTYYEARTIPS <- drop_na(THIRTYYEARTIPS)

#collecting vintages of the SEP Projections, subsetting to 2022 onward, converting dates to month char, and correcting factors
#note-in 2023 change ending date to 2026 to include 2025 projections
SEPUNRATE2022 <- fredr(series_id = "UNRATEMD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2021-01-01")) %>% subset(date < as.Date("2025-01-01"))
SEPUNRATE2022$realtime_start <- as.character(SEPUNRATE2022$realtime_start) %>% { gsub("2022-03-16","March SEP",.) } %>% { gsub("2022-09-21","September SEP",.) } %>% { gsub("2022-06-15","June SEP",.) } %>%factor(levels = c("March SEP","June SEP","September SEP"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

SEPGDP2022 <- fredr(series_id = "GDPC1MD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2021-01-01")) %>% subset(date < as.Date("2025-01-01"))
SEPGDP2022$realtime_start <- as.character(SEPGDP2022$realtime_start) %>% { gsub("2022-03-16","March SEP",.) } %>% { gsub("2022-09-21","September SEP",.) } %>% { gsub("2022-06-15","June SEP",.) } %>%factor(levels = c("March SEP","June SEP","September SEP"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

SEPPCEPI2022 <- fredr(series_id = "PCECTPIMD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2021-01-01")) %>% subset(date < as.Date("2025-01-01"))
SEPPCEPI2022$realtime_start <- as.character(SEPPCEPI2022$realtime_start) %>% { gsub("2022-03-16","March SEP",.) } %>% { gsub("2022-09-21","September SEP",.) } %>% { gsub("2022-06-15","June SEP",.) } %>%factor(levels = c("March SEP","June SEP","September SEP"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

SEPFFR2022 <- fredr(series_id = "FEDTARMD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2021-01-01")) %>% subset(date < as.Date("2025-01-01"))
SEPFFR2022$realtime_start <- as.character(SEPFFR2022$realtime_start) %>% { gsub("2022-03-16","March SEP",.) } %>% { gsub("2022-09-21","September SEP",.) } %>% { gsub("2022-06-15","June SEP",.) } %>%factor(levels = c("March SEP","June SEP","September SEP"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

SPY <- tq_get("VOO", from = "2019-01-01")

#downloading the MOVE interest rate volatility index
MOVE <- tq_get("^MOVE", from = "2018-01-01")
MOVE <- drop_na(MOVE) 

#downloading DXY index
DXY <- tq_get("DX-Y.NYB", from = "2018-01-01")
DXY <- drop_na(DXY) 

#Downloading FFR futures for Jan 23, 24, and 25
FFR_JAN_23 <- tq_get("ZQF23.CBT", from = "2021-01-01")
FFR_JAN_23 <- drop_na(FFR_JAN_23) %>% mutate(Future = "Jan23")
FFR_JAN_24 <- tq_get("ZQF24.CBT", from = "2021-01-01")
FFR_JAN_24 <- drop_na(FFR_JAN_24) %>% mutate(Future = "Jan24")
FFR_JAN_25 <- tq_get("ZQF25.CBT", from = "2021-01-01")
FFR_JAN_25 <- drop_na(FFR_JAN_25) %>% mutate(Future = "Jan25")

FFR_MERGE_23_25 <- rbind(FFR_JAN_23,FFR_JAN_24,FFR_JAN_25) %>% select(date, close, Future) %>% pivot_wider(names_from = Future, values_from = close)
FFR_MERGE_23_25 <- drop_na(FFR_MERGE_23_25)

EFFR <- fredr(series_id = "EFFR",observation_start = as.Date("2019-01-01"), frequency = "m", aggregation_method = "avg")

FIVEYEARBREAKEVEN2019 <- fredr(series_id = "T5YIE", observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN2019 <- fredr(series_id = "T5YIFR",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data

FIVEYEARBREAKEVEN2019 <- drop_na(FIVEYEARBREAKEVEN2019)
FIVEYEARFWDBREAKEVEN2019 <- drop_na(FIVEYEARFWDBREAKEVEN2019)

FFR_FUTURES_MEGA_MERGE_7_5 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/FFR_FUTURES.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

#for most recent data - 7/14/2022
FFR_FUTURES_MEGA_MERGE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/30-day-fed-funds-prices-intraday-07-15-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))


FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  geom_line(data=FFR_FUTURES_MEGA_MERGE_7_5, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path July 5th"), size = 1.25) +
  geom_line(data=FFR_FUTURES_MEGA_MERGE, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path July 15th"), size = 1.25) +
  geom_line(data=EFFR, aes(x=date,y= value/100,color= "Effective Federal Funds Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(0,0.0475), breaks = c(0.01,0.02,0,.03,.04), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("An Inverted Curve") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and CME data",subtitle = "FFR Futures are Forecasting Rate Cuts in 2023 and 2024 as Recession Risks Increase") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Effective Federal Funds Rate","Futures Implied Federal Funds Rate Path July 5th","Futures Implied Federal Funds Rate Path July 15th")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*3500), xmax = as.Date("2019-01-01")-(0.049*3500), ymin = 0-(.3*.0475), ymax = 0) +
  coord_cartesian(clip = "off")

FFR_FUTURES_MEGA_MERGE_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  geom_line(data=FFR_FUTURES_MEGA_MERGE, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path"), size = 1.25) +
  geom_line(data=EFFR, aes(x=date,y= value/100,color= "Effective Federal Funds Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(0,0.0475), breaks = c(0.01,0.02,0,.03,.04), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("An Inverted Curve") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and CME data",subtitle = "FFR Futures are Forecasting Rate Cuts in 2023 and 2024 as Recession Risks Increase") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*3500), xmax = as.Date("2019-01-01")-(0.049*3500), ymin = 0-(.3*.0475), ymax = 0) +
  coord_cartesian(clip = "off")


FFR_MERGE_23_25_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=FFR_MERGE_23_25, aes(x=date,y= (Jan24-Jan23)/-100,color= "2023 Futures Implied FFR Rate Change"), size = 1.25) +
  geom_line(data=FFR_MERGE_23_25, aes(x=date,y= (Jan25-Jan24)/-100,color= "2024 Futures Implied FFR Rate Change"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(-0.008,0.01), breaks = c(-0.008,-0.006,-0.004,-0.002,0,.002,.004,.006,.008,.01), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Higher For Longer") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "FFR Futures Have Stopped Forecasting Rate Cuts in Calendar Year 2023") +
  theme_apricitas + theme(legend.position = c(.35,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(today()-as.Date("2021-01-01"))), xmax = as.Date("2021-01-01")-(0.049*(today()-as.Date("2021-01-01"))), ymin = -0.008-(.3*.018), ymax = -0.008) +
  coord_cartesian(clip = "off")

MOVE_Graph <- ggplot() + #plotting MOVE
  geom_line(data=MOVE, aes(x=date,y= close,color= "MOVE Interest Rate Volatility Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,200), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Lacking Forward Guidance") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "Interest Rate Volatility is Still High as Economic Conditions Remain Unstable") +
  theme_apricitas + theme(legend.position = c(.25,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

DXY_Graph <- ggplot() + #plotting DXY
  geom_line(data=DXY, aes(x=date,y= close,color= "DXY US Dollar Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(85,115), breaks = c(85,90,95,100,105,110,115), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Dollar Dominance") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "The Dollar Has Been Appreciating Against a Basket of Currencies as Monetary Policy Tightens") +
  theme_apricitas + theme(legend.position = c(.25,.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*30), ymax = 85) +
  coord_cartesian(clip = "off")

SEPUNRATE2022_Graph <- ggplot(data = SEPUNRATE2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.055), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Unemployment Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC"), breaks = c("March SEP","June SEP","September SEP")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

SEPGDP2022_Graph <- ggplot(data = SEPGDP2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.055), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Real GDP Growth") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC"), breaks = c("March SEP","June SEP","September SEP")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

SEPPCEPI2022_Graph <- ggplot(data = SEPPCEPI2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.055), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Inflation (PCEPI)") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC"), breaks = c("March SEP","June SEP","September SEP")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

SEPFFR2022_Graph <- ggplot(data = SEPFFR2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.055), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Interest Rates (FFR)") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC"), breaks = c("March SEP","June SEP","September SEP")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

#arranging the SEP graphs into one items and adding background and border colors to match the theme
SEP_ARRANGE_GRAPH <- ggarrange(SEPUNRATE2022_Graph, SEPPCEPI2022_Graph, SEPGDP2022_Graph, SEPFFR2022_Graph,  ncol = 2, nrow = 2, heights = 20, widths = 10, common.legend = TRUE, legend = "bottom") + bgcolor("#252A32") + border("#252A32")

REAL_RATES_GRAPH <- ggplot() + #plotting inflation breakevens
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=FIVEYEARTIPS, aes(x=date,y= value/100 ,color= "5-Year Real Treasury Yield"), size = 1.25) +
  geom_line(data=SEVENYEARTIPS, aes(x=date,y= value/100 ,color= "7-Year Real Treasury Yield (Fitted)"), size = 1.25) +
  geom_line(data=TENYEARTIPS, aes(x=date,y= value/100 ,color= "10-Year Real Treasury Yield"), size = 1.25) +
  geom_line(data=TWENTYYEARTIPS, aes(x=date,y= value/100 ,color= "20-Year Real Treasury Yield (Fitted)"), size = 1.25) +
  geom_line(data=THIRTYYEARTIPS, aes(x=date,y= value/100 ,color= "30-Year Real Treasury Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.02,.02), breaks = c(-0.02,-0.01,0,0.01,0.02), expand = c(0,0)) +
  ylab("TIPS Yield, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Interest Rates are Above Pre-COVID Levels and the Real Yield Curve is Inverted") +
  theme_apricitas + theme(legend.position = c(.62,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("5-Year Real Treasury Yield","7-Year Real Treasury Yield (Fitted)","10-Year Real Treasury Yield","20-Year Real Treasury Yield (Fitted)","30-Year Real Treasury Yield")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.02-(.3*.04), ymax = -0.02) +
  coord_cartesian(clip = "off")

T5RATES_Graph <- ggplot(T5Bind, aes(fill=series_id, x=date, y=value/100)) + 
  geom_area(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = T5, aes(x=date, y = value/100, color = "5-Year Nominal Bond Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.02,0.045), breaks = c(-0.02,-.01,0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Effects of Monetary Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Interest Rates are Rising and Inflation Expectations are Falling as the Fed Tightens") +
  theme_apricitas + theme(legend.position = c(.42,.87), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("5-Year Real Bond Yield","5-Year Breakeven Inflation Expectations")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -.02-(.3*0.065), ymax = -0.02) +
  coord_cartesian(clip = "off")

NFCI_Graph <- ggplot() + #plotting national financial conditions indexes
  geom_line(data=NFCI, aes(x=date,y= value,color= "Chicago Fed National Financial Conditions Index"), size = 1.25) +
  geom_line(data=NFCICREDIT, aes(x=date,y= value,color= "Chicago Fed National Financial Conditions Index: Credit Subindex"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1,-0.5,0,0.5,1), expand = c(0,0)) +
  ylab("Index, 0 = Average, Higher Numbers are Tighter") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Financial Conditions were Tightening even before the Federal Reserve Raises Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -1-(.3*2), ymax = -1) +
  coord_cartesian(clip = "off")

SPY_Graph <- ggplot() + #plotting SPY
  geom_line(data=SPY, aes(x=date,y= (close/230)-1,color= "S&P 500"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.1,1), breaks = c(0,.25,.5,0.75,1), expand = c(0,0)) +
  ylab("Total Return") +
  ggtitle("Risk and Reward") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "The S&P 500 is Down in 2022-Possibly Indicating a Rising Equity Premium") +
  theme_apricitas + theme(legend.position = c(.40,.65)) +
  scale_color_manual(name= "Total Return Since Jan 2019",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = -.1-(.3*1.1), ymax = -.1) +
  coord_cartesian(clip = "off")

Corp_Issuance_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=Corp_Issuance, aes(x=Date,y= Investment_Grade,color= "Investment Grade"), size = 1.25) +
  geom_line(data=Corp_Issuance, aes(x=Date,y= High_Yield,color= "High Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,300), breaks = c(100,200,300), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using SIFMA data",subtitle = "Corporate Bond Issuance is Decreasing-Especially for High Yield Bonds") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= "Corporate Bond Issuance",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*750), xmax = as.Date("2020-01-01")-(0.049*750), ymin = 0-(.3*300), ymax = 0) +
  coord_cartesian(clip = "off")

ICECORPORATE_Graph <- ggplot() + #plotting ice corporate index
  geom_line(data=ICECORPORATE, aes(x=date,y= value/100,color= "ICE BofA US Corporate Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.045), breaks = c(0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Financial Conditions were Tightening even before the Federal Reserve Raised Interest Rates") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.045), ymax = 0) +
  coord_cartesian(clip = "off")

ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  annotate("hline", y = 0.089, yintercept = 0.089, color = "#EE6055", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2016/2012 Cycle Peak", x = as.Date("2019-01-01"), y = 0.095, color ="#EE6055", size = 5) +
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Recession Watch") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "High Yield Credit Spreads Remain Elevated") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")


IOER_IORB_Graph <- ggplot() + #plotting durables v services inflation
  geom_line(data=IORB, aes(x=date,y= value/100,color= "IORB"), size = 1.25) +
  geom_line(data=IOER, aes(x=date,y= value/100,color= "IOER"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.035), breaks = c(0,0.005,.01,.015,.02,.025,0.03,0.035), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Interest Rates are Now Well Above Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.50,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*4150), xmax = as.Date("2010-01-01")-(0.049*4150), ymin = 0-(.3*.035), ymax = 0) +
  coord_cartesian(clip = "off")

TenYR_Graph <- ggplot() + #plotting durables v services inflation
  geom_line(data=TENYR, aes(x=date,y= value/100,color= "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity"), size = 1.25) +
  geom_line(data=TENYRREAL, aes(x=date,y= value/100,color= "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Inflation-Indexed"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(-0.015,.2), breaks = c(0,0.05,.1,.15,.2), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Nominal (and Real) Treasury Yields have Been Declining for Decades") +
  theme_apricitas + theme(legend.position = c(.50,.94),legend.text = element_text(size = 13, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*15411), xmax = as.Date("1980-01-01")-(0.049*15411), ymin = -0.015-(.3*.215), ymax = -0.015) +
  coord_cartesian(clip = "off")

Yield_Curve_Graph <- ggplot() + #plotting national financial conditions indexes
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Dec/100,color= "December 1st 2021"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Dec/100,color= "December 1st 2021"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Jan/100,color= "January 3rd 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Jan/100,color= "January 3rd 2022"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Feb/100,color= "February 1st 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Feb/100,color= "February 1st 2022"), size = 2) +
  geom_line(data=Yield_Curve, aes(x=Months/12,y= Mar/100,color= "March 1st 2022"), size = 1.25) +
  geom_point(data=Yield_Curve, aes(x=Months/12,y= Mar/100,color= "March 1st 2022"), size = 2) +
  xlab("Years") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.03), breaks = c(0,0.005,.01,.015,.02,.025,0.03), expand = c(0,0)) +
  scale_x_continuous(labels = c("1yr","2yr","3yr","5yr","7yr","10yr","20yr","30yr"), breaks = c(1,2,3,5,7,10,20,30)) +
  ylab("Yield, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "The Yield Curve is Flattening") +
  theme_apricitas + theme(legend.position = c(.20,.85), panel.grid.minor = element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*30), xmax = 0-(0.049*30), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

Workingagepop_Graph <- ggplot() + #plotting corporate bond issuance
  geom_line(data=WORKINGAGEPOP, aes(x=date,y=value/1000,color= "US Working Age (25-54) Population"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"), limits = c(50,150), breaks = c(50,100,150), expand = c(0,0)) +
  ylab("Millions") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "America's Working Age Population is Not Growing Anymore") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1948-01-01")-(.1861*26664), xmax = as.Date("1948-01-01")-(0.049*26664), ymin = 50-(.3*100), ymax = 50) +
  coord_cartesian(clip = "off")

Top1Pct_Graph <- ggplot() + #plotting top 1% share of total assets
  geom_line(data=Top1pct, aes(x=date,y= value/100,color= "Share of Total Assets Held by the Top 1%"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.2,.3), breaks = c(.2,.25,.3), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("How High Can Rates Go?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Wealth and Income Inequality is Rising, Which Keeps Real Rates Down") +
  theme_apricitas + theme(legend.position = c(.50,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11323), xmax = as.Date("1990-01-01")-(0.049*11323), ymin = .20-(.3*.1), ymax = .20) +
  coord_cartesian(clip = "off")

T5YIE2019 <- ggplot() + #plotting inflation breakevens
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.0225, ymax = 0.0275, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=FIVEYEARFWDBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  annotate("text", label = "Breakevens Approximately Consistent With 2% Inflation Target", x = as.Date("2020-01-01"), y = 0.0287, color = "#EE6055", alpha = 0.6, size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Short and Long Term Inflation Expectations are Within a Level Consistent with the Fed's Target") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

UNRATE <- fredr(series_id = c("UNRATE"), units ="ch1")


UNRATE_Graph <- ggplot() + #yield curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = UNRATE, aes(x = date, y = value/100, color = "1-Year Change in the Unemployment Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.10,0.125), breaks = c(-0.05,-.10,0,0.05,0.10), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("A Dire Forecast") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "A 1% Increase in the Unemployment Rate Has Only and Always Occurred in a Recession") +
  theme_apricitas + theme(legend.position = c(.50,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1949-01-01")-(.1861*25889), xmax = as.Date("1949-01-01")-(0.049*25889), ymin = -0.10-(.3*0.225), ymax = -0.10) +
  coord_cartesian(clip = "off") +
  annotate("hline", y = 0.01, yintercept = 0.01, color = "white", size = 1.25, linetype = "dashed") +
  annotate("rect", xmin = as.Date("1948-11-01"), xmax = as.Date("1949-10-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1953-07-01"), xmax = as.Date("1954-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1960-04-01"), xmax = as.Date("1961-02-28"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1969-12-01"), xmax = as.Date("1970-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1973-11-01"), xmax = as.Date("1975-03-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1980-07-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1981-07-01"), xmax = as.Date("1982-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("1990-07-01"), xmax = as.Date("1991-03-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", y = 0.035, x = as.Date("1994-01-01"), color = "white", label = ("1% Current Fed-Projected Increase"))
  


ggsave(dpi = "retina",plot = NFCI_Graph, "NFCI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = IOER_IORB_Graph, "IOER IORB.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TenYR_Graph, "Ten Year.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ICECORPORATE_Graph, "ICE Corporate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "ICE CCC Corporate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Corp_Issuance_Graph, "Corporate Issuance.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Yield_Curve_Graph, "Yield Curve.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Workingagepop_Graph, "Working Age Population.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Top1Pct_Graph, "Top 1 pct.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5RATES_Graph, "T5 Rates.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SPY_Graph, "SPY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = REAL_RATES_GRAPH, "Real Rates.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SEP_ARRANGE_GRAPH, "SEP Arrange.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = MOVE_Graph, "MOVE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = FFR_MERGE_23_25_Graph, "FFR Merge.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = FFR_FUTURES_MEGA_MERGE_Graph, "FFR Futures Mega Merge.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = DXY_Graph, "DXY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5YIE2019, "T5YIE2019.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph, "FFR MEGA COMPARISON.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = UNRATE_Graph, "UNRATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
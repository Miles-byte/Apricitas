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

FFR_FUTURES_MEGA_MERGE_11_2 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Good%20News%20is%20Bad%20News/30-day-fed-funds-prices-intraday-11-02-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

FFR_FUTURES_MEGA_MERGE_11_4 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Good%20News%20is%20Bad%20News/30-day-fed-funds-prices-intraday-11-04-2022.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  geom_line(data=EFFR, aes(x=date,y= value/100,color= "Effective Federal Funds Rate"), size = 1.25) +
  geom_line(data=FFR_FUTURES_MEGA_MERGE_7_14, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path July 14th"), size = 1.25) +
  #geom_line(data=FFR_FUTURES_MEGA_MERGE_9_16, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path September 16th"), size = 1.25) +
  geom_line(data=FFR_FUTURES_MEGA_MERGE_9_27, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path September 27th"), size = 1.25) +
  geom_line(data=FFR_FUTURES_MEGA_MERGE_11_4, aes(x=Contract,y= (100-Last)/100,color= "Futures Implied Federal Funds Rate Path November 4th"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(0,0.07), breaks = c(0.01,0.02,0,.03,.04,.05,.06,0.07), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Higher For Longer") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and CME data",subtitle = "Federal Funds Futures Price In Higher Interest Rates") +
  theme_apricitas + theme(legend.position = c(.40,.88)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Effective Federal Funds Rate","Futures Implied Federal Funds Rate Path July 14th","Futures Implied Federal Funds Rate Path September 27th","Futures Implied Federal Funds Rate Path November 4th")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+1825-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+1825-as.Date("2019-01-01"))), ymin = 0-(.3*.07), ymax = 0) +
  coord_cartesian(clip = "off")

#Long Run Interest Rate Projections
LONG_RUN_FOMC_MEDIAN <- fredr(series_id = "FEDTARMDLR",observation_start = as.Date("2015-01-01"))
LONG_RUN_LOWER_CENTRAL <- fredr(series_id = "FEDTARCTLLR",observation_start = as.Date("2015-01-01"))
LONG_RUN_HIGHER_CENTRAL <- fredr(series_id = "FEDTARCTHLR",observation_start = as.Date("2015-01-01"))
LONG_RUN_HIGH_RANGE <- fredr(series_id = "FEDTARRHLR",observation_start = as.Date("2015-01-01"))
LONG_RUN_LOW_RANGE <- fredr(series_id = "FEDTARRLLR",observation_start = as.Date("2015-01-01"))
THIRTY_YEAR_TSY <- fredr(series_id = "DGS30",observation_start = as.Date("2015-01-01")) %>%
  drop_na()
TEN_YEAR_TSY <- fredr(series_id = "DGS10",observation_start = as.Date("2015-01-01")) %>%
  drop_na()


FOMC_VS_30YR_Graph <- ggplot() + #plotting Long Run Median FOMC Predictions
  geom_line(data=TEN_YEAR_TSY, aes(x=date,y= value/100,color= "Ten-Year Treasury Yield"), size = 1.25) +
  geom_line(data=THIRTY_YEAR_TSY, aes(x=date,y= value/100,color= "Thirty-Year Treasury Yield"), size = 1.25) +
  #geom_line(data=LONG_RUN_LOWER_CENTRAL, aes(x=date,y= value/100,color= "Central Tendency"), size = 1, linetype = "dashed", alpha = 0.75) +
  #geom_line(data=LONG_RUN_HIGHER_CENTRAL, aes(x=date,y= value/100,color= "Central Tendency"), size = 1, linetype = "dashed", alpha = 0.75) +
  geom_line(data=LONG_RUN_HIGH_RANGE, aes(x=date,y= value/100,color= "\nLonger-Run FOMC Fed Funds Rate Projections:"), size = 0.75, linetype = "dashed", alpha = 0) +
  geom_line(data=LONG_RUN_HIGH_RANGE, aes(x=date,y= value/100,color= "Range"), size = 3, alpha = 0.75) +
  geom_line(data=LONG_RUN_LOW_RANGE, aes(x=date,y= value/100,color= "Range"), size = 3, alpha = 0.75) +
  geom_line(data=LONG_RUN_FOMC_MEDIAN, aes(x=date,y= value/100,color= "Median"), size = 1.25, linetype = "dashed", alpha = 0.90) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.0525), breaks = c(0.01,0.02,0,.03,.04,.05), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Searching for a \"Natural\" Rate") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "FOMC Officials' September Projections Sit Well Below Today's Bond Rates") +
  theme_apricitas + theme(legend.position = c(.57,.86)) + theme(legend.text = element_text(size = 13), legend.key.height = unit(0,"cm"), legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","white","#EE6055","#A7ACD9","#9A348E"), breaks = c("Ten-Year Treasury Yield","Thirty-Year Treasury Yield","\nLonger-Run FOMC Fed Funds Rate Projections:","Median","Range"), guide=guide_legend(override.aes=list(linetype=c(1,1,1,2,1), lwd = c(1.25,1.25,.75,.75,3), alpha = c(1,1,0,1,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*.0525), ymax = 0) +
  coord_cartesian(clip = "off")

#TIPS Graph
TENYR <- fredr(series_id = "DGS10",observation_start = as.Date("1980-01-01"),realtime_start = NULL, realtime_end = NULL)
TENYRREAL <- fredr(series_id = "DFII10",realtime_start = NULL, realtime_end = NULL)
TENYR <- drop_na(TENYR)
TENYRREAL <- drop_na(TENYRREAL)

FIVEYEARTIPS <- fredr(series_id = "DFII5",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
FIVEYEARTIPS <- drop_na(FIVEYEARTIPS)

SEVENYEARTIPS <- fredr(series_id = "DFII7",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
SEVENYEARTIPS <- drop_na(SEVENYEARTIPS)

TENYEARTIPS <- fredr(series_id = "DFII10",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
TENYEARTIPS <- drop_na(TENYEARTIPS)

TWENTYYEARTIPS <- fredr(series_id = "DFII20",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
TWENTYYEARTIPS <- drop_na(TWENTYYEARTIPS)

THIRTYYEARTIPS <- fredr(series_id = "DFII30",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
THIRTYYEARTIPS <- drop_na(THIRTYYEARTIPS)

REAL_RATES_GRAPH <- ggplot() + #plotting inflation breakevens
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=FIVEYEARTIPS, aes(x=date,y= value/100 ,color= "5-Year Real Treasury Yield"), size = 1.25) +
  geom_line(data=SEVENYEARTIPS, aes(x=date,y= value/100 ,color= "7-Year Real Treasury Yield (Fitted)"), size = 1.25) +
  geom_line(data=TENYEARTIPS, aes(x=date,y= value/100 ,color= "10-Year Real Treasury Yield"), size = 1.25) +
  geom_line(data=TWENTYYEARTIPS, aes(x=date,y= value/100 ,color= "20-Year Real Treasury Yield (Fitted)"), size = 1.25) +
  geom_line(data=THIRTYYEARTIPS, aes(x=date,y= value/100 ,color= "30-Year Real Treasury Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.02,.04), breaks = c(-0.02,-0.01,0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("TIPS Yield, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Interest Rates are Above Pre-COVID Levels and the Real Yield Curve is Flat") +
  theme_apricitas + theme(legend.position = c(.72,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("5-Year Real Treasury Yield","7-Year Real Treasury Yield (Fitted)","10-Year Real Treasury Yield","20-Year Real Treasury Yield (Fitted)","30-Year Real Treasury Yield")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = -0.02-(.3*.06), ymax = -0.02) +
  coord_cartesian(clip = "off")

#30yr and 30yr Tips
THIRTYYEARTIPS <- fredr(series_id = "DFII30",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
THIRTYYEARTIPS <- drop_na(THIRTYYEARTIPS)

THIRTYYEAR <- fredr(series_id = "DGS30",observation_start = as.Date("1980-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates
THIRTYYEAR <- drop_na(THIRTYYEAR)

THIRTY_GRAPH <- ggplot() + #plotting inflation breakevens
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=THIRTYYEAR, aes(x=date,y= value/100 ,color= "30-Year Treasury Yield"), size = 1.25) +
  geom_line(data=THIRTYYEARTIPS, aes(x=date,y= value/100 ,color= "30-Year Real Treasury Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.01,.15), breaks = c(0,.05,.1,.15), expand = c(0,0)) +
  ylab("TIPS Yield, %") +
  ggtitle("Long-Term Bonds in the Long-Term") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Bond Yields Were at Historic Lows Before Today's Inflation Burst") +
  theme_apricitas + theme(legend.position = c(.72,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("30-Year Treasury Yield","30-Year Real Treasury Yield")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*(today()-as.Date("1980-01-01"))), xmax = as.Date("1980-01-01")-(0.049*(today()-as.Date("1980-01-01"))), ymin = -0.01-(.3*.16), ymax = -0.01) +
  coord_cartesian(clip = "off")

#Yield curve inversions
TWOTEN <- fredr(series_id = "T10Y2Y",observation_start = as.Date("1985-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  drop_na()

THREETEN <- fredr(series_id = "T10Y3M",observation_start = as.Date("1985-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  drop_na()

TWOTEN_THREETEN_graph <- ggplot() + #plotting inflation breakevens
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("rect", xmin = as.Date("1990-07-01"), xmax = as.Date("1991-03-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=TWOTEN, aes(x=date,y= value/100 ,color= "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity"), size = 1.25) +
  geom_line(data=THREETEN, aes(x=date,y= value/100 ,color= "10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.01,.05), breaks = c(-0.01,0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("TIPS Yield, %") +
  ggtitle("An Early Warning?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Though Not A Perfect Signal, Inverted Yield Curves Can Indicate Elevated Recession Risks") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1985-01-01")-(.1861*(today()-as.Date("1985-01-01"))), xmax = as.Date("1985-01-01")-(0.049*(today()-as.Date("1985-01-01"))), ymin = -0.01-(.3*.06), ymax = -0.01) +
  coord_cartesian(clip = "off")

#5 Year Yield Decomposition

T5 <- fredr(series_id = "DGS5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 year nominal interest rates
T5IE <- fredr(series_id = "T5YIE",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 year inflation expectations
T5RL <- fredr(series_id = "DFII5",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #inflation-indexed real interest rates

T5Bind <- rbind(T5IE,T5RL) #binding real yields and inflation expectations
T5Bind <- drop_na(T5Bind)
T5Bind$series_id <- gsub("T5YIE","5-Year Breakeven Inflation Expectations",T5Bind$series_id)
T5Bind$series_id <- gsub("DFII5","5-Year Real Bond Yield",T5Bind$series_id)
T5 <- drop_na(T5)

T5RATES_Graph <- ggplot(T5Bind, aes(fill=series_id, x=date, y=value/100)) + 
  geom_area(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = T5, aes(x=date, y = value/100, color = "5-Year Nominal Bond Yield"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.02,0.045), breaks = c(-0.02,-.01,0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Breaking Down Rising Rates") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Higher Real Rates Drive Higher Yields as Inflation Expectations Hold Steady") +
  theme_apricitas + theme(legend.position = c(.42,.87), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("5-Year Real Bond Yield","5-Year Breakeven Inflation Expectations")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -.02-(.3*0.065), ymax = -0.02) +
  coord_cartesian(clip = "off")

#High Yield Yield vs Spread
HYOAS <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  select(date,value) %>%
  drop_na()#5 year nominal interest rates
HY <- fredr(series_id = "BAMLH0A0HYM2EY",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  select(date,value) %>%
  drop_na() #effective yield
BORROW_YIELD <- merge(HYOAS,HY, by = "date") %>%
  mutate(value = value.y-value.x) %>%
  select(-value.x,-value.y) %>%
  mutate(type = "Approximate Risk-Free Average Borrowing Rate")

HYOAS <- HYOAS %>%
  mutate(type = "Option-Adjusted High Yield Credit Spread")

HYOAS_rbind <- rbind(BORROW_YIELD,HYOAS) %>%
  pivot_wider(names_from = "type") %>%
  drop_na() %>%
  pivot_longer(cols = `Approximate Risk-Free Average Borrowing Rate`:`Option-Adjusted High Yield Credit Spread`)

HY <- HY %>%
    mutate(name = "Approximate Average High Yield Borrowing Rate")

HYOAS_RBIND_Graph <- ggplot(HYOAS_rbind, aes(fill=name, x=date, y=value/100)) + 
  geom_area(position=position_stack(reverse = TRUE), stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  geom_line(data = HY, aes(x=date, y = value/100, color = "Approximate Average High Yield Borrowing Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.13), breaks = c(0,0.02,0.04,0.06,0.08,0.10,.12), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Breaking Down Rising Rates") +
  labs(caption = "Graph created by @JosephPolitano using ICE-BofA data",subtitle = "The Relationship Between Risk-Free Rates and Financial Conditions is Not Perfectly Stable") +
  theme_apricitas + theme(legend.position = c(.35,.88), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Option-Adjusted High Yield Credit Spread","Approximate Risk-Free Average Borrowing Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")
  
#HYOAS VS T5YIE


T5IE_2010 <- fredr(series_id = "T5YIE",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  drop_na()#5 year inflation expectations

HYOAS_T5YIE_Graph <- ggplot() + 
  geom_line(data = T5IE_2010, aes(x=date, y = value/100, color = "5-Year Breakeven Inflation Expectations"), size = 1.25) +
  geom_line(data = HYOAS, aes(x=date, y = value/100, color = "Option-Adjusted High Yield Credit Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.11), breaks = c(0,0.02,0.04,0.06,0.08,0.10,.12), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Breaking Down Rising Rates") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and ICE-BofA data",subtitle = "The Relationship Between Financial Conditions and Inflation Expectations is Not Perfectly Stable") +
  theme_apricitas + theme(legend.position = c(.35,.90), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FFR_FUTURES_MEGA_MERGE_COMPARISON_Graph, "FFR Futures Mega Merge.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = FOMC_VS_30YR_Graph , "FOMC 30YR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = THIRTY_GRAPH , "30YR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = REAL_RATES_GRAPH , "Real Rates.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TWOTEN_THREETEN_graph , "TWOTEN.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = T5RATES_Graph , "5 Year Rates.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = HYOAS_RBIND_Graph , "HYOAS Rbind.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = HYOAS_T5YIE_Graph , "HYOAS T5YIE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



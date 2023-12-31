pacman::p_load(readxl,openxlsx,eia,eurostat,restatapi,stringi,jsonlite,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

install_github("keberwein/blscrapeR")
library(blscrapeR)

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

MEDIAN_SPF_GDP <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/data-files/files/median_rgdp_level.xlsx") %>%
  filter(YEAR == 2022, QUARTER == 4) %>%
  select(-YEAR,-QUARTER,-RGDP1,-RGDPA,-RGDPB,-RGDPC,-RGDPD) %>%
  transpose() %>%
  transmute(value = as.numeric(V1)/as.numeric(V1)[1],date = seq.Date(from = as.Date("2022-10-01"), by = "3 months", length.out = nrow(.)))

MEDIAN_SPF_UNRATE <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/data-files/files/median_unemp_level.xlsx")

SEP_GDP <- fredr(series_id = "GDPC1MD", realtime_start = as.Date("2022-12-14")) %>% subset(realtime_start == as.Date("2022-12-14")) %>% subset(date > as.Date("2022-01-01")) %>% subset(date < as.Date("2024-01-01")) %>%
  mutate(date = as.Date("2023-10-01")) %>%
  select(date,value) %>%
  add_row(date = as.Date("2022-10-01"), value = 0)
  
SEP_UNRATE <- fredr(series_id = "UNRATE", observation_start = as.Date("2022-07-01"), frequency = "q", aggregation_method = "avg")
 
REAL_GDP <- fredr(series_id = "GDPC1", observation_start = as.Date("2022-10-01")) %>%
  mutate(value = value/value[1]) 

REAL_UNRATE <- fredr(series_id = "UNRATE", observation_start = as.Date("2022-07-01"), frequency = "q", aggregation_method = "avg")


REAL_GDP_Projections_Graph <- ggplot() + #plotting real battery shipments
  geom_line(data=REAL_GDP, aes(x=date,y= value-1,color="Actual 2023 Real GDP Growth"), size = 2.25) +
  geom_line(data=MEDIAN_SPF_GDP, aes(x=date,y= value-1,color="2023 Median GDP Growth Forecasts, Q4 2022 Survey of Professional Forecasters"), size = 1.25, linetype = "dashed") +
  geom_line(data=SEP_GDP, aes(x=date,y= value/100,color="2023 Median GDP Growth FOMC Projections, December 2022"), size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0, 0.03), expand = c(0,0)) +
  scale_x_date(labels = function(x) paste0("Q", quarter(x), " ", year(x)),
               breaks = c(as.Date("2022-10-01"),as.Date("2023-01-01"),as.Date("2023-04-01"),as.Date("2023-07-01"),as.Date("2023-10-01"))) +
  ylab("Percent Growth From Q4 2022") +
  ggtitle("US Growth: Defying Expectations") +
  labs(caption = "Graph created by @JosephPolitano using BEA and Federal Reserve Data",subtitle = "The US Economy Has Beaten Last Year's Bleak Growth Forecasts") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Actual 2023 Real GDP Growth","2023 Median GDP Growth Forecasts, Q4 2022 Survey of Professional Forecasters","2023 Median GDP Growth FOMC Projections, December 2022"), guide = guide_legend(override.aes = list(linetype = c(1,2,2), lwd = c(2.25,0.75,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-10-01")-(.1861*(as.Date("2023-10-01")-as.Date("2022-10-01"))), xmax = as.Date("2022-10-01")-(0.049*(as.Date("2023-10-01")-as.Date("2022-10-01"))), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_GDP_Projections_Graph, "Real GDP Projections Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TOTAL_ASSETS <- fredr("QBPBSTAS", observation_start = as.Date("2005-01-01")) %>%
  mutate(date = floor_date(date + 97, "month")) %>%
  transmute(date, total_assets = value)

FAILURES_TOTAL_ASSETS <- merge(FAILURES, TOTAL_ASSETS, by = "date")

FAILURES_TOTAL_ASSETS_graph <- ggplot(data = FAILURES_TOTAL_ASSETS, aes(x = date, y = (asset/total_assets), fill = "Failed Banks, Share of All US Banks' Assets")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets of US Commercial Banks") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03), limits = c(0,0.035), expand = c(0,0)) +
  ggtitle("The Banking Crisis") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "The Total Size of Bank Failures in 2023 Has Been the Largest Since the Great Recession") +
  theme_apricitas + theme(legend.position = c(.6,.87)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*.035), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FAILURES_TOTAL_ASSETS_graph, "Failures as a Share of Total Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)

ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  annotate("hline", y = 0.089, yintercept = 0.089, color = "#EE6055", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2016/2012 Cycle Peak", x = as.Date("2019-01-01"), y = 0.095, color ="#EE6055", size = 5) +
  annotate("vline", x = as.Date("2023-03-10"), xintercept = as.Date("2023-03-10"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "SVB Failure", x = as.Date("2022-10-01"), y = 0.065, color ="white", size = 4) +
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Credit Conditions Have Eased Since SVB") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Credit Conditions are at the Easiest Levels in 2 Years, After Tightening Significantly in Early 2023") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "ICECCCORPORATE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


WORK_STOPPAGES_80 <- bls_api("WSU002", startyear = 1980, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
WORK_STOPPAGES_00 <- bls_api("WSU002", startyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
WORK_STOPPAGES_20 <- bls_api("WSU002", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

WORK_STOPPAGES <- rbind(WORK_STOPPAGES_80,WORK_STOPPAGES_00,WORK_STOPPAGES_20) %>%
  group_by(year) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, "-01-01")), value)

WORK_STOPPAGES_graph <- ggplot(data = WORK_STOPPAGES, aes(x = date, y = (value), fill = "Work Stoppages From Stikes, % of Total Estimated Working Time")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Estimated Working Time") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.02,0.04,0.06,0.08), limits = c(0,0.08), expand = c(0,0)) +
  ggtitle("A Historic Year for Labor Action") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "US Work Stoppages Hit the Highest Levels Since the Early 2000s") +
  theme_apricitas + theme(legend.position = c(.6,.95)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1981-01-01")-(.1861*(today()-as.Date("1981-01-01"))), xmax = as.Date("1981-01-01")-(0.049*(today()-as.Date("1981-01-01"))), ymin = 0-(.3*.08), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WORK_STOPPAGES_graph, "Work Stoppages Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

FFR_JAN_24 <- tq_get("ZQF24.CBT", from = "2021-01-01")
FFR_JAN_24 <- drop_na(FFR_JAN_24) %>% mutate(Future = "Jan24") %>% 
  select(date, close, Future)

FFR_JAN_23 <- data.frame(date = FFR_JAN_24$date, close = 100-4.33)
FFR_JAN_23 <- drop_na(FFR_JAN_23) %>% mutate(Future = "Jan23") 

FFR_JAN_25 <- tq_get("ZQF25.CBT", from = "2021-01-01")
FFR_JAN_25 <- drop_na(FFR_JAN_25) %>% mutate(Future = "Jan25") %>% 
  select(date, close, Future)

FFR_MERGE_23_25 <- rbind(FFR_JAN_23,FFR_JAN_24,FFR_JAN_25) %>% pivot_wider(names_from = Future, values_from = close)
FFR_MERGE_23_25 <- drop_na(FFR_MERGE_23_25)


FFR_MERGE_23_25_Graph <- ggplot() + #plotting FFR rate changes in 2023 and 2024
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(FFR_MERGE_23_25, date >= as.Date("2022-01-01")), aes(x=date,y= (Jan25-Jan24)/-100,color= "2024 Futures Implied FFR Rate Change"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(-0.02,0.005), breaks = c(-0.02,-0.015,-0.01,-0.005,0,0.05), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Coming Cuts?") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "Markets are Pricing in Significant Cuts for 2024, But Pacing and Size are Still in Question") +
  theme_apricitas + theme(legend.position = c(.55,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -0.02-(.3*.025), ymax = -0.02) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FFR_MERGE_23_25_Graph, "FFR Merge Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

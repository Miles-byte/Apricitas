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


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

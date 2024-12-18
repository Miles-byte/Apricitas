pacman::p_load(bea.R,janitor,tidyverse,ggpubr,purrr,sf,seasonal,tigris,maps,readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Renovation Nation Chart
REAL_PRIVATE_FIXED_INVEST_SPECS_QUARTERLY <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50406',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_PRIVATE_FIXED_INVEST_QUARTERLY <- beaGet(REAL_PRIVATE_FIXED_INVEST_SPECS_QUARTERLY, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2007-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

REAL_PRIVATE_FIXED_INVEST_SPECS_ANNUAL <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'T50403',
  'Frequency' = 'A',
  'Year' = paste(seq(from = 1990, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_PRIVATE_FIXED_INVEST_ANNUAL <- beaGet(REAL_PRIVATE_FIXED_INVEST_SPECS_ANNUAL, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1990-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names()

REAL_IMPROVEMENTS_DATA <- REAL_PRIVATE_FIXED_INVEST_ANNUAL %>%
  transmute(date,improvements = t50403_b946ra_44_improvements_fisher_quantity_index_level_0) %>%
  mutate(improvements = improvements/100*mean(REAL_PRIVATE_FIXED_INVEST_QUARTERLY$u50406_a946rx_44_improvements_chained_dollars_level_6[41:44])) %>%
  filter(date <= as.Date("2007-01-01")) %>%
  rbind(REAL_PRIVATE_FIXED_INVEST_QUARTERLY %>% transmute(date, improvements = u50406_a946rx_44_improvements_chained_dollars_level_6))

REAL_US_IMPROVEMENTS_Graph <- ggplot() + #indexed investment growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = REAL_IMPROVEMENTS_DATA, aes(x=date, y = improvements/1000, color = "Real US Investment: Residential Improvements"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,300), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("Renovation Nation") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Investment in Renovating Existing Homes is Near Record Highs") +
  theme_apricitas + theme(legend.position = c(.35,.875)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_US_IMPROVEMENTS_Graph, "Real US Improvements Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

AGE_FIXED_RESIDENTIAL_ASSETS_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'FixedAssets',
  'TableName' = 'FAAt209',
  'Frequency' = 'A',
  'Year' = paste(seq(from = 1929, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

AGE_FIXED_RESIDENTIAL_ASSETS <- beaGet(AGE_FIXED_RESIDENTIAL_ASSETS_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1929-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names()

AGE_FIXED_RESIDENTIAL_ASSETS_Graph <- ggplot() + #indexed investment growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = AGE_FIXED_RESIDENTIAL_ASSETS, aes(x=date, y = fa_at209_e1r53101hh00_68_housing_units_year_level_0, color = "Average Age of US Housing Units"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(), limits = c(20,40), expand = c(0,0)) +
  ylab("Years") +
  ggtitle("America's Aging Housing Stock") +
  labs(caption = "Graph created by @JosephPolitano using BEA data Weighted by Current Cost",subtitle = "America's Housing Stock is the Oldest It's Even Been, And Still Getting Older") +
  theme_apricitas + theme(legend.position = c(.35,.875)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1929-01-01")-(.1861*(today()-as.Date("1929-01-01"))), xmax = as.Date("1929-01-01")-(0.049*(today()-as.Date("1929-01-01"))), ymin = 20-(.3*20), ymax = 20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AGE_FIXED_RESIDENTIAL_ASSETS_Graph, "Age Fixed Residential Assets Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
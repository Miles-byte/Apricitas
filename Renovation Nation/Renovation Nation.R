pacman::p_load(bea.R,janitor,tidyverse,ggpubr,purrr,sf,seasonal,tigris,maps,readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


install_github("keberwein/blscrapeR")
library(blscrapeR)

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

REAL_SINGLE_FAMILY_DATA <- REAL_PRIVATE_FIXED_INVEST_ANNUAL %>%
  transmute(date,single_family = t50403_b943ra_38_permanent_site_fisher_quantity_index_level_0) %>%
  mutate(single_family = single_family/100*mean(REAL_PRIVATE_FIXED_INVEST_QUARTERLY$u50406_a944rx_39_single_family_structures_chained_dollars_level_6[41:44])) %>%
  filter(date <= as.Date("2007-01-01")) %>%
  rbind(REAL_PRIVATE_FIXED_INVEST_QUARTERLY %>% transmute(date, single_family = u50406_a944rx_39_single_family_structures_chained_dollars_level_6))


REAL_US_IMPROVEMENTS_Graph <- ggplot() + #indexed investment growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = REAL_IMPROVEMENTS_DATA, aes(x=date, y = improvements/1000, color = "Real US Investment: Residential Improvements"), size = 1.25) +
  #geom_line(data = REAL_SINGLE_FAMILY_DATA, aes(x=date, y = single_family/1000, color = "Real US Investment: New Single-Family Homes"), size = 1.25) +
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
  'Year' = paste(seq(from = 1925, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

AGE_FIXED_RESIDENTIAL_ASSETS <- beaGet(AGE_FIXED_RESIDENTIAL_ASSETS_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1925-01-01"), length.out = nrow(.), by = "1 year"))) %>%
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
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1925-01-01")-(.1861*(today()-as.Date("1925-01-01"))), xmax = as.Date("1925-01-01")-(0.049*(today()-as.Date("1925-01-01"))), ymin = 20-(.3*20), ymax = 20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AGE_FIXED_RESIDENTIAL_ASSETS_Graph, "Age Fixed Residential Assets Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


RENOVATION_EMPLOYMENT <- bls_api("CES2023611801", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("CES2023611801", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date)

SF_EMPLOYMENT <- bls_api("CES2023611501", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("CES2023611501", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date)


RENOVATION_EMPLOYMENT_Graph <- ggplot() + #indexed investment growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = RENOVATION_EMPLOYMENT, aes(x=date, y = value, color = "All Employees: Residential Remodelers"), size = 1.25) +
  geom_line(data = SF_EMPLOYMENT, aes(x=date, y = value, color = "All Employees: New Single-Family Construction"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,650), expand = c(0,0)) +
  ylab("Thousands of Employees") +
  ggtitle("Renovation Nation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "A Record Number of People are Now Working for Professional Remodelers") +
  theme_apricitas + theme(legend.position = c(.5,.175)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RENOVATION_EMPLOYMENT_Graph, "Renovation Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ACS_MOVING_RATES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Renovation%20Nation/acs_moving_rates.csv") %>%
  transmute(date = as.Date(paste0(date,"-01-01")), value = moving_rate)

ACS_MOVING_RATE_Graph <- ggplot() + #indexed investment growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = ACS_MOVING_RATES, aes(x=date, y = value, color = "% of US Residents Moving Each Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.2), expand = c(0,0)) +
  ylab("Percent of Americans Moving Each Year") +
  ggtitle("Staying Put") +
  labs(caption = "Graph created by @JosephPolitano using Census ACS data",subtitle = "Americans are Moving at Record Low Rates—Instead Choosing (or Forced) to Stay in their Homes") +
  theme_apricitas + theme(legend.position = c(.5,.875)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*.20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ACS_MOVING_RATE_Graph, "ACS Moving Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RENOVATION_SPENDING <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Renovation%20Nation/Renovation_spending.csv") %>%
  mutate(category = c("Disaster Repairs", "Room Additions/\nRenovations", "Kitchen/Bath Remodel","Exterior\n(Roof/Door/Porch/etc)","Interior\n(Flooring/Utilities/HVAC/etc)","Yard\n(Pool/Driveway/Fence/etc)")) %>%
  mutate(category = factor(category, levels = c("Disaster Repairs", "Room Additions/\nRenovations","Yard\n(Pool/Driveway/Fence/etc)","Kitchen/Bath Remodel","Exterior\n(Roof/Door/Porch/etc)","Interior\n(Flooring/Utilities/HVAC/etc)")))


RENOVATION_SPENDING_graph <- ggplot(data = RENOVATION_SPENDING, aes(x = category, y = value/2000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,125), expand = c(0,0)) +
  ggtitle("US Annual Home Renovation\nSpending by Category") +
  labs(caption = "Graph created by @JosephPolitano using Census AHS Data") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 18, color = "gray85"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 27)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = RENOVATION_SPENDING_graph, "Renovation Spending Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RENOVATION FINANCING

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
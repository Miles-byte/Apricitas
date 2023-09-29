pacman::p_load(purrr,forecast,imputeTS,tsibble,sf,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

GDP_FRED_NOMINAL <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP

GDP_FRED_DEFLATOR <- fredr(series_id = "GDPDEF",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(value = value/(((value[1]+value[2]+value[3]+value[4])/4)))#GDP Deflator

GDP_FRED_REAL_2017 <- merge(GDP_FRED_NOMINAL,GDP_FRED_DEFLATOR, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  mutate(value = value)
  
RGDP_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10106',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

GDP_BEA_REAL_2017 <- beaGet(RGDP_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

GDP_Revision_Graph <- ggplot() +
  geom_line(data = GDP_FRED_REAL_2017, aes(x=date, y = value/1000, color = "Real GDP: Old Data"), size = 1.25) + 
  geom_line(data = GDP_BEA_REAL_2017, aes(x=date, y = t10106_a191rx_1_gross_domestic_product_chained_dollars_level_6/1000000, color = "Real GDP: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"),limits = c(18.5,22.5), breaks = c(19,20,21,22), expand = c(0,0)) +
  ylab("Trillions of 2017 Dollars") +
  ggtitle("Revisions to US GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Gross Domestic Product Has Been Revised, Shifting our Understanding of the Economy") +
  theme_apricitas + theme(legend.position = c(.40,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 18.5-(.3*4), ymax = 18.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_Revision_Graph, "GDP Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PCE_FRED_NOMINAL <- fredr(series_id = "PCEC",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP

PCE_FRED_DEFLATOR <- fredr(series_id = "PCECTPI",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(value = value/(((value[1]+value[2]+value[3]+value[4])/4)))#GDP Deflator

PCE_FRED_REAL_2017 <- merge(PCE_FRED_NOMINAL,PCE_FRED_DEFLATOR, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  mutate(value = value)

CONSUMPTION_REVISIONS_Graph <- ggplot() +
  geom_line(data = PCE_FRED_REAL_2017, aes(x=date, y = value/1000, color = "Real Personal Consumption Expenditures: Old Data"), size = 1.25) + 
  geom_line(data = GDP_BEA_REAL_2017, aes(x=date, y = t10106_dpcerx_2_personal_consumption_expenditures_chained_dollars_level_6/1000000, color = "Real Personal Consumption Expenditures: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"),limits = c(12,16), breaks = c(12,13,14,15,16), expand = c(0,0)) +
  ylab("Trillions of 2017 Dollars") +
  ggtitle("Revisions to US Real Consumption") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Gross Domestic Product Has Been Revised, Shifting our Understanding of the Economy") +
  theme_apricitas + theme(legend.position = c(.40,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 12-(.3*4), ymax = 12) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSUMPTION_REVISIONS_Graph, "Consumption Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FPI_FRED_NOMINAL <- fredr(series_id = "FPI",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP

FPI_FRED_DEFLATOR <- fredr(series_id = "B007RG3Q086SBEA",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(value = value/(((value[1]+value[2]+value[3]+value[4])/4)))#GDP Deflator

FPI_FRED_REAL_2017 <- merge(FPI_FRED_NOMINAL,FPI_FRED_DEFLATOR, by = "date") %>%
  transmute(date, value = value.x/value.y) %>%
  mutate(value = value)

FIXED_INVESTMENT_REVISIONS_Graph <- ggplot() +
  geom_line(data = FPI_FRED_REAL_2017, aes(x=date, y = value/1000, color = "Real Private Fixed Investment: Old Data"), size = 1.25) + 
  geom_line(data = GDP_BEA_REAL_2017, aes(x=date, y = t10106_a007rx_8_fixed_investment_chained_dollars_level_6/1000000, color = "Real Private Fixed Investment: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.25),limits = c(3,4), breaks = c(3,3.25,3.5,3.75,4), expand = c(0,0)) +
  ylab("Trillions of 2017 Dollars") +
  ggtitle("Revisions to US Fixed Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real US Fixed Investment Has Been Revised, Shifting our Understanding of the Economy") +
  theme_apricitas + theme(legend.position = c(.325,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 3-(.3*1), ymax = 3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_INVESTMENT_REVISIONS_Graph, "Fixed Investment Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

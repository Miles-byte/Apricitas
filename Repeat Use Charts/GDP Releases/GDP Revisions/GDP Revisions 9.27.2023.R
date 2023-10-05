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



NGDP <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)
NGDI <- fredr(series_id = "GDI",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL)

NGDP_OLD <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25"))
NGDI_OLD <- fredr(series_id = "GDI",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25"))

NGDP_GDI_REVISIONS_GRAPH <- ggplot() +
  geom_line(data = NGDP_OLD, aes(x=date, y = value/1000, color = "Nominal GDP: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = NGDI_OLD, aes(x=date, y = value/1000, color = "Nominal GDI: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = NGDP, aes(x=date, y = value/1000, color = "Nominal GDP: New Revised Data"), size = 1.25) + 
  geom_line(data = NGDI, aes(x=date, y = value/1000, color = "Nominal GDI: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(19,28), breaks = c(19,20,21,22,23,24,25,26,27,28), expand = c(0,0)) +
  ylab("Trillions of US Dollars") +
  ggtitle("The Statistical Discrepancy in Context") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "'Equivalent' Official Measures of Aggregate Output Are Diverging Even Post GDP Revisions") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"), breaks = c("Nominal GDP: New Revised Data","Nominal GDP: Old Data","Nominal GDI: New Revised Data","Nominal GDI: Old Data"), guide = guide_legend(override.aes=list(linetype = c(1,2,1,2), lwd = c(1.25,0.75,1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 19-(.3*9), ymax = 19) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_GDI_REVISIONS_GRAPH, "NGDP GDI Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

NOMINAL_FIXED_MF_HOUSING_REVISED <- fredr(series_id = "C292RC1Q027SBEA",observation_start = as.Date("1980-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)
FIXED_MF_HOUSING_PRICE_REVISED <- fredr(series_id = "B292RG3Q086SBEA",observation_start = as.Date("1980-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

REAL_FIXED_MF_HOUSING_REVISED <- merge(NOMINAL_FIXED_MF_HOUSING_REVISED,FIXED_MF_HOUSING_PRICE_REVISED, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

NOMINAL_FIXED_MF_HOUSING_OLD <- fredr(series_id = "C292RC1Q027SBEA",observation_start = as.Date("1980-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date, value)
FIXED_MF_HOUSING_PRICE_OLD <- fredr(series_id = "B292RG3Q086SBEA",observation_start = as.Date("1980-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  transmute(date,
    value = value * (100 / mean(value[year(date) == 2017]))
  )

REAL_FIXED_MF_HOUSING_OLD <- merge(NOMINAL_FIXED_MF_HOUSING_OLD,FIXED_MF_HOUSING_PRICE_OLD, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

REAL_FIXED_MF_HOUSING_REVISIONS_Graph <- ggplot() +
  geom_line(data = REAL_FIXED_MF_HOUSING_OLD, aes(x=date, y = value, color = "Real Private Multifamily Residential Fixed Investment: Old Data"), size = 1.25) + 
  geom_line(data = REAL_FIXED_MF_HOUSING_REVISED, aes(x=date, y = value, color = "Real Private Multifamily Residential Fixed Investment: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,105), breaks = c(0,50,100), expand = c(0,0)) +
  ylab("2017 Dollars, Annualized") +
  ggtitle("Revisions to US Multifamily Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Revisions Have Brought Real US Multifamily Construction to the Highest Levels Since the 1970s") +
  theme_apricitas + theme(legend.position = c(.5,.125)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*(today()-as.Date("1980-01-01"))), xmax = as.Date("1980-01-01")-(0.049*(today()-as.Date("1980-01-01"))), ymin = 3-(.3*105), ymax = 3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_FIXED_MF_HOUSING_REVISIONS_Graph, "Real Fixed MF Housing Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

NOMINAL_FIXED_MANU_REVISED <- fredr(series_id = "C307RC1Q027SBEA",observation_start = as.Date("1960-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)
FIXED_MF_MANU_PRICE_REVISED <- fredr(series_id = "B307RG3Q086SBEA",observation_start = as.Date("1960-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

REAL_FIXED_MANU_REVISED <- merge(NOMINAL_FIXED_MANU_REVISED,FIXED_MF_MANU_PRICE_REVISED, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

NOMINAL_FIXED_MANU_OLD <- fredr(series_id = "C307RC1Q027SBEA",observation_start = as.Date("1960-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date, value)
FIXED_MF_MANU_PRICE_OLD <- fredr(series_id = "B307RG3Q086SBEA",observation_start = as.Date("1960-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  transmute(date,
            value = value * (100 / mean(value[year(date) == 2017]))
  )

REAL_FIXED_MANU_OLD <- merge(NOMINAL_FIXED_MANU_OLD,FIXED_MF_MANU_PRICE_OLD, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

REAL_FIXED_MANU_REVISIONS_Graph <- ggplot() +
  geom_line(data = REAL_FIXED_MANU_OLD, aes(x=date, y = value, color = "Real Private Manufacturing Structures Fixed Investment: Old Data"), size = 1.25) + 
  geom_line(data = REAL_FIXED_MANU_REVISED, aes(x=date, y = value, color = "Real Private Manufacturing Structures Fixed Investment: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,130), breaks = c(0,50,100), expand = c(0,0)) +
  ylab("2017 Dollars, Annualized") +
  ggtitle("Revisions to US Manufacturing Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Revisions Have Brought Real US Manufacturing Construction to the Highest Levels on Record") +
  theme_apricitas + theme(legend.position = c(.5,.125), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*(today()-as.Date("1960-01-01"))), xmax = as.Date("1960-01-01")-(0.049*(today()-as.Date("1960-01-01"))), ymin = 3-(.3*125), ymax = 3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_FIXED_MANU_REVISIONS_Graph, "Real Fixed Manu Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



NOMINAL_FIXED_SOFTWARE_REVISED <- fredr(series_id = "B985RC1Q027SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)
FIXED_MF_SOFTWARE_REVISED <- fredr(series_id = "B985RG3Q086SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

REAL_FIXED_SOFTWARE_REVISED <- merge(NOMINAL_FIXED_SOFTWARE_REVISED,FIXED_MF_SOFTWARE_REVISED, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

NOMINAL_FIXED_SOFTWARE_OLD <- fredr(series_id = "B985RC1Q027SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date, value)
FIXED_MF_SOFTWARE_OLD <- fredr(series_id = "B985RG3Q086SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  transmute(date,
            value = value * (100 / mean(value[year(date) == 2017]))
  )

REAL_FIXED_SOFTWARE_OLD <- merge(NOMINAL_FIXED_SOFTWARE_OLD,FIXED_MF_SOFTWARE_OLD, by = "date") %>%
  transmute(date, value = value.x/value.y*100)

REAL_FIXED_SOFTWARE_REVISIONS_Graph <- ggplot() +
  geom_line(data = REAL_FIXED_SOFTWARE_OLD, aes(x=date, y = value, color = "Real Private Software Fixed Investment: Old Data"), size = 1.25) + 
  geom_line(data = REAL_FIXED_SOFTWARE_REVISED, aes(x=date, y = value, color = "Real Private Software Fixed Investment: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,750), breaks = c(0,250,500,750), expand = c(0,0)) +
  ylab("2017 Dollars, Annualized") +
  ggtitle("Revisions to US Software Investment") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Updates to Own-Account Spending Data Raised Real Software Investment") +
  theme_apricitas + theme(legend.position = c(.5,.125)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 3-(.3*750), ymax = 3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_FIXED_SOFTWARE_REVISIONS_Graph, "Real Fixed Software Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIXED_NOMINAL_STRUCTURE_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50405',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2010, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

FIXED_NOMINAL_STRUCTURE_INVEST <- beaGet(FIXED_NOMINAL_STRUCTURE_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2010-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

ALT_POWER_FIXED_INVEST <- FIXED_NOMINAL_STRUCTURE_INVEST %>%
  select(date,u50405_la001174_18_alternative_electric_current_dollars_level_6,u50405_la001175_19_all_other_electric_current_dollars_level_6) %>%
  setNames(c("date","Alternative Electric Power (Wind, Solar, Dry-Waste and Geothermal)","Conventional Electric Power (Coal, Natural Gas, Nuclear, etc)")) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = rev(c("Alternative Electric Power (Wind, Solar, Dry-Waste and Geothermal)","Conventional Electric Power (Coal, Natural Gas, Nuclear, etc)"))))

ALT_POWER_FIXED_INVEST_graph <- ggplot(data = ALT_POWER_FIXED_INVEST, aes(x = date, y = value/1000, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Dollars") +
  ggtitle("America's Changing Power Investments") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,25,50,75,100), limits = c(0,115), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "New BEA Data Shows that Alternative Energy is Making Up a Higher Share of Power Investment") +
  theme_apricitas + theme(legend.position = c(.425,.89)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Nominal Fixed Investment in Structures",values = c("#EE6055","#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*115), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALT_POWER_FIXED_INVEST_graph, "Alt Power Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

POWER_FIXED_NOMINAL_PRICE_INDEXES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/GDP%20Releases/GDP%20Revisions/POWER_REVISIONS.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(NOMINAL_ELECTRIC_OLD = as.numeric(gsub(",","",NOMINAL_ELECTRIC_OLD))) %>%
  mutate(NOMINAL_ELECTRIC_NEW = as.numeric(gsub(",","",NOMINAL_ELECTRIC_NEW))) %>%
  mutate(PRICE_INDEX_ELECTRIC_OLD = PRICE_INDEX_ELECTRIC_OLD * (100 / mean(PRICE_INDEX_ELECTRIC_OLD[year(date) == 2017])))

POWER_FIXED_NOMINAL_PRICE_INDEXES_Graph <- ggplot() +
  geom_line(data = filter(POWER_FIXED_NOMINAL_PRICE_INDEXES, date >= as.Date("2000-01-01")), aes(x=date, y = (NOMINAL_ELECTRIC_OLD/PRICE_INDEX_ELECTRIC_OLD)/10, color = "Real Private Electric Power Structures Fixed Investment: Old Data"), size = 1.25) + 
  geom_line(data = filter(POWER_FIXED_NOMINAL_PRICE_INDEXES, date >= as.Date("2000-01-01")), aes(x=date, y = (NOMINAL_ELECTRIC_NEW/PRICE_INDEX_ELECTRIC_NEW)/10, color = "Real Private Electric Power Structures Fixed Investment: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,100), breaks = c(0,50,100), expand = c(0,0)) +
  ylab("2017 Dollars, Annualized") +
  ggtitle("Revisions to US Electric Power Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Revisions Incorporating Price Declines in Renewables Have Raised Real Power Sector Investment") +
  theme_apricitas + theme(legend.position = c(.5,.125), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 3-(.3*100), ymax = 3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = POWER_FIXED_NOMINAL_PRICE_INDEXES_Graph, "Real Fixed Power Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CORPORATE_PROFITS_NEW <- fredr(series_id = "A053RC1Q027SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

CORPORATE_PROFITS_OLD <- fredr(series_id = "A053RC1Q027SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date, value)

NONFINANCIAL_CORPORATE_PROFITS_NEW <- fredr(series_id = "A464RC1Q027SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)
  
NONFINANCIAL_CORPORATE_PROFITS_OLD <- fredr(series_id = "A464RC1Q027SBEA",observation_start = as.Date("2010-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date,value)

CORPORATE_PROFITS_REVISIONS_GRAPH <- ggplot() +
  geom_line(data = NONFINANCIAL_CORPORATE_PROFITS_OLD, aes(x=date, y = value/1000, color = "Nonfinancial Corporate Profits: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = CORPORATE_PROFITS_OLD, aes(x=date, y = value/1000, color = "Corporate Profits: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = NONFINANCIAL_CORPORATE_PROFITS_NEW, aes(x=date, y = value/1000, color = "Nonfinancial Corporate Profits: New Revised Data"), size = 1.25) + 
  geom_line(data = CORPORATE_PROFITS_NEW, aes(x=date, y = value/1000, color = "Corporate Profits: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(0,4), breaks = c(0,1,2,3,4), expand = c(0,0)) +
  ylab("Trillions of US Dollars") +
  ggtitle("Revisions to US Corporate Profits") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Corporate Profits Were Revised Up, Helping Close a Gap Between GDP and GDI") +
  theme_apricitas + theme(legend.position = c(.5,.82)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= "Pretax Corporate Profits Without Inventory and Capital Consumption Adjustments",values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"), breaks = c("Corporate Profits: New Revised Data","Corporate Profits: Old Data","Nonfinancial Corporate Profits: New Revised Data","Nonfinancial Corporate Profits: Old Data"), guide = guide_legend(override.aes=list(linetype = c(1,2,1,2), lwd = c(1.25,0.75,1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*4), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CORPORATE_PROFITS_REVISIONS_GRAPH, "Corporate Profits Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


WAGES_SALARIES_NEW <- fredr(series_id = "A4102C1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02"), units = "pc1") %>%
  select(date,value)

WAGES_SALARIES_OLD <- fredr(series_id = "A4102C1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25"), units = "pc1") %>%
  select(date,value)

WAGES_SALARIES_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=WAGES_SALARIES_OLD, aes(x=date,y= value/100, color= "Compensation of Employees, Wages and Salaries: Old Data"), size = 1.25) +
  geom_line(data=WAGES_SALARIES_NEW, aes(x=date,y= value/100, color= "Compensation of Employees, Wages and Salaries: New Revised Data"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.04,.15), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Revisions to US Wages") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Wage Levels were Revised Slightly Upward, But More Importantly the Growth Path Changed") +
  theme_apricitas + theme(legend.position = c(.475,.975)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(.1861*(today()-as.Date("2017-01-01")))), ymin = -0.04-(.3*.19), ymax = -0.04) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WAGES_SALARIES_Graph, "Wages Salaries Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PETROL_PROFITS_OLD <-fredr(series_id = "N404RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date,value)

PETROL_PROFITS_NEW <- fredr(series_id = "N404RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

CHEMICAL_PROFITS_OLD <-fredr(series_id = "N403RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date,value)

CHEMICAL_PROFITS_NEW <- fredr(series_id = "N403RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

COMPUTER_PROFITS_OLD <-fredr(series_id = "N501RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date,value)

COMPUTER_PROFITS_NEW <- fredr(series_id = "N501RC1Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)


CORPORATE_PROFITS_CATEGORY_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = PETROL_PROFITS_OLD, aes(x=date, y = value, color = "Petroleum and Coal Manufacturing: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = CHEMICAL_PROFITS_OLD, aes(x=date, y = value, color = "Chemical Manufacturing: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = COMPUTER_PROFITS_OLD, aes(x=date, y = value, color = "Computer and Electronics Manufacturing: Old Data"), size = 0.75, linetype = "dashed") + 
  geom_line(data = PETROL_PROFITS_NEW, aes(x=date, y = value, color = "Petroleum and Coal Manufacturing: New Revised Data"), size = 1.25) + 
  geom_line(data = CHEMICAL_PROFITS_NEW, aes(x=date, y = value, color = "Chemical Manufacturing: New Revised Data"), size = 1.25) + 
  geom_line(data = COMPUTER_PROFITS_NEW, aes(x=date, y = value, color = "Computer and Electronics Manufacturing: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-75,190), breaks = c(-50,0,50,100,150), expand = c(0,0)) +
  ylab("US Dollars") +
  ggtitle("Revisions to US Corporate Profits") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Corporate Profits in Chemical, Computers, and Energy Manufacturing Were Revised Up") +
  theme_apricitas + theme(legend.position = c(.34,.855)) +
  theme(legend.key.width =  unit(.82, "cm"), legend.key.height = unit(0, "cm"), legend.text = element_text(size = 12)) +
  scale_color_manual(name= "Corporate Profits With Inventory Valuation Adjustments",values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#EE6055","#A7ACD9","#A7ACD9"), breaks = c("Chemical Manufacturing: New Revised Data","Chemical Manufacturing: Old Data","Computer and Electronics Manufacturing: New Revised Data","Computer and Electronics Manufacturing: Old Data","Petroleum and Coal Manufacturing: New Revised Data","Petroleum and Coal Manufacturing: Old Data"), guide = guide_legend(override.aes=list(linetype = c(1,2,1,2,1,2), lwd = c(1.25,0.75,1.25,0.75,1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*4), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CORPORATE_PROFITS_CATEGORY_GRAPH, "Corporate Profits Detailed Category Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GDP_GROWTH_NEW <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02"), units = "pc1") %>%
  select(date,value)

GDP_GROWTH_OLD <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25"), units = "pc1") %>%
  select(date,value)

GDI_GROWTH_NEW <- fredr(series_id = "GDI",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02"), units = "pc1") %>%
  select(date,value)

GDI_GROWTH_OLD <- fredr(series_id = "GDI",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25"), units = "pc1") %>%
  select(date,value)


NGDP_NGDI_Growth_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=GDP_GROWTH_OLD, aes(x=date,y= value/100, color= "Growth in Nominal GDP: Old Data"), size = 0.75, linetype = "dashed") +
  geom_line(data=GDI_GROWTH_OLD, aes(x=date,y= value/100, color= "Growth in Nominal GDI: Old Data"), size = 0.75, linetype = "dashed") +
  geom_line(data=GDI_GROWTH_NEW, aes(x=date,y= value/100, color= "Growth in Nominal GDI: New Revised Data"), size = 1.25) +
  geom_line(data=GDP_GROWTH_NEW, aes(x=date,y= value/100, color= "Growth in Nominal GDP: New Revised Data"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.05,0,0.05,0.10,0.15), limits = c(-0.075,.175), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Nominal Growth Continues Decelerating") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Revisions Have Not Changed That Fundamentally Nominal Spending Growth Has Decelerated") +
  theme_apricitas + theme(legend.position = c(.34,.89)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Growth in Nominal GDP: New Revised Data","Growth in Nominal GDP: Old Data","Growth in Nominal GDI: New Revised Data","Growth in Nominal GDI: Old Data"), guide = guide_legend(override.aes=list(linetype = c(1,2,1,2), lwd = c(1.25,0.75,1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(.1861*(today()-as.Date("2017-01-01")))), ymin = -0.075-(.3*.25), ymax = -0.075) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_NGDI_Growth_Graph, "NGDP GDI Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

DISCREPANCY_NEW <- fredr(series_id = "SB0000081Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-10-02"), realtime_end = as.Date("2023-10-02")) %>%
  select(date,value)

DISCREPANCY_OLD <- fredr(series_id = "SB0000081Q027SBEA",observation_start = as.Date("2017-01-01"),realtime_start = as.Date("2023-09-25"), realtime_end = as.Date("2023-09-25")) %>%
  select(date,value)

STAT_DISCREPANCY_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=DISCREPANCY_OLD, aes(x=date,y= value/100, color= "Statistical Discrepancy Between GDP and GDI: Old Data"), size = 1.25) +
  geom_line(data=DISCREPANCY_NEW, aes(x=date,y= value/100, color= "Statistical Discrepancy Between GDP and GDI: New Revised Data"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.02,-0.01,0,0.01,0.02), limits = c(-0.025,.025), expand = c(0,0)) +
  ylab("Statistical Discrepancy, % of GDP") +
  ggtitle("The Surviving Statistical Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Revisions Have Not Fully Closed the NGDP-NGDI Statistical Discrepancy") +
  theme_apricitas + theme(legend.position = c(.5,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Statistical Discrepancy Between GDP and GDI: New Revised Data","Statistical Discrepancy Between GDP and GDI: Old Data","Growth in Nominal GDI: New Revised Data","Growth in Nominal GDI: Old Data")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(.1861*(today()-as.Date("2017-01-01")))), ymin = -0.025-(.3*.05), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STAT_DISCREPANCY_Graph, "STAT DISCREPANCY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


PCE_PRICE_INDEX_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T20804',
  'Frequency' = 'M',
  'Year' = paste(seq(from = 2016, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_PRICE_INDEX <- beaGet(PCE_PRICE_INDEX_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "1 month"))) %>%
  clean_names() %>%
  drop_na() %>%
  select(date, t20804_dpcerg_1_personal_consumption_expenditures_pce_fisher_price_index_level_0, t20804_dpccrg_25_pce_excluding_food_and_energy_fisher_price_index_level_0, t20804_ia001260_28_pce_services_excluding_energy_and_housing_fisher_price_index_level_0) %>%
  setNames(c("date","pce","pce_lfe","pce_nhs")) %>%
  arrange(date) %>%
  transmute(date, pce = (pce-lag(pce,12))/lag(pce,12),pce_lfe = (pce_lfe-lag(pce_lfe,12))/lag(pce_lfe,12), pce_nhs = (pce_nhs-lag(pce_nhs,12))/lag(pce_nhs,12)) %>%
  drop_na()

PCE_PRICE_INDEX_Graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=PCE_PRICE_INDEX, aes(x=date,y= pce, color= "Headline PCE Inflation"), size = 1.25) +
  geom_line(data=PCE_PRICE_INDEX, aes(x=date,y= pce_lfe, color= "PCE Less Food and Energy Inflation"), size = 1.25) +
  geom_line(data=PCE_PRICE_INDEX, aes(x=date,y= pce_nhs, color= "Core PCE Services Ex Housing Inflation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07), limits = c(0,.072), expand = c(0,0)) +
  ylab("Percent Growth, Year on Year") +
  ggtitle("Inflation Continues Cooling") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "PCE Inflation Has Decelerated—With Headline Data Falling The Fastest") +
  theme_apricitas + theme(legend.position = c(.34,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Headline PCE Inflation","PCE Less Food and Energy Inflation","Core PCE Services Ex Housing Inflation")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(.1861*(today()-as.Date("2018-01-01")))), ymin = 0-(.3*.072), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_PRICE_INDEX_Graph, "PCE Price Index Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing




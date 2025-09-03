pacman::p_load(wbstats,openxlsx,jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Indicator_Search <- wbsearch("GDP per Capita")

new_cache <- wb_cache()

GDP_PER_CAPITA_PPP_BULK_2019 <- wb_data("NY.GDP.PCAP.PP.KD", start_date = 2019, end_date = 2050) 

GDP_PER_CAPITA_PPP_GROWTH <- GDP_PER_CAPITA_PPP_BULK_2019 %>%
  select(iso3c, date, `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = iso3c, values_from = `NY.GDP.PCAP.PP.KD`) %>%
  mutate(across(-date, ~ last(.x) / first(.x))) %>%
  select(-date) %>%
  unique() %>%
  pivot_longer(cols = everything())

GDP_PER_CAPITA_PPP_GROWTH_MINUS_US <- GDP_PER_CAPITA_PPP_GROWTH %>%
  mutate(value = value - GDP_PER_CAPITA_PPP_GROWTH$value[GDP_PER_CAPITA_PPP_GROWTH$name == "USA"])
  
GDP_PER_CAPITA_LEVEL <- GDP_PER_CAPITA_PPP_BULK_2019 %>%
  select(country, date, `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country, values_from = `NY.GDP.PCAP.PP.KD`) %>%
  slice(nrow(.)) %>%
  select(-date) %>%
  pivot_longer(cols = everything())
  
GDP_PER_CAPITA_LEVEL_FILTERED <- GDP_PER_CAPITA_LEVEL %>%
  filter(name %in% c("India", "China", "United States", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Ethiopia","Philippines")) %>%
  arrange(value) %>%
  mutate(name = factor(name, levels = c("Ethiopia","Pakistan","Nigeria","Bangladesh","India","Philippines","Indonesia","Brazil","China","United States")))

GDP_PER_CAPITA_LEVEL_FILTERED_graph <- ggplot(data = GDP_PER_CAPITA_LEVEL_FILTERED, aes(x = name, y = value/1000,fill = "Real GDP/Capita, Adjusted for Purchasing Power Parity")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("Real GDP Per Capita (2021 PPP)") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"), limits = c(0,ceiling(max(GDP_PER_CAPITA_LEVEL_FILTERED$value)/10000)*10), expand = c(0,0)) +
  ggtitle("The Scale of Global Economic Disparities") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  theme_apricitas + theme(legend.position = c(.6,.6), axis.text.y = element_text(size = 16), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = GDP_PER_CAPITA_LEVEL_FILTERED_graph, "Real GDP Per Capita Level, Filtered Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




WORLD_MAP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform('EPSG:3395') %>%
  transmute(name = iso_a3_eh) %>%
  full_join(.,GDP_PER_CAPITA_PPP_GROWTH_MINUS_US, by = "name") %>%
  mutate(value_bucket = cut(value, breaks = c(-Inf,-0.06,-0.02,0.02,0.06, Inf), labels = c("6% Slower or More", "2-6% Slower", "About the Same", "2-6% Faster", "6% Faster or More")))
  

WORLD_MAP_GRAPH <- ggplot() +
  geom_sf(data = WORLD_MAP, aes(fill = value_bucket), color = NA) +
  #coord_sf("mercator", lims_method = "geometry_bbox") +
  scale_fill_manual(name= "GDP Per Capita,\nPPP-Adjusted\n2019-24 Growth\nRelative to the US", breaks = c("6% Slower or More", "2-6% Slower", "About the Same", "2-6% Faster", "6% Faster or More"), values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")) +
  ggtitle("   Post-COVID GDP/Capita Growth,\n   Relative to the United States") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-13000000, 18500000)) +
  scale_y_continuous(limits = c(-8000000, 10000000)) +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data") +
  theme(plot.title = element_text(size = 30),
        legend.position = c(1.135,.6),
        #legend.key.height = unit(0, "cm"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 1.8, 0, 0.1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = WORLD_MAP_GRAPH, "World Convergence Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

POVERTY_SHARE_215 <- wb_data("SI.POV.DDAY", start_date = 1960, end_date = 2050, country = "all") %>%
  filter(country == "World") %>%
  transmute(date = as.Date(paste0(date, "-01-01")), value = `SI.POV.DDAY`)

POVERTY_SHARE_365 <- wb_data("SI.POV.LMIC", start_date = 1960, end_date = 2050, country = "all") %>%
  filter(country == "World") %>%
  transmute(date = as.Date(paste0(date, "-01-01")), value = `SI.POV.LMIC`)

POVERTY_SHARE_685 <- wb_data("SI.POV.UMIC", start_date = 1960, end_date = 2050, country = "all") %>%
  filter(country == "World") %>%
  transmute(date = as.Date(paste0(date, "-01-01")), value = `SI.POV.UMIC`)


GLOBAL_POVERTY_graph <- ggplot() +
  geom_line(data=filter(POVERTY_SHARE_215, date >= as.Date("1980-01-01")), aes(x=date,y= value/100,color="$2.15 a Day"), size = 1.25) +
  geom_line(data=filter(POVERTY_SHARE_365, date >= as.Date("1980-01-01")), aes(x=date,y= value/100,color="$3.65 a Day"), size = 1.25) +
  geom_line(data=filter(POVERTY_SHARE_685, date >= as.Date("1980-01-01")), aes(x=date,y= value/100,color="$6.85 a Day"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of Global Population") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.75), expand = c(0,0)) +
  ggtitle("COVID Slowed Global Poverty Reduction") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "The COVID Pandemic & Ensuing Inflation Interrupted the Poverty Reduction Progress of the 2010s") +
  theme_apricitas + theme(legend.position = "right") +
  scale_color_manual(name= "Share of World Population\nUnder Select Poverty Lines\n(2017 PPP)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("$6.85 a Day","$3.65 a Day","$2.15 a Day")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.26*(today()-as.Date("1980-01-01"))), xmax = as.Date("1980-01-01")-(0.049*(today()-as.Date("1980-01-01"))), ymin = 0-(.3*(.75)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLOBAL_POVERTY_graph, "Global Poverty Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GDP_PER_CAPITA_PPP_BULK_LONG <- wb_data("NY.GDP.PCAP.PP.KD", start_date = 1950, end_date = 2050) 

INDIA_CHINA_US_COMPARISON <- GDP_PER_CAPITA_PPP_BULK_LONG %>%
  filter(country %in% c("India","China","United States")) %>%
  transmute(date = as.Date(paste0(date, "-01-01")), country, value = `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country) %>%
  drop_na()

INDIA_CHINA_US_COMPARISON_graph <- ggplot() +
  geom_line(data=filter(INDIA_CHINA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= China/`United States`,color="China"), size = 1.25) +
  geom_line(data=filter(INDIA_CHINA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= India/`United States`,color="India"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of US GDP/Capita") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.35), expand = c(0,0)) +
  ggtitle("Development in the Two Largest Countries") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "China & India are Both Converging with US Economic Activity, But India has Been Much Slower") +
  theme_apricitas + theme(legend.position = c(.25,.70)) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita\nRelative to the United States",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("China","India")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*((today()-as.Date("1990-01-01")))), ymin = 0-(.3*(.35)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INDIA_CHINA_US_COMPARISON_graph, "India China US Comparison Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ASIA_US_COMPARISON <- GDP_PER_CAPITA_PPP_BULK_LONG %>%
  filter(country %in% c("Pakistan","Bangladesh","Indonesia","Philippines","Viet Nam","United States")) %>%
  transmute(date = as.Date(paste0(date, "-01-01")), country, value = `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country) %>%
  drop_na()

ASIA_US_COMPARISON_graph <- ggplot() +
  geom_line(data=filter(ASIA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Bangladesh/`United States`,color="Bangladesh"), size = 1.25) +
  geom_line(data=filter(ASIA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Indonesia/`United States`,color="Indonesia"), size = 1.25) +
  geom_line(data=filter(ASIA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Pakistan/`United States`,color="Pakistan"), size = 1.25) +
  geom_line(data=filter(ASIA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Philippines/`United States`,color="Philippines"), size = 1.25) +
  geom_line(data=filter(ASIA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= `Viet Nam`/`United States`,color="Vietnam"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of US GDP/Capita") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.22), expand = c(0,0)) +
  ggtitle("Asian Catch-Up Growth Has Weakened") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "Catch-Up Growth Has Slowed in Many Major South/Southeast Asian Nations Post-COVID") +
  theme_apricitas + theme(legend.position = c(.2,.81), legend.key.height = unit(0,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita\nRelative to the United States",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Indonesia","Vietnam","Philippines","Bangladesh","Pakistan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*((today()-as.Date("1990-01-01")))), ymin = 0-(.3*(.21)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ASIA_US_COMPARISON_graph, "Asia US Comparison Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

AFRICA_US_COMPARISON <- GDP_PER_CAPITA_PPP_BULK_LONG %>%
  filter(country %in% c("Nigeria","Ethiopia","Tanzania","Congo, Dem. Rep.","South Africa","Kenya","United States")) %>%
  transmute(date = as.Date(paste0(date, "-01-01")), country, value = `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country) %>%
  drop_na()

AFRICA_US_COMPARISON_graph <- ggplot() +
  geom_line(data=filter(AFRICA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Nigeria/`United States`,color="Nigeria"), size = 1.25) +
  geom_line(data=filter(AFRICA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Ethiopia/`United States`,color="Ethiopia"), size = 1.25) +
  geom_line(data=filter(AFRICA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Tanzania/`United States`,color="Tanzania"), size = 1.25) +
  geom_line(data=filter(AFRICA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= `Congo, Dem. Rep.`/`United States`,color="DRC"), size = 1.25) +
  geom_line(data=filter(AFRICA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= `Kenya`/`United States`,color="Kenya"), size = 1.25) +
  #geom_line(data=filter(AFRICA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= `South Africa`/`United States`,color="South Africa"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of US GDP/Capita") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.125), expand = c(0,0)) +
  ggtitle("African Catch-Up Growth Has Weakened") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "Major African Nations like Ethiopia and Tanzania Have Seen Catch-Up Growth Falter Post-COVID") +
  theme_apricitas + theme(legend.position = c(.4,.81), legend.key.height = unit(0,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita\nRelative to the United States",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Nigeria","Kenya","Tanzania","Ethiopia","DRC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*((today()-as.Date("1990-01-01")))), ymin = 0-(.3*(.125)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AFRICA_US_COMPARISON_graph, "Africa US Comparison Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


RWANDA_US_COMPARISON <- GDP_PER_CAPITA_PPP_BULK_LONG %>%
  filter(country %in% c("Rwanda","United States")) %>%
  transmute(date = as.Date(paste0(date, "-01-01")), country, value = `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country) %>%
  drop_na()

RWANADA_US_COMPARISON_graph <- ggplot() +
  geom_line(data=filter(RWANDA_US_COMPARISON, date >= as.Date("1980-01-01")), aes(x=date,y= Rwanda/`United States`,color="Rwanda's PPP-Adjusted GDP/Capita\nRelative to the US"), size = 1.25) +
  annotate(geom = "segment", x = as.Date("1994-01-01"), xend = as.Date("1994-01-01"), y = 0, yend = .03, color = "white",linetype = "dashed", size = 1) +
  annotate("text", label = "Rwandan\nGenocide", x = as.Date("1994-06-01"), y = 0.0275, color = "white", size = 4, hjust = 0, lineheight = 0.8) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of US GDP/Capita") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.045), expand = c(0,0)) +
  ggtitle("Rwanda's Catch-Up Growth") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "Rwanda's GDP Has Been Steadily Converging with America's Since the Turn of the Millenium") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*((today()-as.Date("1990-01-01")))), ymin = 0-(.3*(.045)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RWANADA_US_COMPARISON_graph, "Rwanda US Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

POPULATION_BULK_LONG <- wb_data("SP.POP.TOTL", start_date = 1950, end_date = 2050) 

CATEGORY_BULK <- wb_countries()

#https://datahelpdesk.worldbank.org/knowledgebase/articles/906519#High_income
#CATEGORIES CHANGE OVER TIME—LOOK HERE!

CONVERGENCE_DATA <- GDP_PER_CAPITA_PPP_BULK_LONG %>%
  transmute(date = as.Date(paste0(date, "-01-01")), country, GDP = `NY.GDP.PCAP.PP.KD`) %>%
  merge(., POPULATION_BULK_LONG %>% transmute(date = as.Date(paste0(date, "-01-01")), country, POP = `SP.POP.TOTL`), by = c("country", "date")) %>%
  left_join(., CATEGORY_BULK %>% select(country,income_level), by = "country") %>%
  mutate(income_level = ifelse(country == "Bulgaria" & date <= as.Date("2023-01-01"), "middle_income", income_level)) %>% #Adding countries that graduated from middle to high income for the periods they were middle income. From: https://en.wikipedia.org/wiki/World_Bank_high-income_economy
  mutate(income_level = ifelse(country == "Chile" & date <= as.Date("2012-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Croatia" & date <= as.Date("2017-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Czechia" & date <= as.Date("2006-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Estonia" & date <= as.Date("2006-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Greece" & date <= as.Date("1996-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Hungary" & date <= as.Date("2014-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Latvia" & date <= as.Date("2012-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Lithuania" & date <= as.Date("2012-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Oman" & date <= as.Date("2007-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Panama" & date <= as.Date("2021-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Poland" & date <= as.Date("2009-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Portugal" & date <= as.Date("1994-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Russian Federation" & date <= as.Date("2023-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Saudi Arabia" & date <= as.Date("2004-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Korea, Rep." & date <= as.Date("2001-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Uruguay" & date <= as.Date("2012-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Aruba" & date <= as.Date("1994-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Puerto Rico" & date <= as.Date("2002-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Macao SAR, China" & date <= as.Date("1994-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "New Caledonia" & date <= as.Date("1995-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "St. Martin (French Part)" & date <= as.Date("2010-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Trinidad and Tobago" & date <= as.Date("2006-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Slovak Republic" & date <= as.Date("2007-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Romania" & date <= as.Date("2021-01-01"), "middle_income", income_level)) %>%
  mutate(income_level = ifelse(country == "Antigua and Barbuda" & date <= as.Date("2012-01-01"), "middle_income", income_level)) %>%
  filter(income_level != "High income" | country == "United States") %>%
  drop_na()

CONVERGENCE_DATA_GROWTH_US <- CONVERGENCE_DATA %>%
  filter(country == "United States") %>%
  group_by(country) %>%
  mutate(US_GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  select(date,US_GDP) %>%
  drop_na()
  
CONVERGENCE_DATA_GROWTH_RAW_AVG <- CONVERGENCE_DATA %>%
  filter(country != "United States") %>%
  group_by(country) %>%
  mutate(GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(date) %>%
  summarize(Raw_mean_GDP = mean(GDP, na.rm = TRUE))

CONVERGENCE_DATA_GROWTH_WEIGHTED_AVG <- CONVERGENCE_DATA %>%
  filter(country != "United States") %>%
  group_by(country) %>%
  mutate(GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(date) %>%
  summarize(weighted_avg_GDP = sum(GDP * POP, na.rm = TRUE) / sum(POP, na.rm = TRUE))


CONVERGENCE_FINAL_DATA <- merge(CONVERGENCE_DATA_GROWTH_US,CONVERGENCE_DATA_GROWTH_RAW_AVG, by = "date") %>%
  merge(.,CONVERGENCE_DATA_GROWTH_WEIGHTED_AVG, by = "date")

CONVERGENCE_FINAL_graph <- ggplot() +
  geom_line(data=filter(CONVERGENCE_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= weighted_avg_GDP-US_GDP, color="Population-Weighted Average"), size = 1.25) +
  geom_line(data=filter(CONVERGENCE_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= Raw_mean_GDP-US_GDP, color="Country-Weighted Average"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Growth, Relative to the US, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.1,.30), expand = c(0,0)) +
  ggtitle("Global Economic Convergence Has Slowed") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "The COVID Pandemic & Ensuing Inflation Interrupted the Economic Convergence of the 2010s") +
  theme_apricitas + theme(legend.position = c(.22,.85)) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita Growth\nLow/Middle Income Countries\nRelative to the US\n5-Year Rolling Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Population-Weighted Average","Country-Weighted Average")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*(today()-as.Date("1980-01-01"))), xmax = as.Date("1980-01-01")-(0.049*(today()-as.Date("1980-01-01"))), ymin = 0-(.3*(.75)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONVERGENCE_FINAL_graph, "Convergence Final Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CONVERGENCE_DATA_LOW_INCOME <- CONVERGENCE_DATA %>%
  mutate(income_level = ifelse(country == "Lao PDR" & date <= as.Date("2010-01-01"), "Low income", income_level)) %>% #Adding countries that graduated from middle to high income for the periods they were middle income. From: https://en.wikipedia.org/wiki/World_Bank_high-income_economy
  mutate(income_level = ifelse(country == "Zambia" & date <= as.Date("2022-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Mauritania" & date <= as.Date("2012-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Bangladesh" & date <= as.Date("2014-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Kenya" & date <= as.Date("2014-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Myanmar" & date <= as.Date("2014-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Tajikistan" & date <= as.Date("2020-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Cambodia" & date <= as.Date("2015-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Comoros" & date <= as.Date("2018-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Zimbabwe" & date <= as.Date("2018-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Benin" & date <= as.Date("2019-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Nepal" & date <= as.Date("2019-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Tanzania" & date <= as.Date("2019-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Guinea" & date <= as.Date("2022-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Haiti" & date <= as.Date("2020-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Indonesia" & date <= as.Date("2003-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Egypt, Arab Rep." & date <= as.Date("1995-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "China" & date <= as.Date("1999-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Equatorial Guinea" & date <= as.Date("2004-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Lesotho" & date <= as.Date("2005-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Bhutan" & date <= as.Date("2006-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "India" & date <= as.Date("2007-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Nigeria" & date <= as.Date("2008-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Pakistan" & date <= as.Date("2008-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Sao Tome" & date <= as.Date("2008-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Viet Nam" & date <= as.Date("2009-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Ghana" & date <= as.Date("2010-01-01"), "Low income", income_level)) %>%
  filter(income_level == "Low income" | country == "United States") %>%
  drop_na()

CONVERGENCE_DATA_GROWTH_US <- CONVERGENCE_DATA %>%
  filter(country == "United States") %>%
  group_by(country) %>%
  mutate(US_GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  select(date,US_GDP) %>%
  drop_na()

CONVERGENCE_DATA_LOW_INCOME_GROWTH_RAW_AVG <- CONVERGENCE_DATA_LOW_INCOME %>%
  filter(country != "United States") %>%
  group_by(country) %>%
  mutate(GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(date) %>%
  summarize(Raw_mean_GDP = mean(GDP, na.rm = TRUE))

CONVERGENCE_DATA_LOW_INCOME_GROWTH_WEIGHTED_AVG <- CONVERGENCE_DATA_LOW_INCOME %>%
  filter(country != "United States") %>%
  group_by(country) %>%
  mutate(GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(date) %>%
  summarize(weighted_avg_GDP = sum(GDP * POP, na.rm = TRUE) / sum(POP, na.rm = TRUE))


CONVERGENCE_LOW_INCOME_FINAL_DATA <- merge(CONVERGENCE_DATA_GROWTH_US,CONVERGENCE_DATA_LOW_INCOME_GROWTH_RAW_AVG, by = "date") %>%
  merge(.,CONVERGENCE_DATA_LOW_INCOME_GROWTH_WEIGHTED_AVG, by = "date")


CONVERGENCE_LOW_INCOME_FINAL_graph <- ggplot() +
  geom_line(data=filter(CONVERGENCE_LOW_INCOME_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= weighted_avg_GDP-US_GDP, color="Population-Weighted Average"), size = 1.25) +
  geom_line(data=filter(CONVERGENCE_LOW_INCOME_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= Raw_mean_GDP-US_GDP, color="Country-Weighted Average"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Growth, Relative to the US, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.15,.30), breaks = c(-.1,0,0.1,0.2,0.3), expand = c(0,0)) +
  ggtitle("Global Economic Convergence Has Slowed") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "The COVID Pandemic & Ensuing Inflation Interrupted the Economic Convergence of the 2010s") +
  theme_apricitas + theme(legend.position = c(.22,.85)) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita Growth\nLow-Income Countries\nRelative to the US\n5-Year Rolling Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Population-Weighted Average","Country-Weighted Average")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*(today()-as.Date("1994-01-01"))), xmax = as.Date("1994-01-01")-(0.049*(today()-as.Date("1994-01-01"))), ymin = -.15-(.3*(.4)), ymax = -0.15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONVERGENCE_LOW_INCOME_FINAL_graph, "Convergence Low-Income Final Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CONVERGENCE_DATA_MIDDLE_INCOME <- CONVERGENCE_DATA %>%
  mutate(income_level = ifelse(country == "Lao PDR" & date <= as.Date("2010-01-01"), "Low income", income_level)) %>% #Adding countries that graduated from middle to high income for the periods they were middle income. From: https://en.wikipedia.org/wiki/World_Bank_high-income_economy
  mutate(income_level = ifelse(country == "Zambia" & date <= as.Date("2022-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Mauritania" & date <= as.Date("2012-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Bangladesh" & date <= as.Date("2014-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Kenya" & date <= as.Date("2014-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Myanmar" & date <= as.Date("2014-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Tajikistan" & date <= as.Date("2020-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Cambodia" & date <= as.Date("2015-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Comoros" & date <= as.Date("2018-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Zimbabwe" & date <= as.Date("2018-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Benin" & date <= as.Date("2019-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Nepal" & date <= as.Date("2019-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Tanzania" & date <= as.Date("2019-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Guinea" & date <= as.Date("2022-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Haiti" & date <= as.Date("2020-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Indonesia" & date <= as.Date("2003-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Egypt, Arab Rep." & date <= as.Date("1995-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "China" & date <= as.Date("1999-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Equatorial Guinea" & date <= as.Date("2004-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Lesotho" & date <= as.Date("2005-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Bhutan" & date <= as.Date("2006-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "India" & date <= as.Date("2007-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Nigeria" & date <= as.Date("2008-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Pakistan" & date <= as.Date("2008-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Sao Tome" & date <= as.Date("2008-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Viet Nam" & date <= as.Date("2009-01-01"), "Low income", income_level)) %>%
  mutate(income_level = ifelse(country == "Ghana" & date <= as.Date("2010-01-01"), "Low income", income_level)) %>%
  filter(income_level != "Low income" | country == "United States") %>%
  drop_na()

CONVERGENCE_DATA_GROWTH_US <- CONVERGENCE_DATA %>%
  filter(country == "United States") %>%
  group_by(country) %>%
  mutate(US_GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  select(date,US_GDP) %>%
  drop_na()

CONVERGENCE_DATA_MIDDLE_INCOME_GROWTH_RAW_AVG <- CONVERGENCE_DATA_MIDDLE_INCOME %>%
  filter(country != "United States") %>%
  group_by(country) %>%
  mutate(GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(date) %>%
  summarize(Raw_mean_GDP = mean(GDP, na.rm = TRUE))

CONVERGENCE_DATA_MIDDLE_INCOME_GROWTH_WEIGHTED_AVG <- CONVERGENCE_DATA_MIDDLE_INCOME %>%
  filter(country != "United States") %>%
  group_by(country) %>%
  mutate(GDP = GDP/lag(GDP,5)) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(date) %>%
  summarize(weighted_avg_GDP = sum(GDP * POP, na.rm = TRUE) / sum(POP, na.rm = TRUE))


CONVERGENCE_MIDDLE_INCOME_FINAL_DATA <- merge(CONVERGENCE_DATA_GROWTH_US,CONVERGENCE_DATA_MIDDLE_INCOME_GROWTH_RAW_AVG, by = "date") %>%
  merge(.,CONVERGENCE_DATA_MIDDLE_INCOME_GROWTH_WEIGHTED_AVG, by = "date")

CONVERGENCE_MIDDLE_INCOME_FINAL_graph <- ggplot() +
  geom_line(data=filter(CONVERGENCE_MIDDLE_INCOME_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= weighted_avg_GDP-US_GDP, color="Population-Weighted Average"), size = 1.25) +
  geom_line(data=filter(CONVERGENCE_MIDDLE_INCOME_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= Raw_mean_GDP-US_GDP, color="Country-Weighted Average"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Growth, Relative to the US, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.1,.40), breaks = c(-.1,0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  ggtitle("Global Economic Convergence Has Slowed") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "The COVID Pandemic & Ensuing Inflation Interrupted the Economic Convergence of the 2010s") +
  theme_apricitas + theme(legend.position = c(.22,.85)) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita Growth\nMiddle-Income Countries\nRelative to the US\n5-Year Rolling Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Population-Weighted Average","Country-Weighted Average")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*(today()-as.Date("1994-01-01"))), xmax = as.Date("1994-01-01")-(0.049*(today()-as.Date("1994-01-01"))), ymin = -.1-(.3*(.5)), ymax = -0.1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONVERGENCE_MIDDLE_INCOME_FINAL_graph, "Convergence Middle-Income Final Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CONVERGENCE_MIDDLE_LOW_INCOME_FINAL_graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(CONVERGENCE_MIDDLE_INCOME_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= weighted_avg_GDP-US_GDP, color="Middle-Income Countries"), size = 1.25) +
  geom_line(data=filter(CONVERGENCE_LOW_INCOME_FINAL_DATA, date >= as.Date("1980-01-01")), aes(x=date,y= weighted_avg_GDP-US_GDP, color="Low-Income Countries"), size = 1.25) +
  xlab("Date") +
  ylab("Growth, Relative to the US, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.1,.35), breaks = c(-.15,0,0.1,0.2,0.3), expand = c(0,0)) +
  ggtitle("Global Economic Convergence Has Slowed") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data. NOTE: Growth is Population-Weighted", subtitle = "The COVID Pandemic & Ensuing Inflation Interrupted the Economic Convergence of the 2010s") +
  theme_apricitas + theme(legend.position = c(.22,.82), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "PPP-Adjusted GDP/Capita Growth\nRelative to the US\n4-Year Rolling Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Middle-Income Countries","Low-Income Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*(today()-as.Date("1994-01-01"))), xmax = as.Date("1994-01-01")-(0.049*(today()-as.Date("1994-01-01"))), ymin = -.15-(.3*(.45)), ymax = -0.15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONVERGENCE_MIDDLE_LOW_INCOME_FINAL_graph, "Convergence Middle and Low Income Final Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CONFLICT_DEATHS <- read.csv("https://ourworldindata.org/grapher/deaths-in-state-based-conflicts-by-region.csv?v=1&csvType=full&useColumnShortNames=true") %>%
  setNames(c("region","code","date","deaths")) %>%
  transmute(region,date = as.Date(paste0(date,"-01-01")),deaths) %>%
  filter(region != "World")
  
CONFLICT_DEATHS_graph <- ggplot(data = filter(CONFLICT_DEATHS, date >= as.Date("1980-01-01")), aes(x = date, y = deaths/1000, fill = region)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Global Conflict Deaths") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200,300), limits = c(0,300), expand = c(0,0)) +
  ggtitle("Conflict Deaths Have Risen Post-COVID") +
  labs(caption = "Graph created by @JosephPolitano using Uppsala Conflict Data Program data", subtitle = "Conflicts in Gaza, Ethiopia, Sudan, Myanmar, Ukraine, & More Have Driven an Increase in Deaths") +
  scale_fill_manual(name= "Annual Conflict Deaths by Region",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Africa","Americas","Asia and Oceania","Europe","Middle East")) +
  theme_apricitas + theme(legend.position = c(.45,.77)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*(today()-as.Date("1980-01-01"))), xmax = as.Date("1980-01-01")-(0.049*(today()-as.Date("1980-01-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONFLICT_DEATHS_graph, "Conflict Deaths Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

IMF_GLOBAL_FOOD_INDEX <- fredr("PFOODINDEXM", observation_start = as.Date("2016-01-01"))

IMF_GLOBAL_ENERGY_INDEX <- fredr("PNRGINDEXM", observation_start = as.Date("2016-01-01"))

IMF_GLOBAL_PRICE_INDEX_graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(IMF_GLOBAL_FOOD_INDEX, date >= as.Date("1980-01-01")), aes(x=date,y= value, color="Global Price of Food Index"), size = 1.25) +
  geom_line(data=filter(IMF_GLOBAL_ENERGY_INDEX, date >= as.Date("1980-01-01")), aes(x=date,y= value, color="Global Price of Energy Index"), size = 1.25) +
  xlab("Date") +
  ylab("Index, 2016 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,400), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ggtitle("Food & Energy Prices Spiked Post-COVID") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "The Post-COVID Era Saw a Massive Increase in Global Food and Energy Prices") +
  theme_apricitas + theme(legend.position = c(.22,.82), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "IMF Commodity Price Indexes",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Global Price of Energy Index","Global Price of Food Index")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*(400)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IMF_GLOBAL_PRICE_INDEX_graph, "IMF Global Price Index Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#https://prosperitydata360.worldbank.org/en/indicator/IMF+WEO+GGX_NGDP
GOV_EXPENSE_SHARE_GDP <- data.frame(date = seq.Date(from = as.Date("2016-01-01"), to = as.Date("2023-01-01"), by = "1 year"),
                          high_income = c(37.26,36.48,36.74,37.69,44.64,41.84,38.23,37.15), #High Income
                          upper_mid_income =c(29.99,29.32,30.15,31.39,33.54,32.12,30.12,33.86), #Upper Middle
                          lower_mid_income = c(25.62,25.72,26.04,24.66,26.24,26.5,26.67,25.52), #Lower Middle
                          low_income = c(20.13,20.25,19.43,20.29,24.54,21.79,21.59,24.6)) #Lower

GOV_EXPENSE_SHARE_GDP_graph <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(GOV_EXPENSE_SHARE_GDP, date >= as.Date("1980-01-01")), aes(x=date,y= (high_income-high_income[4])/100, color="High Income Countries"), size = 1.25) +
  geom_line(data=filter(GOV_EXPENSE_SHARE_GDP, date >= as.Date("1980-01-01")), aes(x=date,y= (upper_mid_income-upper_mid_income[4])/100, color="Upper Middle Income Countries"), size = 1.25) +
  geom_line(data=filter(GOV_EXPENSE_SHARE_GDP, date >= as.Date("1980-01-01")), aes(x=date,y= (lower_mid_income-lower_mid_income[4])/100, color="Lower Middle Income Countries"), size = 1.25) +
  geom_line(data=filter(GOV_EXPENSE_SHARE_GDP, date >= as.Date("1980-01-01")), aes(x=date,y= (low_income-low_income[4])/100, color="Low Income Countries"), size = 1.25) +
  xlab("Date") +
  ylab("Percentage Points of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.025,0.08), breaks = c(-0.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ggtitle("Fiscal Impulses Were Larger for Rich Nations") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data", subtitle = "Rich Nations Could Deficit Spend in Response to COVID Easier Than Low/Middle Income Countries") +
  theme_apricitas + theme(legend.position = c(.42,.81), plot.title = element_text(size = 26)) +
  scale_color_manual(name= "Median Change in Government Spending From 2019, % of GDP",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("High Income Countries","Upper Middle Income Countries","Lower Middle Income Countries","Low Income Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-720-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-720-as.Date("2016-01-01"))), ymin = -0.025-(.3*(.105)), ymax = -0.025) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GOV_EXPENSE_SHARE_GDP_graph, "Gov Expense Share GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all packages using the package manager


# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

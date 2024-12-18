BEA_GDP_COUNTIES_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

#Just checking the raw increase in GDP by Metro Since 2019
BEA_GDP_COUNTY_INCREASES <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(INCREASE = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  #slice(-nrow(.)) %>%
  top_n(100, DataValue) %>%
  transmute(GEOID = GeoFips, INCREASE)


BEA_GDP_COUNTIES <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  #mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, CAGR)




BEA_GDP_METRO_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2015, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_METRO_15_19_23 <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth_15_19 = DataValue[5]/DataValue[1]-1) %>%
  mutate(Growth_19_23 = DataValue[9]/DataValue[5]-1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  mutate(Difference = Growth_19_23-Growth_15_19) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, GeoName,DataValue,Growth_15_19, Growth_19_23,Difference) %>%
  mutate(GeoName = sub("[-,].*", "", GeoName))

BEA_GDP_METRO_15_19_23_TOP_5 <- BEA_GDP_METRO_15_19_23  %>%
  filter(grepl("^(New York|Los Angeles|Chicago|San Francisco|Washington)", GeoName))

US_GDP_15_19_23 <- fredr("GDPC1", observation_start = as.Date("2015-01-01"), frequency = "a") %>%
  drop_na() %>%
  mutate(Growth_15_19 = value[5]/value[1]-1) %>%
  mutate(Growth_19_23 = value[9]/value[5]-1) %>%
  mutate(Difference = Growth_19_23-Growth_15_19) %>%
  transmute(GEOID = "12345", GeoName = "United States", DataValue = value,Growth_15_19,Growth_19_23,Difference) %>%
  slice(nrow(.)) %>%
  rbind(BEA_GDP_METRO_15_19_23_TOP_5) %>%
  transmute(GeoName, `2015-2019 RGDP Growth` = Growth_15_19,`2019-2023 RGDP Growth` = Growth_19_23) %>%
  pivot_longer(-GeoName) %>%
  mutate(GeoName = factor(GeoName, rev(c("United States","San Francisco","Los Angeles","Washington","New York","Chicago")))) %>%
  mutate(name = factor(name, rev(c("2015-2019 RGDP Growth","2019-2023 RGDP Growth"))))


REAL_GDP_CITIES_BAR_Graph <- ggplot(data = filter(US_GDP_15_19_23, GeoName != "United States"), aes(x = GeoName, y = value, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("% GDP Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.35), expand = c(0,0)) +
  ggtitle("Big Metro Areas' GDP Growth\nHas Slowed Post-COVID") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055"), breaks = c("2015-2019 RGDP Growth","2019-2023 RGDP Growth")) +
  labs(caption = "Graph created by @JosephPolitano using BEA Data. Top 5 Based on 2019 GDP.") +
  theme_apricitas + theme(legend.position = c(.75,.80), axis.text.y = element_text(size = 16, color = "grey80"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 29)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = REAL_GDP_CITIES_BAR_Graph, "Real GDP Cities Bar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2014, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_INF_METRO_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 45, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2014, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_TECH_METROS <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  filter(GeoFips %in% c(42660,41860,41940)) %>%
  mutate(GeoName = sub("[-,].*", "", GeoName)) %>%
  transmute(GeoName,date = as.Date(paste0(TimePeriod,"-01-01")), value = DataValue)

BEA_GDP_INFO_METROS <- beaGet(BEA_INF_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  filter(GeoFips %in% c(42660,41860,41940)) %>%
  mutate(GeoName = sub("[-,].*", "", GeoName)) %>%
  transmute(GeoName,date = as.Date(paste0(TimePeriod,"-01-01")), value = DataValue) %>%
  filter(value != 0)


PLOT_GDP_TECH_METROS <- ggplot(BEA_GDP_TECH_METROS, aes(x = date, y = value/1000000, color = GeoName)) +
  geom_line(size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,750), expand = c(0,0)) +
  ylab("Real GDP, 2017 Dollars") +
  labs(subtitle = "Real GDP") +
  scale_color_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, 0, 0, 0), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"))

PLOT_INF_TECH_METROS <- ggplot(BEA_GDP_INFO_METROS, aes(x = date, y = value/1000000, color = GeoName)) +
  geom_line(size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,175), expand = c(0,0)) +
  ylab("Real GVA, 2017 Dollars") +
  labs(subtitle = "Real Information Value-Add") +
  scale_color_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, 0, 0, 0), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"))

# Create a text grob
tgrob <- text_grob(expression(bold("Major Tech Metro's Economies")),size = 30, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-0.15,-0.15,-0.15),"cm"))  

TECH_METRO_GROWTH_ARRANGE <- ggarrange(PLOT_GDP_TECH_METROS,PLOT_INF_TECH_METROS, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32") 

TECH_METRO_GROWTH_ARRANGE <- ggarrange(plot_0,TECH_METRO_GROWTH_ARRANGE, nrow = 2, heights = c(2,20), widths = 10)

ggsave(dpi = "retina",plot = TECH_METRO_GROWTH_ARRANGE, "Tech Metro Arrange Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

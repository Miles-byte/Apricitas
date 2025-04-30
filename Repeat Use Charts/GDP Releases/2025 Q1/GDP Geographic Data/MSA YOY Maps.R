BEA_GDP_METRO_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_METRO_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth_yoy = (DataValue/lag(DataValue,1)) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, Growth_yoy)

#Just checking the raw increase in GDP by Metro Since 2019
BEA_GDP_METRO_INCREASES_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(INCREASE_YOY = DataValue-lag(DataValue,1)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, INCREASE_YOY)

MSA_map <- core_based_statistical_areas(cb = TRUE, year = 2021)

states <- states(cb = TRUE, year = 2021) %>%
  mutate(state_abbv = STUSPS) %>%
  filter(!state_abbv %in% c("AK", "HI", "PR", "AS", "GU", "MP","VI"))

MSA_map_US_yoy <- merge(MSA_map, BEA_GDP_METRO_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth_yoy, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

BEA_GDP_MSA_GRADIENT_YOY <- MSA_map_US_yoy %>%
  mutate(Growth_yoy = case_when(
    Growth_yoy < 0 ~ 0,
    Growth_yoy > 0.06 ~ 0.06,
    TRUE ~ Growth_yoy
  )) %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_yoy), color = "black", lwd = 0.5) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  ggtitle("                 Real GDP Growth, 2023\n            50 Largest Metro Areas by GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BEA_GDP_MSA_GRADIENT_YOY, "BEA GDP MSA GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Texas


BEA_GDP_METRO_TX_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("TX", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)


MSA_map_TX_yoy <- merge(MSA_map, BEA_GDP_METRO_TX_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

MSA_map_TX_centroids_yoy <- MSA_map_TX_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_TX_yoy, .) %>%
  st_centroid()

BEA_GDP_MSA_TX_GRADIENT_YOY <- MSA_map_TX_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06 ~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("TX")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("      Real GDP Growth, 2023\n         Texas Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids_yoy, GEOID %in% c("12420")), #Austin
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -350000, # adjust these values as needed
    nudge_x = 450000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids_yoy, GEOID %in% c("26420")), #Houston
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000, # adjust these values as needed
    nudge_x = 600000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids_yoy, GEOID %in% c("19100")), #Dallas
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 300000, # adjust these values as needed
    nudge_x = 200000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids_yoy, GEOID %in% c("41700")), #San Antonio
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -300000, # adjust these values as needed
    nudge_x = -550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = BEA_GDP_MSA_TX_GRADIENT_YOY, "BEA GDP MSA TX GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#CA

BEA_GDP_METRO_CA_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("CA", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_CA_yoy <- merge(MSA_map, BEA_GDP_METRO_CA_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_CA_centroids_yoy <- MSA_map_CA_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_CA_yoy, .) %>%
  st_centroid()

BEA_GDP_MSA_CA_GRADIENT_YOY <- MSA_map_CA_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("CA")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("                Real GDP Growth, 2023\n                California Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids_yoy, GEOID %in% c("31080")), #Los Angeles
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids_yoy, GEOID %in% c("41860")), #San Francisco
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +50000, # adjust these values as needed
    nudge_x = -650000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids_yoy, GEOID %in% c("41940")), #San Jose
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids_yoy, GEOID %in% c("41740")), #San Diego
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids_yoy, GEOID %in% c("40140")), #Riverside
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = +650000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids_yoy, GEOID %in% c("40900")), #Sacramento
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = +400000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_CA_GRADIENT_YOY, "BEA GDP MSA CA GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Florida


BEA_GDP_METRO_FL_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("FL", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_FL_yoy <- merge(MSA_map, BEA_GDP_METRO_FL_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_FL_centroids_yoy <- MSA_map_FL_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_FL_yoy, .) %>%
  st_centroid()

BEA_GDP_MSA_FL_GRADIENT_YOY <- MSA_map_FL_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("FL")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("            Real GDP Growth, 2023\n             Florida Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids_yoy, GEOID %in% c("33100")), #Miami
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids_yoy, GEOID %in% c("45300")), #Tampa
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -150000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids_yoy, GEOID %in% c("36740")), #Orlando
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids_yoy, GEOID %in% c("27260")), #Jacksonville
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -175000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_FL_GRADIENT_YOY, "BEA GDP MSA FL GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#NE

BEA_GDP_METRO_NE_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("MA", "RI", "CT", "NY","NJ","PA","DE","MD","DC","VA","NH","VT","ME"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_NE_yoy <- merge(MSA_map, BEA_GDP_METRO_NE_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

MSA_map_NE_centroids_yoy <- MSA_map_NE_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_NE_yoy, .) %>%
  st_centroid()

BEA_GDP_MSA_NE_GRADIENT_YOY <- MSA_map_NE_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("MA", "RI", "CT", "NY","NJ","PA","DE","MD","DC","VA","NH","VT","ME")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("               Real GDP Growth, 2023\n   Northeast and Mid-Atlantic Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("35620")), #New York
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +150000, # adjust these values as needed
    nudge_x = +650000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("47900")), #Washington
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    nudge_x = +550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("14460")), #Boston
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = +750000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("37980")), #Philadelphia
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +80000, # adjust these values as needed
    nudge_x = +600000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("12580")), #Baltimore
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = +500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("38300")), #Pittsburgh
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("25540")), #Hartford
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 250000, # adjust these values as needed
    nudge_x = -700000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) + 
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("47260")), #Virginia Beach
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 40000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("40060")), #Richmond
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids_yoy, GEOID %in% c("14860")), #Bridgeport
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    nudge_x = -850000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_NE_GRADIENT_YOY, "BEA GDP MSA NE GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Rocky Mountain


BEA_GDP_METRO_RM_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("WA", "OR", "ID", "UT","CO","AZ","NV","NM"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_RM_yoy <- merge(MSA_map, BEA_GDP_METRO_RM_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

MSA_map_RM_centroids_yoy <- MSA_map_RM_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_RM_yoy, .) %>%
  st_centroid()


BEA_GDP_MSA_RM_GRADIENT_YOY <- MSA_map_RM_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("WA", "OR", "ID", "UT","CO","AZ","NV","NM")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("               Real GDP Growth, 2023\n          Rocky Mountain Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids_yoy, GEOID %in% c("42660")), #Seattle
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +150000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE,
    fill = "#3083DC", #MANUAL FILL BECAUSE ITS CLIPPED
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids_yoy, GEOID %in% c("38060")), #Phoenix
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids_yoy, GEOID %in% c("19740")), #Denver
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +400000, # adjust these values as needed
    nudge_x = +750000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids_yoy, GEOID %in% c("38900")), #Portland
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +75000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids_yoy, GEOID %in% c("29820")), #Las Vegas
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids_yoy, GEOID %in% c("41620")), #Salt Lake City
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 500000, # adjust these values as needed
    nudge_x = +950000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_RM_GRADIENT_YOY, "BEA GDP MSA RM GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Midwest


BEA_GDP_METRO_MW_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("OH", "MI", "IN", "IL","WI","MN","IA","MO"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_MW_yoy <- merge(MSA_map, BEA_GDP_METRO_MW_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_MW_centroids_yoy <- MSA_map_MW_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_MW_yoy, .) %>%
  st_centroid()

BEA_GDP_MSA_MW_GRADIENT_YOY <- MSA_map_MW_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("OH", "MI", "IN", "IL","WI","MN","IA","MO")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("                 Real GDP Growth, 2023\n                   Midwest Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("16980")), #Chicago
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +700000, # adjust these values as needed
    nudge_x = +900000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("33460")), #Minneapolis
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -80000, # adjust these values as needed
    nudge_x = -850000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("19820")), #Detroit
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +300000, # adjust these values as needed
    nudge_x = +550000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("41180")), #STL
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    nudge_x = -800000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("17140")), #Cincinnati
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -50000, # adjust these values as needed
    nudge_x = +600000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("26900")), #Indianapolis
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -350000, # adjust these values as needed
    nudge_x = +300000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("28140")), #Kansas City
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +100000, # adjust these values as needed
    nudge_x = -500000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("18140")), #Columbus
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    nudge_x = +750000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) + 
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("17460")), #Cleveland
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000, # adjust these values as needed
    nudge_x = +400000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids_yoy, GEOID %in% c("33340")), #Milwaukee
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = 600000, # adjust these values as needed
    nudge_x = +250000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_MW_GRADIENT_YOY, "BEA GDP MSA MW GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_GDP_METRO_SO_YOY <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/lag(DataValue,1)) - 1) %>%
  mutate(Increase = (DataValue-lag(DataValue,1))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("NC", "SC", "GA", "AL","MS","TN","KY","AR","LA"), collapse = "|"), GeoName)) %>%
  filter(GeoFips != "47260" & GeoFips != "17140") %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_SO_yoy <- merge(MSA_map, BEA_GDP_METRO_SO_YOY, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_SO_centroids_yoy <- MSA_map_SO_yoy %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_SO_yoy, .) %>%
  st_centroid()

BEA_GDP_MSA_SO_GRADIENT_YOY <- MSA_map_SO_yoy %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.06~ 0.06,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("NC", "SC", "GA", "AL","MS","TN","KY","AR","LA")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), labels = c("<0%","2%","4%","6%+"), breaks = c(0,0.02,0.04,0.06), expand = c(0,0), limits = c(0,0.06)) +
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-.12,-.10,-0.08,-0.06,-0.04,-0.02,.0,.02,.04,.06,0.08,0.1,0.12,0.14,0.16,0.18,0.20), expand = c(0,0), limits = c(0,0.07)) +
  ggtitle("      Real GDP Growth, 2023\n            South Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids_yoy, GEOID %in% c("12060")), #Atlanta
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +750000, # adjust these values as needed
    nudge_x = -100000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids_yoy, GEOID %in% c("16740")), #Charlotte
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = -700000, # adjust these values as needed
    nudge_x = +450000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids_yoy, GEOID %in% c("34980")), #Nashville
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +300000, # adjust these values as needed
    nudge_x = -450000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids_yoy, GEOID %in% c("39580")), #Raleigh
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +200000, # adjust these values as needed
    nudge_x = +00000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids_yoy, GEOID %in% c("32820")), #Memphis
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +250000, # adjust these values as needed
    nudge_x = -300000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BEA_GDP_MSA_SO_GRADIENT_YOY, "BEA GDP MSA SO GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

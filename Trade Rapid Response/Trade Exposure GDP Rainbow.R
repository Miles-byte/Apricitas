timeseries/intltrade/imports/statehs

sahie_variables <- listCensusMetadata(
  name = "timeseries/intltrade/exports/statehsexport",
  type = "variables")

US_COUNTRIES_IMPORTS_BULK_STATE <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/imports/statehs",
  vars = c("CON_VAL_YR","CTY_NAME","CTY_CODE","STATE"),
  time = "2024-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[1],
  CTY_NAME = Countries[2],
  CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_EXPORTS_BULK_STATE <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/statehs",
  vars = c("ALL_VAL_YR","CTY_NAME","CTY_CODE","STATE"),
  time = "2023-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[1],
  CTY_NAME = Countries[2],
  CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_EXPORTS_BULK_STATE <- US_COUNTRIES_EXPORTS_BULK_STATE %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR)) %>%
  group_by(STATE) %>%
  summarise(TOTAL_VAL_YR = sum(ALL_VAL_YR, na.rm = TRUE))

US_COUNTRIES_IMPORTS_BULK_STATE <- US_COUNTRIES_IMPORTS_BULK_STATE %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(STATE) %>%
  summarise(TOTAL_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE))

US_COUNTRIES_IMPORTS_EXPORTS_BULK_STATE <- merge(US_COUNTRIES_EXPORTS_BULK_STATE,US_COUNTRIES_IMPORTS_BULK_STATE, by = "STATE") %>%
  transmute(state = STATE, exports = TOTAL_VAL_YR.x, imports = TOTAL_VAL_YR.y, total = TOTAL_VAL_YR.x + TOTAL_VAL_YR.y)

BEA_GDP_STATE_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SAGDP1", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 3, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" = "2023") # Specify the year

BEA_GDP_STATE <- beaGet(BEA_GDP_STATE_SPECS, iTableStyle = TRUE) %>%
  mutate(state = case_when(
      GeoName == "District of Columbia" ~ "DC",
      TRUE ~ state.abb[match(GeoName, state.name)]
    )
  ) %>%
  drop_na() %>%
  transmute(state,GDP = DataValue_2023)

TRADE_EXPOSURE_GDP_STATE <- merge(US_COUNTRIES_IMPORTS_EXPORTS_BULK_STATE,BEA_GDP_STATE, by = "state") %>%
  mutate(trade_GDP_share = (total/1000000)/GDP, exports_GDP_share = (exports/1000000)/GDP, imports_GDP_share = (imports/1000000)/GDP)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, TRADE_EXPOSURE_GDP_STATE, by = c("state_abbv"="state"))

TRADE_EXPOSURE_GDP_STATE_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids <- states %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states, .) %>%
  st_centroid()


TRADE_EXPOSURE_GDP_RAINBOW_CHART <- states %>%
  ggplot(aes(fill = trade_GDP_share)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradientn(colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45), expand = c(0,0)) +
  ggtitle("        Trade With Mexico, Canada, & China\n                 As a Share of State GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA & Census data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0 & trade_GDP_share <= .1, "  ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    segment.color = NA,
    hjust = 0.5,
    direction = "y",
    nudge_y = 4000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    box.padding = 0.75,  # Increase box padding
    point.padding = 0.5,
    max.overlaps = 5,
    force = 4,
    force_pull = 1,
    max.iter = 2000000000,
    max.time = 30,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN","ME","SC","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("FL","TN","IN","ME","SC","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(trade_GDP_share >= 0, " ", ""), sprintf("%.1f", round(trade_GDP_share * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(trade_GDP_share*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.15, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = TRADE_EXPOSURE_GDP_STATE_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(trade_GDP_share,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(trade_GDP_share*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels



ggsave(dpi = "retina",plot = TRADE_EXPOSURE_GDP_RAINBOW_CHART, "TRADE EXPOSURE GDP STATE RAINBOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

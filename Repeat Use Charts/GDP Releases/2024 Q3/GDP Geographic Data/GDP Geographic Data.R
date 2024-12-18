pacman::p_load(prismatic,ggpubr,ggrepel,tigris,purrr,forecast,imputeTS,tsibble,sf,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

install_github("keberwein/blscrapeR")
library(blscrapeR)

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

##BEA STATE GRAPH
 
BEA_GDP_STATE_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP9", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE <- beaGet(BEA_GDP_STATE_SPECS, iTableStyle = FALSE) %>%
  rename_with(~ coalesce(stringr::str_extract(., "(?<=\\s)[A-Za-z ]+(?=\\sMillions)"), "Unknown"), .cols = everything()) %>%
  select(-`United States`,-`Rocky Mountain`,-`Unknown`,-`Far West`,-`Rocky Mountain`,-`Southwest`,-`Southeast`,-`Plains`,-`Great Lakes`,-`Mideast`,-`New England`) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  subset(date >= as.Date("2019-07-01")) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "GDP") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(CAGR = (GDP / first(GDP)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  mutate(GROWTH = (GDP / first(GDP)) - 1) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, BEA_GDP_STATE, by = "state_name")

BEA_GDP_STATE_GRADIENT <- ggplot() +
  geom_sf(data = states, aes(fill = CAGR)) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.05,0.05)) +
  ggtitle("       Annualized GDP Growth Since Q3 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

BEA_GDP_STATE_BINS <- states %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  ggplot(aes(fill = CAGR_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("     Annualized Real GDP Growth Since Q3 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank())

states <- states %>%
  mutate(GROWTH_bucket = cut(GROWTH, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+")))

BEA_GDP_STATE_BINS_LABELS <- get_urbn_labels(map = "states") %>%
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

BEA_GDP_STATE_BINS_RAW <- states %>%
  ggplot(aes(fill = GROWTH_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16%+"),
                    drop = FALSE) +
  ggtitle("             Real GDP Growth Since Q3 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0 & GROWTH <= .1, "  ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN","ME","SC")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("FL","TN","IN","ME","SC")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_BINS_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
  #geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_GDP_STATE_BINS_RAW, "BEA GDP STATE BINS RAW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_GDP_STATE_GRADIENT, "BEA GDP STATE GRADIENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_GDP_STATE_BINS, "BEA GDP STATE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_STATE_YOY <- beaGet(BEA_GDP_STATE_SPECS, iTableStyle = FALSE) %>%
  rename_with(~ coalesce(stringr::str_extract(., "(?<=\\s)[A-Za-z ]+(?=\\sMillions)"), "Unknown"), .cols = everything()) %>%
  select(-`United States`,-`Rocky Mountain`,-`Unknown`,-`Far West`,-`Rocky Mountain`,-`Southwest`,-`Southeast`,-`Plains`,-`Great Lakes`,-`Mideast`,-`New England`) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  subset(date >= as.Date("2019-07-01")) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "GDP") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(GROWTH_YOY = (GDP / lag(GDP,4)) - 1) %>%
  filter(date == max(date))

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, BEA_GDP_STATE_YOY, by = "state_name")

BEA_GDP_STATE_LABELS_YOY <- get_urbn_labels(map = "states") %>%
  left_join(states, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids_YOY <- states %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states, .) %>%
  st_centroid()

BEA_GDP_STATE_YOY_GRAPH <- states %>%
  ggplot(aes(fill = GROWTH_YOY)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-0.03,-0.02,-.01,.0,.01,.02,.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1), expand = c(0,0)) +
  ggtitle(paste0("     Real GDP Growth, Year-on-Year—", "Q", quarter(states$date[1]), " ", year(states$date[1]))) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3,
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
    data = filter(states_centroids_YOY, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25,check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_GDP_STATE_YOY_GRAPH, "BEA GDP STATE YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_STATE_YOY_RAINBOW_GRAPH <- states %>%
  ggplot(aes(fill = GROWTH_YOY)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-0.03,-0.02,-.01,.0,.01,.02,.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1), expand = c(0,0)) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1),breaks = c(-.1,-0.08,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.1,0.12), expand = c(0,0)) +
  ggtitle(paste0("     Real GDP Growth, Year-on-Year—", "Q", quarter(states$date[1]), " ", year(states$date[1]))) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3,
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
    data = filter(states_centroids_YOY, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids_YOY, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25,check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_STATE_LABELS_YOY, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_GDP_STATE_YOY_RAINBOW_GRAPH, "BEA GDP STATE YOY RAINBOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



test <- beaParams(beaKey = Sys.getenv("BEA_KEY"), "Regional")
test <- beaParamVals(beaKey = Sys.getenv("BEA_KEY"),"Regional","TableName")

BEA_PCE_PC_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SARPI", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 4, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_PCE_PC_STATE <- beaGet(BEA_PCE_PC_SPECS, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_trim(), starts_with("SARPI-4")) %>%
  rename_with(~str_replace(., " Constant", "")) %>%
  select(-`United States`, -TimePeriod) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "PCE_PER_CAPITA") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(GROWTH = (PCE_PER_CAPITA / first(PCE_PER_CAPITA)) - 1) %>%
  mutate(GROWTH_YOY = (PCE_PER_CAPITA / lag(PCE_PER_CAPITA,1)) - 1) %>%
  filter(date == max(date)) %>%
  ungroup()

states_PCE_PC <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states_PCE_PC <- left_join(states_PCE_PC, BEA_PCE_PC_STATE, by = "state_name")

states_PCE_PC <- states_PCE_PC %>%
  mutate(states = state_name) %>%
  mutate(GROWTH_bucket = cut(GROWTH, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+")))


BEA_GDP_PCE_PC_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states_PCE_PC, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids_PCE_PC <- states_PCE_PC %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_PCE_PC, .) %>%
  st_centroid()

BEA_PCE_PC_STATE_BINS_RAW <- states_PCE_PC %>%
  ggplot(aes(fill = GROWTH_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states_PCE_PC, color = "grey25", aes(fill = GROWTH_bucket), lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50",
                    breaks = c("<0","0-0.04","0.04-0.08","0.08-0.12","0.12-0.16","0.16+"),
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D","#3083DC"))),
                    drop = FALSE) +
  ggtitle("         Real PCE Per Capita Growth 2019-2023\n                    At State Price Parities") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_PCE_PC_STATE_BINS_RAW, "BEA PCE PER CAPITA STATE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_PCE_PC_STATE_BINS_GRADIENT_RAINBOW <- states_PCE_PC %>%
  mutate(GROWTH = case_when(
    GROWTH < 0 ~ 0,
    GROWTH > 0.16 ~ 0.16,
    TRUE ~ GROWTH
  )) %>%
  ggplot(aes(fill = GROWTH)) +
  geom_sf(color = NA) +
  geom_sf(color = "grey25", aes(fill = GROWTH), lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),limits = c(0,.16), label = c("<0%","4%","8%","12%","16%+"),breaks = c(0,0.04,0.08,0.12,0.16), expand = c(0,0)) +
  ggtitle("         Real PCE Per Capita Growth 2019-2023\n                    At State Price Parities") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, "  ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, "  ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, "  ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, "  ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("FL","TN","IN","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_PCE_PC_STATE_BINS_GRADIENT_RAINBOW, "BEA PCE PER CAPITA STATE GRADIENT RAINBOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_PCE_PC_STATE_GRADIENT_YOY <- states_PCE_PC %>%
  ggplot(aes(fill = GROWTH_YOY)) +
  geom_sf(color = NA) +
  geom_sf(data = states_PCE_PC, color = "grey25", aes(fill = GROWTH_YOY), lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1),breaks = c(-.1,-0.08,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.1,0.12), expand = c(0,0)) +
  ggtitle("             Real PCE Per Capita Growth 2023\n                    At State Price Parities") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
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
    data = filter(states_centroids_PCE_PC, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_GDP_PCE_PC_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH_YOY >= 0, " ", ""), sprintf("%.1f", round(GROWTH_YOY * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_PCE_PC_STATE_GRADIENT_YOY, "BEA PCE PER CAPITA STATE GRADIENT YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_RPP_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SARPP", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_RPP_STATE <- beaGet(BEA_RPP_SPECS, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_trim(), starts_with("SARPP-1")) %>%
  rename_with(~str_replace(., " Index", "")) %>%
  select(-`United States`, -TimePeriod) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "RPP") %>%
  arrange(state_name, date) %>%
  group_by(state_name) %>%
  mutate(GROWTH = (RPP - first(RPP))/100) %>%
  filter(date == max(date)) %>%
  ungroup()


states_RPP <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states_RPP <- left_join(states_RPP, BEA_RPP_STATE, by = "state_name")

states_RPP <- states_RPP %>%
  mutate(states = state_name) %>%
  mutate(GROWTH_bucket = cut(GROWTH, breaks = c(-Inf, -0.02, 0,.02, Inf), labels = c("<-2%", "-2%-0%", "0%-2%", "2%+")))


BEA_RPP_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states_RPP, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids_RPP <- states_RPP %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_RPP, .) %>%
  st_centroid()



BEA_RPP_STATE_BINS_RAW <- states_RPP %>%
  ggplot(aes(fill = GROWTH_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_manual(
    values = c("<-2%" = "#FFE98F", "-2%-0%" = "#F5B041", "0%-2%" = "#FF8E72", "2%+" = "#EE6055"),
    na.value = "grey50", 
    breaks = rev(c("<-2%", "-2%-0%", "0%-2%", "2%+")),
    guide = guide_legend(override.aes = list(color = c("#EE6055", "#FF8E72", "#F5B041", "#FFE98F")))
  ) +
  ggtitle("       Change in State Price Parities 2019-2023\n                  Relative to National Average") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_RPP_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_RPP_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_RPP_STATE_BINS_RAW, "BEA RPP STATE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_RPP_MSA_GRADIENT <- RPP_MSA_map_US %>%
  mutate(Growth = case_when(
    Growth < -0.04 ~ -0.04,
    Growth > 0.04 ~ 0.04,
    TRUE ~ Growth
  )) %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth), color = "black", lwd = 0.5) +
  scale_fill_gradientn(colors = rev(c("#EE6055","#FF8E72","#F5B041","#FFE98F")),limits = c(-0.04,0.04), breaks = c(-.04,-.02,0,0.02,0.04), labels = c("-4%","-2%","0%","2%","4%"), expand = c(0,0)) +
  ggtitle("Change in Metro Area Price Parities, 2019-2023\n               Relative to National Average\n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24))


BEA_RPP_STATE_GRADIENT <- states_RPP %>%
  mutate(GROWTH = case_when(
    GROWTH < -0.04 ~ -0.04,
    GROWTH > 0.04 ~ 0.04,
    TRUE ~ GROWTH
  )) %>%
  ggplot(aes(fill = GROWTH)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65, alpha = 0) + # Black borders for states
  scale_fill_gradientn(colors = rev(c("#EE6055","#FF8E72","#F5B041","#FFE98F")),limits = c(-0.04,0.04), breaks = c(-.04,-.02,0,0.02,0.04), labels = c("-4%","-2%","0%","2%","4%"), expand = c(0,0)) +
  ggtitle("       Change in State Price Parities 2019-2023\n                  Relative to National Average") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(BEA_RPP_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
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
    data = filter(states_centroids_RPP, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(BEA_RPP_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("FL","TN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(BEA_RPP_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(GROWTH >= 0, " ", ""), sprintf("%.1f", round(GROWTH * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = BEA_GDP_STATE_BINS_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(GROWTH,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(GROWTH*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = BEA_RPP_STATE_GRADIENT, "BEA RPP STATE GRADIENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_STATE_INDUSTRIES_SPECS_TOTAL <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2005, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_TOTAL <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_TOTAL, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2005-01-01"), length.out = nrow(.), by = "3 months")))

BEA_GDP_STATE_INDUSTRIES_SPECS_INFO <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 45, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2005, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_INFO <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_INFO, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2005-01-01"), length.out = nrow(.), by = "3 months")))

BEA_GDP_STATE_INDUSTRIES_SPECS_PROF <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 60, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2005, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_PROF <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_PROF, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2005-01-01"), length.out = nrow(.), by = "3 months")))

BEA_GDP_STATE_INDUSTRIES_SPECS_CONS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SQGDP2", # Specify table within the dataset
  "Frequency" = "Q", # Specify the line code
  "LineCode" = 11, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"UnitofMeasure" = "Percent of US",
  "Year" =  paste(seq(from = 2005, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_INDUSTRIES_CONS <- beaGet(BEA_GDP_STATE_INDUSTRIES_SPECS_CONS, iTableStyle = FALSE, asWide = TRUE) %>%
  rename_with(~str_extract(., "(?<=\\d{5} )[A-Za-z ]+") %>% str_replace("Millions of current dollars", "") %>% str_trim(), starts_with("SQGDP2")) %>%
  select(`United States`,`California`,`Texas`,`New York`,`Florida`) %>%
  mutate(across(-`United States`, ~./`United States`)) %>%
  select(-`United States`) %>%
  mutate(date = (seq(as.Date("2005-01-01"), length.out = nrow(.), by = "3 months")))


BEA_GDP_STATE_INDUSTRIES_TOTAL_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_TOTAL, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.15), breaks = c(0,0.05,0.10,0.15), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Total") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_INFO_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_INFO, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.30), breaks = c(0,0.05,0.10,0.15,0.20,0.25,0.30), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Information") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_PROF_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_PROF, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.20), breaks = c(0,0.05,0.10,0.15,0.20,0.25,0.30), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Professional/Scientific/Technical") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_CONS_GRAPH <- ggplot() +
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `California`, color = "California"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `New York`, color = "New York"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `Texas`, color = "Texas"), size = 1.25) + 
  geom_line(data = BEA_GDP_STATE_INDUSTRIES_CONS, aes(x=date, y = `Florida`, color = "Florida"), size = 1.25) + 
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.15), breaks = c(0,0.05,0.10,0.15), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Construction") +
  #labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = "top",plot.title = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("California","New York","Texas","Florida")) +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm"))

BEA_GDP_STATE_INDUSTRIES_GRAPH <- ggarrange(BEA_GDP_STATE_INDUSTRIES_TOTAL_GRAPH, BEA_GDP_STATE_INDUSTRIES_PROF_GRAPH, BEA_GDP_STATE_INDUSTRIES_CONS_GRAPH, BEA_GDP_STATE_INDUSTRIES_INFO_GRAPH,  ncol = 2, nrow = 2, heights = 20, widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

text <- c("State GDP, Percent of US GDP",fontface = "bold")

# Create a text grob
tgrob <- text_grob(expression(bold("                    State GDP, Percent of US GDP")),size = 25, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "top", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-0.15,-0.15,-0.15),"cm"))  
blank <- ""
blankgrob <- text_grob(blank,size = 20)
plot_blank <- as_ggplot(blankgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))
BEA_GDP_STATE_INDUSTRIES_GRAPH <- ggarrange(plot_0,plot_blank,BEA_GDP_STATE_INDUSTRIES_TOTAL_GRAPH, BEA_GDP_STATE_INDUSTRIES_PROF_GRAPH, BEA_GDP_STATE_INDUSTRIES_CONS_GRAPH, BEA_GDP_STATE_INDUSTRIES_INFO_GRAPH,  ncol = 2, nrow = 3, heights = c(5,20,20), widths = 10, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = BEA_GDP_STATE_INDUSTRIES_GRAPH, "BEA GDP STATE INDUSTRIES.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_STATE_ANNUAL_TOTAL_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SAGDP2N", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_STATE_ANNUAL_TOTAL <- beaGet(BEA_GDP_STATE_ANNUAL_TOTAL_SPECS, iTableStyle = FALSE) %>%
  select(-TimePeriod) %>%
  rename_with(~ coalesce(stringr::str_extract(., "(?<=\\s)[A-Za-z ]+(?=\\sMillions)"), "Unknown"), .cols = everything()) %>%
  select(-`Rocky Mountain`,-`Unknown`,-`Far West`,-`Rocky Mountain`,-`Southwest`,-`Southeast`,-`Plains`,-`Great Lakes`,-`Mideast`,-`New England`) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  filter(date == max(date)) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "GDP")
  
BEA_MANU_STATE_ANNUAL_TOTAL_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SAGDP2N", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 12, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  #"Year" =  2023 # Specify the year
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_MANU_STATE_ANNUAL_TOTAL <- beaGet(BEA_MANU_STATE_ANNUAL_TOTAL_SPECS, iTableStyle = FALSE) %>%
  select(-TimePeriod) %>%
  rename_with(~ coalesce(stringr::str_extract(., "(?<=\\s)[A-Za-z ]+(?=\\sMillions)"), "Unknown"), .cols = everything()) %>%
  select(-`Rocky Mountain`,-`Unknown`,-`Far West`,-`Rocky Mountain`,-`Southwest`,-`Southeast`,-`Plains`,-`Great Lakes`,-`Mideast`,-`New England`) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  filter(date == max(date)) %>%
  pivot_longer(-date, names_to = "state_name", values_to = "MANU")

BEA_MANU_STATE_SHARE <- merge(BEA_GDP_STATE_ANNUAL_TOTAL,BEA_MANU_STATE_ANNUAL_TOTAL, by = "state_name") %>%
  transmute(state_name, share = MANU/GDP)

states <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

STATES_MANU_SHARE_MAP <- left_join(states, BEA_MANU_STATE_SHARE, by = "state_name") %>%
  drop_na() %>%
  mutate(across(is.numeric, ~na_if(., 0)))

STATES_MANU_SHARE_MAP_centroids <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66 & state_fips != 72 & state_fips != 78) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  cross_join(STATES_MANU_SHARE_MAP, .) %>%
  st_centroid()

STATES_MANU_SHARE_MAP_LABELS <- get_urbn_labels(map = "territories") %>%
  left_join(BEA_MANU_STATE_SHARE, by = "state_name") %>%
  mutate(across(is.numeric, ~na_if(., 0))) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

MANU_SHARE_GDP_MAP_GRAPH <- STATES_MANU_SHARE_MAP  %>%
  ggplot(aes(fill = share)) +
  geom_sf(color = NA) +
  geom_sf(data = STATES_MANU_SHARE_MAP, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA  "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(STATES_MANU_SHARE_MAP_centroids, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\n   NA   "), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(STATES_MANU_SHARE_MAP_LABELS, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MN","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\nNA"), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(STATES_MANU_SHARE_MAP_LABELS, state_abbv %in% c("FL","IN","TN","MS","MN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\nNA"), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(STATES_MANU_SHARE_MAP_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\nNA"), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(STATES_MANU_SHARE_MAP_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = ifelse(is.na(share), paste0(state_abbv, "\nNA"), paste0(state_abbv, "\n", ifelse(share >= 0, " ", ""), sprintf("%.1f", round(share * 100, 1)), "%")),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(paste("        Manufacturing % of State GDP, 2023")) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 26))

ggsave(dpi = "retina",plot = MANU_SHARE_GDP_MAP_GRAPH, "Manufacturing Share of State GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, BEA_GDP_STATE, by = "state_name")

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

counties_map <- get_urbn_map(map = "counties", sf = TRUE)

#NOTE: BEA Data Combines Several Small Counties, Particularly in VA, into Neighboring Counties. This code Manually copies over the CAGR values for those large counties into the Geometry for the smaller, constituent counties 

counties_map <- full_join(counties_map, BEA_GDP_COUNTIES, by = "county_fips") %>%
  mutate(CAGR = if_else(county_fips == 51003, .$CAGR[which(.$county_fips == 51901)], CAGR)) %>% #Albemarle
  mutate(CAGR = if_else(county_fips == 51540, .$CAGR[which(.$county_fips == 51901)], CAGR)) %>% #Charlottesville
  mutate(CAGR = if_else(county_fips == 51005, .$CAGR[which(.$county_fips == 51903)], CAGR)) %>% #Alleghany
  mutate(CAGR = if_else(county_fips == 51580, .$CAGR[which(.$county_fips == 51903)], CAGR)) %>% #Covington
  mutate(CAGR = if_else(county_fips == 51015, .$CAGR[which(.$county_fips == 51907)], CAGR)) %>% #Augusta
  mutate(CAGR = if_else(county_fips == 51790, .$CAGR[which(.$county_fips == 51907)], CAGR)) %>% #Staunton
  mutate(CAGR = if_else(county_fips == 51820, .$CAGR[which(.$county_fips == 51907)], CAGR)) %>% #Waynesboro
  mutate(CAGR = if_else(county_fips == 51031, .$CAGR[which(.$county_fips == 51911)], CAGR)) %>% #Campbell
  mutate(CAGR = if_else(county_fips == 51680, .$CAGR[which(.$county_fips == 51911)], CAGR)) %>% #Lynchburg
  mutate(CAGR = if_else(county_fips == 51035, .$CAGR[which(.$county_fips == 51913)], CAGR)) %>% #Carroll
  mutate(CAGR = if_else(county_fips == 51640, .$CAGR[which(.$county_fips == 51913)], CAGR)) %>% #Galax
  mutate(CAGR = if_else(county_fips == 51053, .$CAGR[which(.$county_fips == 51918)], CAGR)) %>% #Dinwiddie
  mutate(CAGR = if_else(county_fips == 51570, .$CAGR[which(.$county_fips == 51918)], CAGR)) %>% #Colonial Heights
  mutate(CAGR = if_else(county_fips == 51730, .$CAGR[which(.$county_fips == 51918)], CAGR)) %>% #Petersburg
  mutate(CAGR = if_else(county_fips == 51059, .$CAGR[which(.$county_fips == 51919)], CAGR)) %>% #Fairfax
  mutate(CAGR = if_else(county_fips == 51600, .$CAGR[which(.$county_fips == 51919)], CAGR)) %>% #Fairfax city
  mutate(CAGR = if_else(county_fips == 51610, .$CAGR[which(.$county_fips == 51919)], CAGR)) %>% #Falls Church
  mutate(CAGR = if_else(county_fips == 51069, .$CAGR[which(.$county_fips == 51921)], CAGR)) %>% #Frederick
  mutate(CAGR = if_else(county_fips == 51840, .$CAGR[which(.$county_fips == 51921)], CAGR)) %>% #Winchester
  mutate(CAGR = if_else(county_fips == 51081, .$CAGR[which(.$county_fips == 51923)], CAGR)) %>% #Greensville
  mutate(CAGR = if_else(county_fips == 51595, .$CAGR[which(.$county_fips == 51923)], CAGR)) %>% #Emporia
  mutate(CAGR = if_else(county_fips == 51089, .$CAGR[which(.$county_fips == 51929)], CAGR)) %>% #Henry
  mutate(CAGR = if_else(county_fips == 51690, .$CAGR[which(.$county_fips == 51929)], CAGR)) %>% #Martinsville
  mutate(CAGR = if_else(county_fips == 51095, .$CAGR[which(.$county_fips == 51931)], CAGR)) %>% #James City
  mutate(CAGR = if_else(county_fips == 51830, .$CAGR[which(.$county_fips == 51931)], CAGR)) %>% #Williamsburg
  mutate(CAGR = if_else(county_fips == 51121, .$CAGR[which(.$county_fips == 51933)], CAGR)) %>% #Montgomery
  mutate(CAGR = if_else(county_fips == 51750, .$CAGR[which(.$county_fips == 51933)], CAGR)) %>% #Radford
  mutate(CAGR = if_else(county_fips == 51143, .$CAGR[which(.$county_fips == 51939)], CAGR)) %>% #Pittsylvania
  mutate(CAGR = if_else(county_fips == 51590, .$CAGR[which(.$county_fips == 51939)], CAGR)) %>% #Danville
  mutate(CAGR = if_else(county_fips == 51149, .$CAGR[which(.$county_fips == 51941)], CAGR)) %>% #Prince George
  mutate(CAGR = if_else(county_fips == 51670, .$CAGR[which(.$county_fips == 51941)], CAGR)) %>% #Hopewell
  mutate(CAGR = if_else(county_fips == 51683, .$CAGR[which(.$county_fips == 51942)], CAGR)) %>% #Manassas City
  mutate(CAGR = if_else(county_fips == 51685, .$CAGR[which(.$county_fips == 51942)], CAGR)) %>% #Manassas Park City
  mutate(CAGR = if_else(county_fips == 51153, .$CAGR[which(.$county_fips == 51942)], CAGR)) %>% #Prince Williams
  mutate(CAGR = if_else(county_fips == 51161, .$CAGR[which(.$county_fips == 51944)], CAGR)) %>% #Roanoke
  mutate(CAGR = if_else(county_fips == 51775, .$CAGR[which(.$county_fips == 51944)], CAGR)) %>% #Salem
  mutate(CAGR = if_else(county_fips == 51163, .$CAGR[which(.$county_fips == 51945)], CAGR)) %>% #Rockbridge
  mutate(CAGR = if_else(county_fips == 51530, .$CAGR[which(.$county_fips == 51945)], CAGR)) %>% #Buena Vista
  mutate(CAGR = if_else(county_fips == 51678, .$CAGR[which(.$county_fips == 51945)], CAGR)) %>% #Lexington
  mutate(CAGR = if_else(county_fips == 51165, .$CAGR[which(.$county_fips == 51947)], CAGR)) %>% #Rockingham
  mutate(CAGR = if_else(county_fips == 51660, .$CAGR[which(.$county_fips == 51947)], CAGR)) %>% #Harrisonburg
  mutate(CAGR = if_else(county_fips == 51175, .$CAGR[which(.$county_fips == 51949)], CAGR)) %>% #Southampton
  mutate(CAGR = if_else(county_fips == 51620, .$CAGR[which(.$county_fips == 51949)], CAGR)) %>% #Franklin
  mutate(CAGR = if_else(county_fips == 51177, .$CAGR[which(.$county_fips == 51951)], CAGR)) %>% #Spotsylvania
  mutate(CAGR = if_else(county_fips == 51630, .$CAGR[which(.$county_fips == 51951)], CAGR)) %>% #Fredericksburg
  mutate(CAGR = if_else(county_fips == 51191, .$CAGR[which(.$county_fips == 51953)], CAGR)) %>% #Washington
  mutate(CAGR = if_else(county_fips == 51520, .$CAGR[which(.$county_fips == 51953)], CAGR)) %>% #Bristol
  mutate(CAGR = if_else(county_fips == 51195, .$CAGR[which(.$county_fips == 51955)], CAGR)) %>% #Wise
  mutate(CAGR = if_else(county_fips == 51720, .$CAGR[which(.$county_fips == 51955)], CAGR)) %>% #Norton
  mutate(CAGR = if_else(county_fips == 51199, .$CAGR[which(.$county_fips == 51958)], CAGR)) %>% #York
  mutate(CAGR = if_else(county_fips == 51735, .$CAGR[which(.$county_fips == 51958)], CAGR)) %>% #Poquoson
  mutate(CAGR = if_else(county_fips == 15009, .$CAGR[which(.$county_fips == 15901)], CAGR)) %>% #Kalawao
  mutate(CAGR = if_else(county_fips == 15005, .$CAGR[which(.$county_fips == 15901)], CAGR)) %>% #Maui
  drop_na()
  
BEA_GDP_COUNTY_BINS <- counties_map %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  ggplot(aes(fill = CAGR_bucket, color = CAGR_bucket), lwd = 0) +
  geom_sf() +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    #guide = FALSE, 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = FALSE, 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Annualized Real GDP Growth by County, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26))
  
ggsave(dpi = "retina",plot = BEA_GDP_COUNTY_BINS, "BEA GDP COUNTY BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_COUNTIES_Growth <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, Growth)

counties_map_Growth <- get_urbn_map(map = "counties", sf = TRUE)

counties_map_Growth <- full_join(counties_map_Growth, BEA_GDP_COUNTIES_Growth, by = "county_fips") %>%
  mutate(Growth = if_else(county_fips == 51003, .$Growth[which(.$county_fips == 51901)], Growth)) %>% #Albemarle
  mutate(Growth = if_else(county_fips == 51540, .$Growth[which(.$county_fips == 51901)], Growth)) %>% #Charlottesville
  mutate(Growth = if_else(county_fips == 51005, .$Growth[which(.$county_fips == 51903)], Growth)) %>% #Alleghany
  mutate(Growth = if_else(county_fips == 51580, .$Growth[which(.$county_fips == 51903)], Growth)) %>% #Covington
  mutate(Growth = if_else(county_fips == 51015, .$Growth[which(.$county_fips == 51907)], Growth)) %>% #Augusta
  mutate(Growth = if_else(county_fips == 51790, .$Growth[which(.$county_fips == 51907)], Growth)) %>% #Staunton
  mutate(Growth = if_else(county_fips == 51820, .$Growth[which(.$county_fips == 51907)], Growth)) %>% #Waynesboro
  mutate(Growth = if_else(county_fips == 51031, .$Growth[which(.$county_fips == 51911)], Growth)) %>% #Campbell
  mutate(Growth = if_else(county_fips == 51680, .$Growth[which(.$county_fips == 51911)], Growth)) %>% #Lynchburg
  mutate(Growth = if_else(county_fips == 51035, .$Growth[which(.$county_fips == 51913)], Growth)) %>% #Carroll
  mutate(Growth = if_else(county_fips == 51640, .$Growth[which(.$county_fips == 51913)], Growth)) %>% #Galax
  mutate(Growth = if_else(county_fips == 51053, .$Growth[which(.$county_fips == 51918)], Growth)) %>% #Dinwiddie
  mutate(Growth = if_else(county_fips == 51570, .$Growth[which(.$county_fips == 51918)], Growth)) %>% #Colonial Heights
  mutate(Growth = if_else(county_fips == 51730, .$Growth[which(.$county_fips == 51918)], Growth)) %>% #Petersburg
  mutate(Growth = if_else(county_fips == 51059, .$Growth[which(.$county_fips == 51919)], Growth)) %>% #Fairfax
  mutate(Growth = if_else(county_fips == 51600, .$Growth[which(.$county_fips == 51919)], Growth)) %>% #Fairfax city
  mutate(Growth = if_else(county_fips == 51610, .$Growth[which(.$county_fips == 51919)], Growth)) %>% #Falls Church
  mutate(Growth = if_else(county_fips == 51069, .$Growth[which(.$county_fips == 51921)], Growth)) %>% #Frederick
  mutate(Growth = if_else(county_fips == 51840, .$Growth[which(.$county_fips == 51921)], Growth)) %>% #Winchester
  mutate(Growth = if_else(county_fips == 51081, .$Growth[which(.$county_fips == 51923)], Growth)) %>% #Greensville
  mutate(Growth = if_else(county_fips == 51595, .$Growth[which(.$county_fips == 51923)], Growth)) %>% #Emporia
  mutate(Growth = if_else(county_fips == 51089, .$Growth[which(.$county_fips == 51929)], Growth)) %>% #Henry
  mutate(Growth = if_else(county_fips == 51690, .$Growth[which(.$county_fips == 51929)], Growth)) %>% #Martinsville
  mutate(Growth = if_else(county_fips == 51095, .$Growth[which(.$county_fips == 51931)], Growth)) %>% #James City
  mutate(Growth = if_else(county_fips == 51830, .$Growth[which(.$county_fips == 51931)], Growth)) %>% #Williamsburg
  mutate(Growth = if_else(county_fips == 51121, .$Growth[which(.$county_fips == 51933)], Growth)) %>% #Montgomery
  mutate(Growth = if_else(county_fips == 51750, .$Growth[which(.$county_fips == 51933)], Growth)) %>% #Radford
  mutate(Growth = if_else(county_fips == 51143, .$Growth[which(.$county_fips == 51939)], Growth)) %>% #Pittsylvania
  mutate(Growth = if_else(county_fips == 51590, .$Growth[which(.$county_fips == 51939)], Growth)) %>% #Danville
  mutate(Growth = if_else(county_fips == 51149, .$Growth[which(.$county_fips == 51941)], Growth)) %>% #Prince George
  mutate(Growth = if_else(county_fips == 51670, .$Growth[which(.$county_fips == 51941)], Growth)) %>% #Hopewell
  mutate(Growth = if_else(county_fips == 51683, .$Growth[which(.$county_fips == 51942)], Growth)) %>% #Manassas City
  mutate(Growth = if_else(county_fips == 51685, .$Growth[which(.$county_fips == 51942)], Growth)) %>% #Manassas Park City
  mutate(Growth = if_else(county_fips == 51153, .$Growth[which(.$county_fips == 51942)], Growth)) %>% #Prince Williams
  mutate(Growth = if_else(county_fips == 51161, .$Growth[which(.$county_fips == 51944)], Growth)) %>% #Roanoke
  mutate(Growth = if_else(county_fips == 51775, .$Growth[which(.$county_fips == 51944)], Growth)) %>% #Salem
  mutate(Growth = if_else(county_fips == 51163, .$Growth[which(.$county_fips == 51945)], Growth)) %>% #Rockbridge
  mutate(Growth = if_else(county_fips == 51530, .$Growth[which(.$county_fips == 51945)], Growth)) %>% #Buena Vista
  mutate(Growth = if_else(county_fips == 51678, .$Growth[which(.$county_fips == 51945)], Growth)) %>% #Lexington
  mutate(Growth = if_else(county_fips == 51165, .$Growth[which(.$county_fips == 51947)], Growth)) %>% #Rockingham
  mutate(Growth = if_else(county_fips == 51660, .$Growth[which(.$county_fips == 51947)], Growth)) %>% #Harrisonburg
  mutate(Growth = if_else(county_fips == 51175, .$Growth[which(.$county_fips == 51949)], Growth)) %>% #Southampton
  mutate(Growth = if_else(county_fips == 51620, .$Growth[which(.$county_fips == 51949)], Growth)) %>% #Franklin
  mutate(Growth = if_else(county_fips == 51177, .$Growth[which(.$county_fips == 51951)], Growth)) %>% #Spotsylvania
  mutate(Growth = if_else(county_fips == 51630, .$Growth[which(.$county_fips == 51951)], Growth)) %>% #Fredericksburg
  mutate(Growth = if_else(county_fips == 51191, .$Growth[which(.$county_fips == 51953)], Growth)) %>% #Washington
  mutate(Growth = if_else(county_fips == 51520, .$Growth[which(.$county_fips == 51953)], Growth)) %>% #Bristol
  mutate(Growth = if_else(county_fips == 51195, .$Growth[which(.$county_fips == 51955)], Growth)) %>% #Wise
  mutate(Growth = if_else(county_fips == 51720, .$Growth[which(.$county_fips == 51955)], Growth)) %>% #Norton
  mutate(Growth = if_else(county_fips == 51199, .$Growth[which(.$county_fips == 51958)], Growth)) %>% #York
  mutate(Growth = if_else(county_fips == 51735, .$Growth[which(.$county_fips == 51958)], Growth)) %>% #Poquoson
  mutate(Growth = if_else(county_fips == 15009, .$Growth[which(.$county_fips == 15901)], Growth)) %>% #Kalawao
  mutate(Growth = if_else(county_fips == 15005, .$Growth[which(.$county_fips == 15901)], Growth)) %>% #Maui
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+")))


BEA_GDP_COUNTY_BINS_GROWTH <- counties_map_Growth %>%
  ggplot(aes(fill = Growth_bucket, color = Growth_bucket), lwd = 0) +
  geom_sf() +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = FALSE, 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16%+"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                     na.value = "grey50", 
                     guide = FALSE, 
                     labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("         Real GDP Growth by County, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26))

ggsave(dpi = "retina",plot = BEA_GDP_COUNTY_BINS_GROWTH, "BEA GDP COUNTY BINS GROWTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_COUNTIES_INCREASE <- beaGet(BEA_GDP_COUNTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Increase = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, Increase)

counties_map_increase <- get_urbn_map(map = "counties", sf = TRUE)

#NOTE: BEA Data Combines Several Small Counties, Particularly in VA, into Neighboring Counties. This code Manually copies over the CAGR values for those large counties into the Geometry for the smaller, constituent counties 

counties_map_increase <- full_join(counties_map_increase, BEA_GDP_COUNTIES_INCREASE, by = "county_fips") %>%
  mutate(Increase = if_else(county_fips == 51003, .$Increase[which(.$county_fips == 51901)], Increase)) %>% #Albemarle
  mutate(Increase = if_else(county_fips == 51005, .$Increase[which(.$county_fips == 51903)], Increase)) %>% #Alleghany
  mutate(Increase = if_else(county_fips == 51820, .$Increase[which(.$county_fips == 51907)], Increase)) %>% #Waynesboro
  mutate(Increase = if_else(county_fips == 51680, .$Increase[which(.$county_fips == 51911)], Increase)) %>% #Lynchburg
  mutate(Increase = if_else(county_fips == 51640, .$Increase[which(.$county_fips == 51913)], Increase)) %>% #Galax
  mutate(Increase = if_else(county_fips == 51730, .$Increase[which(.$county_fips == 51918)], Increase)) %>% #Petersburg
  mutate(Increase = if_else(county_fips == 51610, .$Increase[which(.$county_fips == 51919)], Increase)) %>% #Falls Church
  mutate(Increase = if_else(county_fips == 51840, .$Increase[which(.$county_fips == 51921)], Increase)) %>% #Winchester
  mutate(Increase = if_else(county_fips == 51595, .$Increase[which(.$county_fips == 51923)], Increase)) %>% #Emporia
  mutate(Increase = if_else(county_fips == 51690, .$Increase[which(.$county_fips == 51929)], Increase)) %>% #Martinsville
  mutate(Increase = if_else(county_fips == 51830, .$Increase[which(.$county_fips == 51931)], Increase)) %>% #Williamsburg
  mutate(Increase = if_else(county_fips == 51750, .$Increase[which(.$county_fips == 51933)], Increase)) %>% #Radford
  mutate(Increase = if_else(county_fips == 51590, .$Increase[which(.$county_fips == 51939)], Increase)) %>% #Danville
  mutate(Increase = if_else(county_fips == 51670, .$Increase[which(.$county_fips == 51941)], Increase)) %>% #Hopewell
  mutate(Increase = if_else(county_fips == 51153, .$Increase[which(.$county_fips == 51942)], Increase)) %>% #Prince Williams
  mutate(Increase = if_else(county_fips == 51775, .$Increase[which(.$county_fips == 51944)], Increase)) %>% #Salem
  mutate(Increase = if_else(county_fips == 51678, .$Increase[which(.$county_fips == 51945)], Increase)) %>% #Lexington
  mutate(Increase = if_else(county_fips == 51165, .$Increase[which(.$county_fips == 51947)], Increase)) %>% #Rockingham
  mutate(Increase = if_else(county_fips == 51620, .$Increase[which(.$county_fips == 51949)], Increase)) %>% #Franklin
  mutate(Increase = if_else(county_fips == 51630, .$Increase[which(.$county_fips == 51951)], Increase)) %>% #Fredericksburg
  mutate(Increase = if_else(county_fips == 51520, .$Increase[which(.$county_fips == 51953)], Increase)) %>% #Bristol
  mutate(Increase = if_else(county_fips == 51720, .$Increase[which(.$county_fips == 51955)], Increase)) %>% #Norton
  mutate(Increase = if_else(county_fips == 51199, .$Increase[which(.$county_fips == 51958)], Increase)) %>% #York
  mutate(Increase = if_else(county_fips == 15005, .$Increase[which(.$county_fips == 15901)], Increase)) %>% #Maui
  drop_na()

counties_map_increase_centroids <- counties_map_increase %>%
  st_centroid()

BEA_GDP_COUNTY_INCREASES_GRAPH <- counties_map %>%
  ggplot() +
  geom_sf(fill = "grey75") +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = Increase > 0, size = Increase/1000000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = Increase/1000000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(name = NULL,
                     values = c("#3083DC","#EE6055"),
                     breaks = c(TRUE, FALSE), 
                     labels = c("Increase", "Decrease"),
                     guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of Change\n2017 Dollars",
                  max_size = 15,
                  breaks = c(0,20,40,60),
                  labels = c("$0","$20B","$40B","$60B"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("         Real GDP Growth by County, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = BEA_GDP_COUNTY_INCREASES_GRAPH, "BEA GDP COUNTY Increases.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_COUNTIES_NOMINAL_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP2", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GDP_COUNTIES_INCREASE <- beaGet(BEA_GDP_COUNTIES_NOMINAL_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(TimePeriod) %>% mutate(DataValue = if_else(GeoFips == "02261" & DataValue == 0, DataValue[GeoFips == "02063"] + DataValue[GeoFips == "02066"], DataValue)) %>% ungroup %>% #fixing the Valdez-Cordova Census Area, Which Got Split Up in 2019
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Increase = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  transmute(county_fips = GeoFips, Increase)

counties_map_increase <- get_urbn_map(map = "counties", sf = TRUE)

#NOTE: BEA Data Combines Several Small Counties, Particularly in VA, into Neighboring Counties. This code Manually copies over the CAGR values for those large counties into the Geometry for the smaller, constituent counties 

counties_map_increase <- full_join(counties_map_increase, BEA_GDP_COUNTIES_INCREASE, by = "county_fips") %>%
  mutate(Increase = if_else(county_fips == 51003, .$Increase[which(.$county_fips == 51901)], Increase)) %>% #Albemarle
  mutate(Increase = if_else(county_fips == 51005, .$Increase[which(.$county_fips == 51903)], Increase)) %>% #Alleghany
  mutate(Increase = if_else(county_fips == 51820, .$Increase[which(.$county_fips == 51907)], Increase)) %>% #Waynesboro
  mutate(Increase = if_else(county_fips == 51680, .$Increase[which(.$county_fips == 51911)], Increase)) %>% #Lynchburg
  mutate(Increase = if_else(county_fips == 51640, .$Increase[which(.$county_fips == 51913)], Increase)) %>% #Galax
  mutate(Increase = if_else(county_fips == 51730, .$Increase[which(.$county_fips == 51918)], Increase)) %>% #Petersburg
  mutate(Increase = if_else(county_fips == 51610, .$Increase[which(.$county_fips == 51919)], Increase)) %>% #Falls Church
  mutate(Increase = if_else(county_fips == 51840, .$Increase[which(.$county_fips == 51921)], Increase)) %>% #Winchester
  mutate(Increase = if_else(county_fips == 51595, .$Increase[which(.$county_fips == 51923)], Increase)) %>% #Emporia
  mutate(Increase = if_else(county_fips == 51690, .$Increase[which(.$county_fips == 51929)], Increase)) %>% #Martinsville
  mutate(Increase = if_else(county_fips == 51830, .$Increase[which(.$county_fips == 51931)], Increase)) %>% #Williamsburg
  mutate(Increase = if_else(county_fips == 51750, .$Increase[which(.$county_fips == 51933)], Increase)) %>% #Radford
  mutate(Increase = if_else(county_fips == 51590, .$Increase[which(.$county_fips == 51939)], Increase)) %>% #Danville
  mutate(Increase = if_else(county_fips == 51670, .$Increase[which(.$county_fips == 51941)], Increase)) %>% #Hopewell
  mutate(Increase = if_else(county_fips == 51153, .$Increase[which(.$county_fips == 51942)], Increase)) %>% #Prince Williams
  mutate(Increase = if_else(county_fips == 51775, .$Increase[which(.$county_fips == 51944)], Increase)) %>% #Salem
  mutate(Increase = if_else(county_fips == 51678, .$Increase[which(.$county_fips == 51945)], Increase)) %>% #Lexington
  mutate(Increase = if_else(county_fips == 51165, .$Increase[which(.$county_fips == 51947)], Increase)) %>% #Rockingham
  mutate(Increase = if_else(county_fips == 51620, .$Increase[which(.$county_fips == 51949)], Increase)) %>% #Franklin
  mutate(Increase = if_else(county_fips == 51630, .$Increase[which(.$county_fips == 51951)], Increase)) %>% #Fredericksburg
  mutate(Increase = if_else(county_fips == 51520, .$Increase[which(.$county_fips == 51953)], Increase)) %>% #Bristol
  mutate(Increase = if_else(county_fips == 51720, .$Increase[which(.$county_fips == 51955)], Increase)) %>% #Norton
  mutate(Increase = if_else(county_fips == 51199, .$Increase[which(.$county_fips == 51958)], Increase)) %>% #York
  mutate(Increase = if_else(county_fips == 15005, .$Increase[which(.$county_fips == 15901)], Increase)) %>% #Maui
  drop_na()

counties_map_increase_centroids <- counties_map_increase %>%
  st_centroid()

BEA_NOMINAL_GDP_COUNTY_INCREASES_GRAPH <- counties_map %>%
  ggplot() +
  geom_sf(fill = "grey75") +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = Increase > 0, size = Increase/1000000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = Increase/1000000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of Change",
                  max_size = 10,
                  breaks = c(0,30,60,90),
                  labels = c("$0","$30B","$60B","$90B"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("         Nominal GDP Growth by County, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = BEA_NOMINAL_GDP_COUNTY_INCREASES_GRAPH, "BEA GDP COUNTY Increases Nominal.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_COUNTIES_CATEGORIES_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAGDP9", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" =  paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)


BEA_GDP_COUNTIES_CATEGORIES_CODES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/GDP%20Releases/2023%20Q3/NCHSURCodes2013.csv") %>%
  mutate(GeoFips = str_pad(FIPS, width = 5, pad = "0")) %>%
  mutate(Category = X2013_code) %>%
  select(GeoFips,Category)

BEA_GDP_COUNTIES_CATEGORIES <- beaGet(BEA_GDP_COUNTIES_CATEGORIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  inner_join(BEA_GDP_COUNTIES_CATEGORIES_CODES, by = "GeoFips") %>%
  group_by(Category, TimePeriod) %>%
  summarise(DataValue = sum(DataValue, na.rm = TRUE)) %>%
  mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Index = (DataValue/nth(DataValue, 3)) * 100) %>%
  ungroup() %>%
  mutate(Category = case_when(
    Category == 1 ~ "Large (Pop >1M) Metro, Central Counties",
    Category == 2 ~ "Large (Pop >1M) Metro, Suburban Counties",
    Category == 3 ~ "Medium (Pop 250k-1M) Metro",
    Category == 4 ~ "Small (Pop <250k) Metro",
    Category == 5 ~ "Micropolitan Areas",
    Category == 6 ~ "Non-Core Counties",
    TRUE ~ as.character(Category)  # handles other cases
  )) %>%
  mutate(TimePeriod = as.Date(paste0(TimePeriod,"-01-01")))

BEA_GDP_COUNTIES_CATEGORIES_Graph <- ggplot() +
  geom_line(data = filter(BEA_GDP_COUNTIES_CATEGORIES, Category != "Large (Pop >1M) Metro, Central Counties"), aes(x=TimePeriod, y = Index, color = Category), size = 1.25) + 
  geom_line(data = filter(BEA_GDP_COUNTIES_CATEGORIES, Category == "Large (Pop >1M) Metro, Central Counties"), aes(x=TimePeriod, y = Index, color = Category), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(92,112), breaks = c(92,94,96,98,100,102,104,106,108,110,112), expand = c(0,0)) +
  ylab("Index, 2019 = 100") +
  ggtitle("Centers of Major Metros Lead US Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Despite Remote Work, Major Metros' Central Counties Remain the Largest Source of Growth") +
  theme_apricitas + theme(legend.position = c(.30,.7)) +
  scale_color_manual(name= "Real GDP, Index, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#F5B041","#3083DC","#AED581"), breaks = c("Large (Pop >1M) Metro, Central Counties","Large (Pop >1M) Metro, Suburban Counties","Medium (Pop 250k-1M) Metro","Small (Pop <250k) Metro","Micropolitan Areas","Non-Core Counties")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-730-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-730-as.Date("2017-01-01"))), ymin = 92-(.3*20), ymax = 92) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_GDP_COUNTIES_CATEGORIES_Graph, "BEA GDP COUNTIES CATEGORIES Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

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

BEA_GDP_METRO <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, Growth)

#Just checking the raw increase in GDP by Metro Since 2019
BEA_GDP_METRO_INCREASES <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(INCREASE = DataValue-first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, INCREASE)

MSA_map <- core_based_statistical_areas(cb = TRUE, year = 2021)

states <- states(cb = TRUE, year = 2021) %>%
  mutate(state_abbv = STUSPS) %>%
  filter(!state_abbv %in% c("AK", "HI", "PR", "AS", "GU", "MP","VI"))

MSA_map_US <- merge(MSA_map, BEA_GDP_METRO, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

BEA_GDP_MSA_BINS <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("              Real GDP Growth, 2019-2023\n            50 Largest Metro Areas by GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BEA_GDP_MSA_BINS, "BEA GDP MSA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_MSA_GRADIENT <- MSA_map_US %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.25 ~ 0.25,
    TRUE ~ Growth
  )) %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth), color = "black", lwd = 0.5) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),breaks = c(0,0.05,0.1,0.15,0.2,0.25), labels = c("<0%","5%","10%","15%","20%","25%+"), expand = c(0,0)) +
  ggtitle("              Real GDP Growth, 2019-2023\n            50 Largest Metro Areas by GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BEA_GDP_MSA_GRADIENT, "BEA GDP MSA GRADIENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_RPP_MSA_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "MARPP", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 1, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)


TOP_50_METROS_GDP <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  filter(DataValue == max(DataValue)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, DataValue)

BEA_RPP_MSA <- beaGet(BEA_RPP_MSA_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(Growth = (DataValue - first(DataValue))/100) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  #top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, Growth, GeoName) %>%
  merge(.,TOP_50_METROS_GDP, by = "GEOID")

MSA_map <- core_based_statistical_areas(cb = TRUE, year = 2021)

RPP_MSA_map_US <- merge(MSA_map, BEA_RPP_MSA, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, -0.02, 0,.02, Inf), labels = c("<-2%", "-2%-0%", "0%-2%", "2%+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

BEA_RPP_MSA_BINS <- RPP_MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  scale_fill_manual(
    values = c("<-2%" = "#FFE98F", "-2%-0%" = "#F5B041", "0%-2%" = "#FF8E72", "2%+" = "#EE6055"),
    na.value = "grey50", 
    breaks = rev(c("<-2%", "-2%-0%", "0%-2%", "2%+")),
    guide = guide_legend(override.aes = list(color = c("#EE6055", "#FF8E72", "#F5B041", "#FFE98F")))
  ) +
  ggtitle("Change in Metro Area Price Parities, 2019-2023\n               Relative to National Average\n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24))


ggsave(dpi = "retina",plot = BEA_RPP_MSA_BINS, "BEA RPP MSA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_RPP_MSA_GRADIENT <- RPP_MSA_map_US %>%
  mutate(Growth = case_when(
    Growth < -0.04 ~ -0.04,
    Growth > 0.04 ~ 0.04,
    TRUE ~ Growth
  )) %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth), color = "black", lwd = 0.5) +
  scale_fill_gradientn(colors = rev(c("#EE6055","#FF8E72","#F5B041","#FFE98F")),limits = c(-0.04,0.04), breaks = c(-.04,-.02,0,0.02,0.04), labels = c("-4%","-2%","0%","2%","4%"), expand = c(0,0)) +
  ggtitle("Change in Metro Area Price Parities, 2019-2023\n               Relative to National Average\n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24))


ggsave(dpi = "retina",plot = BEA_RPP_MSA_GRADIENT, "BEA RPP MSA GRADIENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_RPI_MSA_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "MARPI", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 2, # Specify the line code
  "GeoFips" = "MSA", # Specify the geographical level
  "Year" =  paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

TOP_50_METROS_GDP <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  filter(DataValue == max(DataValue)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, DataValue)

BEA_RPI_MSA <- beaGet(BEA_RPI_MSA_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  mutate(Growth = (DataValue - first(DataValue))/first(DataValue)) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  #top_n(50, DataValue) %>%
  transmute(GEOID = GeoFips, Growth, GeoName) %>%
  merge(.,TOP_50_METROS_GDP, by = "GEOID")

MSA_map <- core_based_statistical_areas(cb = TRUE, year = 2021)

RPI_MSA_map_US <- merge(MSA_map, BEA_RPI_MSA, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


BEA_RPI_MSA_BINS <- RPI_MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("Real Per-Capita Personal Income Growth, 2019-2023\n              At Metro Area Price Parities \n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 23))

ggsave(dpi = "retina",plot = BEA_RPI_MSA_BINS, "BEA RPI MSA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_RPI_MSA_GRADIENT <- RPI_MSA_map_US %>%
  mutate(Growth = case_when(
    Growth < 0 ~ 0,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  )) %>%
  ggplot() +
  geom_sf(data = filter(states, state_abbv != c("HI","AK")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth), color = "black", lwd = 0.5) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),limits = c(0,.15),breaks = c(0,0.05,0.1,0.15), labels = c("<0%","5%","10%","15%+"), expand = c(0,0)) +
  ggtitle("Real Per-Capita Personal Income Growth, 2019-2023\n              At Metro Area Price Parities \n           50 Largest Metro Areas by GDP") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 23))

ggsave(dpi = "retina",plot = BEA_RPI_MSA_GRADIENT, "BEA RPI MSA GRADIENT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_TX <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("TX", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)


MSA_map_TX <- merge(MSA_map, BEA_GDP_METRO_TX, by = "GEOID") %>%
    mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
    mutate(NAME = sub("-.*", "", NAME)) %>%
    st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")

MSA_map_TX_centroids <- MSA_map_TX %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_TX, .) %>%
  st_centroid()

BEA_GDP_MSA_TX_BINS <- MSA_map_TX %>%
    ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
    geom_sf(data = filter(states, state_abbv == c("TX")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
    geom_sf() +
    scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                      na.value = "grey50", 
                      #guide = "legend", 
                      labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                      guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
    ggtitle("   Real GDP Growth, 2019-2023\n         Texas Metro Areas") +
    theme(plot.title = element_text(size = 24)) +
    labs(caption = "Graph created by @JosephPolitano using BEA data") +
    labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_TX_centroids, GEOID %in% c("12420")), #Austin
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
    data = filter(MSA_map_TX_centroids, GEOID %in% c("26420")), #Houston
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
    data = filter(MSA_map_TX_centroids, GEOID %in% c("19100")), #Dallas
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
    data = filter(MSA_map_TX_centroids, GEOID %in% c("41700")), #San Antonio
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
  

ggsave(dpi = "retina",plot = BEA_GDP_MSA_TX_BINS, "BEA GDP MSA TX BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
  

BEA_GDP_METRO_CA <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("CA", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_CA <- merge(MSA_map, BEA_GDP_METRO_CA, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")
  

MSA_map_CA_centroids <- MSA_map_CA %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_CA, .) %>%
  st_centroid()

BEA_GDP_MSA_CA_BINS <- MSA_map_CA %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("CA")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  geom_sf(data = filter(counties_map_Growth, Growth_bucket == "0.12-0.16" & state_abbv == "CA"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("           Real GDP Growth, 2019-2023\n                California Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_CA_centroids, GEOID %in% c("31080")), #Los Angeles
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
    data = filter(MSA_map_CA_centroids, GEOID %in% c("41860")), #San Francisco
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
    data = filter(MSA_map_CA_centroids, GEOID %in% c("41940")), #San Jose
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
    data = filter(MSA_map_CA_centroids, GEOID %in% c("41740")), #San Diego
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
    data = filter(MSA_map_CA_centroids, GEOID %in% c("40140")), #Riverside
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
    data = filter(MSA_map_CA_centroids, GEOID %in% c("40900")), #Sacramento
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

ggsave(dpi = "retina",plot = BEA_GDP_MSA_CA_BINS, "BEA GDP MSA CA BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_FL <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl("FL", GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_FL <- merge(MSA_map, BEA_GDP_METRO_FL, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_FL_centroids <- MSA_map_FL %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_FL, .) %>%
  st_centroid()

BEA_GDP_MSA_FL_BINS <- MSA_map_FL %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv == c("FL")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  geom_sf(data = filter(counties_map_Growth, Growth_bucket %in% c("<0","0.04-0.08") & state_abbv == "FL"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend",
                    breaks = c("<0","0-0.04","0.04-0.08","0.08-0.12","0.12-0.16","0.16+"), 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("       Real GDP Growth, 2019-2023\n             Florida Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_FL_centroids, GEOID %in% c("33100")), #Miami
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
    data = filter(MSA_map_FL_centroids, GEOID %in% c("45300")), #Tampa
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
    data = filter(MSA_map_FL_centroids, GEOID %in% c("36740")), #Orlando
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
    data = filter(MSA_map_FL_centroids, GEOID %in% c("27260")), #Jacksonville
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

ggsave(dpi = "retina",plot = BEA_GDP_MSA_FL_BINS, "BEA GDP MSA FL BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_GDP_METRO_NE <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("MA", "RI", "CT", "NY","NJ","PA","DE","MD","DC","VA","NH","VT","ME"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_NE <- merge(MSA_map, BEA_GDP_METRO_NE, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_NE_centroids <- MSA_map_NE %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_NE, .) %>%
  st_centroid()

BEA_GDP_MSA_NE_BINS <- MSA_map_NE %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("MA", "RI", "CT", "NY","NJ","PA","DE","MD","DC","VA","NH","VT","ME")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("          Real GDP Growth, 2019-2023\n   Northeast and Mid-Atlantic Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_NE_centroids, GEOID %in% c("35620")), #New York
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("47900")), #Washington
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("14460")), #Boston
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("37980")), #Philadelphia
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("12580")), #Baltimore
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("38300")), #Pittsburgh
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("25540")), #Hartford
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("47260")), #Virginia Beach
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("40060")), #Richmond
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
    data = filter(MSA_map_NE_centroids, GEOID %in% c("14860")), #Bridgeport
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

ggsave(dpi = "retina",plot = BEA_GDP_MSA_NE_BINS, "BEA GDP MSA NE BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BEA_GDP_METRO_RM <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("WA", "OR", "ID", "UT","CO","AZ","NV","NM"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_RM <- merge(MSA_map, BEA_GDP_METRO_RM, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_RM_centroids <- MSA_map_RM %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_RM, .) %>%
  st_centroid()

BEA_GDP_MSA_RM_BINS <- MSA_map_RM %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("WA", "OR", "ID", "UT","CO","AZ","NV","NM")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("          Real GDP Growth, 2019-2023\n          Rocky Mountain Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("42660")), #Seattle
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(NAME, "\n", ifelse(Growth >= 0, "+", ""), sprintf("%.1f", round(Growth * 100, 1)), "%  ", paste0(ifelse(Increase >= 0, "+", "-"),"$", format(round(abs(Increase) / 1e5 / 10, 1), nsmall = 1),"B"))), 
    size = 6, 
    color = "black",
    hjust = 0.5,
    nudge_y = +150000, # adjust these values as needed
    nudge_x = -1000000, # adjust these values as needed
    segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = filter(MSA_map_RM_centroids, GEOID %in% c("38060")), #Phoenix
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
    data = filter(MSA_map_RM_centroids, GEOID %in% c("19740")), #Denver
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
    data = filter(MSA_map_RM_centroids, GEOID %in% c("38900")), #Portland
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
    data = filter(MSA_map_RM_centroids, GEOID %in% c("29820")), #Las Vegas
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
    data = filter(MSA_map_RM_centroids, GEOID %in% c("41620")), #Salt Lake City
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

ggsave(dpi = "retina",plot = BEA_GDP_MSA_RM_BINS, "BEA GDP MSA RM BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_MW <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("OH", "MI", "IN", "IL","WI","MN","IA","MO"), collapse = "|"), GeoName)) %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_MW <- merge(MSA_map, BEA_GDP_METRO_MW, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_MW_centroids <- MSA_map_MW %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_MW, .) %>%
  st_centroid()

BEA_GDP_MSA_MW_BINS <- MSA_map_MW %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("OH", "MI", "IN", "IL","WI","MN","IA","MO")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("            Real GDP Growth, 2019-2023\n                   Midwest Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_MW_centroids, GEOID %in% c("16980")), #Chicago
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("33460")), #Minneapolis
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("19820")), #Detroit
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("41180")), #STL
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("17140")), #Cincinnati
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("26900")), #Indianapolis
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("28140")), #Kansas City
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("18140")), #Columbus
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("17460")), #Cleveland
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
    data = filter(MSA_map_MW_centroids, GEOID %in% c("33340")), #Milwaukee
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

ggsave(dpi = "retina",plot = BEA_GDP_MSA_MW_BINS, "BEA GDP MSA MW BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BEA_GDP_METRO_SO <- beaGet(BEA_GDP_METRO_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  group_by(GeoFips) %>%
  arrange(GeoFips,TimePeriod) %>%
  #mutate(CAGR = (DataValue/first(DataValue )) ^ (1 / ((row_number() - 1))) - 1) %>%
  mutate(Growth = (DataValue/first(DataValue )) - 1) %>%
  mutate(Increase = (DataValue-first(DataValue))) %>%
  filter(TimePeriod == max(TimePeriod)) %>%
  ungroup() %>%
  arrange(DataValue) %>%
  slice(-nrow(.)) %>%
  filter(grepl(paste(c("NC", "SC", "GA", "AL","MS","TN","KY","AR","LA"), collapse = "|"), GeoName)) %>%
  filter(GeoFips != "47260" & GeoFips != "17140") %>%
  transmute(GEOID = GeoFips, Growth, Increase)

MSA_map_SO <- merge(MSA_map, BEA_GDP_METRO_SO, by = "GEOID") %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.04, 0.08, 0.12,0.16, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  mutate(NAME = sub("-.*", "", NAME)) %>%
  mutate(NAME = sub(",.*", "", NAME)) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")


MSA_map_SO_centroids <- MSA_map_SO %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(MSA_map_SO, .) %>%
  st_centroid()

BEA_GDP_MSA_SO_BINS <- MSA_map_SO %>%
  ggplot(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  geom_sf(data = filter(states, state_abbv %in% c("NC", "SC", "GA", "AL","MS","TN","KY","AR","LA")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf() +
  #geom_sf(data = filter(counties_map_Growth,Growth_bucket == "0.16+" & state_abbv == "NY"), aes(fill = Growth_bucket), alpha = 0, color = NA, size = 0) + # Invisible layer to make legend work
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50",
                    #guide = "legend", 
                    labels = c("<0%", "0-4%", "4-8%", "8-12%", "12-16%","16+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"))), drop = FALSE) +
  ggtitle("       Real GDP Growth, 2019-2023\n            South Metro Areas") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data. Real Growth Figures Calculated Using 2017 US Dollars") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(MSA_map_SO_centroids, GEOID %in% c("12060")), #Atlanta
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
    data = filter(MSA_map_SO_centroids, GEOID %in% c("16740")), #Charlotte
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
    data = filter(MSA_map_SO_centroids, GEOID %in% c("34980")), #Nashville
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
    data = filter(MSA_map_SO_centroids, GEOID %in% c("39580")), #Raleigh
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
    data = filter(MSA_map_SO_centroids, GEOID %in% c("32820")), #Memphis
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

ggsave(dpi = "retina",plot = BEA_GDP_MSA_SO_BINS, "BEA GDP MSA SO BINS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()


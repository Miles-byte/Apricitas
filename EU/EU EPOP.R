pacman::p_load(jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

devtools::install_github("oddworldng/INEbaseR")
library(INEbaseR)
#Spain INE Database API

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD")))
save_credentials(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD"))


EU_EPOP_BULK <- get_eurostat("lfsi_emp_q",legacy_bulk_download = FALSE)


EU_EPOP <- EU_EPOP_BULK %>%
  subset(age == "Y25-54" & s_adj == "SA" & indic_em == "EMP_LFS" & unit == "PC_POP" & sex == "T") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  ungroup() %>%
  filter(time == max(time)) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"))

EU_EPOP_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

EU_EPOP_SHP <- full_join(EU_EPOP_SHP, EU_EPOP,by = "geo") %>%
  select(geometry, values, geo, name, time) %>%
  mutate(label = values) %>%
  st_as_sf()

EU_EPOP_CENTROIDS <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(EU_EPOP_SHP %>% st_drop_geometry(), .) %>%
  mutate(lat = if_else(geo == "FR", lat + 350000, lat)) %>%
  mutate(long = if_else(geo == "FR", long + 850000, long)) %>%
  mutate(lat = if_else(geo == "FI", lat - 150000, lat)) %>%
  mutate(long = if_else(geo == "FI", long + 50000, long)) %>%
  mutate(lat = if_else(geo == "SE", lat - 250000, lat)) %>%
  mutate(long = if_else(geo == "SE", long - 50000, long)) %>%
  mutate(lat = if_else(geo == "IT", lat + 280000, lat)) %>%
  mutate(long = if_else(geo == "IT", long - 150000, long)) %>%
  mutate(long = if_else(geo == "DE", long - 50000, long)) %>%
  mutate(long = if_else(geo == "AT", long + 25000, long)) %>%
  mutate(lat = if_else(geo == "HU", lat - 25000, lat)) %>%
  mutate(long = if_else(geo == "HU", long - 50000, long)) %>%
  st_as_sf(coords = c("long","lat"), crs = 3035) %>%
  st_centroid()


EU_EPOP_GRAPH <- ggplot(data = EU_EPOP_SHP, aes(fill = values)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), label = scales::number_format(suffix = "%", accuracy = 1), expand = c(0,0)) +
  #ggtitle("EU Year-on-Year Real GDP Growth: Q2 2025") +
  ggtitle(paste0("  EU 25-54 Employment Rate, %: ", "Q", quarter(EU_EPOP_SHP$time[10]), " ", year(EU_EPOP_SHP$time[10]))) +
  scale_x_continuous(limits = c(1600000, 7150000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("SI")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-230000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("HR")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-360000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("DK")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+450000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("NL")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+620000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("BE")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+560000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("LU")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+430000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("EE")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]+30000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("LV")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("LT")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]-60000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("CY")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("GR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("MT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -100000,nudge_x = 0, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("PT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = -300000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("IE")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = -400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(EU_EPOP_CENTROIDS, geo %in% c("SK")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000,nudge_x = 500000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(EU_EPOP_CENTROIDS, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(EU_EPOP_CENTROIDS, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.1,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = EU_EPOP_GRAPH, "EU EPOP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

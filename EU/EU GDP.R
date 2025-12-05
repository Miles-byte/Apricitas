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

UK_GDP <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/abmi/ukea") %>%
  `colnames<-`(c("time","values")) %>%
  transmute(time = as.Date(as.yearqtr(time, "%Y Q%q")), values) %>%
  subset(., values > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  mutate(geo = "GB") %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

DE_GDP <- read.csv("https://api.statistiken.bundesbank.de/rest/download/BBKRT/Q.DE.Y.A.AG1.CA010.A.I?format=csv&lang=en") %>%
  select(ncol(.)) %>%
  mutate_at(vars(ncol(.)), as.numeric) %>% 
  drop_na() %>%
  slice(-(1:2)) %>%
  setNames("values") %>%
  mutate(time = seq.Date(from = as.Date("1991-01-01"), by = "3 months", length.out = nrow(.))) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  mutate(geo = "DE") %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

EU_GDP <- get_eurostat("namq_10_gdp",legacy_bulk_download = FALSE)

IT_GDP <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_156_DF_DCCN_SQCQ_3,1.0/Q...../ALL/?detail=full&startPeriod=2019-07-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(NOTE_VALUATION == "VAL__L_2015_N2") %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  transmute(values = obsValue, time = as.Date(as.yearqtr(obsTime, "%Y-Q%q"))) %>%
  mutate(geo = "IT") %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

# AL_GDP <- read.csv("http://databaza.instat.gov.al/sq/feca8039-955f-45fd-afc7-9fd78a2a1690") %>%
#   .[-2,] %>%
#   transpose() %>%
#   transmute(time = seq.Date(from = as.Date("2007-04-01"), by = "3 months", length = nrow(.)), values = as.numeric(V1), geo = "AL") %>%
#   drop_na() %>%
#   group_by(geo) %>%
#   mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
#   filter(time == max(time))

AL_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & s_adj == "SA" & na_item == "B1GQ" & geo %in% c("AL")) %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

IS_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & s_adj == "SA" & na_item == "B1GQ" & geo %in% c("IS")) %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

ME_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & na_item == "B1GQ" & geo %in% c("ME")) %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time) %>%
  select(-time) %>%
  ts(., start = c(2006,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(time = seq(from = as.Date("2006-01-01"), by = "3 month", length = nrow(.)), values = x, geo = "ME") %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

BA_GDP <- EU_GDP %>%
  subset(unit == "CLV10_MEUR" & na_item == "B1GQ" & geo %in% c("BA")) %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time) %>%
  select(-time) %>%
  ts(., start = c(2006,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(time = seq(from = as.Date("2006-01-01"), by = "3 month", length = nrow(.)), values = x, geo = "BA") %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

XK_GDP <- read.csv("https://askdata.rks-gov.net/sq/8980eb97-7d70-456d-be5c-30a9a576e31f") %>%
  transpose() %>%
  mutate(V1 = as.numeric(V1)) %>%
  drop_na() %>%
  arrange(desc(row_number())) %>%
  ts(., start = c(2011,1), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(time = seq.Date(from = as.Date("2011-01-01"), by = "3 months", length = nrow(.)), values = as.numeric(x), geo = "XK") %>%
  drop_na() %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time))

EU_GDP_CAGR <- EU_GDP %>%
  subset(unit == "CLV20_MEUR" & s_adj == "SCA" & na_item == "B1GQ") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(CAGR = (values / first(values)) ^ (1 / ((row_number() - 1) / 4)) - 1) %>%
  filter(time == max(time)) %>%
  #subset(!(geo %in% c("UK","DE","IT"))) %>%
  mutate(geo = gsub("EL","GR",geo)) #%>%
  #rbind(.,IS_GDP,UK_GDP,AL_GDP,ME_GDP,BA_GDP,XK_GDP,DE_GDP,IT_GDP)
  

EU_BROAD_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

# EU_SHP <- get_eurostat_geospatial(resolution = 20, 
#                                  nuts_level = 0, 
#                                  year = 2021) %>%
#   st_transform(., crs = 3035) %>%
#   st_as_sf() 

EU_BROAD_GDP_CAGR_SHP <- inner_join(EU_GDP_CAGR, EU_BROAD_SHP, by = "geo") %>%
  select(geometry, CAGR) %>%
  mutate(CAGR_bucket = cut(CAGR, breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), labels = c("<0", "0-0.01", "0.01-0.02", "0.02-0.03", "0.03+"))) %>%
  st_as_sf()

EU_BROAD_GDP_CAGR_SHP_GRAPH <- ggplot() +
  geom_sf(data = EU_BROAD_GDP_CAGR_SHP, color = NA, aes(fill = CAGR_bucket)) +
  geom_sf(data = EU_BROAD_GDP_CAGR_SHP, color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1%", "1-2%", "2-3%", "3%+")) +
  ggtitle("  Annualized Real GDP
 Growth Since Q3 2019") +
  scale_x_continuous(limits = c(2800000, 7150000)) +
  scale_y_continuous(limits = c(1380000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = c(.75,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank())

ggsave(dpi = "retina",plot = EU_BROAD_GDP_CAGR_SHP_GRAPH, "EU GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


EU_GDP_YOY <- EU_GDP %>%
  subset(unit == "CLV20_MEUR" & s_adj == "NSA" & na_item == "B1GQ") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(yoy = values/lag(values,4)-1) %>%
  ungroup() %>%
  filter(time == max(time)) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"))

#FOR FLASH GDP ESTIMATES
# EU_GDP_YOY <- EU_GDP %>%
#   subset(unit == "CLV_PCH_SM" & s_adj == "SCA" & na_item == "B1GQ") %>%
#   transmute(geo, time = TIME_PERIOD, values) %>%
#   subset(time >= as.Date("2019-07-01")) %>%
#   arrange(geo, time) %>%
#   group_by(geo) %>%
#   mutate(yoy = values/100) %>%
#   ungroup() %>%
#   filter(time == max(time)) %>%
#   mutate(geo = gsub("EL","GR",geo)) %>%
#   filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"))


EU_GDP_YOY_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

EU_GDP_YOY_SHP <- full_join(EU_GDP_YOY_SHP, EU_GDP_YOY, by = "geo") %>%
  select(geometry, yoy, geo, name, time) %>%
  mutate(label = yoy) %>%
  mutate(yoy = case_when(
    yoy > 0.05 ~ 0.05,
    yoy < -0.05 ~ -0.05,
    TRUE ~ yoy
  )) %>%
  st_as_sf()

EU_GDP_CENTROIDS <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(EU_GDP_YOY_SHP %>% st_drop_geometry(), .) %>%
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
  st_centroid() %>%
  drop_na()


EU_BROAD_GDP_YOY_SHP_GRAPH <- ggplot(data = EU_GDP_YOY_SHP, aes(fill = yoy)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), label = c("-5%+","-4%","-3%","-2%","-1%","0%","1%","2%","3%","4%","5%+"),breaks = c(-0.05,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  #ggtitle("EU Year-on-Year Real GDP Growth: Q2 2025") +
  ggtitle(paste0("EU Real GDP Growth, Year-on-Year: ", "Q", quarter(EU_GDP_YOY_SHP$time[10]), " ", year(EU_GDP_YOY_SHP$time[10]))) +
  scale_x_continuous(limits = c(1600000, 7150000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  geom_label(
    data = filter(EU_GDP_CENTROIDS, geo %in% c("SI")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-230000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("HR")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-360000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("DK")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+450000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("NL")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+620000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("BE")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+560000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("LU")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+430000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("EE")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]+30000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("LV")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("LT")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]-60000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("CY")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("GR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("MT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("PT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("IE")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS, geo %in% c("SK")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000,nudge_x = 500000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(EU_GDP_CENTROIDS, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(EU_GDP_CENTROIDS, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.1,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = EU_BROAD_GDP_YOY_SHP_GRAPH, "EU GDP Yoy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EU_GDP_2019 <- EU_GDP %>%
  subset(unit == "CLV20_MEUR" & s_adj == "SCA" & na_item == "B1GQ") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(yoy = values/values[1]-1) %>%
  ungroup() %>%
  filter(time == max(time)) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"))

EU_GDP_2019_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

EU_GDP_2019_SHP <- full_join(EU_GDP_2019_SHP, EU_GDP_2019, by = "geo") %>%
  select(geometry, yoy, geo, name) %>%
  mutate(label = yoy) %>%
  mutate(yoy = case_when(
    yoy > 0.25 ~ 0.25,
    yoy < -0.05 ~ -0.05,
    TRUE ~ yoy
  )) %>%
  st_as_sf()

EU_GDP_CENTROIDS_2019 <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(EU_GDP_2019_SHP %>% st_drop_geometry(), .) %>%
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


EU_BROAD_GDP_2019_SHP_GRAPH <- ggplot(data = EU_GDP_2019_SHP, aes(fill = yoy)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), label = c("-5%+","0%","5%","10%","15%","20%","25%+"),breaks = c(-0.05,0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ggtitle("    EU Real GDP Growth Since Q3 2019") +
  scale_x_continuous(limits = c(1600000, 7150000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = paste0("Graph created by @JosephPolitano using Eurostat data as of ", "Q", quarter(EU_GDP_2019$time[10]), " ", year(EU_GDP_2019$time[10]))) +
  labs(fill = NULL) +
  geom_label(
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("SI")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-230000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("HR")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-360000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("DK")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+450000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("NL")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+620000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("BE")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+560000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("LU")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+430000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("EE")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]+30000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("LV")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("LT")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]-60000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("CY")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("GR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("MT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("PT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("IE")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("SK")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000,nudge_x = 500000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(EU_GDP_CENTROIDS_2019, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(EU_GDP_CENTROIDS_2019, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.1,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = EU_BROAD_GDP_2019_SHP_GRAPH, "EU GDP 2019 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



EU_POP_BULK <- get_eurostat("namq_10_pe",legacy_bulk_download = FALSE)

EU_GDP <- get_eurostat("namq_10_gdp",legacy_bulk_download = FALSE)

EU_POP <- EU_POP_BULK %>%
  subset(s_adj == "NSA" & na_item == "POP_NC" & unit == "THS_PER") %>%
  transmute(geo, time = TIME_PERIOD, pop = values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  ungroup() %>%
  #filter(time == max(time)) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"))


EU_GDPpc_YOY <- EU_GDP %>%
  subset(unit == "CLV20_MEUR" & s_adj == "NSA" & na_item == "B1GQ") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")) %>%
  arrange(geo, time) %>%
  merge(.,EU_POP, by = c("time","geo")) %>%
  group_by(geo) %>%
  mutate(GDPpc = values/pop) %>%
  mutate(yoy = GDPpc/lag(GDPpc,4)-1) %>%
  ungroup() %>%
  filter(time == max(time))


EU_GDPpc_YOY_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

EU_GDPpc_YOY_SHP <- full_join(EU_GDPpc_YOY_SHP, EU_GDPpc_YOY, by = "geo") %>%
  select(geometry, yoy, geo, name, time) %>%
  mutate(label = yoy) %>%
  mutate(yoy = case_when(
    yoy > 0.05 ~ 0.05,
    yoy < -0.05 ~ -0.05,
    TRUE ~ yoy
  )) %>%
  st_as_sf()

EU_GDPpc_CENTROIDS <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(EU_GDPpc_YOY_SHP %>% st_drop_geometry(), .) %>%
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


EU_BROAD_GDPpc_YOY_SHP_GRAPH <- ggplot(data = EU_GDPpc_YOY_SHP, aes(fill = yoy)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), label = c("-5%+","-4%","-3%","-2%","-1%","0%","1%","2%","3%","4%","5%+"),breaks = c(-0.05,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  #ggtitle("EU Year-on-Year Real GDP Growth: Q2 2025") +
  ggtitle(paste0("EU Real GDP/Capita Growth, YoY: ", "Q", quarter(EU_GDPpc_YOY_SHP$time[10]), " ", year(EU_GDPpc_YOY_SHP$time[10]))) +
  scale_x_continuous(limits = c(1600000, 7150000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  geom_label(
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("SI")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-230000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("HR")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-360000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("DK")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+450000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("NL")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+620000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("BE")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+560000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("LU")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+430000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("EE")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]+30000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("LV")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("LT")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]-60000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("CY")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("GR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("MT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("PT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("IE")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS, geo %in% c("SK")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000,nudge_x = 500000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(EU_GDPpc_CENTROIDS, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(EU_GDPpc_CENTROIDS, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.1,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = EU_BROAD_GDPpc_YOY_SHP_GRAPH, "EU GDPpc Yoy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing




EU_GDPpc_2019 <- EU_GDP %>%
  subset(unit == "CLV20_MEUR" & s_adj == "SCA" & na_item == "B1GQ") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")) %>%
  arrange(geo, time) %>%
  merge(.,EU_POP, by = c("time","geo")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(GDPpc = values/pop) %>%
  mutate(yoy = GDPpc/GDPpc[1]-1) %>%
  ungroup() %>%
  filter(time == max(time))


EU_GDPpc_2019_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

EU_GDPpc_2019_SHP <- full_join(EU_GDPpc_2019_SHP, EU_GDPpc_2019, by = "geo") %>%
  select(geometry, yoy, geo, name) %>%
  mutate(label = yoy) %>%
  mutate(yoy = case_when(
    yoy > 0.25 ~ 0.25,
    yoy < -0.05 ~ -0.05,
    TRUE ~ yoy
  )) %>%
  st_as_sf()

EU_GDPpc_CENTROIDS_2019 <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(EU_GDPpc_2019_SHP %>% st_drop_geometry(), .) %>%
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


EU_BROAD_GDPpc_2019_SHP_GRAPH <- ggplot(data = EU_GDPpc_2019_SHP, aes(fill = yoy)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), label = c("-5%+","0%","5%","10%","15%","20%","25%+"),breaks = c(-0.05,0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ggtitle("EU Real GDP/Capita Growth Since Q3 2019") +
  scale_x_continuous(limits = c(1600000, 7150000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = paste0("Graph created by @JosephPolitano using Eurostat data as of ", "Q", quarter(EU_GDPpc_2019$time[10]), " ", year(EU_GDPpc_2019$time[10]))) +
  labs(fill = NULL) +
  geom_label(
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("SI")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-230000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("HR")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-360000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("DK")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+450000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("NL")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+620000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("BE")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+560000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("LU")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+430000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", "  "), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("EE")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]+30000, label = paste0(geo, "\n", ifelse(label >= 0, "   ", "  "), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("LV")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("LT")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]-60000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("CY")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("GR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("MT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("PT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("IE")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
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
    data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("SK")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000,nudge_x = 500000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(EU_GDPpc_CENTROIDS_2019, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(EU_GDPpc_CENTROIDS_2019, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.1,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = EU_BROAD_GDPpc_2019_SHP_GRAPH, "EU GDPpc 2019 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing








INSEE_dataset_list = get_dataset_list()

FRANCE_GDP_INSEE_list_selected <-
  get_idbank_list("CNT-2020-PIB-EQB-RF") %>% # Gross domestic product balance
  filter(OPERATION_label_en == "GDP - Gross domestic product") %>%
  filter(FREQ == "T") %>% #quarter
  add_insee_title() %>% #add titles
  filter(cleFlow == "T.CNT-EQUILIBRE_PIB.SO.PIB.SO.VALEUR_ABSOLUE.FE.L.EUROS.CVS-CJO.FALSE")#GDP

FRANCE_GDP_INSEE <- FRANCE_GDP_INSEE_list_selected %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  arrange(date) %>%
  mutate(value = value/value[7]*100)

GERMANY_GDP_BUNDESBANK <- read.csv("https://api.statistiken.bundesbank.de/rest/download/BBKRT/Q.DE.Y.A.AG1.CA010.A.I?format=csv&lang=en") %>%
  select(ncol(.)) %>%
  mutate_at(vars(ncol(.)), as.numeric) %>% 
  drop_na() %>%
  slice(-(1:2)) %>%
  setNames("value") %>%
  mutate(date = seq.Date(from = as.Date("1991-01-01"), by = "3 months", length.out = nrow(.))) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)

ITALY_GDP_ISTAT <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_156_DF_DCCN_SQCQ_3,1.0/Q...../ALL/?detail=full&startPeriod=2018-01-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(NOTE_VALUATION == "VAL__L_2015_N2") %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  transmute(value = obsValue, date = as.Date(as.yearqtr(obsTime, "%Y-Q%q"))) %>%
  mutate(value = value/value[7]*100)
  
EU_GDP_EUROSTAT <- EU_GDP %>%
  filter(unit == "CLV10_MEUR" & s_adj == "SCA" & na_item == "B1GQ" & geo == "EU27_2020" & TIME_PERIOD >= as.Date("2018-01-01")) %>%
  transmute(value = values, date = as.Date(as.yearqtr(TIME_PERIOD, "%Y-Q%q"))) %>%
  arrange(date) %>%
  mutate(value = value/value[7]*100)
  
SPAIN_DIR <- fromJSON("https://servicios.ine.es/wstempus/js/EN/OPERACIONES_DISPONIBLES")
SPAIN_PUB_LIST <- get_publications(lang = "en", det = 2)
SPAIN_TEST_GDP_TABLES <- get_tables(code = "237",lang = "en")
SPAIN_GDP_TABLE <- get_series(30679, resource = "table", lang = "en")
SPAIN_GDP_INE <- get_series("CNTR4851", resource = "data", nlast = 700, lang = "es") %>%
  .$Data %>%
  transmute(date = seq.Date(from = as.Date("1995-01-01"), by = "3 months", length.out = nrow(.)), value = Valor) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
  
get_series("CNTR4851")

EU_MAIN_GDP <- ggplot() +
  geom_line(data = SPAIN_GDP_INE, aes(x=date, y = value, color = "Spain"), size = 1.25) + 
  geom_line(data = ITALY_GDP_ISTAT, aes(x=date, y = value, color = "Italy"), size = 1.25) + 
  geom_line(data = FRANCE_GDP_INSEE, aes(x=date, y = value, color = "France"), size = 1.25) + 
  geom_line(data = GERMANY_GDP_BUNDESBANK, aes(x=date, y = value, color = "Germany"), size = 1.25) + 
  geom_line(data = EU_GDP_EUROSTAT, aes(x=date, y = value, color = "European Union"), size = 2.25) + 
  annotate("text",label = "Pre-COVID GDP", x = as.Date("2019-01-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,105), expand = c(0,0)) +
  ylab("Index, Q3 2019 = 100") +
  ggtitle("Eurozone GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using INSEE, ISTAT, DeStatis, INE, and Eurostat Data",subtitle = "Major Economies are Slightly Larger than Pre-Pandemic but Trailing Eurozone Average Growth") +
  theme_apricitas + theme(legend.position = c(.62,.24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany","France","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 75-(.3*30), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MAIN_GDP, "EU Main GDP Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FRANCE_FOOD_CONSUMPTION <- get_idbank_list("CONSO-MENAGES-2014") %>% # France Food ex-Tobacco Consumption 
  filter(PRODUIT_CONSO_MENAGES_label_en == "Food excepting tobacco") %>%
  add_insee_title() %>%
  pull(idbank) %>%
  get_insee_idbank(.) %>% 
  add_insee_metadata() %>%
  transmute(date = DATE, value = OBS_VALUE) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  arrange(date) %>%
  mutate(value = value/value[23]*100)

#NOTE: EDIT TO REMOVE END DATE
ITALY_FOOD_CONSUMPTION <- as.data.frame(readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_24_DF_DCCN_QNA_4,1.0/Q........./ALL/?detail=full&startPeriod=2018-01-01&dimensionAtObservation=TIME_PERIOD")) %>%
  subset(EDITION == EDITION[nrow(.)]) %>%
  subset(EXPEND_PURPOSE_COICOPCOFOG == "CP01" & VALUATION == "L_2015" & ADJUSTMENT == "Y") %>%
  transmute(value = obsValue/obsValue[8]*100, date = as.Date(as.yearqtr(obsTime, "%Y-Q%q")))

#Downloading Netherlands Consumption data
NETHERLANDS_FOOD_CONSUMPTION <- cbs_get_data('82608ENG') %>%
  subset(ConsumptionByHouseholds == "A047875") %>%
  filter(str_detect(`Periods`, fixed("MM", ignore_case = TRUE))) %>%
  transmute(value = Indices_1) %>%
  ts(., start = c(2000,1), frequency = 12) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  transmute(date = seq.Date(from = as.Date("2000-01-01"), by = "month", length.out = nrow(.)), value = x/x[239]*100) %>%
  subset(date >= as.Date("2018-01-01"))
  
GERMANY_FOOD_CONSUMPTION <- retrieve_data(tablename = "81000BV016", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  subset(CC93Z1 == "CC01-01") %>%
  select(JAHR, QUARTG, VGR102_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = VGR102_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(value = value/value[8]*100)

EU_FOOD_CONSUMPTION_GRAPH <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=ITALY_FOOD_CONSUMPTION, aes(x=date+45,y= value,color="Italy, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco"), size = 1.25) +
  geom_line(data=FRANCE_FOOD_CONSUMPTION, aes(x=date+15,y= value,color="France, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco"), size = 1.25) +
  #geom_line(data=NETHERLANDS_FOOD_CONSUMPTION, aes(x=date,y= value,color="Netherlands, Real Consumption of Food, Drink, and Tobacco"), size = 1.25) +
  geom_line(data=GERMANY_FOOD_CONSUMPTION, aes(x=date+45,y= value,color="Germany, Real Consumption of Food, Drink, and Tobacco"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,110), expand = c(0,0)) +
  ylab("Index, Nov/Q4 2019 = 100") +
  ggtitle("The European Food Crisis") +
  labs(caption = "Graph created by @JosephPolitano using INSEE, ISTAT, and DeStatis Data",subtitle = "Rising Food Prices Have Forced European Households to Cut Back on Food Consumption") +
  theme_apricitas + theme(legend.position = c(.42,.14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Germany, Real Consumption of Food, Drink, and Tobacco","France, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco","Italy, Real Consumption of Food & Nonalcoholic Drink Ex Tobacco")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*20), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_FOOD_CONSUMPTION_GRAPH, "EU Food Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EMP_EXP_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/EI_BSEE_M_R2/1.0?compress=false"))

EMP_EXP_NATIONAL <- EMP_EXP_BULK %>%
  subset(unit == "INX") %>%
  transmute(geo, time = as.Date(as.yearmon(TIME_PERIOD, format = "%Y-%m")), value = as.numeric(OBS_VALUE)) %>%
  subset(time >= as.Date("2018-01-01")) %>%
  pivot_wider(names_from = geo, values_from = value)

EMP_EXP_NATIONAL_graph <- ggplot() + #plotting regular vs non-regular employment
  annotate(geom = "hline",y = 100,yintercept = 100, size = 0.5,color = "white") +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `ES`,color="Spain"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `IT`,color="Italy"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `FR`,color="France"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `DE`,color="Germany"), size = 1.25) +
  geom_line(data=EMP_EXP_NATIONAL, aes(x=time,y= `EU27_2020`,color="European Union"), size = 2.25) +
  annotate("text", label = "above line = above normal growth\nbelow line = below normal growth", x = as.Date("2019-01-01"), y = 95, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(40,120), expand = c(0,0), breaks = c(40,60,80,100,120)) +
  ylab("Index, Net Increase") +
  ggtitle("Europe's Labor Market Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "European Employment Expectations Are Weak—Especially in Germany") +
  theme_apricitas + theme(legend.position = c(.80,.40)) +
  scale_color_manual(name= "Employment Expectations, Next 3M",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany","France","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 40-(.3*80), ymax = 40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_NATIONAL_graph, "EMP EXP NATIONAL GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EMP_EXP_INDUSTRY <- EMP_EXP_BULK %>%
  subset(unit == "BAL" & indic == "BS-IEME-BAL") %>%
  transmute(geo, time = as.Date(as.yearmon(TIME_PERIOD, format = "%Y-%m")), value = as.numeric(OBS_VALUE)) %>%
  subset(time >= as.Date("2000-01-01")) %>%
  pivot_wider(names_from = geo, values_from = value)

EMP_EXP_INDUSTRY_graph <- ggplot() + #plotting regular vs non-regular employment
  annotate(geom = "hline",y = 0,yintercept = 00, size = 0.5,color = "white") +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `ES`,color="Spain"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `IT`,color="Italy"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `FR`,color="France"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `DE`,color="Germany"), size = 1.25) +
  geom_line(data=EMP_EXP_INDUSTRY, aes(x=time,y= `EU27_2020`,color="European Union"), size = 2.25) +
  annotate("text", label = "above line = net expansion\nbelow line = net contraction", x = as.Date("2015-01-01"), y = -27.5, color = "white", size = 4.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-40,25), expand = c(0,0), breaks = c(-40,-20,0,20)) +
  ylab("Balance, Net Increase") +
  ggtitle("Europe's Industrial Labor Market Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "European Industry's Employment Expectations Are Weak—Especially in Germany") +
  theme_apricitas + theme(legend.position = c(.40,.85), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Industry, Employment Expectations, Next 3M",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Germany","France","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -40-(.3*65), ymax = -40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_EXP_INDUSTRY_graph, "EMP EXP INDUSTRY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GERMAN_GFCF_PRIVATE_EQUIPMENT <- retrieve_data(tablename = "81000BV015", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, VGR105_val,BAU020_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), value = VGR105_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2000-01-01"))

GERMAN_GFCF_PRIVATE_EQUIPMENT_graph <- ggplot() + #plotting GDP For US vs Germany
  geom_line(data=GERMAN_GFCF_PRIVATE_EQUIPMENT, aes(x=date,y= value,color="Real Private Fixed Investment in Equipment, Germany"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", prefix = "€"),limits = c(30,60), expand = c(0,0)) +
  ylab("Chained Billions of Dollars") +
  ggtitle("Germany's Slowdown") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis and BEA Data",subtitle = "Since 2018, German Economic Growth Has Been Especially Weak") +
  theme_apricitas + theme(legend.position = c(.6,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 30-(.3*30), ymax = 30) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GERMAN_GFCF_PRIVATE_EQUIPMENT_graph, "GERMAN GFCF PRIVATE EQUIPMENT GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GERMAN_GFCF_EQUIPMENT_CATEGORIES <- retrieve_data(tablename = "81000BV009", genesis=c(db='de'), language = "en") %>%
  subset(VGRPB5 == "VGRPVK") %>%
  subset(WERT05 == "X13JDKSB") %>%
  select(JAHR, QUARTG, VGR008_val, INV006_val, INV012_val) %>%
  transmute(date = as.Date(as.yearqtr(paste0(JAHR,QUARTG),"%YQUART%q")), Equipment = VGR008_val, Machines = INV006_val, Vehicles = INV012_val) %>%
  arrange(date) %>%
  subset(date >= as.Date("2016-01-01")) %>%
  mutate(across(where(is.numeric), ~if_else(.x == 0, NA_real_, .x))) %>%
  mutate(across(where(is.numeric), ~ .x / .x[9]*100))

GERMAN_GFCF_EQUIPMENT_CATEGORIES_graph <- ggplot() + #plotting Fixed Investment
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Machines,color="Equipment: Machinery and Other Devices"), size = 1.25) +
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Vehicles,color="Equipment: Vehicles"), size = 1.25) +
  geom_line(data=GERMAN_GFCF_EQUIPMENT_CATEGORIES, aes(x=date,y= Equipment,color="Equipment"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(60,120), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Germany's Slow Investment Rebound") +
  labs(caption = "Graph created by @JosephPolitano using DeStatis Data",subtitle = "German Investment In Fixed Manufacturing Assets Has Not Recovered to Pre-Pandemic Lvels") +
  theme_apricitas + theme(legend.position = c(.3,.27)) +
  scale_color_manual(name= "Germany: Real Fixed Investment",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 60-(.3*60), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GERMAN_GFCF_EQUIPMENT_CATEGORIES_graph, "GERMAN GFCF EQUIPMENT CATEGORIES GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CONSTRUCTION_PROD_EU <- get_eurostat("sts_copr_m") %>%
  subset(geo %in% c("IT","EU27_2020","DE","FR","ES")) %>%
  subset(time>= as.Date("2018-01-01")) %>%
  subset(nace_r2 == "F") %>%
  subset(unit == "I15") %>%
  subset(s_adj == "SCA") %>%
  transmute(geo,date = time,value = values) %>%
  pivot_wider(names_from = geo) %>%
  arrange(date) %>%
  mutate(across(where(is.numeric), ~ .x / .x[25]*100))

CONSTRUCTION_PROD_EU_GRAPH <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= DE,color="Germany"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= ES,color="Spain"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= FR,color="France"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= IT,color="Italy"), size = 1.25) +
  geom_line(data=CONSTRUCTION_PROD_EU, aes(x=date,y= EU27_2020,color="European Union"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(20,130), breaks = c(20,40,60,80,100,120,140), expand = c(0,0)) +
  ylab("Index, Nov/Q4 2019 = 100") +
  ggtitle("European Construction Output") +
  labs(caption = "Graph created by @JosephPolitano using INSEE, ISTAT, and CBS Data",subtitle = "Headline EU Construction Output is Tepid—But Boosted by Superbonus Schemes in Italy") +
  theme_apricitas + theme(legend.position = c(.2,.24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","France","Germany","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 20-(.3*110), ymax = 20) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_PROD_EU_GRAPH, "CONSTRUCTION PROD EU GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EA_MANU_SURVEY <- get_eurostat("ei_bsin_q_r2") %>%
  subset(s_adj == "SA" & geo == "EA20" & time >= as.Date("2004-01-01") &
           indic %in% c("BS-FLP2-PC","BS-FLP3-PC","BS-FLP4-PC","BS-FLP5-PC","BS-FLP6-PC")) %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EA_MANU_SURVEY_DEMAND_Materials_graph <- ggplot() + #plotting BIE
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= (`BS-FLP1-PC`+`BS-FLP2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP6-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP5-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP4-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.60), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Eurozone Supply Chain Crisis is Easing") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Manufacturers Say Materials Constraints are Easing Significantly") +
  theme_apricitas + theme(legend.position = c(.45,.45), plot.title = element_text(size = 26)) +
  scale_color_manual(name= "Factors Limiting Production in EA-20 Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials and Equipment","Other (Including COVID)","Financial Constraints")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 0-(.3*.60), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_MANU_SURVEY_DEMAND_Materials_graph, "EA_MANU_SURVEY_DEMAND_MATERIALS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#
EA_SERV_SURVEY <- get_eurostat("ei_bsse_q_r2") %>%
  subset(s_adj == "SA" & geo == "EA20" & time >= as.Date("2004-01-01") &
           indic %in% c("BS-FLB1-PC","BS-FLB2-PC","BS-FLB3-PC","BS-FLB4-PC","BS-FLB5-PC","BS-FLB6-PC")) %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EA_SERV_SURVEY_graph <- ggplot() + #plotting BIE
  #geom_line(data=EA_SERV_SURVEY, aes(x=time,y= (`BS-FLB1-PC`+`BS-FLB2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB4-PC`/100,color= "Shortage of Materials, Equipment, or Space"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB5-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB6-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.6), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Eurozone Service Sector Constraints") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Service Sector Firms Say Labor Constraints are Tight as COVID Constraints Ease") +
  theme_apricitas + theme(legend.position = c(.40,.67)) +
  scale_color_manual(name= "Factors Limiting Production in EA-20 Service Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials, Equipment, or Space","Other (Including COVID)","Financial Constraints")) + #, breaks = c("None or Insufficient Demand","Shortage of Materials, Equipment, or Space","Shortage of Labor","Financial Constraints","Other (Including COVID)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.6), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_SERV_SURVEY_graph, "EA_Serv Survey.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EA_CONS_SURVEY <- get_eurostat("ei_bsbu_m_r2") %>%
  subset(s_adj == "SA" & geo == "EA20" & time >= as.Date("2004-01-01") &
           indic %in% c("BS-FLBA2-PC","BS-FLBA4-PC","BS-FLBA5-PC","BS-FLBA6-PC","BS-FLBA7-PC")) %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EA_CONS_SURVEY_graph <- ggplot() + #plotting BIE
  geom_line(data=EA_CONS_SURVEY, aes(x=time,y= `BS-FLBA7-PC`/100,color= "Financial Constraints"), size = 1.25) +
  geom_line(data=EA_CONS_SURVEY, aes(x=time,y= `BS-FLBA6-PC`/100,color= "Other (Including COVID)"), size = 1.25) +
  geom_line(data=EA_CONS_SURVEY, aes(x=time,y= `BS-FLBA4-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EA_CONS_SURVEY, aes(x=time,y= `BS-FLBA5-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.60), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Eurozone Construction Sector Constraints") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EA Construction Companies Say Materials Constraints are Easing But Labor Shortages are High") +
  theme_apricitas + theme(legend.position = c(.45,.60), plot.title = element_text(size = 26)) +
  scale_color_manual(name= "Factors Limiting Production in EA-20 Construction Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Shortage of Labor","Shortage of Materials and Equipment","Other (Including COVID)","Financial Constraints")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 0-(.3*.60), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_CONS_SURVEY_graph, "EA_CONS Survey.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EA_FIN_CONSTRAINTS_graph <- ggplot() +
  geom_line(data=EA_CONS_SURVEY, aes(x=time,y= `BS-FLBA7-PC`/100,color= "Construction Sector"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB5-PC`/100,color= "Service Sector"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP6-PC`/100,color= "Manufacturing Sector"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = c(0,.05,0.1,0.15,0.2), limits = c(0,.225), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Eurozone's Credit Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Firms' Production is Being Hit By Rising Rates and Tightening Monetary Policy") +
  theme_apricitas + theme(legend.position = c(.50,.91), legend.key.height = unit(0.6,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Share of EA-20 Firms Citing Financial Constraints as an Impediment to Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Service Sector","Manufacturing Sector","Construction Sector")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.23), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_FIN_CONSTRAINTS_graph, "EA Fin Constraints Survey.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Lack of Demand
EA_NO_DEMAND_graph <- ggplot() + #plotting BIE
  geom_line(data=EA_CONS_SURVEY, aes(x=time,y= `BS-FLBA2-PC`/100,color= "Construction Sector"), size = 1.25) +
  geom_line(data=EA_SERV_SURVEY, aes(x=time,y= `BS-FLB2-PC`/100,color= "Service Sector"), size = 1.25) +
  geom_line(data=EA_MANU_SURVEY, aes(x=time,y= `BS-FLP2-PC`/100,color= "Manufacturing Sector"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = c(0.2,0.4,0.6), limits = c(0,.65), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Eurozone Demand is Waning") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "The Share of EA Firms Citing Demand Shortfalls as a Major Constraint is Up Significantly") +
  theme_apricitas + theme(legend.position = c(.50,.15), legend.key.height = unit(-1,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Share of EA-20 Firms Citing Insufficient Demand as an Impediment to Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Service Sector","Manufacturing Sector","Construction Sector")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(.1861*(today()-as.Date("2004-01-01")))), ymin = 0-(.3*.65), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EA_NO_DEMAND_graph, "EA Demand Constraints.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

RETAILER_ORDERS <- get_eurostat("ei_bsrt_m_r2") %>%
  subset(geo %in% c("IT","EU27_2020","DE","FR","ES")) %>%
  subset(time>= as.Date("2018-01-01")) %>%
  subset(indic == "BS-ROP") %>%
  subset(s_adj == "SA") %>%
  transmute(geo,time,value = values) %>%
  pivot_wider(names_from = geo)

RETAILER_ORDERS_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=RETAILER_ORDERS, aes(x=time,y= `IT`,color="Italy"), size = 1.25) +
  geom_line(data=RETAILER_ORDERS, aes(x=time,y= `DE`,color="Germany"), size = 1.25) +
  geom_line(data=RETAILER_ORDERS, aes(x=time,y= `FR`,color="France"), size = 1.25) +
  geom_line(data=RETAILER_ORDERS, aes(x=time,y= `ES`,color="Spain"), size = 1.25) +
  geom_line(data=RETAILER_ORDERS, aes(x=time,y= `EU27_2020`,color="European Union"), size = 2.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  annotate("text", label = "above line = net expansion\nbelow line = net contraction", x = as.Date("2022-01-01"), y = -30, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .2),limits = c(-65,30), expand = c(0,0), breaks = c(-60,-30,0,30)) +
  ylab("Balance, Increase minus Decrease") +
  ggtitle("European Retailer Orders") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat Data",subtitle = "European Retailers Expect to Cut Back on Orders From Their Suppliers Over the Next 3M") +
  theme_apricitas + theme(legend.position = c(.2,.27)) +
  scale_color_manual(name= "Retailers' Expectations\n# of Orders Placed With Suppliers\nNext 3 Months",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","France","Germany","Italy","Spain"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25, 1.25, 1.25, 1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -65-(.3*95), ymax = -65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAILER_ORDERS_graph, "RETAILER ORDERS GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

EU_HOUSING_CONSTRUCTION_BULK <- get_eurostat("sts_cobp_m")

EU_SF_MF_CONSTRUCTION <- EU_HOUSING_CONSTRUCTION_BULK %>%
  filter(indic_bt == "PSQM" & s_adj == "SCA" & geo == "EU27_2020" & unit == "I15" & cpa2_1 %in% c("CPA_F410011","CPA_F410012_410013")) %>%
  transmute(category = cpa2_1, date = time,value = values) %>%
  mutate(category = gsub("CPA_F410011","Single-Family Homes",category)) %>%
  mutate(category = gsub("CPA_F410012_410013","Multi-Family Homes",category)) %>%
  pivot_wider(names_from = category) %>%
  mutate(`Single-Family Homes` = `Single-Family Homes`*.87) %>%
  mutate(`Multi-Family Homes` = `Multi-Family Homes`*.709) %>%
  pivot_longer(cols = `Single-Family Homes`:`Multi-Family Homes`)

EU_SF_MF_CONSTRUCTION_graph <- ggplot(data = EU_SF_MF_CONSTRUCTION, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  ylab("Millions of Square Meters, Annual Rate") +
  ggtitle("EU Housing Permits are Falling") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M m2"), breaks = c(0,100,200,300,400,500,600), limits = c(0,650), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "EU Housing Starts are Down Significantly and Approaching Record Lows") +
  theme_apricitas + theme(legend.position = c(.7,.5)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "EU-27 Building Permits, Millions of Square Meters",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_SF_MF_CONSTRUCTION_graph, "EU SF MF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
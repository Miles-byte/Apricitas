pacman::p_load(ggpubr,jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

devtools::install_github("oddworldng/INEbaseR")
library(INEbaseR)
#Spain INE Database API

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

test_login(genesis=c(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD")))
save_credentials(db='de', user=Sys.getenv("DESTATIS_USER"), password=Sys.getenv("DESTATIS_PASSWORD"))


EU_PERMITS_ANNUAL_BULK <- get_eurostat("sts_cobp_a",legacy_bulk_download = FALSE)
EU_PERMITS_QUARTERLY_BULK <- get_eurostat("sts_cobp_q",legacy_bulk_download = FALSE)
EU_POP_QUARTERLY_BULK <- get_eurostat("namq_10_pe",legacy_bulk_download = FALSE)


EU_PERMITS_ANNUAL <- EU_PERMITS_ANNUAL_BULK %>%
  filter(TIME_PERIOD == as.Date("2021-01-01")) %>%
  filter(unit == "THS", cpa2_1 == "CPA_F41001_X_410014", indic_bt == "BPRM_DW") %>%
  select(geo,values)

EU_POP_QUARTERLY <- EU_POP_QUARTERLY_BULK %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  filter(unit == "THS_PER", s_adj == "NSA", na_item == "POP_NC") %>%
  select(geo,values)

EU_PERMITS_PC_QUARTERLY <- EU_PERMITS_QUARTERLY_BULK %>%
  filter(unit == "I21", cpa2_1 == "CPA_F41001_X_410014", indic_bt == "BPRM_DW", s_adj == "SCA") %>%
  full_join(.,EU_PERMITS_ANNUAL, by = "geo") %>%
  mutate(values = (values.x * values.y)/100) %>%
  select(-values.x,-values.y) %>%
  full_join(.,EU_POP_QUARTERLY, by = "geo") %>%
  mutate(values = values.x/values.y*1000) %>%
  select(-values.x,-values.y)
  
  
EU_PERMITS_PC_MAP <- EU_PERMITS_PC_QUARTERLY %>%
  group_by(geo) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  ungroup() %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2019-07-01")) %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  ungroup() %>%
  #filter(time == max(time)) %>%
  mutate(geo = gsub("EL","GR",geo)) %>%
  filter(geo %in% c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"))

EU_PERMITS_PC_SHP <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>%
  mutate(geo = iso_a2_eh)

EU_PERMITS_PC_SHP <- full_join(EU_PERMITS_PC_SHP, EU_PERMITS_PC_MAP,by = "geo") %>%
  select(geometry, values, geo, name, time) %>%
  mutate(label = values) %>%
  mutate(values = case_when(
    values > 8 ~ 8,
    TRUE ~ values
  )) %>%
  st_as_sf()

EU_PERMITS_PC_CENTROIDS <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(., continent == "Europe" | sovereignt %in% c("Turkey","Cyprus","Malta")) %>%
  mutate(iso_a2 = ifelse(sovereignt == "Kosovo", "XK", iso_a2)) %>%
  st_transform(., crs = 3035) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(EU_PERMITS_PC_SHP %>% st_drop_geometry(), .) %>%
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


EU_PERMITS_PC_GRAPH <- ggplot(data = EU_PERMITS_PC_SHP, aes(fill = values)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"), label = c("0","2","4","6","8+"), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  #ggtitle("EU Year-on-Year Real GDP Growth: Q2 2025") +
  ggtitle(paste0("EU Housing Permits Per 1k People: Q",
                 quarter(EU_PERMITS_PC_SHP$time[10]),
                 " ", year(EU_PERMITS_PC_SHP$time[10])))+
  scale_x_continuous(limits = c(1600000, 7150000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  geom_label(
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("SI")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-230000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("HR")), 
    aes(x = 4700000, y = st_coordinates(geometry)[,2]-360000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("DK")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+450000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("NL")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+620000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("BE")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+560000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("LU")), 
    aes(x = 3300000, y = st_coordinates(geometry)[,2]+430000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("EE")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]+30000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("LV")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("LT")), 
    aes(x = 5250000, y = st_coordinates(geometry)[,2]-60000, label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("CY")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("GR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("MT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("PT")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("IE")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
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
    data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("SK")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 200000,nudge_x = 500000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(EU_PERMITS_PC_CENTROIDS, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), " ")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.1,.65), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = EU_PERMITS_PC_GRAPH, "EU PERMITS PC Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing




EU_PERMITS_PC_LINE <- EU_PERMITS_PC_QUARTERLY %>%
  #subset(purchase == "TOTAL" & unit == "RCH_A") %>%
  transmute(geo, time = TIME_PERIOD, values) %>%
  subset(time >= as.Date("2013-01-01")) %>%
  filter(geo %in% c("EU27_2020","EA20")) %>%
  pivot_wider(names_from = geo, values_from = values)


EU_PERMITS_PC_LINE_GRAPH <- ggplot() + 
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=EU_PERMITS_PC_LINE, aes(x=time,y= EA20,color="Euro Area"), size = 1.25) +
  geom_line(data=EU_PERMITS_PC_LINE, aes(x=time,y= EU27_2020,color="European Union"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), expand = c(0,0), limits = c(0,5)) +
  ylab(NULL) +
  ggtitle(NULL) +
  #ggtitle("Italy's Job Boom") +
  #labs(caption = "Graph created by @JosephPolitano using ISTAT Data",subtitle = "Italian Employment Rates are at Record Highs") +
  theme_apricitas + theme(legend.position = c(.25,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("European Union","Euro Area")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = min(ITALY_EPOP_ISTAT$value)-(.3*(max(ITALY_EPOP_ISTAT$value)-min(ITALY_EPOP_ISTAT$value))), ymax = min(ITALY_EPOP_ISTAT$value)) +
  coord_cartesian(clip = "off")

EU_PERMITS_PC_MAP_SMALL <- ggplot(data = EU_PERMITS_PC_SHP, aes(fill = values)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = c("0","2","4","6","8+"), breaks = c(0,2,4,6,8), expand = c(0,0)) +
  #ggtitle("EU Year-on-Year Real GDP Growth: Q2 2025") +
  # ggtitle(paste0(" EU Unemployment Rate, %: ",
  #                month(EU_PERMITS_PC_SHP$time[10], label = TRUE, abbr = TRUE),
  #                " ", year(EU_PERMITS_PC_SHP$time[10])))+
  scale_x_continuous(limits = c(2400000, 6100000)) +
  scale_y_continuous(limits = c(1300000, 5300000)) +
  theme(plot.title = element_text(size = 24)) +
  #labs(caption = "Graph created by @JosephPolitano using Eurostat data") +
  labs(fill = NULL) +
  #geom_text(data = filter(EU_PERMITS_PC_CENTROIDS, !geo %in% c("AD","IS","UA","RS","TR","MD","AX","CH","SM","VA","ME","AL","MK","BA","JE","IM","FO","GB","BB","LI","MC","GG","XK","NO","IE","LU","NE","EE","LT","NL","BE","DK","BY","LV","MT","GR","CY","SI","SK","HR","BG","CZ","HU","AT","PT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(data = filter(EU_PERMITS_PC_CENTROIDS, geo %in% c("BG","CZ","HU","AT")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(geo, "\n", ifelse(label >= 0, " ", ""), sprintf("%.1f", round(label, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  theme(plot.title.position = "panel") + theme(plot.title = element_text(hjust = 0, margin = margin(l = -20))) +
  theme_apricitas + theme(legend.position = c(0.90,.68), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, -0.2, 0, -1.5), "in"), legend.key = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank())

tgrob <- text_grob("EU Housing Permits Per 1k People",face = "bold",size = 29,color = "white")

plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0.5,0,0.5, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.25,-.25,-0.25,-.25),"cm"))  

EU_PERMITS_PC_GGARRANGE <- ggarrange(EU_PERMITS_PC_LINE_GRAPH,EU_PERMITS_PC_MAP_SMALL, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = FALSE) + bgcolor("#252A32") + border("#252A32")

EU_PERMITS_PC_GGARRANGE <- ggarrange(plot_0,EU_PERMITS_PC_GGARRANGE, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(.,bottom = text_grob("\nGraph Created by @Josephpolitano Using Eurostat Data", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = EU_PERMITS_PC_GGARRANGE, "EU PERMITS PC GGARANGE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

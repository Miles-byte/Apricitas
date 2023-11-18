ZHVI_ZIP_2020 <- read.csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1685429158") %>%
  select(-RegionID, -SizeRank, - RegionType, - StateName) %>%
  #transpose() %>%
  gather(key = "date", value = "value",-5,-4,-3,-2,-1) %>%
  mutate(date = as.Date(gsub("X","",date), "%Y.%m.%d")) %>%
  mutate(ZIP = str_pad(RegionName, 5, pad = 0)) %>%
  group_by(ZIP) %>%
  mutate(value = (value-lag(value,46))/lag(value,46)) %>%
  subset(date == max(date)) %>%
  ungroup() %>%
  mutate(value = case_when(
    value > 0.5 ~ 0.5,
    value < -0.5 ~ -0.5,
    TRUE ~ value
  ))

options(tigris_use_sf = TRUE)
options(tigris_use_cache = TRUE)

ZIP <- zctas(year = 2020)
ZIP_ZHVI_MERGE_2020 <- left_join(ZIP, ZHVI_ZIP_2020, by = c("ZCTA5CE20" = "ZIP"))

ZIP_NYC <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "NY") %>%
  subset(City == "New York")
ZIP_NYC <- st_union(ZIP_NYC) %>%
  st_cast("POLYGON") 
ZIP_NYC <- nngeo::st_remove_holes(ZIP_NYC)


NYC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("New York-Newark-Jersey City, NY-NJ-PA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_NYC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("New York") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) + 
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_LA <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "CA") %>%
  subset(City == "Los Angeles")
ZIP_LA <- st_union(ZIP_LA) %>%
  st_cast("POLYGON") 
ZIP_LA <- nngeo::st_remove_holes(ZIP_LA)

LA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Los Angeles-Long Beach-Anaheim, CA","Riverside-San Bernardino-Ontario, CA") & ZCTA5CE20 != "90704" & ZCTA5CE20 !="93562"), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_LA, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Greater Los Angeles") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))


ZIP_SF <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "CA") %>%
  subset(City == "San Francisco")
ZIP_SF <- st_union(ZIP_SF) %>%
  st_cast("POLYGON") 
ZIP_SF <- nngeo::st_remove_holes(ZIP_SF)

SF <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("San Francisco-Oakland-Berkeley, CA","San Jose-Sunnyvale-Santa Clara, CA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_SF, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("San Francisco Bay Area") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_Chicago <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "IL") %>%
  subset(City == "Chicago")
ZIP_Chicago <- st_union(ZIP_Chicago) %>%
  st_cast("POLYGON") 
ZIP_Chicago <- nngeo::st_remove_holes(ZIP_Chicago)

CHI <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Chicago-Naperville-Elgin, IL-IN-WI")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_Chicago, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Chicago") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))


ZIP_DFW <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "TX") %>%
  subset(City == "Dallas")
ZIP_DFW <- st_union(ZIP_DFW) %>%
  st_cast("POLYGON") 
ZIP_DFW <- nngeo::st_remove_holes(ZIP_DFW)

DFW <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Dallas-Fort Worth-Arlington, TX")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DFW, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       midpoint = 0,
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       midpoint = 0,
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Dallas-Fort Worth") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_HOU <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "TX") %>%
  subset(City == "Houston")
ZIP_HOU <- st_union(ZIP_HOU) %>%
  st_cast("POLYGON") 
ZIP_HOU <- nngeo::st_remove_holes(ZIP_HOU)

HOU <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Houston-The Woodlands-Sugar Land, TX")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_HOU, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       midpoint = 0,
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       midpoint = 0,
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Houston") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))


ZIP_DC <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "DC")
ZIP_DC <- st_union(ZIP_DC) %>%
  st_cast("POLYGON") 
ZIP_DC <- nngeo::st_remove_holes(ZIP_DC)

DC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Washington-Arlington-Alexandria, DC-VA-MD-WV")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       midpoint = 0,
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       midpoint = 0,
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Washington DC") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))

ZIP_PHI <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "PA") %>%
  subset(City == "Philadelphia")
ZIP_PHI <- st_union(ZIP_PHI) %>%
  st_cast("POLYGON") 
ZIP_PHI <- nngeo::st_remove_holes(ZIP_PHI)

PHI <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_PHI, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       midpoint = 0,
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       midpoint = 0,
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Philadelphia") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))


ZIP_ATL <- ZIP_ZHVI_MERGE_2020 %>%
  subset(State == "GA") %>%
  subset(City == "Atlanta")
ZIP_ATL <- st_union(ZIP_ATL) %>%
  st_cast("POLYGON") 
ZIP_ATL <- nngeo::st_remove_holes(ZIP_ATL)

ATL <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Atlanta-Sandy Springs-Alpharetta, GA")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_ATL, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                       midpoint = 0,
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.5,0.5)) +
  scale_color_gradient(high = "#00A99D",
                       midpoint = 0,
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.5,0.5)) +
  ggtitle("Atlanta") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 13,hjust = 0.5))



ZIP_ZHVI_ARRANGE2020 <- ggarrange(NYC,LA,SF,CHI,DFW,HOU,DC,PHI,ATL,  ncol = 3, nrow = 3, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

ZIP_ZHVI_ARRANGE2020 <- annotate_figure(ZIP_ZHVI_ARRANGE2020, 
                                    top = text_grob("Zillow Home Value Change in Major Metros Since Jan 2020
                                                  ", face = "bold", size = 28, color = "white")) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = ZIP_ZHVI_ARRANGE2020, "ZIP ZHVI ARRANGE 2020.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


MEGA <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, Metro %in% c("Trenton-Princeton, NJ","Washington-Arlington-Alexandria, DC-VA-MD-WV","Baltimore-Columbia-Towson, MD","Philadelphia-Camden-Wilmington, PA-NJ-DE-MD","Boston-Cambridge-Newton, MA-NH","New York-Newark-Jersey City, NY-NJ-PA","New Haven-Milford, CT","Providence-Warwick, RI-MA","Bridgeport-Stamford-Norwalk, CT","Hartford-East Hartford-Middletown, CT","Worcester, MA-CT","Barnstable Town, MA","Norwich-New London, CT")), aes(fill = value, color = value))+
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_PHI, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_NYC, fill = NA, color = "black", lwd = 0.40) +
  geom_sf(data = ZIP_Boston, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.50,0.50)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.50,0.50)) +
  ggtitle("          Zillow 12M Home Price Growth in the Acela Corridor") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = MEGA, "MEGA ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



DONUT_NYC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, 
                        (State %in% c("NY","NJ","CT") & CountyName %in% c("New York County","Kings County","Queens County","Richmond County","Bronx County","Nassau County","Suffolk County","Westchester County","Rockland County","Putnam County","Bergen County","Hudson County","Passaic County","Union County","Middlesex County","Monmouth County","Somerset County","Morris County","Fairfield County","New Haven County")) 
                        | (State == "NJ" & CountyName == "Essex County")), 
          aes(fill = value, color = value)) +#geom_sf(data = DONUT_DC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #geom_sf(data = DONUT_NYC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_NYC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.50,0.50)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.50,0.50)) +
  ggtitle("New York") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))


DONUT_CHI_STATES <- subset(states_boundaries, STUSPS %in% c("IL","IN","WI"))

DONUT_CHI_STATES <- st_transform(DONUT_CHI_STATES, st_crs(ZIP_ZHVI_MERGE))

ZIP_Chicago <- ZIP_ZHVI_MERGE %>%
  subset(State == "IL") %>%
  subset(City == "Chicago")

ZIP_Chicago <- st_union(ZIP_Chicago) %>%
  st_cast("POLYGON") 
ZIP_Chicago <- nngeo::st_remove_holes(ZIP_Chicago)


DONUT_CHI <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, State %in% c("IL","IN","WI") & CountyName %in% c("Cook County","Lake County","DuPage County","Will County","Lake County","Kenosha County")), 
          aes(fill = value, color = value)) +#geom_sf(data = DONUT_DC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #geom_sf(data = DONUT_CHI_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_Chicago, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.50,0.50)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.50,0.50)) +
  ggtitle("Chicago") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))

ZIP_Boston <- ZIP_ZHVI_MERGE %>%
  subset(State == "MA") %>%
  subset(City == "Boston")

ZIP_Boston <- st_union(ZIP_Boston) %>%
  st_cast("POLYGON") 
ZIP_Boston <- nngeo::st_remove_holes(ZIP_Boston)

DONUT_BOS <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, State %in% c("MA") & CountyName %in% c("Suffolk County","Essex County","Middlesex County","Norfolk County","Plymouth County","Bristol County")), 
          aes(fill = value, color = value)) +
  geom_sf(data = ZIP_Boston, fill = NA, color = "black", lwd = 0.40) +
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.50,0.50)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.50,0.50)) +
  ggtitle("Boston") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))


ggsave(dpi = "retina",plot = DONUT_BOS, "DONUT BOS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


DONUT_ZHVI_ARRANGE <- ggarrange(DONUT_NYC,DONUT_CHI,DONUT_BOS,DONUT_DC,  ncol = 2, nrow = 2, common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

DONUT_ZHVI_ARRANGE <- annotate_figure(DONUT_ZHVI_ARRANGE,top = text_grob("The 'Donut Effect' is Hitting Many Cities—Falls in Downtown Home Prices as Suburbs Hold Up Better
                                                  ", face = "bold", size = 10, color = "white")) + bgcolor("#252A32") + border("#252A32")
DONUT_ZHVI_ARRANGE <- annotate_figure(DONUT_ZHVI_ARRANGE, 
                                      top = text_grob("Zillow 12M Home Value Change", face = "bold", size = 28, color = "white")) + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = DONUT_ZHVI_ARRANGE, "DONUT ZHVI ARRANGE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ZIP_DC <- ZIP_ZHVI_MERGE %>%
  subset(State == "DC")
ZIP_DC <- st_union(ZIP_DC) %>%
  st_cast("POLYGON") 
ZIP_DC <- nngeo::st_remove_holes(ZIP_DC)

DONUT_DC <- ggplot() +
  geom_sf(data = subset(ZIP_ZHVI_MERGE_2020, State %in% c("DC","MD","VA") & CountyName %in% c("District of Columbia","Arlington County","Prince Georges County","Fairfax County","Loudoun County","Prince William County","Charles County","Manassas Park City","Manassas City","Fairfax County","Falls Church City","Alexandria City","Howard County","Anne Arundel County")|
                          (State == "MD" & CountyName == "Montgomery County")), aes(fill = value, color = value))+
  #geom_sf(data = DONUT_DC_STATES, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  geom_sf(data = ZIP_DC, fill = NA, color = "black", lwd = 0.40) +
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.1,-0.05,0,0.05,0.1), 
                      labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                      limits = c(-0.50,0.50)) +
  scale_color_gradient(high = "#00A99D",
                       low = "#EE6055",
                       space = "Lab",
                       na.value = "grey50",
                       guide = NULL,
                       aesthetics = "color",
                       breaks = c(-0.1,-0.05,0,0.05,0.1), 
                       labels = c("-10+%","-5%","+0%","+5%","+10+%"),
                       limits = c(-0.50,0.50)) +
  ggtitle("Washington DC") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 16,hjust = 0.5))


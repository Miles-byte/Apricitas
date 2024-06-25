install.packages("sf")
library(sf)

#DC METRO SHAPEFILES FROM
#https://opendata.dc.gov/

# Define the path to the zip file

temp_zip <- tempfile(fileext = ".zip")
download.file("https://github.com/Miles-byte/Apricitas/raw/main/City%20Mapping/DC_Metro_Lines_Regional.zip", temp_zip, mode = "wb")
unzip_dir <- tempfile()
unzip(temp_zip, exdir = unzip_dir)

DC_METRO_LINES <- file.path(unzip_dir) %>%
  st_read()

temp_zip <- tempfile(fileext = ".zip")
download.file("https://github.com/Miles-byte/Apricitas/raw/main/City%20Mapping/DC_Metro_Stations_Regional.zip", temp_zip, mode = "wb")
unzip_dir <- tempfile()
unzip(temp_zip, exdir = unzip_dir)

DC_METRO_STATIONS <- file.path(unzip_dir) %>%
  st_read()



VA <- geo.make(state = "VA",county= c("Arlington","Alexandria city","Fairfax County","Fairfax city","Loudoun","Prince William","Falls Church city"),tract = "*")

VA_CAR_FREE <- acs.fetch(geography = VA,endyear = 2022,table.number="B08201")

VA_CAR_FREE_df <- data.frame(cbind(data.frame(VA_CAR_FREE@geography), data.frame(VA_CAR_FREE@estimate))) %>% 
  summarize(NAME,
    GEOID = paste0(VA_CAR_FREE@geography$state,
                   str_pad(VA_CAR_FREE@geography$county,
                           width=3,
                           side="left",
                           pad="0"),
                   VA_CAR_FREE@geography$tract
    ),
    percent_car_free=B08201_002/B08201_001
)


MD <- geo.make(state = "MD",county= c("Montgomery","Prince George","Frederick","Charles","Howard","Anne Arundel"),tract = "*")

MD_CAR_FREE <- acs.fetch(geography = MD,endyear = 2022,table.number="B08201")

MD_CAR_FREE_df <- data.frame(cbind(data.frame(MD_CAR_FREE@geography), data.frame(MD_CAR_FREE@estimate))) %>% 
  summarize(NAME,
            GEOID = paste0(MD_CAR_FREE@geography$state,
                           str_pad(MD_CAR_FREE@geography$county,
                                   width=3,
                                   side="left",
                                   pad="0"),
                           MD_CAR_FREE@geography$tract
            ),
            percent_car_free=B08201_002/B08201_001
  )



DC <- geo.make(state = "DC",county = "*",tract = "*")

DC_CAR_FREE <- acs.fetch(geography = DC,endyear = 2022,table.number="B08201")

DC_CAR_FREE_df <- data.frame(cbind(data.frame(DC_CAR_FREE@geography), data.frame(DC_CAR_FREE@estimate))) %>% 
  summarize(NAME,
            GEOID = paste0(DC_CAR_FREE@geography$state,
                           str_pad(DC_CAR_FREE@geography$county,
                                   width=3,
                                   side="left",
                                   pad="0"),
                           DC_CAR_FREE@geography$tract
            ),
            percent_car_free=B08201_002/B08201_001
  )

DC_VA_MD_CAR_FREE_df <- rbind(DC_CAR_FREE_df,VA_CAR_FREE_df) %>%
  rbind(.,MD_CAR_FREE_df)

DC_SHAPE <- tracts(state="DC",county= c(001))
VA_SHAPE <- tracts(state="VA",county= c("Arlington","Alexandria city","Fairfax County","Fairfax city","Loudoun","Prince William","Falls Church city"))
MD_SHAPE <- tracts(state="MD",county= c("Montgomery","Prince George","Frederick","Charles","Howard","Anne Arundel"))
# DC_SHAPE <- DC_SHAPE %>% 
#   left_join(., DC_INCOME_df,by="GEOID") %>% 
#   filter(median_income>=0) %>%
#   select(geometry,median_income)

DC_VA_MD_SHAPE <- rbind(DC_SHAPE, VA_SHAPE) %>%
  rbind(.,MD_SHAPE) %>%
  left_join(., DC_VA_MD_CAR_FREE_df, by = "GEOID") %>% 
  select(geometry, percent_car_free) %>%
  st_make_valid()

# Load water area data
DC_VA_MD_WATER <- rbind(area_water("DC", "01"), area_water("VA", c("Arlington","Alexandria city","Fairfax County","Fairfax city","Loudoun","Prince William","Falls Church city"))) %>%
  rbind(.,area_water("MD",c("Montgomery","Prince George","Frederick","Charles","Howard","Anne Arundel"))) %>%
  st_make_valid()

# Simplify geometries to avoid issues with st_union
#DC_VA_SHAPE <- st_simplify(DC_VA_SHAPE, dTolerance = 0.001)
#DC_VA_WATER <- st_simplify(DC_VA_WATER, dTolerance = 0.001)

DC_VA_MD_unioned_shapes <- st_union(DC_VA_MD_SHAPE$geometry) %>% st_make_valid()
DC_VA_MD_unioned_water <- st_union(DC_VA_MD_SHAPE$geometry) %>% st_make_valid()

DC_VA_MD_combined_areas <- st_union(DC_VA_MD_unioned_shapes, DC_VA_MD_unioned_water) %>% st_make_valid()

DC_COUNTY_SHAPE <- counties(state="DC",cb = FALSE)
VA_COUNTY_SHAPE <- counties(state="VA",cb = FALSE) %>%
  filter(NAME %in% c("Arlington","Alexandria city","Fairfax County","Fairfax city","Loudoun","Prince William","Falls Church city"))
MD_COUNTY_SHAPE <- counties(state="VA",cb = FALSE) %>%
  filter(NAME %in% c("Montgomery","Prince George","Frederick","Charles","Howard","Anne Arundel"))


DC_VA_MD_COUNTY_SHAPES <- rbind(DC_COUNTY_SHAPE,VA_COUNTY_SHAPE) %>%
  rbind(.,MD_COUNTY_SHAPE)

DC_VA_MD_COUNTY_SHAPES <- st_union(DC_VA_COUNTY_SHAPES$geometry) %>%
  st_make_valid()

DC_VA_MD_precise_polygon <- st_convex_hull(st_union(st_combine(DC_VA_MD_combined_areas)))
DC_VA_MD_refined_bounding_polygon <- st_intersection(DC_VA_precise_polygon, DC_VA_MD_COUNTY_SHAPES)
DC_VA_MD_non_tract_areas <- st_difference(DC_VA_refined_bounding_polygon, DC_VA_MD_combined_areas)

DC_VA_MD_SHAPE <- DC_VA_MD_SHAPE %>%
  st_make_valid() %>%
  erase_water()

DC_OUTLINE <- places(state="DC",cb = TRUE)

DC_CAR_FREE_MAP <- ggplot() + 
  geom_sf(data = DC_VA_MD_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = DC_VA_MD_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = DC_VA_MD_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = DC_METRO_LINES, aes(color = NAME), lwd = 1.25) +
  scale_color_manual(values = c("#1574c4","#0fab4a","#f68713","#e51535","#9d9f9c","#fdd005")) +
  geom_sf(data = DC_METRO_STATIONS, color = "black", size = 0.5) +
  geom_sf(data = DC_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  scale_fill_viridis_c(option = "turbo",name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Car-Free Households in the DC Area") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-77.3,-76.8)) +
  scale_y_continuous(limits = c(38.75,39.12)) +
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(color = "none")

ggsave(dpi = "retina",plot = DC_CAR_FREE_MAP, "DC Car Free Map Turbo.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

DC_CAR_FREE_MAP_NO_LINES <- ggplot() + 
  geom_sf(data = DC_VA_MD_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = DC_VA_MD_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = DC_VA_MD_SHAPE, aes(fill = percent_car_free), color = NA) +
  #geom_sf(data = DC_METRO_LINES, aes(color = NAME), lwd = 1.25) +
  #scale_color_manual(values = c("#1574c4","#0fab4a","#f68713","#e51535","#9d9f9c","#fdd005")) +
  #geom_sf(data = DC_METRO_STATIONS, color = "black", size = 0.5) +
  geom_sf(data = DC_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  scale_fill_viridis_c(name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Car-Free Households in the DC Area") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-77.3,-76.8)) +
  scale_y_continuous(limits = c(38.75,39.12)) +
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(color = "none")

ggsave(dpi = "retina",plot = DC_CAR_FREE_MAP_NO_LINES, "DC Car Free Map No Lines.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


DC_CAR_FREE_MAP_ZOOMED <- ggplot() + 
  geom_sf(data = DC_VA_MD_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = DC_VA_MD_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = DC_VA_MD_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = DC_METRO_LINES, aes(color = NAME), lwd = 1.25) +
  scale_color_manual(values = c("#1574c4","#0fab4a","#f68713","#e51535","#9d9f9c","#fdd005")) +
  geom_sf(data = DC_METRO_STATIONS, color = "black", size = 0.5) +
  geom_sf(data = DC_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  scale_fill_viridis_c(option = "turbo",name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Car-Free Households in DC") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-77.15,-76.9)) +
  scale_y_continuous(limits = c(38.7916,38.9955)) +
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(color = "none")

ggsave(dpi = "retina",plot = DC_CAR_FREE_MAP_ZOOMED, "DC Car Free Map Turbo Zoomed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


DC_CAR_FREE_MAP_ZOOMED_NO_LINES <- ggplot() + 
  geom_sf(data = DC_VA_MD_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = DC_VA_MD_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = DC_VA_MD_SHAPE, aes(fill = percent_car_free), color = NA) +
  #geom_sf(data = DC_METRO_LINES, aes(color = NAME), lwd = 1.25) +
  #scale_color_manual(values = c("#1574c4","#0fab4a","#f68713","#e51535","#9d9f9c","#fdd005")) +
  #geom_sf(data = DC_METRO_STATIONS, color = "black", size = 0.5) +
  geom_sf(data = DC_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  scale_fill_viridis_c(name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Car-Free Households in DC") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-77.15,-76.9)) +
  scale_y_continuous(limits = c(38.7916,38.9955)) +
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(color = "none")

ggsave(dpi = "retina",plot = DC_CAR_FREE_MAP_ZOOMED_NO_LINES, "DC Car Free Map No Lines Zoomed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

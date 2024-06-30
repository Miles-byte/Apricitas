pacman::p_load(pacman,ggrepel,dots,ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
p_load("acs","leaflet","tigris","dplyr","stringr","lwgeom")

install.packages("sf")
library(sf)

api.key.install(key=Sys.getenv("CENSUS_KEY"))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

#DC METRO SHAPEFILES FROM
#https://opendata.dc.gov/

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

DC_SHAPE <- tracts(state="DC",county= c(001), year = 2022)
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
MD_COUNTY_SHAPE <- counties(state="MD",cb = FALSE) %>%
  filter(NAME %in% c("Montgomery","Prince George","Frederick","Charles","Howard","Anne Arundel"))


DC_VA_MD_COUNTY_SHAPES <- rbind(DC_COUNTY_SHAPE,VA_COUNTY_SHAPE) %>%
  rbind(.,MD_COUNTY_SHAPE)

DC_VA_MD_COUNTY_SHAPES <- st_union(DC_VA_MD_COUNTY_SHAPES$geometry) %>%
  st_make_valid()

DC_VA_MD_precise_polygon <- st_convex_hull(st_union(st_combine(DC_VA_MD_combined_areas))) %>% st_make_valid()
DC_VA_MD_refined_bounding_polygon <- st_intersection(DC_VA_MD_precise_polygon, DC_VA_MD_COUNTY_SHAPES) %>% st_make_valid()
DC_VA_MD_non_tract_areas <- st_difference(DC_VA_MD_refined_bounding_polygon, DC_VA_MD_combined_areas)

DC_VA_MD_SHAPE <- DC_VA_MD_SHAPE %>%
  st_make_valid() %>%
  erase_water()

DC_OUTLINE <- places(state="DC",cb = TRUE)

DC_CAR_FREE_MAP <- ggplot() + 
  geom_sf(data = DC_VA_MD_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = DC_VA_MD_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = DC_VA_MD_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = DC_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  geom_sf(data = DC_METRO_LINES, aes(color = NAME), lwd = 1.25) +
  scale_color_manual(values = c("#1574c4","#0fab4a","#f68713","#e51535","#9d9f9c","#fdd005")) +
  geom_sf(data = DC_METRO_STATIONS, color = "black", size = 0.5) +
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
  geom_sf(data = DC_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  geom_sf(data = DC_METRO_LINES, aes(color = NAME), lwd = 1.25) +
  scale_color_manual(values = c("#1574c4","#0fab4a","#f68713","#e51535","#9d9f9c","#fdd005")) +
  geom_sf(data = DC_METRO_STATIONS, color = "black", size = 0.5) +
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


IL <- geo.make(state = "IL",county= c("Cook County","Lake County","Kendall","Kane","DeKalb","McHenry","Grundy","DuPage", "Will County"),tract = "*")

IL_CAR_FREE <- acs.fetch(geography = IL,endyear = 2022,table.number="B08201")

IL_CAR_FREE_df <- data.frame(cbind(data.frame(IL_CAR_FREE@geography), data.frame(IL_CAR_FREE@estimate))) %>% 
  summarize(NAME,
            GEOID = paste0(IL_CAR_FREE@geography$state,
                           str_pad(IL_CAR_FREE@geography$county,
                                   width=3,
                                   side="left",
                                   pad="0"),
                           IL_CAR_FREE@geography$tract
            ),
            percent_car_free=B08201_002/B08201_001
  )

IN <- geo.make(state = "IN",county= c("Jasper","Lake","Newton","Porter"),tract = "*")

IN_CAR_FREE <- acs.fetch(geography = IN,endyear = 2022,table.number="B08201")

IN_CAR_FREE_df <- data.frame(cbind(data.frame(IN_CAR_FREE@geography), data.frame(IN_CAR_FREE@estimate))) %>% 
  summarize(NAME,
            GEOID = paste0(IN_CAR_FREE@geography$state,
                           str_pad(IN_CAR_FREE@geography$county,
                                   width=3,
                                   side="left",
                                   pad="0"),
                           IN_CAR_FREE@geography$tract
            ),
            percent_car_free=B08201_002/B08201_001
  )

IL_IN_CAR_FREE_df <- rbind(IL_CAR_FREE_df,IN_CAR_FREE_df)

IL_SHAPE <- tracts(state = "IL",county= c("Cook County","Lake County","Kendall","Kane","DeKalb","McHenry","Grundy","DuPage", "Will County"))
IN_SHAPE <- tracts(state = "IN",county= c("Jasper","Lake","Newton","Porter"))

IL_IN_SHAPE <- rbind(IL_SHAPE, IN_SHAPE) %>%
  left_join(., IL_IN_CAR_FREE_df, by = "GEOID") %>% 
  select(geometry, percent_car_free) %>%
  st_make_valid()

# Load water area data
IL_IN_WATER <- rbind(area_water("IL", c("Cook County","Lake County","Kendall","Kane","DeKalb","McHenry","Grundy","DuPage", "Will County")), area_water("IN", c("Jasper","Lake","Newton","Porter"))) %>%
  st_make_valid()

# Simplify geometries to avoid issues with st_union
#DC_VA_SHAPE <- st_simplify(DC_VA_SHAPE, dTolerance = 0.001)
#DC_VA_WATER <- st_simplify(DC_VA_WATER, dTolerance = 0.001)

IL_IN_unioned_shapes <- st_union(IL_IN_SHAPE$geometry) %>% st_make_valid()
IL_IN_unioned_water <- st_union(IL_IN_SHAPE$geometry) %>% st_make_valid()

IL_IN_combined_areas <- st_union(IL_IN_unioned_shapes, IL_IN_unioned_water) %>% st_make_valid()

IL_COUNTY_SHAPE <- counties(state="IL",cb = FALSE) %>%
  filter(NAME %in% c("Cook County","Lake County","Kendall","Kane","DeKalb","McHenry","Grundy","DuPage", "Will County"))
IN_COUNTY_SHAPE <- counties(state="IN",cb = FALSE) %>%
  filter(NAME %in% c("Jasper","Lake","Newton","Porter"))


IL_IN_COUNTY_SHAPES <- rbind(IL_COUNTY_SHAPE,IN_COUNTY_SHAPE)

IL_IN_COUNTY_SHAPES <- st_union(IL_IN_COUNTY_SHAPES$geometry) %>%
  st_make_valid()

IL_IN_precise_polygon <- st_convex_hull(st_union(st_combine(IL_IN_combined_areas))) %>% st_make_valid()
IL_IN_refined_bounding_polygon <- st_intersection(IL_IN_precise_polygon, IL_IN_COUNTY_SHAPES) %>% st_make_valid()
IL_IN_non_tract_areas <- st_difference(IL_IN_refined_bounding_polygon, IL_IN_combined_areas)

IL_IN_SHAPE <- IL_IN_SHAPE %>%
  st_make_valid() %>%
  erase_water()

CHICAGO_OUTLINE <- places(state="IL",cb = TRUE) %>%
  filter(NAME == "Chicago")

temp_zip <- tempfile(fileext = ".zip")
download.file("https://github.com/Miles-byte/Apricitas/raw/main/City%20Mapping/CTA_RAIL.zip", temp_zip, mode = "wb")
unzip_dir <- tempfile()
unzip(temp_zip, exdir = unzip_dir)

CTA_L_LINES <- file.path(unzip_dir) %>%
  st_read() %>%
  mutate(LEGEND = if_else(ALT_LEGEND == "PKGR", "GR", LEGEND)) %>%
  mutate(LEGEND = factor(LEGEND, levels = c("ML","GR","BR","BL","PK","OR","RD","PR","YL")))

temp_zip <- tempfile(fileext = ".zip")
download.file("https://github.com/Miles-byte/Apricitas/raw/main/City%20Mapping/CTA_STATIONS.zip", temp_zip, mode = "wb")
unzip_dir <- tempfile()
unzip(temp_zip, exdir = unzip_dir)

CTA_L_STATIONS <- file.path(unzip_dir) %>%
  st_read()


CHICAGO_CAR_FREE_MAP_ZOOMED <- ggplot() + 
  geom_sf(data = IL_IN_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = IL_IN_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = IL_IN_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = CHICAGO_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  geom_sf(data = CTA_L_LINES, aes(color = LEGEND), lwd = 1.25) +
  scale_color_manual(values = c("#734200","#01ad4e","#734200","#009cde","#f46ca9","#f77331","#e71831","#482b93","#fff000")) +
  geom_sf(data = CTA_L_STATIONS, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "turbo",name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Car-Free Households in Chicago") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-87.3,-88)) +
  scale_y_continuous(limits = c(41.64,42.075)) +
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

ggsave(dpi = "retina",plot = CHICAGO_CAR_FREE_MAP_ZOOMED, "Chicago Car Free Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHICAGO_CAR_FREE_MAP_NO_LINES <- ggplot() + 
  geom_sf(data = IL_IN_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = IL_IN_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = IL_IN_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = CHICAGO_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  #geom_sf(data = CTA_L_LINES, aes(color = LEGEND), lwd = 1.25) +
  #scale_color_manual(values = c("#734200","#01ad4e","#734200","#009cde","#f46ca9","#f77331","#e71831","#482b93","#fff000")) +
  #geom_sf(data = CTA_L_STATIONS, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "turbo",name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Car-Free Households in Chicago") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-87.3,-88)) +
  scale_y_continuous(limits = c(41.64,42.075)) +
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

ggsave(dpi = "retina",plot = CHICAGO_CAR_FREE_MAP_NO_LINES, "Chicago Car Free Map No Lines.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#

MA <- geo.make(state = "MA",county= c("Essex","Middlesex","Norfolk","Plymouth","Suffolk"),tract = "*")

MA_CAR_FREE <- acs.fetch(geography = MA,endyear = 2022,table.number="B08201")

MA_CAR_FREE_df <- data.frame(cbind(data.frame(MA_CAR_FREE@geography), data.frame(MA_CAR_FREE@estimate))) %>% 
  summarize(NAME,
            GEOID = paste0(MA_CAR_FREE@geography$state,
                           str_pad(MA_CAR_FREE@geography$county,
                                   width=3,
                                   side="left",
                                   pad="0"),
                           MA_CAR_FREE@geography$tract
            ),
            percent_car_free=B08201_002/B08201_001
  )

# IN <- geo.make(state = "IN",county= c("Jasper","Lake","Newton","Porter"),tract = "*")
# 
# IN_CAR_FREE <- acs.fetch(geography = IN,endyear = 2022,table.number="B08201")
# 
# IN_CAR_FREE_df <- data.frame(cbind(data.frame(IN_CAR_FREE@geography), data.frame(IN_CAR_FREE@estimate))) %>% 
#   summarize(NAME,
#             GEOID = paste0(IN_CAR_FREE@geography$state,
#                            str_pad(IN_CAR_FREE@geography$county,
#                                    width=3,
#                                    side="left",
#                                    pad="0"),
#                            IN_CAR_FREE@geography$tract
#             ),
#             percent_car_free=B08201_002/B08201_001
#   )
# 
# IL_IN_CAR_FREE_df <- rbind(IL_CAR_FREE_df,IN_CAR_FREE_df)

MA_SHAPE <- tracts(state = "MA",county= c("Essex","Middlesex","Norfolk","Plymouth","Suffolk"))

MA_SHAPE <- MA_SHAPE %>%
  left_join(., MA_CAR_FREE_df, by = "GEOID") %>% 
  select(geometry, percent_car_free) %>%
  st_make_valid()

# Load water area data
MA_WATER <- area_water("MA", c("Essex","Middlesex","Norfolk","Plymouth","Suffolk")) %>%
  st_make_valid()

# Simplify geometries to avoid issues with st_union
#DC_VA_SHAPE <- st_simplify(DC_VA_SHAPE, dTolerance = 0.001)
#DC_VA_WATER <- st_simplify(DC_VA_WATER, dTolerance = 0.001)

MA_unioned_shapes <- st_union(MA_SHAPE$geometry) %>% st_make_valid()
MA_unioned_water <- st_union(MA_SHAPE$geometry) %>% st_make_valid()

MA_combined_areas <- st_union(MA_unioned_shapes, MA_unioned_water) %>% st_make_valid()

MA_COUNTY_SHAPE <- counties(state="MA",cb = FALSE) %>%
  filter(NAME %in% c("Essex","Middlesex","Norfolk","Plymouth","Suffolk"))
# IN_COUNTY_SHAPE <- counties(state="IN",cb = FALSE) %>%
#   filter(NAME %in% c("Jasper","Lake","Newton","Porter"))


#MA_COUNTY_SHAPES <- rbind(IL_COUNTY_SHAPE,IN_COUNTY_SHAPE)

MA_COUNTY_SHAPES <- st_union(MA_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

MA_precise_polygon <- st_convex_hull(st_union(st_combine(MA_combined_areas))) %>% st_make_valid()
MA_refined_bounding_polygon <- st_intersection(MA_precise_polygon, MA_COUNTY_SHAPES) %>% st_make_valid()
MA_non_tract_areas <- st_difference(MA_refined_bounding_polygon, MA_combined_areas)

MA_SHAPE <- MA_SHAPE %>%
  st_make_valid() %>%
  erase_water() %>%
  mutate(percent_car_free = if_else(percent_car_free > 0.75, 0.75, percent_car_free))

BOSTON_OUTLINE <- places(state="MA",cb = TRUE) %>%
  filter(NAME == "Boston")

FROM: #https://www.mass.gov/info-details/massgis-data-mbta-rapid-transit

temp_zip <- tempfile(fileext = ".zip")
download.file("https://github.com/Miles-byte/Apricitas/raw/main/City%20Mapping/mbta_rapid_transit.zip", temp_zip, mode = "wb")
unzip_dir <- tempfile()
unzip(temp_zip, exdir = unzip_dir)

MBTA_LINES <- file.path(unzip_dir) %>%
  st_read()

temp_zip <- tempfile(fileext = ".zip")
download.file("https://github.com/Miles-byte/Apricitas/raw/main/City%20Mapping/CTA_STATIONS.zip", temp_zip, mode = "wb")
unzip_dir <- tempfile()
unzip(temp_zip, exdir = unzip_dir)

MBTA_STATIONS <- file.path(unzip_dir) %>%
  st_read(layer = "MBTA_NODE")

BOSTON_CAR_FREE_MAP <- ggplot() + 
  geom_sf(data = MA_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = MA_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = MA_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = BOSTON_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  geom_sf(data = MBTA_LINES, aes(color = LINE), lwd = 1.25) +
  scale_color_manual(values = c("#2f4478","#3b7845","#cb8136","#b12f2f","#757e7f")) +
  geom_sf(data = MBTA_STATIONS, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "turbo",name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75), limits = c(0,0.75), labels = c("0%","25%","50%","75+%")) +
  ggtitle("Car-Free Households in Boston") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-70.85,-71.25)) +
  scale_y_continuous(limits = c(42.22,42.43)) +
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

ggsave(dpi = "retina",plot = BOSTON_CAR_FREE_MAP, "Boston Car Free Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BOSTON_CAR_FREE_MAP_NO_LINES <- ggplot() + 
  geom_sf(data = MA_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = MA_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = MA_SHAPE, aes(fill = percent_car_free), color = NA) +
  geom_sf(data = BOSTON_OUTLINE, fill = NA, color = "white", lwd = 0.65) +
  #geom_sf(data = MBTA_LINES, aes(color = LINE), lwd = 1.25) +
  #scale_color_manual(values = c("#2f4478","#3b7845","#cb8136","#b12f2f","#757e7f")) +
  #geom_sf(data = MBTA_STATIONS, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "turbo",name= "Households\n% Car-Free", breaks = c(0,.25,.5,.75), limits = c(0,0.75), labels = c("0%","25%","50%","75+%")) +
  ggtitle("Car-Free Households in Boston") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates") +
  theme_apricitas + 
  scale_x_continuous(limits = c(-70.85,-71.25)) +
  scale_y_continuous(limits = c(42.22,42.43)) +
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

ggsave(dpi = "retina",plot = BOSTON_CAR_FREE_MAP_NO_LINES, "Boston Car Free Map No Lines.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

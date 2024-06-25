pacman::p_load(ggrepel,dots,ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
p_load("acs","leaflet","tigris","dplyr","stringr","lwgeom")

api.key.install(key=Sys.getenv("CENSUS_KEY"))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"


# DMV <- geo.make(state=c("DC","VA","MD"),county = "*",tract = "*") %>%
#   filter(county.name %in% c("District of Columbia","Arlington County","Alexandria city","Loudoun County","Stafford County","Manassas Park city","Prince William County","Falls Church city","Frederick County","Fredericksburg city","Spotsylvania County","Charles County","Manassas city","Culpeper County","Prince George's County","Fauquier County","Montgomery County","Warren County","Jefferson County","Faifax city","Fairfax County","Clarke County","Calvert County","Madison County","Rappahannock County"))
# 
# DC_ARL_ALX <- geo.lookup(state=c("DC","VA","MD"),county = "*") %>%
#   filter(county.name %in% c("District of Columbia","Arlington County","Alexandria city"))

VA <- geo.make(state = "VA",county= c(013,510),tract = "*")

VA_INCOME <- acs.fetch(geography = VA,endyear = 2022,table.number="B19013")

VA_INCOME_df <- data.frame( GEOID = paste0(VA_INCOME@geography$state,
                                                        str_pad(VA_INCOME@geography$county,
                                                                width=3,
                                                                side="left",
                                                                pad="0"),
                                            VA_INCOME@geography$tract
),
median_income = as.numeric(VA_INCOME@estimate),
row.names=NULL)

DC <- geo.make(state = "DC",county = "*",tract = "*")

DC_INCOME <- acs.fetch(geography = DC,endyear = 2022,table.number="B19013")

DC_INCOME_df <- data.frame( GEOID = paste0(DC_INCOME@geography$state,
                                            str_pad(DC_INCOME@geography$county,
                                                    width=3,
                                                    side="left",
                                                    pad="0"),
                                           DC_INCOME@geography$tract
),
median_income = as.numeric(DC_INCOME@estimate),
row.names=NULL)

DC_VA_INCOME <- rbind(DC_INCOME_df,VA_INCOME_df)

DC_SHAPE <- tracts(state="DC",county= c(001))
VA_SHAPE <- tracts(state="VA",county= c(013,510))
# DC_SHAPE <- DC_SHAPE %>% 
#   left_join(., DC_INCOME_df,by="GEOID") %>% 
#   filter(median_income>=0) %>%
#   select(geometry,median_income)

DC_VA_SHAPE <- rbind(DC_SHAPE, VA_SHAPE) %>% 
  left_join(., DC_VA_INCOME, by = "GEOID") %>% 
  filter(median_income >= 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

# Load water area data
DC_VA_WATER <- rbind(area_water("DC", "01"), area_water("VA", "Arlington"), area_water("VA", "Alexandria")) %>%
  st_make_valid()

# Simplify geometries to avoid issues with st_union
#DC_VA_SHAPE <- st_simplify(DC_VA_SHAPE, dTolerance = 0.001)
#DC_VA_WATER <- st_simplify(DC_VA_WATER, dTolerance = 0.001)

DC_VA_unioned_shapes <- st_union(DC_VA_SHAPE$geometry) %>% st_make_valid()
DC_VA_unioned_water <- st_union(DC_VA_WATER$geometry) %>% st_make_valid()

DC_VA_combined_areas <- st_union(DC_VA_unioned_shapes, DC_VA_unioned_water) %>% st_make_valid()

DC_COUNTY_SHAPE <- counties(state="DC",cb = FALSE)
VA_COUNTY_SHAPE <- counties(state="VA",cb = FALSE) %>%
  filter(NAME %in% c("Alexandria","Arlington"))

DC_VA_COUNTY_SHAPES <- rbind(DC_COUNTY_SHAPE,VA_COUNTY_SHAPE) #%>%

DC_VA_COUNTY_SHAPES <- st_union(DC_VA_COUNTY_SHAPES$geometry) %>%
  st_make_valid()

DC_VA_precise_polygon <- st_convex_hull(st_union(st_combine(DC_VA_combined_areas)))
DC_VA_refined_bounding_polygon <- st_intersection(DC_VA_precise_polygon, DC_VA_COUNTY_SHAPES)
DC_VA_non_tract_areas <- st_difference(DC_VA_refined_bounding_polygon, DC_VA_combined_areas)

DC_VA_SHAPE <- DC_VA_SHAPE %>%
  st_make_valid() %>%
  erase_water()

DC_INCOME_MAP <- ggplot() + 
  geom_sf(data = DC_VA_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = DC_VA_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = DC_VA_SHAPE, aes(fill = median_income/1000), color = NA) +
  scale_fill_viridis_c(name= "Median Household Income", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  ggtitle("Income in DC, Arlington, & Alexandria") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = "Median Income is Highest in NW DC and Arlington While Lowest Across the Anacostia River") +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = DC_INCOME_MAP, "DC Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Chicago

IL <- geo.make(state = "IL",county= "Cook",tract = "*")

IL_INCOME <- acs.fetch(geography = IL,endyear = 2022,table.number="B19013")

IL_INCOME_df <- data.frame( GEOID = paste0(IL_INCOME@geography$state,
                                           str_pad(IL_INCOME@geography$county,
                                                   width=3,
                                                   side="left",
                                                   pad="0"),
                                           IL_INCOME@geography$tract
),
median_income = as.numeric(IL_INCOME@estimate),
row.names=NULL)

IL_SHAPE <- tracts(state="IL",county= "Cook")

IL_SHAPE <- IL_SHAPE %>% 
  left_join(., IL_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

IL_WATER <- area_water("IL", "Cook") %>%
  st_make_valid()

IL_unioned_shapes <- st_union(IL_SHAPE$geometry) %>% st_make_valid()
IL_unioned_water <- st_union(IL_WATER$geometry) %>% st_make_valid()

IL_combined_areas <- st_union(IL_unioned_shapes, IL_unioned_water) %>% st_make_valid()

IL_COUNTY_SHAPE <- counties(state="IL",cb = FALSE) %>%
  filter(NAME == "Cook")

IL_COUNTY_SHAPE <- st_union(IL_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

IL_precise_polygon <- st_convex_hull(st_union(st_combine(IL_combined_areas)))
IL_refined_bounding_polygon <- st_intersection(IL_precise_polygon, IL_COUNTY_SHAPE)
IL_non_tract_areas <- st_difference(IL_refined_bounding_polygon, IL_combined_areas)

IL_SHAPE <- IL_SHAPE %>%
  erase_water()

IL_INCOME_MAP <- ggplot() + 
  geom_sf(data = IL_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  #geom_sf(data = IL_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = IL_SHAPE, aes(fill = median_income/1000), color = NA) +
  scale_fill_viridis_c(name= "Median Household Income", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  ggtitle("Income in Cook County") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = "Median Income is Highest in Chicago's North Side and Lowest in the South Side") +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = IL_INCOME_MAP, "IL Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#New York

NY <- geo.make(state = "NY",county= c("New York", "Kings", "Queens", "Bronx", "Richmond"),tract = "*")

NY_INCOME <- acs.fetch(geography = NY,endyear = 2022,table.number="B19013")

NY_INCOME_df <- data.frame(GEOID = paste0(NY_INCOME@geography$state,
                                           str_pad(NY_INCOME@geography$county,
                                                   width=3,
                                                   side="left",
                                                   pad="0"),
                                          NY_INCOME@geography$tract
),
median_income = as.numeric(NY_INCOME@estimate),
row.names=NULL)

NY_SHAPE <- tracts(state="NY",county = c("New York", "Kings", "Queens", "Bronx", "Richmond"))

NY_SHAPE <- NY_SHAPE %>% 
  left_join(., NY_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

#HUDSON COUNTY
NJ <- geo.make(state = "NJ",county= "Hudson",tract = "*")

NJ_INCOME <- acs.fetch(geography = NJ,endyear = 2022,table.number="B19013")

NJ_INCOME_df <- data.frame(GEOID = paste0(NJ_INCOME@geography$state,
                                          str_pad(NJ_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          NJ_INCOME@geography$tract
),
median_income = as.numeric(NJ_INCOME@estimate),
row.names=NULL)

NJ_SHAPE <- tracts(state="NJ",county = "Hudson")

NJ_SHAPE <- NJ_SHAPE %>% 
  left_join(., NJ_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

NY_SHAPE <- rbind(NY_SHAPE,NJ_SHAPE)

NY_WATER <- area_water(state="NY",county = c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
  rbind(., area_water(state = "NJ", county = "Hudson")) %>%
  st_make_valid()

NY_unioned_shapes <- st_union(NY_SHAPE$geometry) %>% st_make_valid()
NY_unioned_water <- st_union(NY_WATER$geometry) %>% st_make_valid()

NY_combined_areas <- st_union(NY_unioned_shapes, NY_unioned_water) %>% st_make_valid()

NY_COUNTY_SHAPE <- counties(state="NY",cb = FALSE) %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
  rbind(counties(state = "NJ", cb= FALSE)) %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond","Hudson"))
  

NY_COUNTY_SHAPE <- st_union(NY_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

NY_precise_polygon <- st_convex_hull(st_union(st_combine(NY_combined_areas)))
NY_refined_bounding_polygon <- st_intersection(NY_precise_polygon, NY_COUNTY_SHAPE) %>% st_make_valid()
NY_non_tract_areas <- st_difference(NY_refined_bounding_polygon, NY_combined_areas)

NY_SHAPE <- NY_SHAPE %>%
  erase_water()

NY_INCOME_MAP <- ggplot() + 
  geom_sf(data = NY_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = NY_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = NY_SHAPE, aes(fill = median_income/1000), color = NA) +
  scale_fill_viridis_c(name= "Median Household Income", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  ggtitle("Income in NYC & Hudson County") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = "Income is Highest in Lower Manhattan & the Upper East Side, Lowest in East Harlem & the Bronx") +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = NY_INCOME_MAP, "NY Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



OH <- geo.make(state = "OH",county= "Lucas",tract = "*")

OH_INCOME <- acs.fetch(geography = OH,endyear = 2022,table.number="B19013")

OH_INCOME_df <- data.frame(GEOID = paste0(OH_INCOME@geography$state,
                                          str_pad(OH_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          OH_INCOME@geography$tract
),
median_income = as.numeric(OH_INCOME@estimate),
row.names=NULL)

OH_SHAPE <- tracts(state="OH",county = "Lucas")

OH_SHAPE <- OH_SHAPE %>% 
  left_join(., OH_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

OH_WATER <- area_water(state="OH",county = "Lucas") %>%
  st_make_valid()

OH_unioned_shapes <- st_union(OH_SHAPE$geometry) %>% st_make_valid()
OH_unioned_water <- st_union(OH_WATER$geometry) %>% st_make_valid()

OH_combined_areas <- st_union(OH_unioned_shapes, OH_unioned_water) %>% st_make_valid()

OH_COUNTY_SHAPE <- counties(state="OH",cb = TRUE) %>%
  filter(NAME == "Lucas")

OH_COUNTY_SHAPE <- st_union(OH_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

OH_precise_polygon <- st_convex_hull(st_union(st_combine(OH_combined_areas)))
OH_refined_bounding_polygon <- st_intersection(OH_precise_polygon, OH_COUNTY_SHAPE) %>% st_make_valid()
OH_non_tract_areas <- st_difference(OH_refined_bounding_polygon, OH_combined_areas)

#NOTE THIS PART WHERE I FIX THE WATER RELIES ON COUNTY SHAPE USING CARTOGRAPHIC BOUNDARIES
OH_WATER <- st_difference(OH_COUNTY_SHAPE, OH_WATER)

OH_SHAPE <- OH_SHAPE %>%
  erase_water()

OH_INCOME_MAP <- ggplot() + 
  geom_sf(data = OH_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = OH_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = OH_SHAPE, aes(fill = median_income/1000), color = NA) +
  scale_fill_viridis_c(name= "Median Household Income", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  ggtitle("Income in Lucas County, Ohio") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = OH_INCOME_MAP, "OH Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#SF
BAY <- geo.make(state = "CA",county= c("San Francisco","Alameda","Santa Clara","San Mateo", "Marin", "Sonoma","Napa","Solano","Contra Costa"),tract = "*")

BAY_INCOME <- acs.fetch(geography = BAY,endyear = 2022,table.number="B19013")

BAY_INCOME_df <- data.frame(GEOID = paste0(BAY_INCOME@geography$state,
                                          str_pad(BAY_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          BAY_INCOME@geography$tract
),
median_income = as.numeric(BAY_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

BAY_SHAPE <- tracts(state="CA",county = c("San Francisco","Alameda","Santa Clara","San Mateo", "Marin", "Sonoma","Napa","Solano","Contra Costa"))

BAY_SHAPE <- BAY_SHAPE %>% 
  left_join(., BAY_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

BAY_WATER <- area_water(state="CA",county = c("San Francisco","Alameda","Santa Clara","San Mateo", "Marin", "Sonoma","Napa","Solano","Contra Costa")) %>%
  st_make_valid()

BAY_unioned_shapes <- st_union(BAY_SHAPE$geometry) %>% st_make_valid()
BAY_unioned_water <- st_union(BAY_WATER$geometry) %>% st_make_valid()

BAY_combined_areas <- st_union(BAY_unioned_shapes, BAY_unioned_water) %>% st_make_valid()

BAY_COUNTY_SHAPE <- counties(state="CA",cb = FALSE) %>%
  filter(NAME %in% c("San Francisco","Alameda","Santa Clara","San Mateo", "Marin", "Sonoma","Napa","Solano","Contra Costa"))

BAY_COUNTY_SHAPE <- st_union(BAY_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

BAY_precise_polygon <- st_convex_hull(st_union(st_combine(BAY_combined_areas)))
BAY_refined_bounding_polygon <- st_intersection(BAY_precise_polygon, BAY_COUNTY_SHAPE) %>% st_make_valid()
BAY_non_tract_areas <- st_difference(BAY_refined_bounding_polygon, BAY_combined_areas)

BAY_SHAPE <- BAY_SHAPE %>%
  erase_water()

BAY_INCOME_MAP <- ggplot() + 
  geom_sf(data = BAY_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = BAY_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = BAY_SHAPE, aes(fill = median_income/1000), color = NA) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  ggtitle("Income in The Bay Area") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = "Median Income is Highest in Chicago's North Side and Lowest in the South Side") +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = BAY_INCOME_MAP, "Bay Area Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#SEATTLE
WA <- geo.make(state = "WA",county= c("King","Pierce","Snohomish","Kitsap"),tract = "*")

WA_INCOME <- acs.fetch(geography = WA,endyear = 2022,table.number="B19013")

WA_INCOME_df <- data.frame(GEOID = paste0(WA_INCOME@geography$state,
                                          str_pad(WA_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          WA_INCOME@geography$tract
),
median_income = as.numeric(WA_INCOME@estimate),
row.names=NULL)

WA_SHAPE <- tracts(state="WA",county = c("King","Pierce","Snohomish","Kitsap"))

WA_SHAPE <- WA_SHAPE %>% 
  left_join(., WA_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

WA_WATER <- area_water(state="WA",county = c("King","Pierce","Snohomish","Kitsap")) %>%
  st_make_valid()

WA_unioned_shapes <- st_union(WA_SHAPE$geometry) %>% st_make_valid()
WA_unioned_water <- st_union(WA_WATER$geometry) %>% st_make_valid()

WA_combined_areas <- st_union(WA_unioned_shapes, WA_unioned_water) %>% st_make_valid()

WA_COUNTY_SHAPE <- counties(state="WA",cb = FALSE) %>%
  filter(NAME %in% c("King","Pierce","Snohomish","Kitsap"))

WA_COUNTY_SHAPE <- st_union(WA_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

WA_precise_polygon <- st_convex_hull(st_union(st_combine(WA_combined_areas)))
WA_refined_bounding_polygon <- st_intersection(WA_precise_polygon, WA_COUNTY_SHAPE) %>% st_make_valid()
WA_non_tract_areas <- st_difference(WA_refined_bounding_polygon, WA_combined_areas)

WA_WATER <- st_difference(WA_COUNTY_SHAPE, WA_WATER)

WA_SHAPE <- WA_SHAPE %>%
  erase_water()

SEATTLE_SHAPE <- places(state="WA",cb = TRUE) %>%
  filter(NAME == "Seattle")


WA_INCOME_MAP <- ggplot() + 
  geom_sf(data = WA_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = WA_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = WA_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = SEATTLE_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-122.6,-122)) +
  scale_y_continuous(limits = c(47.45,47.75)) +
  ggtitle("Income in & Around Seattle, Washington") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = WA_INCOME_MAP, "WA Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#WISCONSIN

WI <- geo.make(state = "WI",county= c("Dane","Columbia","Sauk","Iowa","Lafayette","Green County","Rock","Jefferson"),tract = "*")

WI_INCOME <- acs.fetch(geography = WI,endyear = 2022,table.number="B19013")

WI_INCOME_df <- data.frame(GEOID = paste0(WI_INCOME@geography$state,
                                          str_pad(WI_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          WI_INCOME@geography$tract
),
median_income = as.numeric(WI_INCOME@estimate),
row.names=NULL)

WI_SHAPE <- tracts(state="WI",county = c("Dane","Columbia","Sauk","Iowa","Lafayette","Green County","Rock","Jefferson"))

WI_SHAPE <- WI_SHAPE %>% 
  left_join(., WI_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

WI_WATER <- area_water(state="WI",county = c("Dane","Columbia","Sauk","Iowa","Lafayette","Green County","Rock","Jefferson")) %>%
  st_make_valid()

WI_unioned_shapes <- st_union(WI_SHAPE$geometry) %>% st_make_valid()
WI_unioned_water <- st_union(WI_WATER$geometry) %>% st_make_valid()

WI_combined_areas <- st_union(WI_unioned_shapes, WI_unioned_water) %>% st_make_valid()

WI_COUNTY_SHAPE <- counties(state="WI",cb = FALSE) %>%
  filter(NAME %in% c("Dane","Columbia","Sauk","Iowa","Lafayette","Green County","Rock","Jefferson"))

WI_COUNTY_SHAPE <- st_union(WI_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

WI_precise_polygon <- st_convex_hull(st_union(st_combine(WI_combined_areas)))
WI_refined_bounding_polygon <- st_intersection(WI_precise_polygon, WI_COUNTY_SHAPE) %>% st_make_valid()
WI_non_tract_areas <- st_difference(WI_refined_bounding_polygon, WI_combined_areas)

WI_WATER <- st_difference(WI_COUNTY_SHAPE, WI_WATER)

WI_SHAPE <- WI_SHAPE %>%
  erase_water()

MADISON_SHAPE <- places(state="WI",cb = TRUE) %>%
  filter(NAME == "Madison")


WI_INCOME_MAP <- ggplot() + 
  geom_sf(data = WI_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = WI_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = WI_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = MADISON_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-89.65,-89.1)) +
  scale_y_continuous(limits = c(42.95,43.25)) +
  ggtitle("Income in & Around Madison, Wisconsin") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = WI_INCOME_MAP, "WI Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#PORTLAND

OR <- geo.make(state = "OR",county= c("Clackamas","Columbia","Multnomah","Washington","Yamhill"),tract = "*")

OR_INCOME <- acs.fetch(geography = OR,endyear = 2022,table.number="B19013")

OR_INCOME_df <- data.frame(GEOID = paste0(OR_INCOME@geography$state,
                                          str_pad(OR_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          OR_INCOME@geography$tract
),
median_income = as.numeric(OR_INCOME@estimate),
row.names=NULL)

OR_WA <- geo.make(state = "WA",county= c("Clark","Skamania"),tract = "*")

OR_WA_INCOME <- acs.fetch(geography = OR_WA,endyear = 2022,table.number="B19013")

OR_WA_INCOME_df <- data.frame(GEOID = paste0(OR_WA_INCOME@geography$state,
                                          str_pad(OR_WA_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          OR_WA_INCOME@geography$tract
),
median_income = as.numeric(OR_WA_INCOME@estimate),
row.names=NULL)

OR_WA_INCOME_df <- rbind(OR_INCOME_df,OR_WA_INCOME_df)

OR_WA_SHAPE <- tracts(state="OR",county = c("Clackamas","Columbia","Multnomah","Washington","Yamhill")) %>%
  rbind(.,tracts(state="WA",county = c("Clark","Skamania")))

OR_WA_SHAPE <- OR_WA_SHAPE %>% 
  left_join(., OR_WA_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

OR_WA_WATER <- area_water(state="OR",county = c("Clackamas","Columbia","Multnomah","Washington","Yamhill")) %>%
  rbind(area_water(state = "WA", county = c("Clark","Skamania"))) %>%
  st_make_valid()

OR_WA_unioned_shapes <- st_union(OR_WA_SHAPE$geometry) %>% st_make_valid()
OR_WA_unioned_water <- st_union(OR_WA_WATER$geometry) %>% st_make_valid()

OR_WA_combined_areas <- st_union(OR_WA_unioned_shapes, OR_WA_unioned_water) %>% st_make_valid()

OR_WA_COUNTY_SHAPE <- counties(state="OR",cb = TRUE) %>%
  rbind(.,counties(state="WA",cb = TRUE)) %>%
  filter(NAME %in% c("Clackamas","Columbia","Multnomah","Washington","Clark","Skamania"))

OR_WA_COUNTY_SHAPE <- st_union(OR_WA_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

OR_WA_precise_polygon <- st_convex_hull(st_union(st_combine(OR_WA_combined_areas)))
OR_WA_refined_bounding_polygon <- st_intersection(OR_WA_precise_polygon, OR_WA_COUNTY_SHAPE) %>% st_make_valid()
OR_WA_non_tract_areas <- st_difference(OR_WA_refined_bounding_polygon, OR_WA_combined_areas)

OR_WA_WATER <- st_difference(OR_WA_COUNTY_SHAPE, OR_WA_WATER)

OR_WA_SHAPE <- OR_WA_SHAPE %>%
  erase_water()

PORTLAND_SHAPE <- places(state="OR",cb = TRUE) %>%
  filter(NAME == "Portland")

OR_WA_INCOME_MAP <- ggplot() + 
  geom_sf(data = OR_WA_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = OR_WA_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = OR_WA_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = PORTLAND_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-123,-122.3)) +
  scale_y_continuous(limits = c(45.35,45.75)) +
  ggtitle("Income in & Around Portland, Oregon") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = OR_WA_INCOME_MAP, "Portland Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#MN

MN <- geo.make(state = "MN",county= c("Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Isanti", "Le Sueur", "Mille Lacs", "Ramsey", "Scott", "Sherburne", "Washington", "Wright"),tract = "*")

MN_INCOME <- acs.fetch(geography = MN,endyear = 2022,table.number="B19013")

MN_INCOME_df <- data.frame(GEOID = paste0(MN_INCOME@geography$state,
                                          str_pad(MN_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          MN_INCOME@geography$tract
),
median_income = as.numeric(MN_INCOME@estimate),
row.names=NULL)

MN_SHAPE <- tracts(state="MN",county = c("Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Isanti", "Le Sueur", "Mille Lacs", "Ramsey", "Scott", "Sherburne", "Washington", "Wright"))

MN_SHAPE <- MN_SHAPE %>% 
  left_join(., MN_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

MN_WATER <- area_water(state="MN",county = c("Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Isanti", "Le Sueur", "Mille Lacs", "Ramsey", "Scott", "Sherburne", "Washington", "Wright")) %>%
  st_make_valid()

MN_unioned_shapes <- st_union(MN_SHAPE$geometry) %>% st_make_valid()
MN_unioned_water <- st_union(MN_WATER$geometry) %>% st_make_valid()

MN_combined_areas <- st_union(MN_unioned_shapes, MN_unioned_water) %>% st_make_valid()

MN_COUNTY_SHAPE <- counties(state="MN",cb = FALSE) %>%
  filter(NAME %in% c("Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Isanti", "Le Sueur", "Mille Lacs", "Ramsey", "Scott", "Sherburne", "Washington", "Wright"))

MN_COUNTY_SHAPE <- st_union(MN_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

MN_precise_polygon <- st_convex_hull(st_union(st_combine(MN_combined_areas)))
MN_refined_bounding_polygon <- st_intersection(MN_precise_polygon, MN_COUNTY_SHAPE) %>% st_make_valid()
MN_non_tract_areas <- st_difference(MN_refined_bounding_polygon, MN_combined_areas)

#MN_WATER <- st_difference(MN_COUNTY_SHAPE, MN_WATER)

MN_SHAPE <- MN_SHAPE %>%
  erase_water()

MINNEAPOLIS_ST_PAUL_SHAPE <- places(state="MN",cb = TRUE) %>%
  filter(NAME %in% c("Minneapolis","St. Paul"))


MN_INCOME_MAP <- ggplot() + 
  geom_sf(data = MN_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = MN_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = MN_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = MINNEAPOLIS_ST_PAUL_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-93.75,-92.86)) +
  scale_y_continuous(limits = c(44.75,45.25)) +
  ggtitle("Income in & Around The Twin Cities, MN") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.3), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = MN_INCOME_MAP, "MN Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Denver

CO <- geo.make(state = "CO",county= c("Denver", "Arapahoe","Jefferson", "Adams", "Douglas", "Broomfield", "Elbert", "Park", "Clear Creek", "Gilpin", "Weld", "Boulder"),tract = "*")

CO_INCOME <- acs.fetch(geography = CO,endyear = 2022,table.number="B19013")

CO_INCOME_df <- data.frame(GEOID = paste0(CO_INCOME@geography$state,
                                          str_pad(CO_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          CO_INCOME@geography$tract
),
median_income = as.numeric(CO_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

CO_SHAPE <- tracts(state="CO",county = c("Denver", "Arapahoe","Jefferson", "Adams", "Douglas", "Broomfield", "Elbert", "Park", "Clear Creek", "Gilpin", "Weld", "Boulder"))

CO_SHAPE <- CO_SHAPE %>% 
  left_join(., CO_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

CO_WATER <- area_water(state="CO",county = c("Denver", "Arapahoe","Jefferson", "Adams", "Douglas", "Broomfield", "Elbert", "Park", "Clear Creek", "Gilpin", "Weld", "Boulder")) %>%
  st_make_valid()

CO_unioned_shapes <- st_union(CO_SHAPE$geometry) %>% st_make_valid()
CO_unioned_water <- st_union(CO_WATER$geometry) %>% st_make_valid()

CO_combined_areas <- st_union(CO_unioned_shapes, CO_unioned_water) %>% st_make_valid()

CO_COUNTY_SHAPE <- counties(state="CO",cb = FALSE) %>%
  filter(NAME %in% c("Denver", "Arapahoe","Jefferson", "Adams", "Douglas", "Broomfield", "Elbert", "Park", "Clear Creek", "Gilpin", "Weld", "Boulder"))

CO_COUNTY_SHAPE <- st_union(CO_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

CO_precise_polygon <- st_convex_hull(st_union(st_combine(CO_combined_areas)))
CO_refined_bounding_polygon <- st_intersection(CO_precise_polygon, CO_COUNTY_SHAPE) %>% st_make_valid()
CO_non_tract_areas <- st_difference(CO_refined_bounding_polygon, CO_combined_areas)

#MN_WATER <- st_difference(MN_COUNTY_SHAPE, MN_WATER)

CO_SHAPE <- CO_SHAPE %>%
  erase_water()

DENVER_SHAPE <- places(state="CO",cb = TRUE) %>%
  filter(NAME == "Denver")


CO_INCOME_MAP <- ggplot() + 
  geom_sf(data = CO_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = CO_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = CO_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = DENVER_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-105.5,-104.4)) +
  scale_y_continuous(limits = c(39.4,40.1)) +
  ggtitle("Income in & Around Denver, Colorado") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.3), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = CO_INCOME_MAP, "CO Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#MI

MI <- geo.make(state = "MI",county= c("Wayne", "Oakland","Macomb", "Livingston", "St. Clair", "Lapeer", "Lenawee", "Washtenaw", "Shiawassee", "Genesee"),tract = "*")

MI_INCOME <- acs.fetch(geography = MI,endyear = 2022,table.number="B19013")

MI_INCOME_df <- data.frame(GEOID = paste0(MI_INCOME@geography$state,
                                          str_pad(MI_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          MI_INCOME@geography$tract
),
median_income = as.numeric(MI_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

MI_SHAPE <- tracts(state="MI",county = c("Wayne", "Oakland","Macomb", "Livingston", "St. Clair", "Lapeer", "Lenawee", "Washtenaw", "Shiawassee", "Genesee"))

MI_SHAPE <- MI_SHAPE %>% 
  left_join(., MI_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

MI_WATER <- area_water(state="MI",county = c("Wayne", "Oakland","Macomb", "Livingston", "St. Clair", "Lapeer","Lenawee", "Washtenaw", "Shiawassee", "Genesee")) %>%
  st_make_valid()

MI_unioned_shapes <- st_union(MI_SHAPE$geometry) %>% st_make_valid()
MI_unioned_water <- st_union(MI_WATER$geometry) %>% st_make_valid()

MI_combined_areas <- st_union(MI_unioned_shapes, MI_unioned_water) %>% st_make_valid()

MI_COUNTY_SHAPE <- counties(state="MI",cb = FALSE) %>%
  filter(NAME %in% c("Wayne", "Oakland","Macomb", "Livingston", "St. Clair", "Lapeer", "Lenawee", "Washtenaw", "Shiawassee", "Genesee"))

MI_COUNTY_SHAPE <- st_union(MI_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

MI_precise_polygon <- st_convex_hull(st_union(st_combine(MI_combined_areas)))
MI_refined_bounding_polygon <- st_intersection(MI_precise_polygon, MI_COUNTY_SHAPE) %>% st_make_valid()
MI_non_tract_areas <- st_difference(MI_refined_bounding_polygon, MI_combined_areas)

#MN_WATER <- st_difference(MN_COUNTY_SHAPE, MN_WATER)

MI_SHAPE <- MI_SHAPE %>%
  erase_water()

DETROIT_SHAPE <- places(state="MI",cb = TRUE) %>%
  filter(NAME == "Detroit")


MI_INCOME_MAP <- ggplot() + 
  geom_sf(data = MI_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = MI_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = MI_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = DETROIT_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-84,-82.75)) +
  scale_y_continuous(limits = c(42.1,42.8)) +
  ggtitle("Income in & Around Detroit, Michigan") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.3), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = MI_INCOME_MAP, "MI Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#ME

ME <- geo.make(state = "ME",county= c("York", "Cumberland","Oxford", "Kennebec", "Lincoln","Androscoggin","Sagadahoc"),tract = "*")

ME_INCOME <- acs.fetch(geography = ME,endyear = 2022,table.number="B19013")

ME_INCOME_df <- data.frame(GEOID = paste0(ME_INCOME@geography$state,
                                          str_pad(ME_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          ME_INCOME@geography$tract
),
median_income = as.numeric(ME_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

ME_SHAPE <- tracts(state="ME",county = c("York", "Cumberland","Oxford", "Kennebec", "Lincoln","Androscoggin","Sagadahoc"))

ME_SHAPE <- ME_SHAPE %>% 
  left_join(., ME_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

ME_WATER <- area_water(state="ME",county = c("York", "Cumberland","Oxford", "Kennebec", "Lincoln","Androscoggin","Sagadahoc")) %>%
  st_make_valid()

ME_unioned_shapes <- st_union(ME_SHAPE$geometry) %>% st_make_valid()
ME_unioned_water <- st_union(ME_WATER$geometry) %>% st_make_valid()

ME_combined_areas <- st_union(ME_unioned_shapes, ME_unioned_water) %>% st_make_valid()

ME_COUNTY_SHAPE <- counties(state="ME",cb = FALSE) %>%
  filter(NAME %in% c("York", "Cumberland","Oxford", "Kennebec", "Lincoln","Androscoggin","Sagadahoc"))

ME_COUNTY_SHAPE <- st_union(ME_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

ME_precise_polygon <- st_convex_hull(st_union(st_combine(ME_combined_areas)))
ME_refined_bounding_polygon <- st_intersection(ME_precise_polygon, ME_COUNTY_SHAPE) %>% st_make_valid()
ME_non_tract_areas <- st_difference(ME_refined_bounding_polygon, ME_combined_areas)

#MN_WATER <- st_difference(MN_COUNTY_SHAPE, MN_WATER)

ME_SHAPE <- ME_SHAPE %>%
  erase_water()

PORTLAND_SHAPE <- places(state="ME",cb = TRUE) %>%
  filter(NAME == "Portland")


ME_INCOME_MAP <- ggplot() + 
  geom_sf(data = ME_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = ME_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = ME_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = PORTLAND_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-70.75,-69.75)) +
  scale_y_continuous(limits = c(43.5,44)) +
  ggtitle("Income in & Around Portland, Maine") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.3), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    )

ggsave(dpi = "retina",plot = ME_INCOME_MAP, "ME Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#MEMPHIS

MEM_TN <- geo.make(state = "TN",county= c("Shelby", "Tipton","Fayette"),tract = "*")
MEM_MS <- geo.make(state = "MS",county= c("DeSoto","Marshall","Tate","Coahoma","Tunica","Benton"),tract = "*")
MEM_AR <- geo.make(state = "AR",county= c("Crittenden", "St. Francis"),tract = "*")

MEM_TN_INCOME <- acs.fetch(geography = MEM_TN,endyear = 2022,table.number="B19013")
MEM_MS_INCOME <- acs.fetch(geography = MEM_MS,endyear = 2022,table.number="B19013")
MEM_AR_INCOME <- acs.fetch(geography = MEM_AR,endyear = 2022,table.number="B19013")

MEM_TN_INCOME_df <- data.frame(GEOID = paste0(MEM_TN_INCOME@geography$state,
                                          str_pad(MEM_TN_INCOME@geography$county,
                                                  width=3,
                                                  side="left",
                                                  pad="0"),
                                          MEM_TN_INCOME@geography$tract
),
median_income = as.numeric(MEM_TN_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

MEM_MS_INCOME_df <- data.frame(GEOID = paste0(MEM_MS_INCOME@geography$state,
                                              str_pad(MEM_MS_INCOME@geography$county,
                                                      width=3,
                                                      side="left",
                                                      pad="0"),
                                              MEM_MS_INCOME@geography$tract
),
median_income = as.numeric(MEM_MS_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

MEM_AR_INCOME_df <- data.frame(GEOID = paste0(MEM_AR_INCOME@geography$state,
                                              str_pad(MEM_AR_INCOME@geography$county,
                                                      width=3,
                                                      side="left",
                                                      pad="0"),
                                              MEM_AR_INCOME@geography$tract
),
median_income = as.numeric(MEM_AR_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

MEM_INCOME_df <- rbind(MEM_TN_INCOME_df,MEM_MS_INCOME_df) %>%
  rbind(.,MEM_AR_INCOME_df)

MEM_SHAPE <- tracts(state="TN",county = c("Shelby", "Tipton","Fayette")) %>%
  rbind(.,tracts(state="MS",county = c("DeSoto", "Marshall","Tate","Coahoma","Tunica","Benton"))) %>%
  rbind(.,tracts(state="AR",county = c("Crittenden", "St. Francis")))

MEM_SHAPE <- MEM_SHAPE %>% 
  left_join(., MEM_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

MEM_WATER <- area_water(state="TN",county = c("Shelby", "Tipton","Fayette")) %>%
  rbind(.,area_water(state="MS",county = c("DeSoto", "Marshall","Tate","Coahoma","Tunica","Benton"))) %>%
  rbind(.,area_water(state="AR",county = c("Crittenden", "St. Francis"))) %>%
  st_make_valid()

MEM_unioned_shapes <- st_union(MEM_SHAPE$geometry) %>% st_make_valid()
MEM_unioned_water <- st_union(MEM_WATER$geometry) %>% st_make_valid()

MEM_combined_areas <- st_union(MEM_unioned_shapes, MEM_unioned_water) %>% st_make_valid()

MEM_COUNTY_SHAPE <- counties(state="TN",cb = FALSE) %>%
  rbind(.,counties(state="AR",cb = FALSE)) %>%
  rbind(.,counties(state="MS",cb = FALSE)) %>%
  filter(NAME %in% c("Shelby", "Tipton","Fayette","DeSoto", "Marshall","Tate","Coahoma","Tunica","Benton","Crittenden", "St. Francis"))

MEM_COUNTY_SHAPE <- st_union(MEM_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

MEM_precise_polygon <- st_convex_hull(st_union(st_combine(MEM_combined_areas)))
MEM_refined_bounding_polygon <- st_intersection(MEM_precise_polygon, MEM_COUNTY_SHAPE) %>% st_make_valid()
MEM_non_tract_areas <- st_difference(MEM_refined_bounding_polygon, MEM_combined_areas)

#MN_WATER <- st_difference(MN_COUNTY_SHAPE, MN_WATER)

MEM_SHAPE <- MEM_SHAPE %>%
  erase_water()

MEMPHIS_SHAPE <- places(state="TN",cb = TRUE) %>%
  filter(NAME == "Memphis")


MEM_INCOME_MAP <- ggplot() + 
  geom_sf(data = MEM_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = MEM_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = MEM_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = MEMPHIS_SHAPE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-90.35,-89.45)) +
  scale_y_continuous(limits = c(34.8,35.38)) +
  ggtitle("Income in & Around Memphis, Tennessee") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = MEM_INCOME_MAP, "MEM Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#STL

STL_IL <- geo.make(state = "IL",county= c("Bond", "Calhoun", "Clinton", "Jersey", "Macoupin", "Madison", "Monroe", "St. Clair"),tract = "*")
STL_MO <- geo.make(state = "MO",county= c("Crawford", "Franklin", "Jefferson", "Lincoln", "St. Charles","Warren", "St. Louis County", "St. Louis city"),tract = "*")

STL_IL_INCOME <- acs.fetch(geography = STL_IL,endyear = 2022,table.number="B19013")
STL_MO_INCOME <- acs.fetch(geography = STL_MO,endyear = 2022,table.number="B19013")

STL_IL_INCOME_df <- data.frame(GEOID = paste0(STL_IL_INCOME@geography$state,
                                              str_pad(STL_IL_INCOME@geography$county,
                                                      width=3,
                                                      side="left",
                                                      pad="0"),
                                              STL_IL_INCOME@geography$tract
),
median_income = as.numeric(STL_IL_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

STL_MO_INCOME_df <- data.frame(GEOID = paste0(STL_MO_INCOME@geography$state,
                                              str_pad(STL_MO_INCOME@geography$county,
                                                      width=3,
                                                      side="left",
                                                      pad="0"),
                                              STL_MO_INCOME@geography$tract
),
median_income = as.numeric(STL_MO_INCOME@estimate),
row.names=NULL) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

STL_INCOME_df <- rbind(STL_IL_INCOME_df,STL_MO_INCOME_df)

STL_SHAPE <- tracts(state="IL",county = c("Bond", "Calhoun", "Clinton", "Jersey", "Macoupin", "Madison", "Monroe", "St. Clair")) %>%
  rbind(.,tracts(state="MO",county = c("Crawford", "Franklin", "Jefferson", "Lincoln", "St. Charles", "St. Louis County","St. Louis city", "Warren")))

STL_SHAPE <- STL_SHAPE %>% 
  left_join(., STL_INCOME_df, by = "GEOID") %>% 
  filter(median_income > 0) %>%
  select(geometry, median_income) %>%
  st_make_valid()

STL_WATER <- area_water(state="IL",county = c("Bond", "Calhoun", "Clinton", "Jersey", "Macoupin", "Madison", "Monroe", "St. Clair")) %>%
  rbind(.,area_water(state="MO",county = c("Crawford", "Franklin", "Jefferson", "Lincoln", "St. Charles", "St. Louis County","St. Louis city", "Warren"))) %>%
  st_make_valid()

STL_unioned_shapes <- st_union(STL_SHAPE$geometry) %>% st_make_valid()
STL_unioned_water <- st_union(STL_WATER$geometry) %>% st_make_valid()

STL_combined_areas <- st_union(STL_unioned_shapes, STL_unioned_water) %>% st_make_valid()

STL_COUNTY_SHAPE <- counties(state="IL",cb = FALSE) %>%
  rbind(.,counties(state="MO",cb = FALSE)) %>%
  filter(NAME %in% c("Bond", "Calhoun", "Clinton", "Jersey", "Macoupin", "Madison", "Monroe", "St. Clair","Crawford", "Franklin", "Jefferson", "Lincoln", "St. Charles", "St. Louis County","St. Louis city", "Warren"))

STL_COUNTY_SHAPE <- st_union(STL_COUNTY_SHAPE$geometry) %>%
  st_make_valid()

STL_precise_polygon <- st_convex_hull(st_union(st_combine(STL_combined_areas)))
STL_refined_bounding_polygon <- st_intersection(STL_precise_polygon, STL_COUNTY_SHAPE) %>% st_make_valid()
STL_non_tract_areas <- st_difference(STL_refined_bounding_polygon, STL_combined_areas)

#MN_WATER <- st_difference(MN_COUNTY_SHAPE, MN_WATER)

STL_SHAPE <- STL_SHAPE %>%
  erase_water()

STL_OUTLINE <- places(state="MO",cb = TRUE) %>%
  filter(NAME == "St. Louis")


STL_INCOME_MAP <- ggplot() + 
  geom_sf(data = STL_WATER, fill = "lightblue", color = 'lightblue') +
  geom_sf(data = STL_non_tract_areas, fill = "grey", color = NA, alpha = 0.5) +
  geom_sf(data = STL_SHAPE, aes(fill = median_income/1000), color = NA) +
  geom_sf(data = STL_OUTLINE, fill = NA, color = "black", lwd = 0.65) +
  scale_fill_viridis_c(name= "Median\nHousehold\nIncome", breaks = c(50,100,150,200,250), labels = c("$50k","$100k","$150k","$200k","$250k+")) +
  scale_x_continuous(limits = c(-90.65,-89.8)) +
  scale_y_continuous(limits = c(38.4,39)) +
  ggtitle("Income in & Around St. Louis, Missouri") +
  labs(caption = "Graph created by @JosephPolitano using ACS 2022 5-Yr Estimates", subtitle = NULL) +
  theme_apricitas + 
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -0.5), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STL_INCOME_MAP, "STL Income Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


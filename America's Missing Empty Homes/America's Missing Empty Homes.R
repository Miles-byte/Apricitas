pacman::p_load(zctaCrosswalk,zipcodeR,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,openxlsx,sf,tidycensus,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

counties <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(GEOID = county_fips) %>%
  select(geometry, GEOID)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()


VACANCY_AGG_2022 <- get_acs(
  geography = "county",
  #variables = DP04,
  table = "B25002",
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE
) %>%
  transmute(GEOID,NAME,VACANCY_RATE = B25002_003E/B25002_001E) %>%
  merge(.,counties)

AGG_VACANCY_2019_Counties <- ggplot() +
  geom_sf(data = VACANCY_AGG_2022 %>% mutate(VACANCY_RATE = case_when(
    VACANCY_RATE > 0.20 ~ 0.20,
    TRUE ~ VACANCY_RATE
  )), aes(fill = VACANCY_RATE, geometry = geometry)) +
  geom_sf(data = counties, color = "black", fill = NA, lwd = 0.1) + # Black borders for counties
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(0,0.05,0.1,0.15,0.20),
                      labels = c("0%","5%","10%","15%","20%+"),
                      limits = c(0,0.20)) +
  ggtitle("            Total Vacancy Rate 2014-2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Census data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = AGG_VACANCY_2019_Counties, "Aggregate Vacancy Rate 2014-2019.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


B25136
B25131
b25130

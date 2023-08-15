pacman::p_load(tidycensus,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

usethis::edit_r_environ()

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

HOMEOWN_FREE_CLEAR <- get_acs(
  geography = "state",
  #variables = DP04,
  table = "DP04",
  cache_table = TRUE,
  year = 2021,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs1",
  show_call = TRUE
)

HOMEOWN_FREE_CLEAR_PCT <- HOMEOWN_FREE_CLEAR %>%
  select(GEOID, NAME, DP04_0092PE)

states <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(NAME = state_name)

states <- merge(states, HOMEOWN_FREE_CLEAR_PCT, by = "NAME")

HOMEOWN_FREE_CLEAR_PCT_MAP <- ggplot() +
  geom_sf(data = states, aes(fill = DP04_0092PE/100)) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradient(high = "#F5B041",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(0.3,-0.4,0.5,0.06), 
                      labels = c("30%","40%","50%","60%"),
                      limits = c(0.24,0.54)) +
  ggtitle("       Annualized GDP Growth Since Q4 2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = HOMEOWN_FREE_CLEAR_PCT_MAP, "HOMEOWN_PCT_SHARE_STATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")
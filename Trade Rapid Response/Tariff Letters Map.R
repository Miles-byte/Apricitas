
pacman::p_load(censusapi,ggpubr,prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


TARIFF_MAP <- map_data("world") %>%
  mutate(region = ifelse(region == "Western Sahara", "Morocco", region)) %>%
  mutate(region = ifelse(region == "Palestine", "Israel", region)) %>% #The US does not recognize Western Sahara or Palestine so they are treated as parts of Morocco and Israel for the purposes of tariffs
  filter(long < 170 & long > -170) %>%
  filter(region != "Antarctica")

LETTERS_DATA <- list(
  Japan = 0.35,
  `South Korea` = 0.25,
  Malaysia = 0.25,
  Kazakhstan = 0.25,
  `South Africa` = 0.30,
  Laos = 0.40,
  Myanmar = 0.40,
  Tunisia = 0.25,
  `Bosnia and Herzegovina` = 0.30,
  #Indonesia = 0.32,
  Indonesia = 0.19,
  Vietnam = 0.20,
  Bangladesh = 0.35,
  Serbia = 0.35,
  Cambodia = 0.36,
  Thailand = 0.36,
  #Philippines = 0.20,
  Philippines = 0.19,
  Brunei = 0.25,
  Moldova = 0.25,
  Algeria = 0.30,
  Iraq = 0.30,
  Libya = 0.30,
  `Sri Lanka` = 0.30,
  Brazil = 0.50,
  Canada = 0.35,
  Mexico = 0.30,
  #EU Countries
  Portugal = 0.30,
  Spain = 0.30,
  France = 0.30,
  Ireland = 0.30,
  Belgium = 0.30,
  Netherlands = 0.30,
  Germany = 0.30,
  Italy = 0.30,
  Czechia = 0.30,
  `Czech Republic` = 0.30,
  Austria = 0.30,
  Slovenia = 0.30,
  Croatia = 0.30,
  Hungary = 0.30,
  Romania = 0.30,
  Slovakia = 0.30,
  Poland = 0.30,
  Bulgaria = 0.30,
  Greece = 0.30,
  Cyprus = 0.30,
  Malta = 0.30,
  Denmark = 0.30,
  Sweden = 0.30,
  Finland = 0.30,
  Estonia = 0.30,
  Latvia = 0.30,
  Lithuania = 0.30,
  Luxembourg = 0.30
) %>%
  enframe(name = "region", value = "tariff") %>%
  mutate(tariff = unlist(tariff)) %>%
  as_tibble()


TARIFF_MAP <- map_data("world") %>%
  mutate(region = ifelse(region == "Western Sahara", "Morocco", region)) %>%
  mutate(region = ifelse(region == "Palestine", "Israel", region)) %>% #The US does not recognize Western Sahara or Palestine so they are treated as parts of Morocco and Israel for the purposes of tariffs
  full_join(LETTERS_DATA, by = "region") %>%
  filter(long < 170 & long > -170) %>%
  filter(region != "Antarctica")


TARIFF_LETTERS_MAP_Graph <- TARIFF_MAP %>% 
  ggplot(aes(fill = tariff, map_id = region)) +
  geom_map(map = TARIFF_MAP, linewidth = 1) +
  expand_limits(x = TARIFF_MAP$long, y = TARIFF_MAP$lat) +
  coord_map("mercator") +
  theme_apricitas + 
  ggtitle(paste("Trump's New Tariff Letters (As of 7/22 2EDT)")) +
  scale_y_continuous(limits = c(-50,75)) +
  scale_x_continuous(limits = c(-180,180)) +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "Trump Is Aggressively Raising Tariff Rates on Imports From Most of the World") +
  labs(fill = NULL) +
  scale_fill_gradientn(name= "Tariff Rate",colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0),limits = c(0,.5)) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0,0.1,-0.1,0.5), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = TARIFF_LETTERS_MAP_Graph, "Tariff Letters Map Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

p_load(eia)

?eia_series()
usethis::edit_r_environ()
SOLAR <- eia_series("ELEC.GEN.TSN-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d")%>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Solar")
WIND <- eia_series("ELEC.GEN.WND-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Wind")
HYDRO <- eia_series("ELEC.GEN.HYC-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Hydro")
NUCLEAR <- eia_series("ELEC.GEN.NUC-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Nuclear")
COAL <- eia_series("ELEC.GEN.COW-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Coal")
NATURAL_GAS <- eia_series("ELEC.GEN.NG-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Natural Gas")

ALL_SECTOR_GENERATION <- rbind(SOLAR,WIND,HYDRO,NUCLEAR,COAL,NATURAL_GAS) %>%
  mutate(sector = factor(sector, levels = c("Coal","Natural Gas","Nuclear","Hydro","Solar","Wind")))

ALL_SECTOR_GENERATION_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = ALL_SECTOR_GENERATION, aes(x = date, y = value/1000000, fill = sector), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "TWh"),limits = c(0,4.2), breaks = c(0,1,2,3,4), expand = c(0,0)) +
  ylab("TWh") +
  ggtitle("Going Green") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Low Carbon Energy Sources Make Up a Record Share of America's Electricity") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#FFE98F","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_SECTOR_GENERATION_GRAPH, "All Sector Generation Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NUCLEAR_GENERATION_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = NUCLEAR, aes(x = date, y = value/1000000, color = "Nuclear"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, suffix = "TWh"),limits = c(.750,.850), breaks = c(.750,.800,.850), expand = c(0,0)) +
  ylab("TWh") +
  ggtitle("Nuclear Falloff") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Nuclear Power Generation Has Fallen to a 10-Year Low") +
  theme_apricitas + theme(legend.position = "right") +
  scale_color_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#FFE98F","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")


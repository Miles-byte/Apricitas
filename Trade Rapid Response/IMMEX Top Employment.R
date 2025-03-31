IMMEX_VEHICLE <- inegi_series(serie = "436234", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01")) %>%
  summarise(values = mean(values)) %>%
  mutate(name = "Motor Vehicles & Parts")

IMMEX_COMPUTER <- inegi_series(serie = "436232", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01"))%>%
  summarise(values = mean(values)) %>%
  mutate(name = "Computers & Electronics")

IMMEX_PLASTICS <- inegi_series(serie = "436227", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01")) %>%
  summarise(values = mean(values)) %>%
  mutate(name = "Plastics & Rubber")

IMMEX_APPLIANCE <- inegi_series(serie = "436233", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01")) %>%
  summarise(values = mean(values)) %>%
  mutate(name = "Appliances & Electrical Equipment")

IMMEX_METALS <- inegi_series(serie = "436230", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01")) %>%
  summarise(values = mean(values)) %>%
  mutate(name = "Fabricated Metal Products")

IMMEX_MACHINERY <- inegi_series(serie = "436231", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01")) %>%
  summarise(values = mean(values)) %>%
  mutate(name = "Machinery & Equipment")

IMMEX_FOOD <- inegi_series(serie = "436217", token = Sys.getenv("TOKEN_INEGI")) %>%
  filter(date>=as.Date("2024-01-01")) %>%
  summarise(values = mean(values)) %>%
  mutate(name = "Food")

IMMEX_EMPLOYMENT <- rbind(IMMEX_FOOD,IMMEX_MACHINERY,IMMEX_METALS,IMMEX_APPLIANCE,IMMEX_PLASTICS,IMMEX_COMPUTER,IMMEX_VEHICLE) %>%
  mutate(name = factor(name, levels = rev(c("Motor Vehicles & Parts","Computers & Electronics","Plastics & Rubber","Appliances & Electrical Equipment","Fabricated Metal Products","Food","Machinery & Equipment"))))

IMMEX_TOP_EMPLOYMENT <- ggplot(data = IMMEX_EMPLOYMENT, aes(x = name, y = values/1000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ",name)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 9,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ylab("Annual Employment, 2024") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "M"), limits = c(0,1.1), expand = c(0,0)) +
  ggtitle("Top Mexican Export-Oriented Industries") +
  labs(subtitle = "Employment in the IMMEX Export-Oriented Manufacturing Program is Concentrated in Cars & Computers") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = IMMEX_TOP_EMPLOYMENT, "IMMEX Top Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

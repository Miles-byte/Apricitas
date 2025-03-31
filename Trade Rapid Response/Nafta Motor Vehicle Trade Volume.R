CN_MOTOR_VEHICLE_IMPORT_VOLUME <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("GEN_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "87", #Phones
  CTY_NAME = "CANADA",
)

CN_MOTOR_VEHICLE_EXPORT_VOLUME <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_MO", "E_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "87", #Phones
  CTY_NAME = "CANADA",
)

CN_MOTOR_VEHICLE_TRADE_VOLUME <- merge(CN_MOTOR_VEHICLE_EXPORT_VOLUME,CN_MOTOR_VEHICLE_IMPORT_VOLUME,by = "time") %>%
  select(time, ALL_VAL_MO, GEN_VAL_MO) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO), GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(ROLL_EXP = rollsum(ALL_VAL_MO, 12, na.pad = TRUE, align = "right"), ROLL_IMP = rollsum(GEN_VAL_MO, 12, na.pad = TRUE, align = "right")) %>%
  mutate(country = "Canada")


MX_MOTOR_VEHICLE_IMPORT_VOLUME <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("GEN_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "87", #Phones
  CTY_NAME = "MEXICO",
)

MX_MOTOR_VEHICLE_EXPORT_VOLUME <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_MO", "E_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "87", #Phones
  CTY_NAME = "MEXICO",
)

MX_MOTOR_VEHICLE_TRADE_VOLUME <- merge(MX_MOTOR_VEHICLE_EXPORT_VOLUME,MX_MOTOR_VEHICLE_IMPORT_VOLUME,by = "time") %>%
  select(time, ALL_VAL_MO, GEN_VAL_MO) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO), GEN_VAL_MO = as.numeric(GEN_VAL_MO)) %>%
  mutate(ROLL_EXP = rollsum(ALL_VAL_MO, 12, na.pad = TRUE, align = "right"), ROLL_IMP = rollsum(GEN_VAL_MO, 12, na.pad = TRUE, align = "right")) %>%
  mutate(country = "Mexico")

NAFTA_MOTOR_VEHICLE_TRADE_VOLUME <- rbind(MX_MOTOR_VEHICLE_TRADE_VOLUME,CN_MOTOR_VEHICLE_TRADE_VOLUME) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(time,"-01")))

NAFTA_MOTOR_VEHICLE_TRADE_VOLUME_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=NAFTA_MOTOR_VEHICLE_TRADE_VOLUME, aes(x=date,y= ROLL_EXP/1000000000,color= "Exports"), size = 1.25) + 
  geom_line(data=NAFTA_MOTOR_VEHICLE_TRADE_VOLUME, aes(x=date,y= ROLL_IMP/1000000000,color= "Imports"), size = 1.25) + 
  facet_wrap(~ country) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,150), expand = c(0,0)) +
  ylab("Billions of Dollars, 12MMT") +
  ggtitle("US North American Vehicle & Parts Trade") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Vehicles & Parts are HS Code 87",subtitle = "Vehicles Form the Backbone of North American Trade—and Will be Hit Hard by Tariffs") +
  theme_apricitas + theme(legend.position = c(.15,.85), plot.title = element_text(size = 27)) + theme(strip.text = element_text(size = 17, color = "white", face = "bold")) +
  scale_color_manual(name= "US Trade, 12MMT",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Imports","Exports"))

ggsave(dpi = "retina",plot = NAFTA_MOTOR_VEHICLE_TRADE_VOLUME_GRAPH, "NAFTA MOTOR VEHICLE TRADE VOLUME GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


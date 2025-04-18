
SOLAR_CONSOLE_TARIFF_TIMELINE <- data.frame(date = seq.Date(from = as.Date("2025-04-02"), to = as.Date("2025-04-12"), by = "day"),
                                            console = c(.15525366,.15525366,.15525366,.25525366,.25525366,.25525366,.25525366,.86726098,1.14487744,1.14487744,1.14487744),
                                            solar = c(0.0025524395,0.0025524395,0.0025524395,0.1025524395,0.1025524395,0.1025524395,0.1025524395,0.360686638,0.1172036,0.1172036,0.1172036),
                                            phone = c(0.09576812,0.09576812,0.09576812,0.19576812,0.19576812,0.19576812,0.19576812,0.19576812,0.5909752,0.64912050,.09576812)) %>%
  mutate(solar = solar + 0.016)



SOLAR_CONSOLE_TARIFF_TIMELINE_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  #geom_line(data=IMPLEMENTED_SHARE, aes(x=date,y= value,color= "Share of US Imports Hit By New Tariffs"), size = 1.25) + 
  geom_line(data=SOLAR_CONSOLE_TARIFF_TIMELINE, aes(x=date,y= solar,color= "Solar Panels"), size = 1.25) + 
  geom_line(data=SOLAR_CONSOLE_TARIFF_TIMELINE, aes(x=date,y= console,color= "Video Game Consoles"), size = 1.25) + 
  geom_line(data=SOLAR_CONSOLE_TARIFF_TIMELINE, aes(x=date,y= phone,color= "Phones"), size = 1.25) + 
  
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1.50), breaks = c(0,.25,.5,.75,1,1.25,1.5), expand = c(0,0)) +
  ylab("Tariff Rate on 2024 Import Mix") +
  ggtitle("The Week of Tariff Insanity: an Example") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Solar = HS 8541 ex Semiconductors, Consoles = 9504, Phones = 8517",subtitle = "Tariffs Changed Incredibly Frequently Over the Last Week, Causing Chaos") +
  theme_apricitas + theme(legend.position = c(.25,.79)) +
  scale_color_manual(name= "Tariff Rate on 2024 Import Mix",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-04-02")-(.1861*(today()-as.Date("2025-04-02"))), xmax = as.Date("2025-04-02")-(0.049*(today()-as.Date("2025-04-02"))), ymin = 0-(.3*(1.4)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SOLAR_CONSOLE_TARIFF_TIMELINE_GRAPH, "Solar Console Tariff Timeline Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




BEGINNINING

0.0025524395

10% UNIVERSAL TARIFF

0.1025524395

RECIPROCAL TARIFF

0.360686638

END:
0.1172036


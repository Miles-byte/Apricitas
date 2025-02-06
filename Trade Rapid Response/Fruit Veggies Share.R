FRUIT_VEGGIES_SHARE <- data.frame(category = c("Raspberries", "Papayas", "Limes", "Cucumbers", "Avocados", "Artichokes", "Bell Peppers", "Mangoes", "Asparagus", "Tomatoes"),
                                  share = c(0.910460872,
                                            0.89981175,
                                            0.8987,
                                            0.868269241,
                                            0.804032366,
                                            0.793230227,
                                            0.71743572,
                                            0.6721,
                                            0.661232633,
                                            0.660846006
                                  )) %>%
  mutate(category = factor(category,levels = rev(c("Raspberries", "Papayas", "Limes", "Cucumbers", "Avocados", "Artichokes", "Bell Peppers", "Mangoes", "Asparagus", "Tomatoes"))))

FRUIT_VEGGIES_SHARE_GRAPH <- ggplot(data = FRUIT_VEGGIES_SHARE, aes(x = category, y = share)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ",category)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("Fresh Fruits & Veggies Where a Large % of\nUS Consumption is Mexican/Canadian Imports") +
  ylab("% of Total US Imports in Category, 2023") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "From USDA Import Share of Consumption Data") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = FRUIT_VEGGIES_SHARE_GRAPH, "Fruit Veggies Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

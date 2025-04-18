TARIFF_BY_CATEGORY_SELECT_FOOD_SUMMARY <- AGG_TARIFF_BY_CATEGORY %>%
  filter(I_COMMODITY %in% c("0905","0801","0906","0907","1006","1511","0904","0901","0304","2204","0306","1509","1902","0803","1804")) %>%
  arrange(desc(tariff_val)) %>%
  slice(1:12) %>%
  mutate(I_COMMODITY_LDESC = c("Raw Fish","Coffee","Wine","Crustaceans","Olive Oil","Palm Oil","Cashews","Rice","Pasta","Pepper","Bananas","Cocoa Butter")) %>%
  arrange(desc(value)) %>%
  mutate(I_COMMODITY_LDESC = paste0(as.character(I_COMMODITY_LDESC), ": ", round_tariff*100, "%")) %>%
  mutate(I_COMMODITY_LDESC = replace(I_COMMODITY_LDESC, 1, paste0(I_COMMODITY_LDESC[1], " Tariff"))) %>%
  mutate(I_COMMODITY_LDESC1 = sub(":.*", "", I_COMMODITY_LDESC)) %>%
  mutate(I_COMMODITY_LDESC1 = factor(I_COMMODITY_LDESC1, levels = rev(I_COMMODITY_LDESC1)))


TARIFF_BY_CATEGORY_SELECT_FOOD_SUMMARY_RAINBOW_GRAPH <- ggplot(data = TARIFF_BY_CATEGORY_SELECT_FOOD_SUMMARY, aes(x = I_COMMODITY_LDESC1, y = value/1000000000, fill = round_tariff)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_text(aes(label = sub("^.*?(?=\\d)", " ", I_COMMODITY_LDESC, perl = TRUE)), 
            position = position_stack(vjust = 0), 
            angle = 0, 
            hjust = 0, 
            size = 7, 
            color = "black", 
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("Major Raw Foods Hit By Trump's\nTariffs Through April 9th") +
  ylab("US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,10), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  scale_fill_gradientn(name= "Tariff Rate",colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),limits = c(.09,.4),breaks = c(.1,.20,.30,.4), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Census Data. Note: Data by 4-Digit HS Code. Tariff Calculated as Weighted Avg of 2024 Import Mix", subtitle = "Trump's Tariffs Hit Many Raw Foodstuffs Like Coffee, Wine, Olive Oil, and More") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = TARIFF_BY_CATEGORY_SELECT_FOOD_SUMMARY_RAINBOW_GRAPH, "Tariff By Category Select Food Reciprocal Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


TARIFF_BY_CATEGORY_SELECT_INPUTS_SUMMARY <- AGG_TARIFF_BY_CATEGORY %>%
  filter(I_COMMODITY %in% c("4011","8411","8544","8481","8413","8536","8483","3923","8409","7601","8536")) %>%
  arrange(desc(tariff_val)) %>%
  mutate(I_COMMODITY_LDESC = c("Insulated Wires","Valves, Taps, etc","Rubber Tires","Jets/Turbines","Liquid Pumps","Electrical Switches","Plastic Packaging","Cranks, Shafts, Gears","Unwrought Aluminum","ICE Engine Parts")) %>%
  arrange(desc(value)) %>%
  mutate(I_COMMODITY_LDESC = paste0(as.character(I_COMMODITY_LDESC), ": ", round_tariff*100, "%")) %>%
  mutate(I_COMMODITY_LDESC = replace(I_COMMODITY_LDESC, 1, paste0(I_COMMODITY_LDESC[1], " Tariff"))) %>%
  mutate(I_COMMODITY_LDESC1 = sub(":.*", "", I_COMMODITY_LDESC)) %>%
  mutate(I_COMMODITY_LDESC1 = factor(I_COMMODITY_LDESC1, levels = rev(I_COMMODITY_LDESC1)))



TARIFF_BY_CATEGORY_SELECT_INPUT_SUMMARY_RAINBOW_GRAPH <- ggplot(data = TARIFF_BY_CATEGORY_SELECT_INPUTS_SUMMARY, aes(x = I_COMMODITY_LDESC1, y = value/1000000000, fill = round_tariff)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_text(aes(label = sub("^.*?(?=\\d)", " ", I_COMMODITY_LDESC, perl = TRUE)), 
            position = position_stack(vjust = 0), 
            angle = 0, 
            hjust = 0, 
            size = 7, 
            color = "black", 
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("Major Input Goods Hit By Trump's\nTariffs Through April 9th") +
  ylab("US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,40), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  scale_fill_gradientn(name= "Tariff Rate",colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),limits = c(.09,.4),breaks = c(.1,.20,.30,.4), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Census Data. Note: Data by 4-Digit HS Code. Tariff Calculated as Weighted Avg of 2024 Import Mix", subtitle = "Trump's Tariffs Hit Many Key Inputs Like Wires, Turbines, Gears, & More") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = TARIFF_BY_CATEGORY_SELECT_INPUT_SUMMARY_RAINBOW_GRAPH, "Tariff By Category Select Input Reciprocal Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

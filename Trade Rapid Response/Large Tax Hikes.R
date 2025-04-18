LARGE_TAX_HIKES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Trade%20Rapid%20Response/large_tax_hikes.csv") %>%
  mutate(Percent = as.numeric(gsub("%", "", Percent)) / 100) %>%
  mutate(category = c("Other Peacetime Tax Hikes","Trump Tariffs","Other Peacetime Tax Hikes","Other Peacetime Tax Hikes","Trump Tariffs","Other Peacetime Tax Hikes","Other Peacetime Tax Hikes","Trump Tariffs","Other Peacetime Tax Hikes","Other Peacetime Tax Hikes")) %>%
  arrange(desc(Percent)) %>%
  mutate(Act = factor(Act, levels = rev(Act)))

LARGE_TAX_HIKES_GRAPH <- ggplot(data = LARGE_TAX_HIKES, aes(x = Act, y = Percent, fill = category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  geom_text(aes(label = Act, perl = TRUE), 
            position = position_stack(vjust = 0), 
            angle = 0, 
            hjust = 0, 
            size = 5.5, 
            color = "black", 
            fontface = "bold") +
  xlab(NULL) +
  xlab(NULL) +
  ggtitle("Trump Tariffs Rival Largest Peacetime Taxes") +
  ylab("First Year Revenue, % of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.05), limits = c(0,0.01), breaks = c(0.0025,0.005,0.0075,0.01), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  scale_fill_manual(name= "Modern US Tax Hikes, % of GDP",values = c("#FFE98F","#EE6055")) +
  labs(caption = "Graph created by @JosephPolitano using Tax Foundation Data via Erica York", subtitle = "Trump's Tariffs Are The Largest Peacetime Tax Hikes in Decades") +
  theme_apricitas + theme(legend.position = c(.75,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_blank(), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = LARGE_TAX_HIKES_GRAPH, "Large Tax Hikes Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

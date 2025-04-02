US_SHARE_TOP_VEHICLES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/aac52b8ccad72b6c588e7e282fc77d2c89340014/Trade%20Rapid%20Response/CAR_US_SHARE.csv") %>%
  mutate(name = factor(name, levels = rev(c("Toyota Camry","Toyota RAV4","Tesla Model Y","Honda Civic","Ram 1500","Honda CR-V","GMC Sierra","Chevy Silverado","Ford F-150","Nissan Rogue"))))

US_SHARE_TOP_VEHICLES <- ggplot(data = US_SHARE_TOP_VEHICLES, aes(x = name, y = percent, fill = "Domestic (US/Canada) Content Share")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("US/Canada Content, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("No Such Thing as an All-American Car") +
  labs(subtitle = "No Car—Even US-Assembled US-Nameplate Models—is Made With 100% US Parts", caption = "Graph created by @Josephpolitano using AALA Data") +
  scale_fill_manual(name= "Top Selling 2024 Models",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme_apricitas + theme(legend.position = c(.7,.3), axis.text.y = element_text(size = 16), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 27)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = US_SHARE_TOP_VEHICLES, "US Share Top Vehicles Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


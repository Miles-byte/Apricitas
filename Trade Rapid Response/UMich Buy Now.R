UMICH_BUY_NOW <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Trade%20Rapid%20Response/UMICH_BUY_NOW.csv") %>%
  mutate(date = as.Date(date))

UMICH_BUY_NOW_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  geom_line(data= UMICH_BUY_NOW, aes(x=date,y=buynow/100,color= "% of Consumers Saying\nPrices for Durables Will Increase\nSo It's a Good Time to Buy Now"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.30), expand = c(0,0)) +
  ylab("% of Consumers") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Consumers are Trying to Preempt Tariffs") +
  annotate(geom = "segment", x = as.Date("2018-03-01"), xend = as.Date("2018-11-01"), y = .21, yend = .24, color = "white", lwd = .75) +
  annotate(geom = "text", label = "First US-China Trade War", x = as.Date("2020-05-01"), y = .24, color ="white",size = 4, lineheight = unit(0.85,"cm")) +
  labs(caption = "Graph created by @JosephPolitano using UMich data",subtitle = "Consumers are Trying to Make Large Purchases Ahead of Expected Tariff Increases") +
  theme_apricitas + theme(legend.position = c(.22,.91), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(.30)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UMICH_BUY_NOW_GRAPH, "UMich Buy Now Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


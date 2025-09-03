TARIFF_REV_Q <- fredr("B235RC1Q027SBEA")

GDP_Q <- fredr("GDP")

TARIFF_PCT_GDP_Q <- left_join(TARIFF_REV_Q,GDP_Q, by = "date") %>%
  transmute(date, value = value.x/value.y)

TARIFF_REV_A <- fredr("B235RC1A027NBEA", frequency = "a")

GDP_A <- fredr("GDPA")

TARIFF_PCT_GDP_A <- left_join(TARIFF_REV_A,GDP_A, by = "date") %>%
  transmute(date, value = value.x/value.y)

TARIFF_PCT_GDP <- rbind(
  filter(TARIFF_PCT_GDP_A, date < as.Date("1959-01-01")),
  filter(TARIFF_PCT_GDP_Q, date >= as.Date("1959-01-01"))
)

TARIFF_PCT_GDP_Graph <- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=TARIFF_PCT_GDP, aes(x=date,y= value, color= "Tariffs, % of GDP"), size = 1.25) +
  annotate("segment",x = as.Date("1930-01-01"), xend = as.Date("1932-01-01"),y = TARIFF_PCT_GDP$value[2], yend = 0.0065,color = "white", size = 1) +
  annotate("text",x = as.Date("1930-06-01"), y = 0.00625,label = "Smoot-Hawley Tariff",color = "white", hjust = -0.1, vjust = -0.5, size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),limits = c(0,0.01), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Scale of Trump's Trade War") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Tariffs as a % of GDP are at the Highest Levels in More than a Century") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1929-01-01")-(.1861*(today()-as.Date("1929-01-01"))), xmax = as.Date("1929-01-01")-(0.049*(today()-as.Date("1929-01-01"))), ymin = 0-(.3*.01), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TARIFF_PCT_GDP_Graph, "TARIFF PCT GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

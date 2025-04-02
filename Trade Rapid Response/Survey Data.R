
TPU_BULK <- read.xlsx("https://www.matteoiacoviello.com/tpu_files/tpu_web_latest.xlsx",4) 



TPU <- TPU_BULK %>%
  mutate(DATE = seq.Date(from = as.Date("1960-01-01"), by = "month", length.out = nrow(.)))


TRADE_POLICY_UNCERTAINTY_GRAPH <- ggplot() + #plotting net tightening data
  geom_line(data=filter(TPU, DATE >= as.Date("2004-01-01")), aes(x=DATE,y= TPU ,color= "Trade Policy Uncertainty Index"), size = 1.25) + 
  xlab("Date") +
  ylab("Trade Uncertainty Index Derived from News Coverage") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,100,200,300,400,500,600,700,800,900), limits = c(0,ceiling(max(TPU$TPU)/50)*50), expand = c(0,0)) +
  ggtitle("Trade Policy Uncertainty is at Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using Caldara, Dario, Matteo Iacoviello, Patrick Molligo, Andrea Prestipino, & Andrea Raffo data", subtitle = "Trump's Back and Forth Tariff Moves Have Driven Trade Policy Uncertainty to Record Highs") +
  theme_apricitas + theme(legend.position = c(.52,.92), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 0-(.3*(ceiling(max(TPU$TPU)/50)*50)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TRADE_POLICY_UNCERTAINTY_GRAPH, "TRADE POLICY UNCERTAINTY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

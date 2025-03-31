devtools::install_github("jameelalsalam/eia2")
library("eia2")


PADD_2_REFINERY_INPUTS <- eia1_series("PET.MCRRIP22.A") %>%
  mutate(date = as.Date(paste0(period,"-01-01")))

PADD_2_CANADA_CRUDE <- eia1_series("PET.MCRIPP2CA2.A") %>%
  mutate(date = as.Date(paste0(period,"-01-01")))



PADD_2_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(PADD_2_CANADA_CRUDE, date >= as.Date("2000-01-01")), aes(x=date,y= value/1000,color= "Canadian Crude Oil Imported to the Midwest"), size = 1.25) + 
  geom_line(data=filter(PADD_2_REFINERY_INPUTS, date >= as.Date("2000-01-01")), aes(x=date,y= value/1000,color= "Net Input of Crude Oil to Midwestern Refineries"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "MMbpd", accuracy = 1),limits = c(0,4), breaks = c(0,1,2,3,4), expand = c(0,0)) +
  ylab("Millions of Barrels Per Day") +
  ggtitle("Midwest Refineries Depend on Canada") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Midwest is PADD 2",subtitle = "The Midwestern United States Relies on Canadian Crude to Run Its Refineries") +
  theme_apricitas + theme(legend.position = c(.35,.69), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Net Input of Crude Oil to Midwestern Refineries","Canadian Crude Oil Imported to the Midwest")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*(4)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PADD_2_Graph, "PADD 2 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

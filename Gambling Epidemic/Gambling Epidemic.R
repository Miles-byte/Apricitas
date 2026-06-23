QTAX_Gambling_Data <- getCensus(
  name = "timeseries/eits/qtax",
  #region = "CA",
  vars = c("CELL_VALUE","CATEGORY_CODE","SEASONALLY_ADJ","DATA_TYPE_CODE"),
  time = paste("from 2012 to", format(Sys.Date(), "%Y")),
  region = "state",
  DATA_TYPE_CODE = "T18"
) %>%
  transmute(date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(CELL_VALUE)) %>%
  group_by(date) %>%
  summarise(value = sum(value)*4) %>%
  ungroup()


QTAX_Gambling_Data_Graph <- ggplot() + #indexed employment rate
  geom_line(data = QTAX_Gambling_Data, aes(x=date, y = value/1000, color = "US Sports Betting Tax Revenue (Quarterly Annualized)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,5), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Taxing America's Lottery Boom") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Tax Revenue on Sports Gambling Continues Rising to New Record Highs") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-07-01")-(.1861*(today()-as.Date("2021-07-01"))), xmax = as.Date("2021-07-01")-(0.049*(today()-as.Date("2021-07-01"))), ymin = 0-(.3*5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QTAX_Gambling_Data_Graph, "QTAX Gambling Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

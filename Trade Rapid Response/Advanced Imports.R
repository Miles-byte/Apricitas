?getCensus()

listCensusMetadata(
  name = "timeseries/eits/ftd",
  type = "value",
  variable = "program_code",
)

cbp_dict <- listCensusMetadata(
  name = "timeseries/eits/ftdadv",
  type = "variables",
  include_values = TRUE)

ADVANCED_IMPORTS <- getCensus(
  name = "timeseries/eits/ftdadv",
  vars = c("category_code", "cell_value", "data_type_code", "seasonally_adj", "time_slot_id", "program_code"),
  seasonally_adj = "yes",
  data_type_code = "BAL",
  time = paste("from 2018 to", format(Sys.Date(), "%Y"))
)

ADVANCED_IMPORTS <- ADVANCED_IMPORTS %>%
  mutate(cell_value = as.numeric(cell_value)) %>%
  mutate(time = as.Date(paste0(time,"-01")))

ADVANCED_IMPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=ADVANCED_IMPORTS, aes(x=time,y= -cell_value*12/1000000,color= "US Goods Trade Deficit\nSeasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.25),limits = c(0,2),breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2), expand = c(0,0)) +
  ylab("Dollars, Seasonally Adjusted Annual Rate") +
  ggtitle("Imports Have Spiked Amid Tariff Fears") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Consumers & Businesses Rushed to Buy Goods Before Trump's Tariffs Come In Place") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*(2)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ADVANCED_IMPORTS_Graph, "Advanced Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



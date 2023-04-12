p_load(statcanR,cansim)


test <- statcan_data("14-10-0287-02", "eng")

testQB <- test %>%
  subset(REF_DATE == as.Date("2023-01-01")) %>%
  subset(GEO == "Quebec") %>%
  subset(`Data type` == "Seasonally adjusted") %>%
  subset(`Age group` == "25 to 54 years") %>%
  subset(`Statistics` == "Estimate")

QC_EPOP_MALE <- get_cansim_vector("v2063906")
QC_EPOP_FEMALE <- get_cansim_vector("v2063915")
US_EPOP_MALE <- fredr(series_id = "LREM25MAUSM156S", observation_start = as.Date("1976-01-01"))
US_EPOP_FEMALE <- fredr(series_id = "LREM25FEUSM156S", observation_start = as.Date("1976-01-01"))

EPOP_US_QUEBEC_GRAPH <- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "vline",x = as.Date("1997-09-01"), xintercept = as.Date("1997-09-01"), size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Quebec Universal",x = as.Date("1993-01-01"), y = 0.5, size = 4,color = "white") +
  annotate(geom = "text", label = "Childcare Passed",x = as.Date("1993-01-01"), y = 0.475, size = 4,color = "white") +
  geom_line(data=QC_EPOP_MALE, aes(x=Date,y= VALUE/100, color= "Québécois Men"), size = 1.25) +
  geom_line(data=QC_EPOP_FEMALE, aes(x=Date,y= VALUE/100, color= "Québécoise Women"), size = 1.25) +
  geom_line(data=US_EPOP_MALE, aes(x=date,y= value/100, color= "American Men"), size = 1.25) +
  geom_line(data=US_EPOP_FEMALE, aes(x=date,y= value/100, color= "American Women"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.40,.95), breaks = c(.40,.50,.60,.70,.80,.90), expand = c(0,0)) +
  ylab("Employment Rate") +
  ggtitle("Fuller Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Stats Canada data",subtitle = "Quebec's Employment Rates Have Exceeded Pre-Pandemic Levels—And Beaten America's") +
  theme_apricitas + theme(legend.position = c(.725,.22)) +
  scale_color_manual(name= "25-54 Employment-Population Ratios" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1976-01-01")-(.1861*(today()-as.Date("1976-01-01"))), xmax = as.Date("1976-01-01")-(0.049*(today()-as.Date("1976-01-01"))), ymin = .40-(.3*.55), ymax = .40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_US_QUEBEC_GRAPH, "EPOP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


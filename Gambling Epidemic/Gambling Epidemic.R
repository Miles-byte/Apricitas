QTAX_Gambling_Data <- getCensus(
  name = "timeseries/eits/qtax",
  #region = "CA",
  vars = c("CELL_VALUE","CATEGORY_CODE","SEASONALLY_ADJ","DATA_TYPE_CODE"),
  time = paste("from 2012 to", format(Sys.Date(), "%Y")),
  region = "*",
  DATA_TYPE_CODE = "T18"
) %>%
  transmute(date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(CELL_VALUE)) %>%
  group_by(date) %>%
  summarise(value = sum(value)*4) %>%
  ungroup()

?getCensus

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


write.csv(QTAX_Gambling_Data,"Gambling Tax Data.csv")



PCE_UNDERLYING_DETAIL_NOMINAL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20405',
  'Frequency' = 'M',
  'Year' = paste(seq(from = 2015, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_UNDERLYING_DETAIL_NOMINAL <- beaGet(PCE_UNDERLYING_DETAIL_NOMINAL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2015-01-01"), length.out = nrow(.), by = "1 month"))) %>%
  clean_names() %>%
  drop_na()

PCE_GAMBLING_Graph <- ggplot() + #indexed employment rate
  geom_line(data = PCE_UNDERLYING_DETAIL_NOMINAL, aes(x=date, y = u20405_dgamrc_226_gambling_91_current_dollars_level_6/1000, color = "US Gambling Losses"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,250), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("The Rise of American Gambling") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Gambling Losses Continue Rising to Record Hights") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= "BEA Nominal Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_GAMBLING_Graph, "PCE Gambling Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PCE_LOTTERIES_Graph <- ggplot() + #indexed employment rate
  geom_line(data = PCE_UNDERLYING_DETAIL_NOMINAL, aes(x=date, y = u20405_dlotrc_228_lotteries_current_dollars_level_6/1000, color = "US Lottery Gambling Losses"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,80), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("The Rise of American Lottery Gambling") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Americans are Losing Record Amounts to State Lotteries") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= "BEA Nominal Personal Consumption Expenditures",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*80), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_LOTTERIES_Graph, "PCE Lotteries Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


PCE_GAMBLING_DATA <- PCE_UNDERLYING_DETAIL_NOMINAL %>%
  transmute(date, Gambling_Losses = u20405_dgamrc_226_gambling_91_current_dollars_level_6, Lottery_Losses = u20405_dlotrc_228_lotteries_current_dollars_level_6)
  
write.csv(PCE_GAMBLING_DATA,"Gambling Losses Data.csv")

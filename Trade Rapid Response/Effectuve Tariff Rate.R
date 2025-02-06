US_COUNTRIES_TOTAL_IMPORTS_LINE <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO","CAL_DUT_MO", "CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN <- US_COUNTRIES_TOTAL_IMPORTS_LINE %>%
  mutate(CAL_DUT_MO = as.numeric(CAL_DUT_MO), CON_VAL_MO = as.numeric(CON_VAL_MO), effective_tariff = as.numeric(CAL_DUT_MO)/as.numeric(CON_VAL_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  group_by(CTY_NAME) %>%
  arrange(CTY_NAME,time) %>%
  mutate(ROLL_CAL_DUT_MO = rollsum(CAL_DUT_MO,12, fill = NA, align = "right"), ROLL_CON_VAL_MO = rollsum(CON_VAL_MO,12, fill = NA, align = "right"),effective_tariff_rollsum = ROLL_CAL_DUT_MO/ROLL_CON_VAL_MO)

US_EFFECTIVE_TARIFF_RATE_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CHINA"), aes(x=date,y= effective_tariff_rollsum,color= "China"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2024-11-01"), xend = as.Date("2026-01-01"), y = filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CHINA")$effective_tariff_rollsum[nrow(filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CHINA"))], yend = filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CHINA")$effective_tariff_rollsum[nrow(filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CHINA"))]+.1, color = "#EE6055",linetype = "dashed", size = 1, alpha = 0.75) +
  geom_line(data=filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "MEXICO"), aes(x=date,y= effective_tariff_rollsum,color= "Mexico"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2024-11-01"), xend = as.Date("2026-01-01"), y = filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "MEXICO")$effective_tariff_rollsum[nrow(filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "MEXICO"))], yend = .25, color = "#00A99D",linetype = "dashed", size = 1, alpha = 0.75) +
  geom_line(data=filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CANADA"), aes(x=date,y= effective_tariff_rollsum,color= "Canada"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2024-11-01"), xend = as.Date("2026-01-01"), y = filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CANADA")$effective_tariff_rollsum[nrow(filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CANADA"))], yend = .205, color = "#FFE98F",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-11-01"), xintercept = as.Date("2024-11-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Projected Increase From\nTrump Tariff Executive Orders", x = as.Date("2024-10-01"), hjust = 1, y = .2, color = "white", size = 4, alpha = 0.75, lineheight = 1) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.3), breaks = c(0,.05,.1,.15,.2,.25,.3), expand = c(0,0)) +
  ylab("Average Effective Tariff Rate, %") +
  ggtitle("Trump Actions Would Spike Tariff Rates") +
  labs(caption = "Graph created by @JosephPolitano using Census data. Projectiosn Based on Back of Envelope Math",subtitle = "Effective Tariff Rates Would Spike to Historic Highs if Trumps' Tariffs Went into Effect") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Effective Tariff Rate on Goods Imports\nRolling 12M Average",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("China","Canada","Mexico")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*.3), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EFFECTIVE_TARIFF_RATE_GRAPH, "US Effective Tariff Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_NAT_MAGNET_GRAPH_CHINA <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CHINA"), aes(x=date,y= ROLL_CON_VAL_MO/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "MEXICO"), aes(x=date,y= ROLL_CON_VAL_MO/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=filter(US_COUNTRIES_TOTAL_IMPORTS_LINE_BREAKDOWN, date>= as.Date("2013-01-01") & CTY_NAME == "CANADA"), aes(x=date,y= ROLL_CON_VAL_MO/1000000000,color= "Canada"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,600), breaks = c(0,100,200,300,400,500,600), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Graphite & Magnet Imports From China") +
  labs(caption = "Graph created by @JosephPolitano using Census data seasonally adjusted using X-13ARIMA. Note: China Includes HK & MO",subtitle = "Imports of Natural Graphite and Permanent Magnets From China Have Risen Since COVID") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Gross Imports from China\nSeasonally Adjusted at Annual Rates",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Permanent Magnets","Natural Graphite")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*.75), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NAT_MAGNET_GRAPH_CHINA, "US Nat Magnet Graph China.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

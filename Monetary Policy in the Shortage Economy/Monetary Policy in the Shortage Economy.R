


EU_MANU_SURVEY <- get_eurostat_data("EI_BSIN_Q_R2",
                                                 filters=c("EU27_2020","SA","BS-FLP1-PC","BS-FLP2-PC","BS-FLP3-PC","BS-FLP4-PC","BS-FLP5-PC","BS-FLP6-PC"),
                                                 date_filter=">1990-01-01") %>%
  mutate(time = as.Date(as.yearqtr(time,"%Y-Q%q"))) %>%
  subset(geo == "EU27_2020") %>%
  select(indic, time, values) %>%
  pivot_wider(names_from = indic, values_from = values)

EU_MANU_SURVEY_DEMAND_Materials_graph <- ggplot() + #plotting BIE
  geom_line(data=EU_MANU_SURVEY, aes(x=time,y= (`BS-FLP1-PC`+`BS-FLP2-PC`)/100,color= "None or Insufficient Demand"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP4-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= (`BS-FLP5-PC`+`BS-FLP6-PC`)/100,color= "Other"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "EU Manufacturers are Increasingly Citing Supply Chain Issues as Their Main Production Constraint") +
  theme_apricitas + theme(legend.position = c(.50,.5)) +
  scale_color_manual(name= "Factors Limiting Production in EU-27 Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(.1861*(today()-as.Date("1990-01-01")))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MANU_SURVEY_DEMAND_Materials_graph, "EU_MANU_SURVEY_DEMAND_MATERIALS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EU_MANU_SURVEY_NONE_DEMAND_graph <- ggplot() + #plotting BIE
  geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP1-PC`/100,color= "None"), size = 1.25) +
  geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP2-PC`/100,color= "Insufficient Demand"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP4-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= (`BS-FLP5-PC`+`BS-FLP6-PC`)/100,color= "Other"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,.20,.40,.60,.80,1), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "The Share of Firms Citing Insufficient Demand as a Production Constraint Hit a Record Low") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= "Factors Limiting Production in EU-27 Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(.1861*(today()-as.Date("1990-01-01")))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MANU_SURVEY_NONE_DEMAND_graph, "EU_MANU_SURVEY_NONE_DEMAND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

EU_MANU_SURVEY_FIN_CONSTRAINTS_graph <- ggplot() + #plotting BIE
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP1-PC`/100,color= "None"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP2-PC`/100,color= "Insufficient Demand"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP3-PC`/100,color= "Shortage of Labor"), size = 1.25) +
  #geom_line(data=EU_MANU_SURVEY, aes(x=time,y= `BS-FLP4-PC`/100,color= "Shortage of Materials and Equipment"), size = 1.25) +
  geom_line(data=subset(EU_MANU_SURVEY, time > as.Date("2003-01-01")), aes(x=time,y= (`BS-FLP6-PC`)/100,color= "Financial Constraints"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.1), breaks = c(0,.020,.040,.060,.080,.1), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("The Shortage Economy") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "As the ECB Started Tightening, Financial Constraints are a Rising Impediment to Manufacturing") +
  theme_apricitas + theme(legend.position = c(.35,.90)) +
  scale_color_manual(name= "Factors Limiting Production in EU-27 Manufacturing Firms",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(.1861*(today()-as.Date("2003-01-01")))), ymin = 0-(.3*.1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MANU_SURVEY_FIN_CONSTRAINTS_graph, "EU_MANU_SURVEY_FIN_CONSTRAINTS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#NGDP Graph
USNGDP <- fredr(series_id = "GDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
EANGDP <- fredr(series_id = "EUNNGDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
UKNGDP <- fredr(series_id = "UKNGDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
JPNNGDP <- fredr(series_id = "JPNNGDP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
CANNGDP <- fredr(series_id = "NGDPSAXDCCAQ",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")
AUSNGDP <- fredr(series_id = "NGDPSAXDCAUQ",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1")

NGDP_graph <- ggplot() + #plotting ukr GDP data
  geom_line(data=USNGDP, aes(x=date,y= value/100,color= "USA"), size = 1.25)+ 
  geom_line(data=EANGDP, aes(x=date,y= value/100,color= "Euro Area"), size = 1.25)+ 
  geom_line(data=UKNGDP, aes(x=date,y= value/100,color= "UK"), size = 1.25)+ 
  geom_line(data=JPNNGDP, aes(x=date,y= value/100,color= "Japan"), size = 1.25)+ 
  geom_line(data=CANNGDP, aes(x=date,y= value/100,color= "Canada"), size = 1.25)+ 
  geom_line(data=AUSNGDP, aes(x=date,y= value/100,color= "Australia"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year-on-Year Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.15,-.1,-0.05,0,0.05,0.1,0.15,0.2), limits = c(-.15,.20), expand = c(0,0)) +
  ggtitle("Global Monetary Inflation") +
  labs(caption = "Graph created by @JosephPolitano using FRED data", subtitle = "NGDP Growth is Well Above Pre Pandemic Levels in Most High-Income Countries (Except Japan)") +
  theme_apricitas + theme(legend.position = c(.62,.75)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "NGDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("USA","Euro Area","Japan","UK","Canada","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -.15-(.3*.35), ymax = -.15) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_graph, "NGDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#EU US HYOAS
USHYOAS <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL)
EUHYOAS <- fredr(series_id = "BAMLHE00EHYIOAS",observation_start = as.Date("2006-01-01"),realtime_start = NULL, realtime_end = NULL)

EU_US_HYOAS_Graph <- ggplot() + #plotting ice corporate index
  geom_line(data=USHYOAS, aes(x=date,y= value/100,color= "ICE BofA US Corporate Index Option-Adjusted Spread"), size = 1.25) +
  geom_line(data=EUHYOAS, aes(x=date,y= value/100,color= "ICE BofA Euro Corporate Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.25), breaks = c(0,0.05,0.10,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("The Global Nature of Monetary Policy") +
  labs(caption = "Graph created by @JosephPolitano using ICE data",subtitle = "Financial Conditions Can be Highly Correlated Across Countries") +
  theme_apricitas + theme(legend.position = c(.60,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_US_HYOAS_Graph, "EU US HYOAS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

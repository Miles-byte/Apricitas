#Productivity Nowcasting Exercise

BIZ_GVA <- fredr("A358RX1Q020SBEA", observation_start = as.Date("2004-01-01")) %>%
  transmute(date, GVA = value)
  
BIZ_HOURS_WORKED_OPT <- fredr("HOANBS", observation_start = as.Date("2004-01-01")) %>% #OFFICIAL OPT HOURS WORKED DATA
  transmute(date, OPT_HOURS = value)

BIZ_HOURS_WORKED_NFP <- fredr("AWHAE", observation_start = as.Date("2004-01-01"), frequency = "q", aggregation_method = "avg") %>% #NFP HOURS WORKED DATA USED TO NOWCAST 
  transmute(date, NFP_HOURS = value)

PROD_NOWCASTED <- left_join(BIZ_GVA,BIZ_HOURS_WORKED_OPT, by = "date") %>%
  left_join(.,BIZ_HOURS_WORKED_NFP, by = "date") %>%
  mutate(OPT_HOURS = if_else(
    row_number() == n() & is.na(OPT_HOURS),
    lag(OPT_HOURS) * (NFP_HOURS / lag(NFP_HOURS)),
    OPT_HOURS #Using nonfarm payrolls hours to nowcast the most recent OPT quarterly hours worked data if its not yet available 
  )) %>%
  transmute(date, value = GVA/OPT_HOURS)

US_NOWCASTED_LABOR_PRODUCTIVITY <- ggplot() + 
  geom_line(data=filter(PROD_NOWCASTED, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity\n(Real Output Per Hour Worked)"), size = 1.25) + 
  annotate(geom = "segment", x = as.Date("2015-01-01"), xend = as.Date("2019-10-01"), y = 100, yend = PROD_NOWCASTED$value[64]/PROD_NOWCASTED$value[45]*100, color = "#00A99D",linetype = "dashed", size = 1) +
  #annotate("text", label = paste0("Q1 2015-Q4 2019:\n+",round(((PROD_NOWCASTED$value[64]/PROD_NOWCASTED$value[45])^(4 /(64-45)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2017-02-01"), y = 106, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = paste0("Q1 2015-Q4 2019:\n+", round(((PROD_NOWCASTED$value[64] / PROD_NOWCASTED$value[45]) - 1) * 100, 1), "% Growth"), x = as.Date("2017-02-01"), y = 106, color = "#00A99D", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate(geom = "segment", x = as.Date("2019-10-01"), xend = max(PROD_NOWCASTED$date), y = PROD_NOWCASTED$value[64]/PROD_NOWCASTED$value[45]*100, yend = PROD_NOWCASTED$value[nrow(PROD_NOWCASTED)]/PROD_NOWCASTED$value[45]*100, color = "#EE6055",linetype = "dashed", size = 1) +
  #annotate("text", label = paste0("Q4 2019-",paste0("Q", lubridate::quarter(max(as.Date(PROD_NOWCASTED$date))), " ", lubridate::year(max(as.Date(PROD_NOWCASTED$date)))),"\n+",round(((PROD_NOWCASTED$value[nrow(PROD_NOWCASTED)]/PROD_NOWCASTED$value[64])^(4 /(nrow(PROD_NOWCASTED)-64)) - 1) * 100,2),"% Annualized Growth"), x = as.Date("2023-02-01"), y = 116, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = paste0("Q4 2019-", paste0("Q", lubridate::quarter(max(as.Date(PROD_NOWCASTED$date))), " ", lubridate::year(max(as.Date(PROD_NOWCASTED$date)))), "\n+", round(((PROD_NOWCASTED$value[nrow(PROD_NOWCASTED)] / PROD_NOWCASTED$value[64]) - 1) * 100, 1), "% Growth"), x = as.Date("2023-02-01"), y = 117, color = "#EE6055", size = 3.5, hjust = 0.5, lineheight = 0.8) +
  annotate("text", label = "Productivity Spikes\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-12-01"), hjust = 0.5, y = 112, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Productivity Stalls/Falls\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2020-12-01"), hjust = 0.5, y = 116, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,120), breaks = c(95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index Q1 2015 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Cumulative US Labor Productivity Growth Has Matched Pre-COVID Levels Since 2020") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NOWCASTED_OVERALL_LABOR_PRODUCTIVITY, "US Nowcasted Overall Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_NOWCASTED_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY <- ggplot() +
  geom_line(data=filter(MANUFACTURING_PRODUCTIVITY, date>= as.Date("2005-01-01")), aes(x=date,y= value/value[1]*100,color= "Manufacturing Sector Labor Productivity"), size = 1.25) +
  geom_line(data=filter(PROD_NOWCASTED, date>= as.Date("2005-01-01")), aes(x=date,y= value/value[1]*100,color= "Overall US Labor Productivity"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,140), breaks = c(95,100,105,110,115,120,125,130,135,140), expand = c(0,0)) +
  ylab("Index Q1 2005 = 100") +
  ggtitle("US Labor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS dat. NOTE: Labor Productivity Defined as Output per Hour Worked",subtitle = "US Labor Productivity Has Grown Significantly—Outside of The Manufacturing Sector") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Overall US Labor Productivity","Manufacturing Sector Labor Productivity")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 95-(.3*40), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY, "US Overall vs Manufacturing Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

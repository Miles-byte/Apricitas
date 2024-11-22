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

BIZ_COMP <- fredr("A132RC1", observation_start = as.Date("2004-01-01"), frequency = "q", aggregation_method = "avg") %>%
  transmute(date, GVA = value)

COMP_NOWCASTED <- left_join(BIZ_COMP,BIZ_HOURS_WORKED_OPT, by = "date") %>%
  left_join(.,BIZ_HOURS_WORKED_NFP, by = "date") %>%
  mutate(OPT_HOURS = if_else(
    row_number() == n() & is.na(OPT_HOURS),
    lag(OPT_HOURS) * (NFP_HOURS / lag(NFP_HOURS)),
    OPT_HOURS #Using nonfarm payrolls hours to nowcast the most recent OPT quarterly hours worked data if its not yet available 
  )) %>%
  transmute(date, value = GVA/OPT_HOURS)


US_NOWCASTED_OVERALL_LABOR_PRODUCTIVITY <- ggplot() + 
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
  labs(caption = "Graph created by @JosephPolitano using BLS Productivity data Nowcasted With Updated BEA GDP Data.",subtitle = "Cumulative US Labor Productivity Growth Has Exceeded Pre-COVID Levels Since 2020") +
  theme_apricitas + theme(legend.position = c(.23,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NOWCASTED_OVERALL_LABOR_PRODUCTIVITY, "Nowcasted US Overall Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

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

ggsave(dpi = "retina",plot = US_NOWCASTED_OVERALL_MANUFACTURING_LABOR_PRODUCTIVITY, "Nowcasted US Overall vs Manufacturing Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


MEDIAN_WAGE_NOMINAL <- fredr("LES1252881500Q", observation_start = as.Date("2015-01-01"))

PCEPI <- fredr("PCEPI", observation_start = as.Date("2015-01-01"))

AVERAGE_COMPENSATION_REAL_NOWCASTED <- merge(COMP_NOWCASTED,PCEPI, by = "date") %>%
  transmute(date, value = value.x/value.y)
MEDIAN_WAGE_REAL <- merge(MEDIAN_WAGE_NOMINAL,PCEPI, by = "date") %>%
  transmute(date, value = value.x/value.y)

NOWCASTED_US_PRODUCTIVITY_WAGES <- ggplot() + 
  geom_line(data=filter(PROD_NOWCASTED, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "US Labor Productivity"), size = 1.25) + 
  geom_line(data=filter(AVERAGE_COMPENSATION_REAL_NOWCASTED, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "US Real Average Compensation"), size = 1.25) + 
  geom_line(data=filter(MEDIAN_WAGE_REAL, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "US Real Median Wage"), size = 1.25) + 
  annotate("text", label = "Productivity & Wages Spike\nArtificially When Low-Wage\nWorkers are Disproportionally\nLaid Off in COVID", x = as.Date("2018-10-01"), hjust = 0.5, y = 112, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Productivity & Wages Stall/Fall\nWhen Low-Wage\nWorkers are Rehired", x = as.Date("2021-10-01"), hjust = 0.5, y = 118.5, color = "white", size = 3.5, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,120), breaks = c(95,100,105,110,115), expand = c(0,0)) +
  ylab("Index Q1 2015 = 100") +
  ggtitle("US Labor Productivity and Wages ") +
  labs(caption = "Graph created by @JosephPolitano using BLS data\nWages Deflated by PCEPI. Compensation and Productivity Hourly for Nonfarm Business. Median Wage Weekly",subtitle = "Increased Productivity Has Translated into Rising Wages & Compensation for American Workers") +
  theme_apricitas + theme(legend.position = c(.23,.915)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 97.5-(.3*20), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOWCASTED_US_PRODUCTIVITY_WAGES, "Nowcasted US Productivity Wages.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



#Business Sector
CAN_PRODUCTIVITY <- statcan_data("36-10-0206-01", "eng") %>%
  filter(VECTOR == "v1409153" & REF_DATE >= as.Date("2015-01-01")) %>%
  transmute(date = REF_DATE, value = VALUE)

#Market Sector
AUS_PRODUCTIVITY <- read_abs(series_id = "A3606058X") %>%
  mutate(date = date %m+% months(1)) %>%
  filter(date >= as.Date("2015-01-01")) %>%
  select(date,value)

#Market Sector
UK_PRODUCTIVITY <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/labourproductivity/timeseries/gyy7/prdy") %>%
  setNames(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2015-01-01"))

EU_PRODUCTIVITY_BULK <- get_eurostat("namq_10_lp_ulc",legacy_bulk_download = FALSE)

FRA_PRODUCTIVITY <- EU_PRODUCTIVITY_BULK %>%
  filter(geo == "FR", s_adj == "SCA", na_item == "RLPR_HW", unit == "I20", TIME_PERIOD >= as.Date("2015-01-01")) %>%
  transmute(date = TIME_PERIOD, value = values)

GER_PRODUCTIVITY <- EU_PRODUCTIVITY_BULK %>%
  filter(geo == "DE", s_adj == "SCA", na_item == "RLPR_HW", unit == "I20", TIME_PERIOD >= as.Date("2015-01-01")) %>%
  transmute(date = TIME_PERIOD, value = values)

ITA_PRODUCTIVITY <- EU_PRODUCTIVITY_BULK %>%
  filter(geo == "IT", s_adj == "SCA", na_item == "RLPR_HW", unit == "I20", TIME_PERIOD >= as.Date("2015-01-01")) %>%
  transmute(date = TIME_PERIOD, value = values)

JPN_GDP <- read.csv("https://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2024/qe242/tables/gaku-jk2421.csv",fileEncoding="latin1") %>%
  slice(-1:-6) %>%
  select(X) %>%
  transmute(date = seq.Date(from = as.Date("1994-01-01"), by = "quarter", length.out = nrow(.)), value = as.numeric(gsub(",","",X))) %>%
  drop_na() %>%
  filter(date >= as.Date("2015-01-01")) %>%
  mutate(value = value/value[7]*100)

#DOWNLOADED FROM HERE: https://dashboard.e-stat.go.jp/en/dataSearch
#SELECT TOTAL HOURS WORKED AND EMPLOYMENT LEVEL
JPN_AVG_HOURS_WORKED <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/US%20Service%20Sector%20Productivity/JPN_HOURS_WORKED.csv") %>%
  mutate(date = floor_date(as.Date(date), "quarter")) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarize(value = mean(value, na.rm = TRUE))

JPN_EMP_LEVEL <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/US%20Service%20Sector%20Productivity/JPN_EMPLOYMENT_LEVEL.csv") %>%
  mutate(date = floor_date(as.Date(date), "quarter")) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarize(value = mean(value, na.rm = TRUE))

JPN_HOURS_WORKED <- merge(JPN_EMP_LEVEL,JPN_AVG_HOURS_WORKED, by = "date") %>%
  transmute(date, value = value.x*value.y)

JPN_PRODUCTIVITY <- merge(JPN_GDP,JPN_HOURS_WORKED, by = "date") %>%
  transmute(date, value = value.x/value.y)

NOWCASTED_PRODUCTIVITY_2015_Graph <- ggplot() + #RGDP Index
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 0.5) +
  geom_line(data=UK_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Canada"), size = 1.25) +
  geom_line(data=GER_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "France"), size = 1.25) +
  geom_line(data=AUS_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Australia"), size = 1.25) +
  geom_line(data=JPN_PRODUCTIVITY, aes(x=date,y= value/value[1]*100,color= "Japan"), size = 1.25) +
  geom_line(data=filter(PROD_NOWCASTED, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[1]*100,color= "United States"), size = 1.25) +
  #annotate(geom = "text", label = "USE FIGURES WITH CAUTION:\n Ukrainian Refugees Boosted Pop Growth Significantly, Especially in Germany (~1.2%),\n But Also in Canada (~0.5%), Italy (~0.3%), the UK (~0.2%), and France (~0.2%)", x = as.Date("2020-03-15"), y = 107.5, color ="white", size = 4, alpha = 0.75,lineheight = 0.9) +
  #annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,120), breaks = c(90,100,110,120), expand = c(0,0)) +
  ylab("Index, 2015 Q1 = 100") +
  ggtitle("Comparing Labor Productivity Growth") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED & National Databases",subtitle = "America Has Radically Outshined Its Peers In Terms of Productivity Growth") +
  theme_apricitas + theme(legend.position = c(.19,.69), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name= "Labor Productivity\n(Real Output Per Hour Worked)\n2015 Q1 = 100",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93","#B30089"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-90-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-90-as.Date("2015-01-01"))), ymin = 95-(.3*25), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOWCASTED_PRODUCTIVITY_2015_Graph, "Nowcasted Productivity Comparison 2015 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NOWCASTED_PRODUCTIVITY_2019_Graph <- ggplot() + #RGDP Index
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  geom_line(data=UK_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Canada"), size = 1.25) +
  geom_line(data=GER_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "France"), size = 1.25) +
  geom_line(data=AUS_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Australia"), size = 1.25) +
  geom_line(data=JPN_PRODUCTIVITY, aes(x=date,y= value/value[19]*100,color= "Japan"), size = 1.25) +
  geom_line(data=filter(PROD_NOWCASTED, date>= as.Date("2015-01-01")), aes(x=date,y= value/value[19]*100,color= "United States"), size = 1.25) +
  annotate("text",label = "Pre-COVID Labor Productivity", x = as.Date("2016-03-01"), y =100.75, color = "white", size = 4) +
  #annotate(geom = "text", label = "USE FIGURES WITH CAUTION:\n Ukrainian Refugees Boosted Pop Growth Significantly, Especially in Germany (~1.2%),\n But Also in Canada (~0.5%), Italy (~0.3%), the UK (~0.2%), and France (~0.2%)", x = as.Date("2020-03-15"), y = 107.5, color ="white", size = 4, alpha = 0.75,lineheight = 0.9) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(92.5,117.5), breaks = c(90,95,100,105,110,115,120), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("Comparing Labor Productivity Growth") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED & National Databases",subtitle = "America Has Radically Outshined Its Peers In Terms of Productivity Growth") +
  theme_apricitas + theme(legend.position = c(.19,.70), legend.key.height = unit(0,"cm"), legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= "Labor Productivity\n(Real Output Per Hour Worked)\n2019 Q3 = 100",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93","#B30089"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-90-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-90-as.Date("2015-01-01"))), ymin = 92.5-(.3*25), ymax = 92.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NOWCASTED_PRODUCTIVITY_2019_Graph, "Nowcasted Productivity Comparison 2019 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

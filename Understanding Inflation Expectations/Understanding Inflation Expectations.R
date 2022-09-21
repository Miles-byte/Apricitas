pacman::p_load(readxl,rio,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#umich durable buying conditions and buy in advance of rising prices
UMICH <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/UMICH.csv") %>%
  mutate(date = as.Date(ï..date)) %>%
  mutate(AVG_BIAP = (HOM_BIAP + CAR_BIAP + DUR_BIAP)/3)
#umich inflation expectations
UMICH_IE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/4555697f56553d9e73eb11b5262952f3e28b7361/Understanding%20Inflation%20Expectations/UMICH_IE.csv") %>%
  mutate(date = as.Date(ï..date))

#Adding Atlanta Fed Business Inflation Expectations
BIE_1YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ATL_BIE_1YR.csv") %>%
  mutate(date = as.Date(ï..date))
BIE_5YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ATL_BIE_5YR.csv") %>%
  mutate(date = as.Date(ï..date))

BIE_LABOR_NONLABOR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/bielabornonlabor.csv") %>%
  mutate(date = as.Date(ï..date))


#adding Gorodnichenko firm inflation expectations
SOFIE_1YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/SOFIE_1YR.csv") %>%
  mutate(date = as.Date(ï..date))
SOFIE_5YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/SOFIE_5YR.csv") %>%
  mutate(date = as.Date(ï..date))

FIVEYEARBREAKEVEN2019 <- fredr(series_id = "T5YIE", observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #5 Year Inflation Breakevens data
FIVEYEARFWDBREAKEVEN2019 <- fredr(series_id = "T5YIFR",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) # 5 year 5 year forward breakevens data
FIVEYEARBREAKEVEN2019 <- drop_na(FIVEYEARBREAKEVEN2019)
FIVEYEARFWDBREAKEVEN2019 <- drop_na(FIVEYEARFWDBREAKEVEN2019)

ICE_INF <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ICE_USD_Inflation_Expectations_Index_Family.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

UMICH_BUYING_Graph <- ggplot() + #plotting total quits
  annotate("hline", y = 0, yintercept = 100, color = "white", size = .5) +
  geom_line(data=drop_na(select(UMICH,date,DUR_COND)), aes(x=date,y= DUR_COND,color= "Household Durables"), size = 1.25)+ 
  geom_line(data=drop_na(select(UMICH,date,CAR_CON)), aes(x=date,y= CAR_CON,color= "Motor Vehicles"), size = 1.25)+ 
  geom_line(data=drop_na(select(UMICH,date,HOM_CON)), aes(x=date,y= HOM_CON,color= "Homes"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, 100 = Neutral") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(40,60,80,100,120,140,160,180), limits = c(40,180), expand = c(0,0)) +
  ggtitle("Buyer Beware") +
  labs(caption = "Graph created by @JosephPolitano using UMich data", subtitle = "Consumers Rate Buying Conditions for All Major Purchases Near All-Time Lows") +
  theme_apricitas + theme(legend.position = c(.63,.25)) +
  scale_color_manual(name= "Buying Conditions: 3M Moving Average",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*(today()-as.Date("1960-01-01"))), xmax = as.Date("1960-01-01")-(0.049*(today()-as.Date("1960-01-01"))), ymin = 40-(.3*140), ymax = 40) +
  coord_cartesian(clip = "off")

UMICH_BIAP_Graph <- ggplot() + #plotting total quits
  geom_line(data=drop_na(select(UMICH,date,DUR_BIAP)), aes(x=date,y= DUR_BIAP/100,color= "Household Durables"), size = 1.25)+ 
  geom_line(data=drop_na(select(UMICH,date,CAR_BIAP)), aes(x=date,y= CAR_BIAP/100,color= "Motor Vehicles"), size = 1.25)+ 
  geom_line(data=drop_na(select(UMICH,date,HOM_BIAP)), aes(x=date,y= HOM_BIAP/100,color= "Homes"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50), limits = c(0,.50), expand = c(0,0)) +
  ggtitle("What Were You Expecting?") +
  labs(caption = "Graph created by @JosephPolitano using UMich data", subtitle = "Consumers Aren't Rushing Out to Make Big Purchases Before Prices Rise") +
  theme_apricitas + theme(legend.position = c(.75,.84)) +
  scale_color_manual(name= "Good Time to Buy: Prices are Going Up",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*(today()-as.Date("1960-01-01"))), xmax = as.Date("1960-01-01")-(0.049*(today()-as.Date("1960-01-01"))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

UMICH_BIAP_IE_Graph <- ggplot() + #plotting total quits
  geom_line(data=drop_na(select(UMICH,date,AVG_BIAP)), aes(x=date,y= AVG_BIAP/100*.25,color= "Good Time to Buy: Prices are Going Up, Major Purchases (rhs)"), size = 1.25)+ 
  geom_line(data=drop_na(select(UMICH_IE,date,PX_MD)), aes(x=date,y= PX_MD/100,color= "Inflation Expectations: Next Year (lhs)"), size = 1.25)+ 
  geom_line(data=drop_na(select(UMICH_IE,date,PX5_MD)), aes(x=date,y= PX5_MD/100,color= "Inflation Expectations: Next 5-10 Years (lhs)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.10), breaks = c(0,.05,.1,.15), expand = c(0,0), sec.axis = sec_axis(~.*1/.25, name="Percent",labels = scales::percent_format(accuracy = 1))) +
  scale_x_date(limits = c(as.Date("1975-01-01"),today()),breaks = as.Date(c("1970-01-01","1980-01-01","1990-01-01","2000-01-01","2010-01-01","2020-01-01")), date_labels = "%Y") +
  ggtitle("What Were You Expecting?") +
  labs(caption = "Graph created by @JosephPolitano using UMich data", subtitle = "Despite Rising Inflation Expectations, Consumers Aren't Rushing to Make Big Purchases") +
  theme_apricitas + theme(legend.position = c(.57,.84), legend.text = element_text(size = 13, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Good Time to Buy: Prices are Going Up, Major Purchases (rhs)","Inflation Expectations: Next Year (lhs)","Inflation Expectations: Next 5-10 Years (lhs)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1975-01-01")-(.1861*(today()-as.Date("1975-01-01"))), xmax = as.Date("1975-01-01")-(0.049*(today()-as.Date("1975-01-01"))), ymin = 0-(.3*.10), ymax = 0) +
  coord_cartesian(clip = "off")

BIE_Graph <- ggplot() + #plotting total quits
  geom_line(data=BIE_1YR, aes(x=date,y= bie,color= "Business Unit Cost Inflation Expectations: Next Year"), size = 1.25)+ 
  geom_line(data=BIE_5YR, aes(x=date,y= bie_5yr,color= "Business Unit Cost Inflation Expectations: Next 5-10 Years"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ggtitle("What Were You Expecting?") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Business Unit Cost Inflation Expectations Are Maybe More Accurate Predictions") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Business Unit Cost Inflation Expectations: Next Year","Business Unit Cost Inflation Expectations: Next 5-10 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-10-21")-(.1861*(today()-as.Date("2011-10-21"))), xmax = as.Date("2011-10-21")-(0.049*(today()-as.Date("2011-10-21"))), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

SOFIE_Graph <- ggplot() + #plotting total quits
  geom_line(data=SOFIE_1YR, aes(x=date,y= mean/100,color= "Firm Inflation Expectations: Next Year"), size = 1.25)+ 
  geom_line(data=SOFIE_5YR, aes(x=date,y= X5yr/100,color= "Firm Inflation Expectations: Next 5 Years"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.08), breaks = c(0,.01,.02,.03,.04,.05,0.06,0.07,0.08), expand = c(0,0)) +
  ggtitle("What Were You Expecting?") +
  labs(caption = "Graph created by @JosephPolitano using SoFIE data", subtitle = "Even CEOs Don't Have Anchored Inflation Expectations") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Firm Inflation Expectations: Next Year","Firm Inflation Expectations: Next 5 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-04-01")-(.1861*(today()-as.Date("2018-04-01"))), xmax = as.Date("2018-04-01")-(0.049*(today()-as.Date("2018-04-01"))), ymin = 0-(.3*.08), ymax = 0) +
  coord_cartesian(clip = "off")

T5YIE2019 <- ggplot() + #plotting inflation breakevens
  annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.0225, ymax = 0.0275, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data=FIVEYEARBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year Inflation Breakevens"), size = 1.25) +
  geom_line(data=FIVEYEARFWDBREAKEVEN2019, aes(x=date,y= value/100 ,color= "5 Year, 5 Year Forward Inflation Breakevens"), size = 1.25) +
  annotate("text", label = "Breakevens Approximately Consistent With 2% Inflation Target", x = as.Date("2020-05-01"), y = 0.0287, color = "#EE6055", alpha = 0.6, size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.038), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
  ylab("TIPS Breakevens, %") +
  ggtitle("Here's A Tip:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Short and Long Term Inflation Expectations are Stabilizing") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*.038), ymax = 0) +
  coord_cartesian(clip = "off")

ICE_INF_Graph <- ggplot() + #plotting total quits
  geom_line(data=subset(ICE_INF, date > as.Date("2021-12-31")), aes(x=date,y= X1Y/100,color= "Next 12 Months"), size = 1.25)+ 
  geom_line(data=subset(ICE_INF, date > as.Date("2021-12-31")), aes(x=date,y= CurrentCalendarYear/100,color= "Calendar Year 2022"), size = 1.25)+ 
  geom_line(data=subset(ICE_INF, date > as.Date("2021-12-31")), aes(x=date,y= NextCalendarYear/100,color= "Calendar Year 2023"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08), limits = c(0,.08), expand = c(0,0)) +
  ggtitle("Expect the Unexpected") +
  labs(caption = "Graph created by @JosephPolitano using ICE data", subtitle = "Market Based Measures of Inflation Expectations Expect Price Rises to Fade in 2023") +
  theme_apricitas + theme(legend.position = c(.20,.84)) +
  scale_color_manual(name= "ICE US Inflation Expectations",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-04")-(.1861*(today()-as.Date("2022-01-04"))), xmax = as.Date("2022-01-04")-(0.049*(today()-as.Date("2022-01-04"))), ymin = 0-(.3*.08), ymax = 0) +
  coord_cartesian(clip = "off")

BIE_LABOR_NONLABOR_Graph <- ggplot() + #plotting total quits
  geom_line(data=subset(BIE_LABOR_NONLABOR), aes(x=date,y= labor,color= "Labor"), size = 1.25)+ 
  geom_line(data=subset(BIE_LABOR_NONLABOR), aes(x=date,y= nonlabor,color= "Non-Labor"), size = 1.25)+
  xlab("Date") +
  ylab("Diffusion Index") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,20,40,60,80), limits = c(0,80), expand = c(0,0)) +
  ggtitle("Expect the Unexpected") +
  labs(caption = "Graph created by @JosephPolitano using ICE data", subtitle = "Market Based Measures of Inflation Expectations Expect Price Rises to Fade in 2023") +
  theme_apricitas + theme(legend.position = c(.20,.84)) +
  scale_color_manual(name= "Future Influence of Costs on Prices, Next Year",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-04")-(.1861*(today()-as.Date("2022-01-04"))), xmax = as.Date("2022-01-04")-(0.049*(today()-as.Date("2022-01-04"))), ymin = 0-(.3*.08), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UMICH_BUYING_Graph, "UMICH BUYING.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = UMICH_BIAP_Graph, "UMICH BIAP GRAPH.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = UMICH_BIAP_IE_Graph, "UMICH BIAP IE.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = BIE_Graph, "BIE Graph.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = SOFIE_Graph, "SoFIE Graph.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = T5YIE2019, "T5YIE2019.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = ICE_INF_Graph, "ICE INF Graph.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
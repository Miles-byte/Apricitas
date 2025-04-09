
TPU_BULK <- read.xlsx("https://www.matteoiacoviello.com/tpu_files/tpu_web_latest.xlsx",4) 

TPU <- TPU_BULK %>%
  mutate(DATE = seq.Date(from = as.Date("1960-01-01"), by = "month", length.out = nrow(.)))

TRADE_POLICY_UNCERTAINTY_GRAPH <- ggplot() + #plotting net tightening data
  geom_line(data=filter(TPU, DATE >= as.Date("2004-01-01")), aes(x=DATE,y= TPU ,color= "Trade Policy Uncertainty Index"), size = 1.25) + 
  xlab("Date") +
  ylab("Trade Uncertainty Index Derived from News Coverage") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,100,200,300,400,500,600,700,800,900), limits = c(0,ceiling(max(TPU$TPU)/50)*50), expand = c(0,0)) +
  ggtitle("Trade Policy Uncertainty is at Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using Caldara, Dario, Matteo Iacoviello, Patrick Molligo, Andrea Prestipino, & Andrea Raffo data", subtitle = "Trump's Back and Forth Tariff Moves Have Driven Trade Policy Uncertainty to Record Highs") +
  theme_apricitas + theme(legend.position = c(.52,.92), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 0-(.3*(ceiling(max(TPU$TPU)/50)*50)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TRADE_POLICY_UNCERTAINTY_GRAPH, "TRADE POLICY UNCERTAINTY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

UMICH_INF_EXP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Trade%20Rapid%20Response/UMICH_INF_EXP.csv") %>%
  mutate(date = as.Date(date))

UMICH_INF_EXPT_GRAPH <- ggplot() + #plotting Inflation Expectations
  geom_line(data= UMICH_INF_EXP, aes(x=date,y=X1yr/100,color= "1 Year Inflation Expectations"), size = 1.25) +
  geom_line(data= UMICH_INF_EXP, aes(x=date,y=X5yr/100,color= "5-10 Year Inflation Expectations"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.06), expand = c(0,0)) +
  ylab("%") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Tariffs are Driving Consumer Inflation Fears") +
  labs(caption = "Graph created by @JosephPolitano using UMich data",subtitle = "Consumers' Inflation Expectations Have Surged Thanks to Trump's Frequent Tariff Announcements") +
  theme_apricitas + theme(legend.position = c(.22,.91), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*(.06)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UMICH_INF_EXPT_GRAPH, "UMich Inf Exp Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


TX_FUTURE_BIZ_ACTIVITY <- fredr(series_id = "FBACTSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, aggregation_method = "m")

TX_FUTURE_PRICES_PAID <- fredr(series_id = "FPRMSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, aggregation_method = "m")

TX_FUTURE_CAPEX <- fredr(series_id = "FCEXPSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL, aggregation_method = "m")

TX_DIFFUSION_INDEX_GRAPH <- ggplot() + #plotting Inflation Expectations
  geom_line(data= TX_FUTURE_BIZ_ACTIVITY, aes(x=date,y=value,color= "Future Business Activity"), size = 1.25) +
  geom_line(data= TX_FUTURE_PRICES_PAID, aes(x=date,y=value,color= "Future Input Costs"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-50,85), expand = c(0,0)) +
  ylab("Diffusion Index. Above 0 = Increasing") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Tariffs are Hurting Business Expectations") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed Manufacturing Survey data",subtitle = "Texas Manufacturers Expect Higher Input Costs and Lower Business Activity Thanks to Tariffs") +
  theme_apricitas + theme(legend.position = c(.22,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "TX Manufacturing Diffusion Index",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -50-(.3*(135)), ymax = -50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_DIFFUSION_INDEX_GRAPH, "TX Diffusion Index Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TX_OUTL_UNCR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Trade%20Rapid%20Response/TX_MANU_UNCR.csv") %>%
  mutate(date = as.Date(date))

TX_OUTL_UNCR_GRAPH <- ggplot() + #plotting Inflation Expectations
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data= TX_OUTL_UNCR, aes(x=date,y=uncr,color= "Outlook Uncertainty"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-10,75), expand = c(0,0)) +
  ylab("Diffusion Index. Above 0 = Increasing") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Tariffs are Increasing Business Uncertainty") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed Manufacturing Survey data",subtitle = "Texas Manufacturers are Experiencing Rapid Increases in Uncertainty Thanks to Tariffs") +
  theme_apricitas + theme(legend.position = c(.22,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "TX Manufacturing Diffusion Index",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -10-(.3*(85)), ymax = -10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_OUTL_UNCR_GRAPH, "TX Outlook Uncertainty Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIE_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Trade%20Rapid%20Response/BIE_DATA_2.csv") %>%
  mutate(date = as.Date(date))

BIE_DATA_GRAPH <- ggplot() + #plotting Inflation Expectations
  #geom_line(data= BIE_DATA, aes(x=date,y=var,color= "Variance in 1-Year Inflation Expectations"), size = 1.25) +
  geom_line(data= BIE_DATA, aes(x=date,y=exp,color= "Experienced Inflation over the Last Year"), size = 1.25) +
  geom_line(data= BIE_DATA, aes(x=date,y=mean,color= "1-Year Inflation Expectations"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), expand = c(0,0)) +
  ylab("%") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  ggtitle("Business Inflation Expectations are Up") +
  labs(caption = "Graph created by @JosephPolitano using UMich data",subtitle = "Business' Inflation Expectations Have Rebounded Ahead of Experienced Inflation Due to Tariffs") +
  theme_apricitas + theme(legend.position = c(.27,.85), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Business Unit Cost Inflation",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*(.05)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_DATA_GRAPH, "BIE Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CFO_SURVEY <- data.frame(category = c("Hiring","Capital Spending"),`Immigration Policy`= c(-6.9,-2.5),`Corporate Tax Policy`= c(0.8,7),`Regulatory Policy`= c(-4.6,2.2),`Trade/Tariff Policy` = c(-18.5,-13.9)) %>%
  pivot_longer(-category) %>%
  mutate(name = c("Immigration Policy","Corporate Tax Policy","Regulatory Policy","Trade/Tariff Policy","Immigration Policy","Corporate Tax Policy","Regulatory Policy","Trade/Tariff Policy")) %>%
  mutate(name = factor(name, levels = rev(c("Trade/Tariff Policy","Immigration Policy","Regulatory Policy","Corporate Tax Policy"))))

CFO_SURVEY_GRAPH <- ggplot(data = CFO_SURVEY, aes(x = name, y = value/100, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  facet_wrap(~ category, scales = "fixed") +
  xlab("Date") +
  ylab("Net Percentage of Firms, Above 0 is Net Increasing") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-.2,.1), expand = c(0,0)) +
  ggtitle("Tariffs are Hurting Hiring & Capex") +
  labs(caption = "Graph created by @JosephPolitano using Richmond Fed Data", subtitle = "CFOs expect to Have to Hire Fewer Workers and Reduce Capex Thanks to Tariff Policies") +
  theme_apricitas + theme(legend.position = "right") + theme(strip.text = element_text(size = 15, color = "white", face = "bold")) + #theme(legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  scale_fill_manual(name= "Response to Recent Policy Changes\nNet Percent of Companies",values = c("#9A348E","#00A99D","#EE6055","#FFE98F","#A7ACD9","#3083DC","#FF8E72","#6A4C93")) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CFO_SURVEY_GRAPH, "CFO Survey Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

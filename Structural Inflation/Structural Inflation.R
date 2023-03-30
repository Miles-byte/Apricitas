p_load(Quandl)

ISMProductionLead <- Quandl("ISM/BUY_PROD_MAT") %>%
  select(Date, `Average Days`)


ISM_LEAD_MANUAL <- data.frame(Date = seq.Date(from = as.Date("2022-04-01"), to = as.Date("2023-01-01"), by = "month"),
           `Average Days` = c(100,99,100,100,96,94,93,84,85,87))

colnames(ISM_LEAD_MANUAL) <- c("Date","Average Days")

ISMProductionLead <- rbind(ISMProductionLead,ISM_LEAD_MANUAL)

Quandl.api_key("fEweXnVbZ38SxxcytVJY")

ISMProductionLead_Graph <- ggplot() + 
  geom_line(data = subset(ISMProductionLead, Date > as.Date("2017-12-01")), aes(x = Date, y = `Average Days`, color = "ISM Average Lead Times, Production Materials"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,100), breaks = c(50,60,70,80,90,100), expand = c(0,0)) +
  ylab("Days") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Input Lead Times are Falling Off Record Highs") +
  theme_apricitas + theme(legend.position = c(.50,.12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 50-(.3*50), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ISMProductionLead_Graph, "ISM Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SHORT_SUPPLY <- read.csv("C:/Users/josep/Documents/Structural Inflation/Structural_Inflation.csv") %>%
  select(-total) %>%
  drop_na() %>%
  mutate(date = as.Date(date))

colnames(SHORT_SUPPLY) <- c("date","1 Month","2-3 Months","4-6 Months","7-12 Months","13-24 Months","25+ Months")

SHORT_SUPPLY <- pivot_longer(SHORT_SUPPLY, cols = `1 Month`:`25+ Months`) %>%
  mutate(name =  factor(name, levels = c("1 Month","2-3 Months","4-6 Months","7-12 Months","13-24 Months","25+ Months")))


ISM_SHORT_SUPPLY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = SHORT_SUPPLY, aes(x = date, y = value, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Number") +
  ggtitle("ISM: Commodities in Short Supply") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Semiconductors, Electrical Components, and Electronic Components Have Persistent Supply Issues") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Number of Consecutive Months in Short Supply",values = c("#ea3c2e","#d12215","#a21b10","#74130c","#460b07","#170402")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ISM_SHORT_SUPPLY_GRAPH, "ISM Short Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

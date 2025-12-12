GROSS_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("GEN_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  I_COMMODITY = "-",
)

GROSS_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_MO", "CTY_CODE", "CTY_NAME","E_COMMODITY"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  E_COMMODITY = "-"
)

GROSS_TRADE_BULK <- merge(GROSS_IMPORTS_BULK,GROSS_EXPORTS_BULK, by = "time") %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO), ALL_VAL_MO = as.numeric(ALL_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  mutate(NET_IMPORTS = GEN_VAL_MO-ALL_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01")))

NET_IMPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=GROSS_TRADE_BULK, aes(x=time,y= NET_IMPORTS*12/1000000000000,color= "US Goods Trade Deficit\nNot Seasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.25),limits = c(0,2),breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Imports Have Spiked Amid Tariff Fears") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Consumers & Businesses Rushed to Buy Goods Before Trump's Tariffs Come In Place") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(2)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NET_IMPORTS_Graph, "Net Imports NSA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#ADD NET IMPORTS SA GRAPH


GROSS_TARIFF_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=GROSS_TRADE_BULK, aes(x=time,y= CAL_DUT_MO*12/1000000000,color= "US Tariffs Collected\nNot Seasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,400),breaks = c(0,50,100,150,200,250,300,350,400), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Americans are Paying Billions in Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(400)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GROSS_TARIFF_GRAPH, "Gross Tariffs NSA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


GROSS_IMPORTS_BULK_CON <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  I_COMMODITY = "-",
)

GROSS_IMPORTS_BULK_CON <- GROSS_IMPORTS_BULK_CON %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(tariff_rate = as.numeric(CAL_DUT_MO)/as.numeric(CON_VAL_MO))

GROSS_TARIFF_PCT_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=GROSS_IMPORTS_BULK_CON, aes(x=time,y= tariff_rate,color= "US Average Effective Tariff Rate\n(Tariffs Collected as a % of Imports)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,ceiling(max(GROSS_IMPORTS_BULK_CON$tariff_rate, na.rm = TRUE) / 0.02) * 0.02),breaks = c(0,.02,.04,.06,.08,.1,.12,.14,.16,.18,.20,.22,.24,.26), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US Tariff Rates are Rapidly Rising") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Imports") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(ceiling(max(GROSS_IMPORTS_BULK_CON$tariff_rate, na.rm = TRUE) / 0.02) * 0.02)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GROSS_TARIFF_PCT_GRAPH, "Gross Tariffs PCT NSA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


CN_MX_CA_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "CANADA",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
  I_COMMODITY = "-",
)

CN_MX_CA_IMPORTS <- CN_MX_CA_IMPORTS_BULK %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA", CTY_NAME)) %>%
  group_by(CTY_NAME,time) %>%
  summarize(time,CON_VAL_MO = sum(as.numeric(CON_VAL_MO), na.rm = TRUE),CAL_DUT_MO = sum(as.numeric(CAL_DUT_MO), na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  select(-CAL_DUT_MO) %>%
  ungroup() %>%
  unique() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO)

CN_MX_CA_TARIFFS <- CN_MX_CA_IMPORTS_BULK %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA", CTY_NAME)) %>%
  group_by(CTY_NAME,time) %>%
  summarize(time,CON_VAL_MO = sum(as.numeric(CON_VAL_MO), na.rm = TRUE),CAL_DUT_MO = sum(as.numeric(CAL_DUT_MO), na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  select(-CON_VAL_MO) %>%
  ungroup() %>%
  unique() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CAL_DUT_MO)


GROSS_IMPORTS_CHINA <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  annotate("vline", x = as.Date("2025-02-01"), xintercept = as.Date("2025-02-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "10%\nTariff\nAll\nGoods", x = as.Date("2025-01-25"), y = 625, color = "white", size = 2.6, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-03-01"), xintercept = as.Date("2025-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "10%\nTariff\nAll\nGoods", x = as.Date("2025-02-26"), y = 625, color = "white", size = 2.6, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-04-01"), xintercept = as.Date("2025-04-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "125%\nTariff\nMost\nGoods", x = as.Date("2025-03-28"), y = 625, color = "white", size = 2.6, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("text", label = "115%\nPaused\nMost\nGoods", x = as.Date("2025-04-28"), y = 625, color = "white", size = 2.6, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-05-01"), xintercept = as.Date("2025-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Green\nLine\nOpens", x = as.Date("2014-04-01"), y = 27.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  geom_line(data=filter(CN_MX_CA_IMPORTS, time > as.Date("2023-12-01")), aes(x=time,y= CHINA*12/1000000000,color= "US Imports From China, Monthly Annualized"), size = 1.25) + 
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,700),breaks = c(0,100,200,300,400,500,600,700), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US Imports From China Have Fallen") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "US Imports From China Have Fallen More Than 42% From January to July") +
  theme_apricitas + theme(legend.position = c(.35,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*(600)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GROSS_IMPORTS_CHINA, "Gross Imports China Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



GROSS_TARIFF_MX_CA_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(CN_MX_CA_TARIFFS, time >= as.Date("2023-12-01")), aes(x=time,y= MEXICO*12/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=filter(CN_MX_CA_TARIFFS, time >= as.Date("2023-12-01")), aes(x=time,y= CANADA*12/1000000000,color= "Canada"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,30),breaks = c(0,5,10,15,20,25,30), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Americans are Paying Billions in Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "US Tariffs Collected on Imports From Country\nNot Seasonally Adjusted Annual Rate",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-12-01")-(.1861*(today()-as.Date("2023-12-01"))), xmax = as.Date("2023-12-01")-(0.049*(today()-as.Date("2023-12-01"))), ymin = 0-(.3*(30)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GROSS_TARIFF_MX_CA_GRAPH, "Gross Tariffs MX CA NSA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


GROSS_TARIFF_CN_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(CN_MX_CA_TARIFFS, time >= as.Date("2023-12-01")), aes(x=time,y= CHINA*12/1000000000,color= "China"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,125),breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Americans are Paying Billions in Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "US Tariffs Collected on Imports From Country\nNot Seasonally Adjusted Annual Rate",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(125)), ymax = 0) +
  coord_cartesian(clip = "off")


MX_CA_RP_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO","RP"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "CANADA",
  CTY_NAME = "MEXICO",
  I_COMMODITY = "-",
)

MX_CA_RP_IMPORTS_RP <- MX_CA_RP_IMPORTS_BULK %>%
  filter(RP != "-") %>%
  mutate(tariff = case_when(
    RP %in% c(18, 19)             ~ "USMCA Tariff-Free",
    RP %in% c(69, 61)             ~ "Tariffed",
    TRUE                          ~ "Outside USMCA Tariff-Free"
  )
) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= as.Date("2024-01-01")) %>%
  group_by(CTY_NAME,tariff,time) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE),CAL_DUT_MO = sum(CAL_DUT_MO, na.rm = TRUE)) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME))




TARIFF_BREAKDOWN_CA_MX_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=MX_CA_RP_IMPORTS_RP, aes(x=time,y= CON_VAL_MO*12/1000000000,color= tariff), size = 1.25) + 
  facet_wrap(~CTY_NAME) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,500),breaks = c(100,200,300,400,500), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Breaking Down North American Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Outside USMCA Tariff-Free"           = "#FFE98F",
      "USMCA Tariff-Free"                   = "#00A99D",
      "Tariffed"        = "#EE6055"
    ),
    breaks = c(
      "Outside USMCA Tariff-Free",
      "USMCA Tariff-Free",
      "Tariffed"
    )
  ) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(125)), ymax = 0) +
  coord_cartesian(clip = "off") + theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = TARIFF_BREAKDOWN_CA_MX_GRAPH, "Tariff Breakdown CA MX Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



18 FREE BY LEGISLATION
19 FREE BY HS CHAPTER 99

69 CHAPTER 99 RATES APPLY
61 GENERAL RATES APPLY

10 FREE NON-USMCA

GOLD_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "7108",
  I_COMMODITY = "7115900530",
)

GOLD_IMPORTS <- GOLD_IMPORTS_BULK %>%
  group_by(time, CTY_NAME) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME == "SWITZERLAND", CTY_NAME, "ALL OTHER COUNTRIES")) %>%
  group_by(time, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01")))
  

GOLD_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=GOLD_IMPORTS, aes(x=time,y= `All Other Countries`*12/1000000000,color= "All Other Countries"), size = 1.25) + 
  geom_line(data=GOLD_IMPORTS, aes(x=time,y= Switzerland*12/1000000000,color= "Switzerland"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,300),breaks = c(50,100,150,200,250,300), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Gold Imports Surged Amidst Tariff Fears") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Gold Defined as HS Code 7108 and 7115900530",subtitle = "US Gold Imports—Especially From Switzerland—Hit Record Highs Amidst Universal Tariff Fears") +
  theme_apricitas + theme(legend.position = c(.3,.87)) +
  scale_color_manual(name = "US Gold Imports, Billions, Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Switzerland","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(300)), ymax = 0) +
  coord_cartesian(clip = "off") + theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = GOLD_IMPORTS_GRAPH, "Gold Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


COMPUTER_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8471",
)

COMPUTER_IMPORTS <- COMPUTER_IMPORTS_BULK %>%
  group_by(time, CTY_NAME) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("MEXICO","CHINA","HONG KONG","MACAU","VIETNAM","TAIWAN"), CTY_NAME, "ALL OTHER COUNTRIES")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG","MACAU","CHINA"), "CHINA", CTY_NAME)) %>%
  group_by(time, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  filter(time >= as.Date("2024-01-01"))


COMPUTER_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=COMPUTER_IMPORTS, aes(x=time,y= `All Other Countries`*12/1000000000,color= "All Other Countries"), size = 1.25) + 
  geom_line(data=COMPUTER_IMPORTS, aes(x=time,y= China*12/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=COMPUTER_IMPORTS, aes(x=time,y= Mexico*12/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=COMPUTER_IMPORTS, aes(x=time,y= Taiwan*12/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=COMPUTER_IMPORTS, aes(x=time,y= Vietnam*12/1000000000,color= "Vietnam"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,125),breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Computer Imports Shifted Amidst Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Gold Defined as HS Code 8471",subtitle = "Overall US Computer Imports to Hit Record Highs Even as Imports From China Collapsed") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name = "US Computer Imports, Billions, Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Mexico","Taiwan","China","Vietnam","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*(125)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COMPUTER_IMPORTS_GRAPH, "Computer Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


LAPTOP_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "847130",
)

LAPTOP_IMPORTS <- LAPTOP_IMPORTS_BULK %>%
  group_by(time, CTY_NAME) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("MEXICO","CHINA","HONG KONG","MACAU","VIETNAM","TAIWAN"), CTY_NAME, "ALL OTHER COUNTRIES")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG","MACAU","CHINA"), "CHINA", CTY_NAME)) %>%
  group_by(time, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  filter(time >= as.Date("2024-01-01"))


LAPTOP_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=LAPTOP_IMPORTS, aes(x=time,y= `All Other Countries`*12/1000000000,color= "All Other Countries"), size = 1.25) + 
  geom_line(data=LAPTOP_IMPORTS, aes(x=time,y= China*12/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=LAPTOP_IMPORTS, aes(x=time,y= Mexico*12/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=LAPTOP_IMPORTS, aes(x=time,y= Taiwan*12/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=LAPTOP_IMPORTS, aes(x=time,y= Vietnam*12/1000000000,color= "Vietnam"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,100),breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Laptop Imports Shifted Amidst Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Gold Defined as HS Code 847130",subtitle = "Laptop Imports Shifted From China to Taiwan Amidst Higher Tariffs") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name = "US Laptop Imports, Billions, Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Mexico","Taiwan","China","Vietnam","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*(100)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LAPTOP_IMPORTS_GRAPH, "Laptop Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


PHONE_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "851713",
)

PHONE_IMPORTS <- PHONE_IMPORTS_BULK %>%
  group_by(time, CTY_NAME) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("INDIA","CHINA","HONG KONG","MACAU","VIETNAM","TAIWAN"), CTY_NAME, "ALL OTHER COUNTRIES")) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG","MACAU","CHINA"), "CHINA", CTY_NAME)) %>%
  group_by(time, CTY_NAME) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  ungroup() %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  filter(time >= as.Date("2024-01-01"))


PHONE_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=PHONE_IMPORTS, aes(x=time,y= `All Other Countries`*12/1000000000,color= "All Other Countries"), size = 1.25) + 
  geom_line(data=PHONE_IMPORTS, aes(x=time,y= China*12/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=PHONE_IMPORTS, aes(x=time,y= India*12/1000000000,color= "India"), size = 1.25) + 
  geom_line(data=PHONE_IMPORTS, aes(x=time,y= Vietnam*12/1000000000,color= "Vietnam"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,100),breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Smartphone Imports Fell Amidst Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Gold Defined as HS Code 851713",subtitle = "US Smartphone Imports From China Dropped Dramatically Admist Increased US Tariffs") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name = "US Smartphone Imports, Billions, Monthly Annualized",values = c("#FFE98F","#9A348E","#EE6055","#A7ACD9","#3083DC"), breaks = c("India","Vietnam","China","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*(100)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PHONE_IMPORTS_GRAPH, "Phone Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



US_PHARMA_IMPORTS_BULK_1 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "2903451000",
  I_COMMODITY = "2903599000",
  I_COMMODITY = "2903699000",
  I_COMMODITY = "2903780000",
  I_COMMODITY = "2903799070",
  I_COMMODITY = "2903891500",
  I_COMMODITY = "2903892000",
  I_COMMODITY = "2903897010",
  I_COMMODITY = "2903897090",
  I_COMMODITY = "2903920000",
  I_COMMODITY = "2904994000",
  I_COMMODITY = "2905299000",
  I_COMMODITY = "2905399000",
  I_COMMODITY = "2905591000",
  I_COMMODITY = "2905599000",
  I_COMMODITY = "2906195000",
  I_COMMODITY = "2906296000",
  I_COMMODITY = "2907299000",
  I_COMMODITY = "2908196000",
  I_COMMODITY = "2909191800",
  I_COMMODITY = "2909200000",
  I_COMMODITY = "2909306000",
  I_COMMODITY = "2909491000",
  I_COMMODITY = "2909491500",
  I_COMMODITY = "2909492000",
  I_COMMODITY = "2909496000",
  I_COMMODITY = "2909504010",
  I_COMMODITY = "2909504050",
  I_COMMODITY = "2909504500",
  I_COMMODITY = "2909505000",
  I_COMMODITY = "2912195000",
  I_COMMODITY = "2912492600",
  I_COMMODITY = "2914190000",
  I_COMMODITY = "2914409000",
  I_COMMODITY = "2914503000",
  I_COMMODITY = "2914505000",
  I_COMMODITY = "2914620000",
  I_COMMODITY = "2914692100",
  I_COMMODITY = "2914699000",
  I_COMMODITY = "2914794000",
  I_COMMODITY = "2915293000",
  I_COMMODITY = "2915393100",
  I_COMMODITY = "2915393500",
  I_COMMODITY = "2915394700",
  I_COMMODITY = "2915399000",
  I_COMMODITY = "2915901010",
  I_COMMODITY = "2915901050",
  I_COMMODITY = "2915901400",
  I_COMMODITY = "2915901800",
  I_COMMODITY = "2915901810",
  I_COMMODITY = "2915901890",
  I_COMMODITY = "2915902000",
  I_COMMODITY = "2915905010",
  I_COMMODITY = "2915905050",
  I_COMMODITY = "2916193000",
  I_COMMODITY = "2916195000",
  I_COMMODITY = "2916205000",
  I_COMMODITY = "2916315000",
  I_COMMODITY = "2916394600",
  I_COMMODITY = "2916397900",
  I_COMMODITY = "2917130030",
  I_COMMODITY = "2917130090",
  I_COMMODITY = "2917191000",
  I_COMMODITY = "2917197020",
  I_COMMODITY = "2917197050",
  I_COMMODITY = "2917340110",
  I_COMMODITY = "2917340150",
  I_COMMODITY = "2917393000",
  I_COMMODITY = "2918115100",
  I_COMMODITY = "2918135000",
  I_COMMODITY = "2918165010",
  I_COMMODITY = "2918165050",
  I_COMMODITY = "2918196000",
  I_COMMODITY = "2918199000",
  I_COMMODITY = "2918221000",
  I_COMMODITY = "2918225000",
  I_COMMODITY = "2918233000",
  I_COMMODITY = "2918235000",
  I_COMMODITY = "2918292000",
  I_COMMODITY = "2918296500",
  I_COMMODITY = "2918297500",
  I_COMMODITY = "2918302500",
  I_COMMODITY = "2918303000",
  I_COMMODITY = "2918309000",
  I_COMMODITY = "2918993000",
  I_COMMODITY = "2918994300",
  I_COMMODITY = "2918994700",
  I_COMMODITY = "2918995000",
  I_COMMODITY = "2919903000",
  I_COMMODITY = "2919905010",
  I_COMMODITY = "2919905050",
  I_COMMODITY = "2920905100",
  I_COMMODITY = "2921191100",
  I_COMMODITY = "2921196110",
  I_COMMODITY = "2921196140",
  I_COMMODITY = "2921196195",
  I_COMMODITY = "2921290010",
  I_COMMODITY = "2921290020",
  I_COMMODITY = "2921290030",
  I_COMMODITY = "2921290055",
  I_COMMODITY = "2921301000",
  I_COMMODITY = "2921305000",
  I_COMMODITY = "2921429000",
  I_COMMODITY = "2921460000",
  I_COMMODITY = "2921493800",
  I_COMMODITY = "2921494300",
  I_COMMODITY = "2921494500",
  I_COMMODITY = "2921495000",
  I_COMMODITY = "2921598010",
  I_COMMODITY = "2921598090",
  I_COMMODITY = "2922110000",
  I_COMMODITY = "2922140000",
  I_COMMODITY = "2922190900",
  I_COMMODITY = "2922192000",
  I_COMMODITY = "2922193300",
  I_COMMODITY = "2922196000",
  I_COMMODITY = "2922197000",
  I_COMMODITY = "2922199000",
  I_COMMODITY = "2922199610",
  I_COMMODITY = "2922199619",
  I_COMMODITY = "2922199690",
  I_COMMODITY = "2922292700",
  I_COMMODITY = "2922296100",
  I_COMMODITY = "2922298110",
  I_COMMODITY = "2922298190",
  I_COMMODITY = "2922310000",
  I_COMMODITY = "2922392500",
  I_COMMODITY = "2922394500",
  I_COMMODITY = "2922395000",
  I_COMMODITY = "2922410010",
  I_COMMODITY = "2922410090",
  I_COMMODITY = "2922425000",
  I_COMMODITY = "2922440000",
  I_COMMODITY = "2922491000",
  I_COMMODITY = "2922492600",
  I_COMMODITY = "2922493000",
  I_COMMODITY = "2922493700",
  I_COMMODITY = "2922494910",
  I_COMMODITY = "2922494915",
  I_COMMODITY = "2922494950",
  I_COMMODITY = "2922498000",
  I_COMMODITY = "2922500700",
  I_COMMODITY = "2922501000",
  I_COMMODITY = "2922501100",
  I_COMMODITY = "2922501300",
  I_COMMODITY = "2922501400",
  I_COMMODITY = "2922501700",
  I_COMMODITY = "2922502500",
  I_COMMODITY = "2922503500",
  I_COMMODITY = "2922504000",
  I_COMMODITY = "2922505000",
  I_COMMODITY = "2923100000",
  I_COMMODITY = "2923202010",
  I_COMMODITY = "2923202050",
  I_COMMODITY = "2923900100",
  I_COMMODITY = "2924110000",
  I_COMMODITY = "2924191110",
  I_COMMODITY = "2924191120",
  I_COMMODITY = "2924191130",
  I_COMMODITY = "2924191150",
  I_COMMODITY = "2924198000",
  I_COMMODITY = "2924211600",
  I_COMMODITY = "2924215000",
  I_COMMODITY = "2924291000",
  I_COMMODITY = "2924296210",
  I_COMMODITY = "2924296250",
  I_COMMODITY = "2924297100",
  I_COMMODITY = "2924297710",
  I_COMMODITY = "2924297720",
  I_COMMODITY = "2924297730",
  I_COMMODITY = "2924297790",
  I_COMMODITY = "2924299500",
  I_COMMODITY = "2925120000",
  I_COMMODITY = "2925194200",
  I_COMMODITY = "2925199100",
  I_COMMODITY = "2925210000",
  I_COMMODITY = "2925292000",
  I_COMMODITY = "2925296000",
  I_COMMODITY = "2925299000",
  I_COMMODITY = "2926301000",
  I_COMMODITY = "2926400000",
  I_COMMODITY = "2926901400",
  I_COMMODITY = "2926904300",
  I_COMMODITY = "2926904801",
  I_COMMODITY = "2927004000",
  I_COMMODITY = "2927005000",
  I_COMMODITY = "2928002500",
  I_COMMODITY = "2928003000",
  I_COMMODITY = "2928005000",
  I_COMMODITY = "2929902000",
  I_COMMODITY = "2929905015",
  I_COMMODITY = "2929905018",
  I_COMMODITY = "2929905020",
  I_COMMODITY = "2929905095",
  I_COMMODITY = "2930202010",
  I_COMMODITY = "2930202050",
  I_COMMODITY = "2930209010",
  I_COMMODITY = "2930209020",
  I_COMMODITY = "2930209050",
  I_COMMODITY = "2930306000",
  I_COMMODITY = "2930902900",
  I_COMMODITY = "2930904910",
  I_COMMODITY = "2930904920",
  I_COMMODITY = "2930904950",
  I_COMMODITY = "2930909208",
  I_COMMODITY = "2930909210",
  I_COMMODITY = "2930909212",
  I_COMMODITY = "2930909222",
  I_COMMODITY = "2930909225",
  I_COMMODITY = "2930909231",
  I_COMMODITY = "2930909235",
  I_COMMODITY = "2930909251",
  I_COMMODITY = "2931490005",
  I_COMMODITY = "2931490008",
  I_COMMODITY = "2931490010",
  I_COMMODITY = "2931490020",
  I_COMMODITY = "2931490025",
  I_COMMODITY = "2931490055",
  I_COMMODITY = "2931490080",
  I_COMMODITY = "2931530000",
  I_COMMODITY = "2931902200",
  I_COMMODITY = "2931909010",
  I_COMMODITY = "2931909021",
  I_COMMODITY = "2931909025",
  I_COMMODITY = "2931909029",
  I_COMMODITY = "2931909035",
  I_COMMODITY = "2931909040",
  I_COMMODITY = "2931909052",
  I_COMMODITY = "2932140000",
  I_COMMODITY = "2932195100",
  I_COMMODITY = "2932202000",
  I_COMMODITY = "2932203000",
  I_COMMODITY = "2932205010",
  I_COMMODITY = "2932205020",
  I_COMMODITY = "2932205030",
  I_COMMODITY = "2932205050",
  I_COMMODITY = "2932996100",
  I_COMMODITY = "2932997000",
  I_COMMODITY = "2932999010",
  I_COMMODITY = "2932999090",
  I_COMMODITY = "2933110000",
  I_COMMODITY = "2933193500",
  I_COMMODITY = "2933194500",
  I_COMMODITY = "2933199000",
  I_COMMODITY = "2933210000",
  I_COMMODITY = "2933292000",
  I_COMMODITY = "2933293500",
  I_COMMODITY = "2933294300",
  I_COMMODITY = "2933294500",
  I_COMMODITY = "2933299000",
  I_COMMODITY = "2933330100",
  I_COMMODITY = "2933340000",
  I_COMMODITY = "2933350000",
  I_COMMODITY = "2933370000",
  I_COMMODITY = "2933390800",
  I_COMMODITY = "2933391000",
  I_COMMODITY = "2933392000",
  I_COMMODITY = "2933392100",
  I_COMMODITY = "2933392300",
  I_COMMODITY = "2933392500",
  I_COMMODITY = "2933392700",
  I_COMMODITY = "2933393100",
  I_COMMODITY = "2933394100",
  I_COMMODITY = "2933396110",
  I_COMMODITY = "2933396120",
  I_COMMODITY = "2933396191",
  I_COMMODITY = "2933399200",
  I_COMMODITY = "2933410000",
  I_COMMODITY = "2933490800",
  I_COMMODITY = "2933491000",
  I_COMMODITY = "2933491500",
  I_COMMODITY = "2933491700",
  I_COMMODITY = "2933492000",
  I_COMMODITY = "2933492600",
  I_COMMODITY = "2933493000",
  I_COMMODITY = "2933496000",
  I_COMMODITY = "2933497000",
  I_COMMODITY = "2933521000",
  I_COMMODITY = "2933529000",
  I_COMMODITY = "2933530000",
  I_COMMODITY = "2933540000",
  I_COMMODITY = "2933591000",
  I_COMMODITY = "2933591500",
  I_COMMODITY = "2933591800",
  I_COMMODITY = "2933592100",
  I_COMMODITY = "2933592200",
  I_COMMODITY = "2933593600",
  I_COMMODITY = "2933594600",
  I_COMMODITY = "2933595300",
  I_COMMODITY = "2933595900",
  I_COMMODITY = "2933597000",
  I_COMMODITY = "2933598000",
  I_COMMODITY = "2933598500",
  I_COMMODITY = "2933599500",
  I_COMMODITY = "2933696010",
)


US_PHARMA_IMPORTS_BULK_2 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "2933696015",
  I_COMMODITY = "2933696021",
  I_COMMODITY = "2933696030",
  I_COMMODITY = "2933696050",
  I_COMMODITY = "2933720000",
  I_COMMODITY = "2933790800",
  I_COMMODITY = "2933791500",
  I_COMMODITY = "2933798500",
  I_COMMODITY = "2933910000",
  I_COMMODITY = "2933990100",
  I_COMMODITY = "2933990200",
  I_COMMODITY = "2933990500",
  I_COMMODITY = "2933990600",
  I_COMMODITY = "2933990800",
  I_COMMODITY = "2933991100",
  I_COMMODITY = "2933991200",
  I_COMMODITY = "2933991600",
  I_COMMODITY = "2933991701",
  I_COMMODITY = "2933992200",
  I_COMMODITY = "2933992400",
  I_COMMODITY = "2933992600",
  I_COMMODITY = "2933994200",
  I_COMMODITY = "2933994600",
  I_COMMODITY = "2933995100",
  I_COMMODITY = "2933995300",
  I_COMMODITY = "2933995510",
  I_COMMODITY = "2933995530",
  I_COMMODITY = "2933995590",
  I_COMMODITY = "2933995800",
  I_COMMODITY = "2933996100",
  I_COMMODITY = "2933996500",
  I_COMMODITY = "2933997000",
  I_COMMODITY = "2933997500",
  I_COMMODITY = "2933997900",
  I_COMMODITY = "2933998210",
  I_COMMODITY = "2933998220",
  I_COMMODITY = "2933998290",
  I_COMMODITY = "2933998500",
  I_COMMODITY = "2933998900",
  I_COMMODITY = "2933999000",
  I_COMMODITY = "2933999701",
  I_COMMODITY = "2934101000",
  I_COMMODITY = "2934102000",
  I_COMMODITY = "2934109000",
  I_COMMODITY = "2934204000",
  I_COMMODITY = "2934208000",
  I_COMMODITY = "2934302300",
  I_COMMODITY = "2934302700",
  I_COMMODITY = "2934304300",
  I_COMMODITY = "2934305000",
  I_COMMODITY = "2934910000",
  I_COMMODITY = "2934920000",
  I_COMMODITY = "2934990100",
  I_COMMODITY = "2934990300",
  I_COMMODITY = "2934990500",
  I_COMMODITY = "2934990600",
  I_COMMODITY = "2934990700",
  I_COMMODITY = "2934990800",
  I_COMMODITY = "2934990900",
  I_COMMODITY = "2934991100",
  I_COMMODITY = "2934991200",
  I_COMMODITY = "2934991500",
  I_COMMODITY = "2934991600",
  I_COMMODITY = "2934991800",
  I_COMMODITY = "2934992000",
  I_COMMODITY = "2934993000",
  I_COMMODITY = "2934993900",
  I_COMMODITY = "2934994400",
  I_COMMODITY = "2934994700",
  I_COMMODITY = "2934997000",
  I_COMMODITY = "2934999001",
  I_COMMODITY = "2935500000",
  I_COMMODITY = "2935900600",
  I_COMMODITY = "2935901000",
  I_COMMODITY = "2935901300",
  I_COMMODITY = "2935901500",
  I_COMMODITY = "2935902000",
  I_COMMODITY = "2935903000",
  I_COMMODITY = "2935903200",
  I_COMMODITY = "2935903300",
  I_COMMODITY = "2935904200",
  I_COMMODITY = "2935904800",
  I_COMMODITY = "2935906000",
  I_COMMODITY = "2935907500",
  I_COMMODITY = "2935909500",
  I_COMMODITY = "2936210000",
  I_COMMODITY = "2936220000",
  I_COMMODITY = "2936230000",
  I_COMMODITY = "2936240100",
  I_COMMODITY = "2936250000",
  I_COMMODITY = "2936260000",
  I_COMMODITY = "2936270000",
  I_COMMODITY = "2936280000",
  I_COMMODITY = "2936291000",
  I_COMMODITY = "2936291610",
  I_COMMODITY = "2936291620",
  I_COMMODITY = "2936291630",
  I_COMMODITY = "2936292000",
  I_COMMODITY = "2936295020",
  I_COMMODITY = "2936295030",
  I_COMMODITY = "2936295050",
  I_COMMODITY = "2936900110",
  I_COMMODITY = "2936900150",
  I_COMMODITY = "2937110000",
  I_COMMODITY = "2937120000",
  I_COMMODITY = "2937190000",
  I_COMMODITY = "2937210010",
  I_COMMODITY = "2937210020",
  I_COMMODITY = "2937210030",
  I_COMMODITY = "2937210040",
  I_COMMODITY = "2937220000",
  I_COMMODITY = "2937231010",
  I_COMMODITY = "2937231020",
  I_COMMODITY = "2937231050",
  I_COMMODITY = "2937232500",
  I_COMMODITY = "2937235010",
  I_COMMODITY = "2937235020",
  I_COMMODITY = "2937235050",
  I_COMMODITY = "2937291000",
  I_COMMODITY = "2937299020",
  I_COMMODITY = "2937299040",
  I_COMMODITY = "2937299050",
  I_COMMODITY = "2937299095",
  I_COMMODITY = "2937500000",
  I_COMMODITY = "2937900500",
  I_COMMODITY = "2937901000",
  I_COMMODITY = "2937902000",
  I_COMMODITY = "2937904000",
  I_COMMODITY = "2937904500",
  I_COMMODITY = "2937909000",
  I_COMMODITY = "2938100000",
  I_COMMODITY = "2938900000",
  I_COMMODITY = "2939110000",
  I_COMMODITY = "2939191000",
  I_COMMODITY = "2939192000",
  I_COMMODITY = "2939195000",
  I_COMMODITY = "2939200010",
  I_COMMODITY = "2939200050",
  I_COMMODITY = "2939300000",
  I_COMMODITY = "2939410000",
  I_COMMODITY = "2939420000",
  I_COMMODITY = "2939440000",
  I_COMMODITY = "2939450000",
  I_COMMODITY = "2939490300",
  I_COMMODITY = "2939590000",
  I_COMMODITY = "2939620000",
  I_COMMODITY = "2939630000",
  I_COMMODITY = "2939690000",
  I_COMMODITY = "2939720000",
  I_COMMODITY = "2939790000",
  I_COMMODITY = "2939800050",
  I_COMMODITY = "2940006000",
  I_COMMODITY = "2941101000",
  I_COMMODITY = "2941102000",
  I_COMMODITY = "2941103000",
  I_COMMODITY = "2941105000",
  I_COMMODITY = "2941201000",
  I_COMMODITY = "2941205000",
  I_COMMODITY = "2941300000",
  I_COMMODITY = "2941400000",
  I_COMMODITY = "2941500000",
  I_COMMODITY = "2941901010",
  I_COMMODITY = "2941901050",
  I_COMMODITY = "2941903000",
  I_COMMODITY = "2941905000",
  I_COMMODITY = "2942000500",
  I_COMMODITY = "2942003500",
  I_COMMODITY = "2942005000",
  I_COMMODITY = "3001200000",
  I_COMMODITY = "3001900110",
  I_COMMODITY = "3001900150",
  I_COMMODITY = "3001900190",
  I_COMMODITY = "3002120010",
  I_COMMODITY = "3002120020",
  I_COMMODITY = "3002120030",
  I_COMMODITY = "3002120040",
  I_COMMODITY = "3002120090",
  I_COMMODITY = "3002130010",
  I_COMMODITY = "3002130090",
  I_COMMODITY = "3002140010",
  I_COMMODITY = "3002140090",
  I_COMMODITY = "3002150011",
  I_COMMODITY = "3002150091",
  I_COMMODITY = "3002410000",
  I_COMMODITY = "3002420000",
  I_COMMODITY = "3002490010",
  I_COMMODITY = "3002490050",
  I_COMMODITY = "3002510000",
  I_COMMODITY = "3002590000",
  I_COMMODITY = "3002901000",
  I_COMMODITY = "3002905210",
  I_COMMODITY = "3002905220",
  I_COMMODITY = "3002905250",
  I_COMMODITY = "3003100000",
  I_COMMODITY = "3003200000",
  I_COMMODITY = "3003391000",
  I_COMMODITY = "3003395000",
  I_COMMODITY = "3003410000",
  I_COMMODITY = "3003420000",
  I_COMMODITY = "3003490000",
  I_COMMODITY = "3003900120",
  I_COMMODITY = "3003900140",
  I_COMMODITY = "3003900160",
  I_COMMODITY = "3003900180",
  I_COMMODITY = "3003900190",
  I_COMMODITY = "3004101010",
  I_COMMODITY = "3004101020",
  I_COMMODITY = "3004101045",
  I_COMMODITY = "3004105010",
  I_COMMODITY = "3004105045",
  I_COMMODITY = "3004105055",
  I_COMMODITY = "3004105065",
  I_COMMODITY = "3004105075",
  I_COMMODITY = "3004200010",
  I_COMMODITY = "3004200020",
  I_COMMODITY = "3004200030",
  I_COMMODITY = "3004200045",
  I_COMMODITY = "3004200055",
  I_COMMODITY = "3004200065",
  I_COMMODITY = "3004200075",
  I_COMMODITY = "3004310000",
  I_COMMODITY = "3004320000",
  I_COMMODITY = "3004390010",
  I_COMMODITY = "3004390050",
  I_COMMODITY = "3004410000",
  I_COMMODITY = "3004420000",
  I_COMMODITY = "3004490005",
  I_COMMODITY = "3004490010",
  I_COMMODITY = "3004490020",
  I_COMMODITY = "3004490030",
  I_COMMODITY = "3004490040",
  I_COMMODITY = "3004490050",
  I_COMMODITY = "3004490060",
  I_COMMODITY = "3004490070",
  I_COMMODITY = "3004501000",
  I_COMMODITY = "3004502000",
  I_COMMODITY = "3004503000",
  I_COMMODITY = "3004504000",
  I_COMMODITY = "3004505005",
  I_COMMODITY = "3004505010",
  I_COMMODITY = "3004505020",
  I_COMMODITY = "3004505030",
  I_COMMODITY = "3004505040",
  I_COMMODITY = "3004600000",
  I_COMMODITY = "3004901000",
  I_COMMODITY = "3004909201",
  I_COMMODITY = "3004909202",
  I_COMMODITY = "3004909204",
  I_COMMODITY = "3004909206",
  I_COMMODITY = "3004909208",
  I_COMMODITY = "3004909212",
  I_COMMODITY = "3004909213",
  I_COMMODITY = "3004909214",
  I_COMMODITY = "3004909215",
  I_COMMODITY = "3004909216",
  I_COMMODITY = "3004909217",
  I_COMMODITY = "3004909218",
  I_COMMODITY = "3004909219",
  I_COMMODITY = "3004909221",
  I_COMMODITY = "3004909222",
  I_COMMODITY = "3004909223",
  I_COMMODITY = "3004909225",
  I_COMMODITY = "3004909227",
  I_COMMODITY = "3004909229",
  I_COMMODITY = "3004909231",
  I_COMMODITY = "3004909232",
  I_COMMODITY = "3004909233",
  I_COMMODITY = "3004909234",
  I_COMMODITY = "3004909236",
  I_COMMODITY = "3004909237",
  I_COMMODITY = "3004909238",
  I_COMMODITY = "3004909239",
  I_COMMODITY = "3004909240",
  I_COMMODITY = "3004909242",
  I_COMMODITY = "3004909244",
  I_COMMODITY = "3004909250",
  I_COMMODITY = "3004909255",
  I_COMMODITY = "3004909256",
  I_COMMODITY = "3004909257",
  I_COMMODITY = "3004909259",
  I_COMMODITY = "3004909263",
  I_COMMODITY = "3004909266",
  I_COMMODITY = "3004909268",
  I_COMMODITY = "3004909270",
  I_COMMODITY = "3004909276",
  I_COMMODITY = "3004909280",
  I_COMMODITY = "3004909284",
  I_COMMODITY = "3004909289",
  I_COMMODITY = "3004909291",
  I_COMMODITY = "3006301000",
  I_COMMODITY = "3006305000",
  I_COMMODITY = "3006600000",
  I_COMMODITY = "3006700000",
)




US_PHARMA_IMPORTS_BULK_3 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "3006931000",
  I_COMMODITY = "3006932000",
  I_COMMODITY = "3006935000",
  I_COMMODITY = "3006936000",
  I_COMMODITY = "3006938000",
  I_COMMODITY = "3104200010",
  I_COMMODITY = "3104200050",
  I_COMMODITY = "3104300000",
  I_COMMODITY = "3104900100",
  I_COMMODITY = "3105100000",
  I_COMMODITY = "3105200000",
  I_COMMODITY = "3105600000",
  I_COMMODITY = "3203008000",
  I_COMMODITY = "3204138000",
  I_COMMODITY = "3204172000",
  I_COMMODITY = "3204180000",
  I_COMMODITY = "3206110000",
  I_COMMODITY = "3206190000",
  I_COMMODITY = "3402421000",
  I_COMMODITY = "3402422010",
  I_COMMODITY = "3402422020",
  I_COMMODITY = "3402422050",
  I_COMMODITY = "3402429000",
  I_COMMODITY = "3606903000",
  I_COMMODITY = "3808941000",
  I_COMMODITY = "3808945010",
  I_COMMODITY = "3808945050",
  I_COMMODITY = "3808945080",
  I_COMMODITY = "3808945095",
  I_COMMODITY = "3818000010",
  I_COMMODITY = "3818000090",
  I_COMMODITY = "3824910000",
  I_COMMODITY = "3824992900",
  I_COMMODITY = "3824994900",
  I_COMMODITY = "3824995500",
  I_COMMODITY = "3824999310",
  I_COMMODITY = "3824999320",
  I_COMMODITY = "3824999330",
  I_COMMODITY = "3824999361",
  I_COMMODITY = "3824999386",
  I_COMMODITY = "3824999397",
  I_COMMODITY = "3901909000",
  I_COMMODITY = "3902900010",
  I_COMMODITY = "3902900050",
  I_COMMODITY = "3904610010",
  I_COMMODITY = "3904610090",
  I_COMMODITY = "3905911000",
  I_COMMODITY = "3905998000",
  I_COMMODITY = "3906905000",
  I_COMMODITY = "3907100000",
  I_COMMODITY = "3907210000",
  I_COMMODITY = "3907290000",
  I_COMMODITY = "3907300000",
  I_COMMODITY = "3907610010",
  I_COMMODITY = "3907610050",
  I_COMMODITY = "3907690010",
  I_COMMODITY = "3907690050",
  I_COMMODITY = "3907700000",
  I_COMMODITY = "3907995010",
  I_COMMODITY = "3907995050",
  I_COMMODITY = "3908100000",
  I_COMMODITY = "3910000000",
  I_COMMODITY = "3911902500",
  I_COMMODITY = "3911909110",
  I_COMMODITY = "3911909150",
  I_COMMODITY = "3912310010",
  I_COMMODITY = "3912310090",
  I_COMMODITY = "3912390000",
  I_COMMODITY = "3912900010",
  I_COMMODITY = "3912900090",
  I_COMMODITY = "3913902015",
  I_COMMODITY = "3913902090",
  I_COMMODITY = "3913905000",
  I_COMMODITY = "3914006000",
)



US_PHARMA_IMPORTS <- #US_PHARMA_IMPORTS %>%
  rbind(US_PHARMA_IMPORTS_BULK_1,US_PHARMA_IMPORTS_BULK_2,US_PHARMA_IMPORTS_BULK_3) %>%
  #filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  filter(!CTY_NAME %in% c("AUSTRIA","BELGIUM","BULGARIA","CROATIA","CYPRUS","CZECH REPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HUNGARY","IRELAND","ITALY","LATVIA","LITHUANIA","LUXEMBOURG","MALTA","NETHERLANDS","POLAND","PORTUGAL","ROMANIA","SLOVAKIA","SLOVENIA","SPAIN","SWEDEN")) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  group_by(CTY_NAME, time) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  arrange(time) %>%
  #mutate(across(where(is.numeric), ~rollapply(.x, 12, sum, fill = NA, align = "right"))) %>%
  ungroup() %>%
  pivot_longer(-time) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  #filter(time == as.Date("2024-12-01"))
  mutate(name = case_when(
    name == "EUROPEAN UNION" ~ "EU",
    #name == "IRELAND" ~ "Ireland",
    name == "SINGAPORE" ~ "Singapore",
    name == "SWITZERLAND" ~ "Switzerland",
    name == "INDIA" ~ "India",
    name == "JAPAN" ~ "Japan",
    name == "CHINA" ~ "China",
    name == "HONG KONG" ~ "China",
    name == "MACAU" ~ "China",
    name == "CANADA" ~ "Canada",
    TRUE ~ "Other"
  )) %>%
  group_by(time) %>%
  group_by(name,time) %>%
  summarise(value = sum(value)) %>%
  drop_na() %>%
  pivot_wider() %>%
  filter(time>=as.Date("2024-01-01"))

US_PHARMA_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*Other/1000000000,color= "Other"), size = 1.25) + 
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*China/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*Canada/1000000000,color= "Canada"), size = 1.25) + 
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*Switzerland/1000000000,color= "Switzerland"), size = 1.25) + 
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*India/1000000000,color= "India"), size = 1.25) + 
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*Singapore/1000000000,color= "Singapore"), size = 1.25) + 
  geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= 12*EU/1000000000,color= "EU"), size = 1.25) + 
  #geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= Ireland/1000000000,color= "Ireland"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,500), breaks = c(0,100,200,300,400,500), expand = c(0,0)) +
  ylab("Billions of Dollars, 12MMA") +
  ggtitle("Pharma Imports Spiked Admidst Tariff Fears") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Pharma Imports Spiked to Record Highs in March, Especially From the European Union") +
  theme_apricitas + 
  scale_color_manual(name= "US Pharma Imports, Monthly Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("EU","Singapore","Switzerland","China","India","Canada","South Korea","Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*500), ymax = 0) +
  theme_apricitas + theme(plot.title = element_text(size = 26)) + theme(legend.position = c(.3,.75), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_PHARMA_IMPORTS_GRAPH, "US Pharma Import Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



CAR_IMPORTS_TIMELINE_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO","I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8703220100", 
  I_COMMODITY = "8703220110", 
  I_COMMODITY = "8703220190", 
  I_COMMODITY = "8703230110",
  I_COMMODITY = "8703230120",
  I_COMMODITY = "8703230130",
  I_COMMODITY = "8703230140",
  I_COMMODITY = "8703230160",
  I_COMMODITY = "8703230170",
  I_COMMODITY = "8703230190",
  I_COMMODITY = "8703240110",
  I_COMMODITY = "8703240130",
  I_COMMODITY = "8703240140",
  I_COMMODITY = "8703240150",
  I_COMMODITY = "8703240160",
  I_COMMODITY = "8703240190",
  I_COMMODITY = "8703310100",
  I_COMMODITY = "8703320110",
  I_COMMODITY = "8703320150", 
  I_COMMODITY = "8703330110", 
  I_COMMODITY = "8703330130", 
  I_COMMODITY = "8703330145", 
  I_COMMODITY = "8703330185",
  I_COMMODITY = "870340",
  I_COMMODITY = "870350",
  I_COMMODITY = "870360",
  I_COMMODITY = "870370",
  I_COMMODITY = "870380",
  I_COMMODITY = "8703900100",
  I_COMMODITY = "8704210100",
  I_COMMODITY = "8704310020", 
  I_COMMODITY = "8704310040", 
  I_COMMODITY = "8704310120", 
  I_COMMODITY = "8704310140", 
  I_COMMODITY = "870441", 
  I_COMMODITY = "870451",
  I_COMMODITY = "870460",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "KOREA, SOUTH",
  CTY_NAME = "JAPAN",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CANADA",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
  CTY_NAME = "UNITED KINGDOM",
)

CAR_IMPORTS_TIMELINE <- CAR_IMPORTS_TIMELINE_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA", CTY_NAME)) %>%
  #filter(CTY_NAME != "CHINA") %>%
  group_by(CTY_NAME,time) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(Other = `TOTAL FOR ALL COUNTRIES` - rowSums(select(., where(is.numeric), -`TOTAL FOR ALL COUNTRIES`))) %>%
  select(-`TOTAL FOR ALL COUNTRIES`) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  filter(time >= as.Date("2024-01-01"))

US_CAR_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("vline", x = as.Date("2025-04-01"), xintercept = as.Date("2025-04-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "25%\nCar\nTariff", x = as.Date("2025-03-28"), y = 100, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*CANADA/1000000000,color= "Canada"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*`EUROPEAN UNION`/1000000000,color= "EU"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*JAPAN/1000000000,color= "Japan"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*`KOREA, SOUTH`/1000000000,color= "South Korea"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*MEXICO/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*`UNITED KINGDOM`/1000000000,color= "UK"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*`CHINA`/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=CAR_IMPORTS_TIMELINE, aes(x=time,y= 12*`Other`/1000000000,color= "Other"), size = 1.25) + 
  #geom_line(data=US_PHARMA_IMPORTS, aes(x=time,y= Ireland/1000000000,color= "Ireland"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,110), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Billions of Dollars, Not Seasonally Adjusted") +
  ggtitle("Car Imports Fell Admidst Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Car Imports Dropped in April As Tariffs on Vehicle Imports Jumped to 25%") +
  theme_apricitas + 
  scale_color_manual(name= "US Car Imports, Monthly Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("Mexico","EU","Japan","South Korea","Canada","UK","China","Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*125), ymax = 0) +
  theme_apricitas + theme(legend.position = c(.35,.9), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(ncol = 4, byrow = FALSE))

ggsave(dpi = "retina",plot = US_CAR_IMPORTS_GRAPH, "US Car Import Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



ALUMINUM_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO","I_COMMODITY"), 
  time = paste("from 2023 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "7601",
  I_COMMODITY = "7604",
  I_COMMODITY = "7605",
  I_COMMODITY = "7606",
  I_COMMODITY = "7607",
  I_COMMODITY = "7608",
  I_COMMODITY = "7609",
  I_COMMODITY = "7616995120",
  I_COMMODITY = "7616995130",
  I_COMMODITY = "7616995140",
  I_COMMODITY = "7616995150",
  I_COMMODITY = "7616995160",
  I_COMMODITY = "7616995170",
  I_COMMODITY = "7616995175",
  I_COMMODITY = "7616995190",
)

ALUMINUM_IMPORTS <- ALUMINUM_IMPORTS_BULK %>%
  group_by(time) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  ungroup() %>%
  unique() %>%
  #mutate(across(where(is.numeric), ~ . - lag(., 12))) %>%
  filter(time >= as.Date("2024-01-01"))


STEEL_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO","I_COMMODITY"), 
  time = paste("from 2023 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "7208",
  I_COMMODITY = "7209",
  I_COMMODITY = "7210",
  I_COMMODITY = "7211",
  I_COMMODITY = "7212",
  I_COMMODITY = "7225",
  I_COMMODITY = "7226",
  I_COMMODITY = "7213",
  I_COMMODITY = "7214",
  I_COMMODITY = "7215",
  I_COMMODITY = "7227",
  I_COMMODITY = "7228",
  I_COMMODITY = "7208",
  I_COMMODITY = "7216",
  I_COMMODITY = "7217",
  I_COMMODITY = "7229",
  I_COMMODITY = "730110",
  I_COMMODITY = "730210",
  I_COMMODITY = "730240",
  I_COMMODITY = "730290",
  I_COMMODITY = "7304",
  I_COMMODITY = "7306",
  I_COMMODITY = "7305",
  I_COMMODITY = "7206",
  I_COMMODITY = "7207",
  I_COMMODITY = "7224",
  I_COMMODITY = "7218",
  I_COMMODITY = "7219",
  I_COMMODITY = "7220",
  I_COMMODITY = "7221",
  I_COMMODITY = "7222",
  I_COMMODITY = "7223",
)

STEEL_IMPORTS <- STEEL_IMPORTS_BULK %>%
  group_by(time) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  ungroup() %>%
  unique() %>%
  #mutate(across(where(is.numeric), ~ . - lag(., 12))) %>%
  filter(time >= as.Date("2024-01-01"))


US_METAL_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("vline", x = as.Date("2025-03-01"), xintercept = as.Date("2025-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "25%\nTariff", x = as.Date("2025-02-26"), y = 45, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-06-01"), xintercept = as.Date("2025-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Additional\n25%\nTariff", x = as.Date("2025-05-26"), y = 45, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  
  geom_line(data=STEEL_IMPORTS, aes(x=time,y= 12*CON_VAL_MO/1000000000,color= "Steel"), size = 1.25) + 
  geom_line(data=ALUMINUM_IMPORTS, aes(x=time,y= 12*CON_VAL_MO/1000000000,color= "Aluminum"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,50), breaks = c(0,10,20,30,40,50), expand = c(0,0)) +
  ylab("Billions of Dollars, Not Seasonally Adjusted") +
  ggtitle("Metal Imports Fell Admidst Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using Census data. HS Codes Based on ITA Trade Monitors",subtitle = "US Metal Imports Dropped in April After Tariffs on Imports Jumped to 25%") +
  theme_apricitas + 
  scale_color_manual(name= "US Imports, Monthly Annualized",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("Steel","Aluminum")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*50), ymax = 0) +
  theme_apricitas + theme(legend.position = c(.35,.9), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = US_METAL_IMPORTS_GRAPH, "US Metal Import Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE




CN_MX_CA_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_MO", "CTY_CODE","E_COMMODITY","CTY_NAME","DF"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "CANADA",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
  E_COMMODITY = "-",
  DF = "1",
)

CN_MX_CA_EXPORTS <- CN_MX_CA_EXPORTS_BULK %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA", CTY_NAME)) %>%
  group_by(CTY_NAME,time) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  summarize(time,ALL_VAL_MO = sum(ALL_VAL_MO, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  ungroup() %>%
  unique() %>%
  pivot_wider(names_from = CTY_NAME, values_from = ALL_VAL_MO) %>%
  mutate(across(where(is.numeric), ~ . - lag(., 12))) %>%
  filter(time >= as.Date("2024-01-01"))

CN_MX_CA_EXPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CN_MX_CA_EXPORTS, aes(x=time,y= `CHINA`*12/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=CN_MX_CA_EXPORTS, aes(x=time,y= MEXICO*12/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=CN_MX_CA_EXPORTS, aes(x=time,y= CANADA*12/1000000000,color= "Canada"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-55,55),breaks = c(-50,-25,0,25,50), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US Exports to Key Markets Dropped") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. China Includes HK & MO",subtitle = "In April, US Exports to Canada, Mexico, & China Have Dropped Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name = "Year-on-Year Change in US Exports,\nBillions, Monthly Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Canada","Mexico","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = -55-(.3*(110)), ymax = -55) +
  coord_cartesian(clip = "off") + theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = CN_MX_CA_EXPORTS_GRAPH, "CN MX CA Exports Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE




CAR_EXPORTS_TIMELINE_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_MO","E_COMMODITY","DF"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "8703220100", 
  E_COMMODITY = "8703220110", 
  E_COMMODITY = "8703220190", 
  E_COMMODITY = "8703230110",
  E_COMMODITY = "8703230120",
  E_COMMODITY = "8703230130",
  E_COMMODITY = "8703230140",
  E_COMMODITY = "8703230160",
  E_COMMODITY = "8703230170",
  E_COMMODITY = "8703230190",
  E_COMMODITY = "8703240110",
  E_COMMODITY = "8703240130",
  E_COMMODITY = "8703240140",
  E_COMMODITY = "8703240150",
  E_COMMODITY = "8703240160",
  E_COMMODITY = "8703240190",
  E_COMMODITY = "8703310100",
  E_COMMODITY = "8703320110",
  E_COMMODITY = "8703320150", 
  E_COMMODITY = "8703330110", 
  E_COMMODITY = "8703330130", 
  E_COMMODITY = "8703330145", 
  E_COMMODITY = "8703330185",
  E_COMMODITY = "870340",
  E_COMMODITY = "870350",
  E_COMMODITY = "870360",
  E_COMMODITY = "870370",
  E_COMMODITY = "870380",
  E_COMMODITY = "8703900100",
  E_COMMODITY = "8704210100",
  E_COMMODITY = "8704310020", 
  E_COMMODITY = "8704310040", 
  E_COMMODITY = "8704310120", 
  E_COMMODITY = "8704310140", 
  E_COMMODITY = "870441", 
  E_COMMODITY = "870451",
  E_COMMODITY = "870460",
  DF = "1",
)

CAR_EXPORTS <- CAR_EXPORTS_TIMELINE_BULK %>%
  group_by(time) %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  summarize(time,ALL_VAL_MO = sum(ALL_VAL_MO, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  ungroup() %>%
  unique() %>%
  mutate(across(where(is.numeric), ~ . - lag(., 12))) %>%
  filter(time >= as.Date("2024-01-01"))

CAR_EXPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CAR_EXPORTS, aes(x=time,y= `ALL_VAL_MO`*12/1000000000,color= "Year-on-Year Change in US Car Exports,\nBillions, Monthly Annualized"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-25,25),breaks = c(-25,-20,-15,-10,-5,0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US Vehicle Exports Dropped in April") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Vehicles Defined by Trump 232 Tariff List",subtitle = "In April, US Vehicle Exports Dropped Dramatically Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name = NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-01-01")-(.1861*(today()-as.Date("2024-01-01"))), xmax = as.Date("2024-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = -25-(.3*(50)), ymax = -25) +
  coord_cartesian(clip = "off") + theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = CAR_EXPORTS_GRAPH, "Car Exports Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


GROSS_IMPORTS_BULK_RP <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO","RP"),
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  I_COMMODITY = "-",
)

PERCENT_TARIFFED <- GROSS_IMPORTS_BULK_RP %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  filter(RP != "-") %>%
  mutate(Tariffed = CAL_DUT_MO >0) %>%
  group_by(time, Tariffed) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO), CAL_DUT_MO = sum(CAL_DUT_MO)) %>%
  select(-CAL_DUT_MO) %>%
  pivot_wider(names_from = Tariffed, values_from = CON_VAL_MO) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(time,"-01")), Tariffed = `TRUE`, Not_Tariffed = `FALSE`) %>%
  mutate(pct_tariffed = Tariffed/(Tariffed+Not_Tariffed))
  

PERCENT_TARIFFED_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=PERCENT_TARIFFED, aes(x=date,y= Tariffed/(Tariffed+Not_Tariffed),color= "Percent of US Imports with Tariffs"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1),breaks = c(0,.25,.5,.75,1), expand = c(0,0)) +
  ylab("Percent of Total Imports, Dollar Value") +
  ggtitle("Most US Imports are Still Tariff-Free") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Although Trump Has Dramatically Increased Tariff Rates, Most Imports Remain Unaffected") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name = NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2024-01-01"))), ymin = 0-(.3*(1)), ymax = 0) +
  coord_cartesian(clip = "off") + theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = PERCENT_TARIFFED_GRAPH, "Percent Tariffed Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

IN_BZ_RP_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "CTY_CODE","I_COMMODITY","CTY_NAME","CAL_DUT_MO","RP"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  CTY_NAME = "BRAZIL",
  CTY_NAME = "INDIA",
  I_COMMODITY = "-",
)


IN_BZ_RP_IMPORTS_RP <- IN_BZ_RP_IMPORTS_BULK %>%
  filter(RP != "-") %>%
  mutate(tariff = case_when(
    RP %in% c(18, 19)             ~ "Tariff-Free",
    RP %in% c(69, 61)             ~ "Tariffed",
    TRUE                          ~ "Tariff-Free"
  )
  ) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= as.Date("2024-01-01")) %>%
  group_by(CTY_NAME,tariff,time) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE),CAL_DUT_MO = sum(CAL_DUT_MO, na.rm = TRUE)) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  unique() %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = c("India", setdiff(unique(CTY_NAME), "India"))))




TARIFF_BREAKDOWN_IN_BZ_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_col(data=IN_BZ_RP_IMPORTS_RP, aes(x=time,y= CON_VAL_MO*12/1000000000,fill= tariff, position = "stack"), size = 1.25) + 
  facet_wrap(~CTY_NAME) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,500),breaks = c(100,200,300,400,500), expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("Breaking Down North American Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Tariff-Free"                   = "#00A99D",
      "Tariffed"        = "#EE6055"
    ),
    breaks = c(
      "Tariff-Free",
      "Tariffed"
    )
  ) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(125)), ymax = 0) +
  coord_cartesian(clip = "off") + theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = TARIFF_BREAKDOWN_CA_MX_GRAPH, "Tariff Breakdown CA MX Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

TARIFF_BREAKDOWN_IN_BZ_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("vline",
           x = as.Date("2025-08-08"),
           xintercept = as.Date("2025-08-08"),
           color = "white",
           size = 1,
           alpha = 0.75,
           linetype = "dashed") +
  annotate("text",
           x = as.Date("2025-08-14"),
           y = 125,
           label = "50%\nTariffs",
           color = "white",
           size = 4,
           size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  geom_bar(data = IN_BZ_RP_IMPORTS_RP,
           aes(x = time, y = CON_VAL_MO*12/1000000000, fill = tariff),
           stat = "identity",
           position = "stack",
           color = NA) +
  facet_wrap(~CTY_NAME) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),
                     limits = c(0,150),
                     breaks = c(0,50,100,150,200),
                     expand = c(0,0)) +
  ylab("Dollars, Not Seasonally Adjusted Annual Rate") +
  ggtitle("US Imports From India & Brazil\nWhere Trump Raised Tariffs to 50%") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",
       subtitle = "Trade with Brazil & India are Decreasing Amidst Trump's Ultra-High Tariffs") +
  theme_apricitas + theme(legend.position = c(.15,.85)) +
  scale_fill_manual(
    name = NULL,
    values = c("Tariff-Free" = "#00A99D",
               "Tariffed"    = "#EE6055"),
    breaks = c("Tariff-Free","Tariffed")
  ) +
  annotation_custom(
    apricitas_logo_rast,
    xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))),
    xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))),
    ymin = 0-(.3*(125)),
    ymax = 0
  ) +
  coord_cartesian(clip = "off") +
  theme(strip.text = element_text(size = 15, color = "white", face = "bold"))

ggsave(dpi = "retina",plot = TARIFF_BREAKDOWN_IN_BZ_GRAPH, "Tariff Breakdown IN BZ Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

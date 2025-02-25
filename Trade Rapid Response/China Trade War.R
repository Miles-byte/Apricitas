CHINA_ELECTRONICS_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8517", #Phones
  I_COMMODITY = "8471", #Computers
  I_COMMODITY = "9504", #Video Game Consoles
  I_COMMODITY = "8528", #TVs/Monitors
  I_COMMODITY = "8473", #Computer Parts
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
)

CHINA_ELECTRONICS_IMPORTS <- CHINA_ELECTRONICS_IMPORTS_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO), date = as.Date(paste0(time,"-01-01"))) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "China",
             CTY_NAME == "HONG KONG" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  group_by(date, I_COMMODITY, CTY_NAME) %>%
  summarize(CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  ungroup() %>%
  group_by(I_COMMODITY) %>%
  arrange(I_COMMODITY,date) %>%
  mutate(roll_China = rollsum(China,12,na.pad = TRUE, align = "right"),roll_Total = rollsum(`TOTAL FOR ALL COUNTRIES`,12,na.pad = TRUE, align = "right")) %>%
  mutate(China_share = China/`TOTAL FOR ALL COUNTRIES`, roll_china_share = roll_China/roll_Total) %>%
  ungroup() %>%
  drop_na()

CHINA_ELECTRONICS_IMPORTS_DOLLAR_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8471"), aes(x=date,y=roll_China/1000000000,color= "Computers"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8517"), aes(x=date,y=roll_China/1000000000,color= "Phones"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "9504"), aes(x=date,y=roll_China/1000000000,color= "Video Game Consoles"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8528"), aes(x=date,y=roll_China/1000000000,color= "TVs/Monitors"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8473"), aes(x=date,y=roll_China/1000000000,color= "Computer Parts"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0, 80), expand = c(0,0)) +
  ylab("Billions of Dollars, Rolling 12M Total") +
  #ggtitle("Solar's Exponential Moment") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  labs(subtitle = "By Dollar Value") +
  scale_color_manual(name = NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Phones","Computers","TVs/Monitors","Video Game Consoles","Computer Parts")) +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"))

CHINA_ELECTRONICS_IMPORTS_SHARE_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8471"), aes(x=date,y=roll_china_share,color= "Computers"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8517"), aes(x=date,y=roll_china_share,color= "Phones"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "9504"), aes(x=date,y=roll_china_share,color= "Video Game Consoles"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8528"), aes(x=date,y=roll_china_share,color= "TVs/Monitors"), size = 1.25) +
  geom_line(data= filter(CHINA_ELECTRONICS_IMPORTS, I_COMMODITY == "8473"), aes(x=date,y=roll_china_share,color= "Computer Parts"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1), expand = c(0,0)) +
  ylab("% of US Imports in Category, Rolling 12M Total") +
  #ggtitle("Solar's Exponential Moment") +
  theme_apricitas + theme(legend.position = c(.415,.72)) +
  labs(subtitle = "By % of US Imports") +
  scale_color_manual(name = NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Phones","Computers","TVs/Monitors","Video Game Consoles","Computer Parts")) +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"))

Title_Text <- paste("US Key Electronics Imports From China")

tgrob <- text_grob(paste0(Title_Text),face = "bold",size = 29,color = "white")

# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0.5,0,0.5, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-.15,-0.15,-.15),"cm"))  

CHINA_ELECTRONICS_IMPORTS_GRAPH <- ggarrange(CHINA_ELECTRONICS_IMPORTS_DOLLAR_GRAPH,CHINA_ELECTRONICS_IMPORTS_SHARE_GRAPH, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

CHINA_ELECTRONICS_IMPORTS_GRAPH <- ggarrange(plot_0,CHINA_ELECTRONICS_IMPORTS_GRAPH, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(.,bottom = text_grob("\nGraph Created by @Josephpolitano Using Census Data\nNOTE: Breakdowns Based on 4-Digit HS Codes", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = CHINA_ELECTRONICS_IMPORTS_GRAPH, "China Consumer Electronics Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_CHINA_SOY_EXPORTS_BULK <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","E_COMMODITY_LDESC","CTY_NAME","CTY_CODE","DF"),
  DF = 1,
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  E_COMMODITY = "1201",
  CTY_NAME = Countries[1],
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[2],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

CHINA_SOY_EXPORTS <- US_CHINA_SOY_EXPORTS_BULK %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO), date = as.Date(paste0(time,"-01-01"))) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "China",
                              CTY_NAME == "HONG KONG" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  group_by(date, CTY_NAME) %>%
  summarize(ALL_VAL_MO = sum(ALL_VAL_MO, na.rm = TRUE)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = ALL_VAL_MO) %>%
  ungroup() %>%
  mutate(roll_China = rollsum(China,12,na.pad = TRUE, align = "right"),roll_Total = rollsum(`TOTAL FOR ALL COUNTRIES`,12,na.pad = TRUE, align = "right")) %>%
  mutate(China_share = China/`TOTAL FOR ALL COUNTRIES`, roll_china_share = roll_China/roll_Total) %>%
  drop_na()

CHINA_SOY_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CHINA_SOY_EXPORTS, aes(x=date,y= roll_China/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=CHINA_SOY_EXPORTS, aes(x=date,y= (roll_Total-roll_China)/1000000000,color= "Rest of World"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-07-01"), xintercept = as.Date("2018-07-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "China\nRetaliatory\nTariffs",x = as.Date("2018-08-01"), y = 21, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "Phase 1\nDeal\nSigned",x = as.Date("2020-02-01"), y = 21, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Soy Got Hit Hard in Trump's 1st Trade War") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Soybeans are HS Code 1201",subtitle = "Chinese Trade Restrictions Hit Soybean Exporters During Trump's First Trade War") +
  theme_apricitas + theme(legend.position = c(.175,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Exports of Soybeans",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(25)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_SOY_Graph, "China Soy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

commodities <- c("270111","270112","270119", "270120", "270210", "270220", "271111","2709","8419331000","841934","842441","842449","842482","843210","843221","843229","843231","843239","843241","843242","843280","843311","843319","843320","843330","843340","843351","843352","843353","843359","843360","843410","843610","843621","843629","843680","843710","843780","843860","847920","870110","870130","870191","870192","870193","870194","870195","870323","870323","870324","870421","870431","870441","870451","870460","871620")

results_list <- list()

for (commodity in commodities) {
  data <- getCensus(
    key = Sys.getenv("CENSUS_KEY"),
    name = "timeseries/intltrade/exports/hs",
    vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","E_COMMODITY_LDESC","CTY_NAME", "CTY_CODE", "DF"),
    DF = 1,
    time = paste("from 2013 to", format(Sys.Date(), "%Y")),
    E_COMMODITY = commodity,
    CTY_NAME = Countries[1],
    CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  )
  results_list[[commodity]] <- data
}

US_CHINA_RETALIATION_SECTOR <- bind_rows(results_list)

n_distinct(US_CHINA_RETALIATION_SECTOR$E_COMMODITY_1)

CHINA_RETALIATION_EXPORTS <- US_CHINA_RETALIATION_SECTOR %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO), date = as.Date(paste0(time,"-01-01"))) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "China",
                              CTY_NAME == "HONG KONG" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  #group_by(CTY_NAME, E_COMMODITY) %>%
  select(-CTY_NAME_1,-CTY_CODE) %>%
  pivot_wider(names_from = CTY_NAME, values_from = ALL_VAL_MO) %>%
  mutate(across(c("China", "TOTAL FOR ALL COUNTRIES"), ~ replace_na(.x, 0))) %>%
  group_by(E_COMMODITY) %>%
  mutate(roll_China = rollsum(China,12,na.pad = TRUE, align = "right"),roll_Total = rollsum(`TOTAL FOR ALL COUNTRIES`,12,na.pad = TRUE, align = "right")) %>%
  mutate(China_share = China/`TOTAL FOR ALL COUNTRIES`, roll_china_share = roll_China/roll_Total) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(category = case_when(
    str_starts(E_COMMODITY, "27") ~ "Energy",
    str_starts(E_COMMODITY, "84") ~ "Agricultural Machinery",
    str_starts(E_COMMODITY, "87") ~ "Trucks & Tractors",
    TRUE ~ as.character(NA)
  )) %>%
  group_by(date,category) %>%
  summarize(roll_China = sum(roll_China, na.rm = TRUE))

CHINA_RETALIATION_EXPORTS_Stacked <- ggplot(CHINA_RETALIATION_EXPORTS, aes(fill=category, x=date, y=roll_China/1000000000)) + 
  geom_bar(position="stack", stat="identity", width = 32, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  ylab("GW of Capacity, Monthly Average") + 
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), breaks = c(0,5,10,15,20), limits = c(0,20), expand = c(0,0)) +
  ggtitle("China's Tariff Retaliation") +
  labs(caption = "Graph created by @JosephPolitano using US Census data.\nEnergy is Affected Goods of HS Code Category 27, Ag Machinery 84, Trucks and Tractors 87", subtitle = "China Hit a Small Amount of Goods—Mostly Energy Products—With Retaliatory Tariffs") +
  theme_apricitas + theme(legend.position = c(.3,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "US Exports Hit by Chinese Retaliatory Tariffs",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#F5B041","#9A348E","#00A99D","#EE6055","#FFE98F")),breaks = c("Energy","Trucks & Tractors","Agricultural Machinery")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_RETALIATION_EXPORTS_Stacked, "China Retaliation Exports Stacked.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHINA_TOTAL_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "-", #Phones
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
)

CHINA_TOTAL_IMPORTS <- CHINA_TOTAL_IMPORTS_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01-01"))) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "China",
                              CTY_NAME == "HONG KONG" ~ "China",
                              CTY_NAME == "MACAU" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  group_by(CTY_NAME,date) %>%
  summarize(CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(roll_China = rollsum(China,12,na.pad = TRUE, align = "right"),roll_Total = rollsum(`TOTAL FOR ALL COUNTRIES`,12,na.pad = TRUE, align = "right")) %>%
  drop_na() %>%
  mutate(roll_China_share = roll_China/roll_Total)

CHINA_TOTAL_IMPORTS_SHARE_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CHINA_TOTAL_IMPORTS, aes(x=date,y= roll_China/roll_Total,color= "Share of US Goods Imports\nComing From China\nRolling 12M Average"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-07-01"), xintercept = as.Date("2018-07-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "1st US-China\nTrade War Begins",x = as.Date("2018-09-01"), y = .25, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.3), breaks = c(0,.05,.10,.15,.20,.25,.30), expand = c(0,0)) +
  ylab("% of Total US Imports") +
  ggtitle("China is a Falling Share of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census data.",subtitle = "Existing Trade Restrictions Have Reduced the Share of US Imports Coming From China to 13.4%") +
  theme_apricitas + theme(legend.position = c(.185,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(.3)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_TOTAL_IMPORTS_SHARE_Graph, "China Total Imporst Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CHINA_BATTERY_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8507", #Phones
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
)

CHINA_BATTERY_IMPORTS <- CHINA_BATTERY_IMPORTS_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01-01"))) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "China",
                              CTY_NAME == "HONG KONG" ~ "China",
                              CTY_NAME == "MACAU" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  group_by(CTY_NAME,date) %>%
  summarize(CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(roll_China = rollsum(China,12,na.pad = TRUE, align = "right"),roll_Total = rollsum(`TOTAL FOR ALL COUNTRIES`,12,na.pad = TRUE, align = "right")) %>%
  drop_na()

CHINA_BATTERY_IMPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CHINA_BATTERY_IMPORTS, aes(x=date,y= roll_China/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=CHINA_BATTERY_IMPORTS, aes(x=date,y= (roll_Total-roll_China)/1000000000,color= "Rest of World"), size = 1.25) + 
  annotate("vline", x = as.Date("2024-05-01"), xintercept = as.Date("2024-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate(geom = "text", label = "Tarriffs on\nChinese\nBatteries\nAnnounced",x = as.Date("2024-03-01"), y = 21, size = 3.5,color = "white",hjust = 1, lineheight = 0.8) +
  annotate("vline", x = as.Date("2024-09-01"), xintercept = as.Date("2024-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate(geom = "text", label = "Tariffs on\nChinese\nEV Batteries\nImplemented",x = as.Date("2024-11-01"), y = 21, size = 3.5,color = "white",hjust = 0, lineheight = 0.8) +
  #annotate("vline", x = as.Date("2024-09-01"), xintercept = as.Date("2024-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate(geom = "text", label = "Universal\n10% Tariffs\non China\nImplemented",x = as.Date("2025-02-01"), y = 21, size = 3.5,color = "white",hjust = 0, lineheight = 0.8) +
  #annotate("vline", x = as.Date("2026-01-01"), xintercept = as.Date("2026-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate(geom = "text", label = "Tariffs on\nChinese\nNon-EV Batteries\nImplemented",x = as.Date("2026-03-01"), y = 21, size = 3.5,color = "white",hjust = 0, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("China is a Key Source of US Batteries") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Batteries are HS Code 8507. China Includes HK & MO.",subtitle = "China is the Single Largest Foreign Source of US Batteries, And Tariffs are Rapidly Increasing") +
  theme_apricitas + theme(legend.position = c(.175,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Imports of Batteries",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(25)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_BATTERY_IMPORTS_Graph, "China Battery Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


UMICH
EXPORTS TARIFFED
UMICH
AG EXPORTS
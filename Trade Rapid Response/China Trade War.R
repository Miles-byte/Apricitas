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
  CTY_NAME = "CHINA",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "HONG KONG",
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
  ylab("Billions of Dollars, 12M Moving Average") + 
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), breaks = c(0,5,10,15,20), limits = c(0,20), expand = c(0,0)) +
  ggtitle("China's Tariff Retaliation") +
  labs(caption = "Graph created by @JosephPolitano using US Census data.\nEnergy is Affected Goods of HS Code Category 27, Ag Machinery 84, Trucks and Tractors 87", subtitle = "China Hit a Small Amount of Goods—Mostly Energy Products—With Retaliatory Tariffs") +
  theme_apricitas + theme(legend.position = c(.29,.84)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
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



US_SOY_EXPORTS_BULK <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","CTY_NAME","CTY_CODE"),
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  E_COMMODITY = "1201", #Soy
  #E_COMMODITY = "5201", #Cotton
  #E_COMMODITY = "100790", #Grain Sorghum
  CTY_NAME = "CHINA",
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_SOY_EXPORTS <- US_SOY_EXPORTS_BULK %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  mutate(rollsum = rollsum(ALL_VAL_MO,12,na.pad = TRUE, align = "right")) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(time,"-01-01")))

US_COTTON_EXPORTS_BULK <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","CTY_NAME","CTY_CODE"),
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #E_COMMODITY = "1201", #Soy
  E_COMMODITY = "5201", #Cotton
  #E_COMMODITY = "100790", #Grain Sorghum
  CTY_NAME = "CHINA",
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_COTTON_EXPORTS <- US_COTTON_EXPORTS_BULK %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  mutate(rollsum = rollsum(ALL_VAL_MO,12,na.pad = TRUE, align = "right")) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(time,"-01-01")))
  

US_SORGHUM_EXPORTS_BULK <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY","CTY_NAME","CTY_CODE"),
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #E_COMMODITY = "1201", #Soy
  #E_COMMODITY = "5201", #Cotton
  E_COMMODITY = "100790", #Grain Sorghum
  CTY_NAME = "CHINA",
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_SORGHUM_EXPORTS <- US_SORGHUM_EXPORTS_BULK %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  mutate(rollsum = rollsum(ALL_VAL_MO,12,na.pad = TRUE, align = "right")) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(time,"-01-01")))


CHINA_KEY_AG_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(US_SOY_EXPORTS, date >= as.Date("2016-01-01")), aes(x=date,y= rollsum/1000000000,color= "Soy"), size = 1.25) + 
  geom_line(data=filter(US_COTTON_EXPORTS, date >= as.Date("2016-01-01")), aes(x=date,y= rollsum/1000000000,color= "Cotton"), size = 1.25) + 
  geom_line(data=filter(US_SORGHUM_EXPORTS, date >= as.Date("2016-01-01")), aes(x=date,y= rollsum/1000000000,color= "Sorghum"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-07-01"), xintercept = as.Date("2018-07-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "1st Chinese\nRetaliatory\nTariffs",x = as.Date("2018-08-01"), y = 21, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "Phase 1\nDeal\nSigned",x = as.Date("2020-02-01"), y = 21, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  annotate("vline", x = as.Date("2025-03-01"), xintercept = as.Date("2025-03-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2nd Chinese\nRetaliatory\nTariffs",x = as.Date("2025-02-01"), y = 21, size = 4,color = "white",hjust = 1, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Major US Crops Hit by Retaliatory Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Soybeans are HS Code 1201",subtitle = "Chinese Tariffs Have Hit >$20B in US Agricultural Exports, Especially Soy, Cotton, and Sorghum") +
  theme_apricitas + theme(legend.position = c(.15,.82), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Exports to China",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Soy","Cotton","Sorghum")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*(25)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_KEY_AG_Graph, "China Key Ag Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


COMPUTER_IMPORTS_CHINA_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8471", #Computers
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "CHINA",
  CTY_NAME = "THAILAND",
  CTY_NAME = "MEXICO",
  CTY_NAME = "TAIWAN",
  CTY_NAME = "VIETNAM",
)

COMPUTER_IMPORTS_BREAKDOWN <- COMPUTER_IMPORTS_CHINA_BULK %>%
  select(time,CON_VAL_MO,CTY_NAME) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  pivot_wider(values_from = CON_VAL_MO, names_from = CTY_NAME) %>%
  mutate(OTHER = `TOTAL FOR ALL COUNTRIES`-`THAILAND`-`VIETNAM`-`CHINA`-`TAIWAN`-`MEXICO`) %>%
  mutate(across(where(is.numeric), ~ rollapplyr(.x, width = 12, sum, fill = NA))) %>%
  mutate(date = as.Date(paste0(time,"-01-01")))


LAPTOP_IMPORTS_CHINA_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "847130", #Computers
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "CHINA",
  CTY_NAME = "THAILAND",
  CTY_NAME = "MEXICO",
  CTY_NAME = "TAIWAN",
  CTY_NAME = "VIETNAM",
)

LAPTOP_IMPORTS_BREAKDOWN <- LAPTOP_IMPORTS_CHINA_BULK %>%
  select(time,CON_VAL_MO,CTY_NAME) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  pivot_wider(values_from = CON_VAL_MO, names_from = CTY_NAME) %>%
  mutate(across(where(is.numeric),~ replace_na(.x, 0))) %>%
  mutate(OTHER = `TOTAL FOR ALL COUNTRIES`-`THAILAND`-`VIETNAM`-`CHINA`-`TAIWAN`-`MEXICO`) %>%
  mutate(across(where(is.numeric), ~ rollapplyr(.x, width = 12, sum, fill = NA))) %>%
  mutate(date = as.Date(paste0(time,"-01-01")))

COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN <- merge(COMPUTER_IMPORTS_BREAKDOWN,LAPTOP_IMPORTS_BREAKDOWN, by = "date") %>%
  transmute(date, MEXICO = MEXICO.x-MEXICO.y, CHINA = CHINA.x-CHINA.y, TAIWAN = TAIWAN.x-TAIWAN.y,VIETNAM = VIETNAM.x-VIETNAM.y, THAILAND = THAILAND.x-THAILAND.y, OTHER = OTHER.x-OTHER.y)
  

US_COMPUTER_IMPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(COMPUTER_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `OTHER`/1000000000,color= "Other"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `THAILAND`/1000000000,color= "Thailand"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `VIETNAM`/1000000000,color= "Vietnam"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `TAIWAN`/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `MEXICO`/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `CHINA`/1000000000,color= "China"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-07-01"), xintercept = as.Date("2018-07-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "China\nTrade War\nBegins",x = as.Date("2018-08-01"), y = 59, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  # annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate(geom = "text", label = "Phase 1\nDeal\nSigned",x = as.Date("2020-02-01"), y = 21, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  # annotate("vline", x = as.Date("2025-03-01"), xintercept = as.Date("2025-03-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate(geom = "text", label = "2nd Chinese\nRetaliatory\nTariffs",x = as.Date("2025-02-01"), y = 21, size = 4,color = "white",hjust = 1, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,64), breaks = c(0,20,40,60), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Shifting US Computer Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Computers are HS Code 8471",subtitle = "Mexico is the Number-1 Source of Computer Imports, With Taiwan Surging as Well") +
  theme_apricitas + theme(legend.position = c(.15,.52), plot.title = element_text(size = 27),legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13))) +
  scale_color_manual(name= "US Imports by Country",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("China","Mexico","Taiwan","Vietnam","Thailand","Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*(60)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_COMPUTER_IMPORTS_Graph, "US Computer Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_COMPUTER_XLAPTOP_IMPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=filter(COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `OTHER`/1000000000,color= "Other"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `THAILAND`/1000000000,color= "Thailand"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `VIETNAM`/1000000000,color= "Vietnam"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `TAIWAN`/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `MEXICO`/1000000000,color= "Mexico"), size = 1.25) + 
  geom_line(data=filter(COMPUTER_X_LAPTOP_IMPORTS_BREAKDOWN, date >= as.Date("2015-01-01")), aes(x=date,y= `CHINA`/1000000000,color= "China"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-07-01"), xintercept = as.Date("2018-07-01"), color = "white", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "China\nTrade War\nBegins",x = as.Date("2018-08-01"), y = 40, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  #annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = 1.25, linetype = "dashed") +
  #annotate(geom = "text", label = "Phase 1\nDeal\nSigned",x = as.Date("2020-02-01"), y = 21, size = 4,color = "white",hjust = 0, lineheight = 0.8) +
  #annotate("vline", x = as.Date("2025-03-01"), xintercept = as.Date("2025-03-01"), color = "white", size = 1.25, linetype = "dashed") +
  #annotate(geom = "text", label = "2nd Chinese\nRetaliatory\nTariffs",x = as.Date("2025-02-01"), y = 21, size = 4,color = "white",hjust = 1, lineheight = 0.8) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,60), breaks = c(0,20,40,60), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("US Non-Laptop Computer Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. HS Code 8471 ex 847130",subtitle = "Mexico is the Number-1 Source of Computer Imports Ex-Laptops, With Taiwan Surging to #2") +
  theme_apricitas + theme(legend.position = c(.15,.78), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "US Imports by Country",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Mexico","China","Taiwan","Vietnam","Thailand","Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*(60)), ymax = 0) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = US_COMPUTER_XLAPTOP_IMPORTS_Graph, "US Computer X-Laptop Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CHINA_YOY_TRADE_GROWTH <- data.frame(US = c(-0.117,4.658,7.442,8.089,5.081,2.832,8.036,7.238,15.896,12.369,-11.374,8.834,-21.03), 
                                     XUS = c(-0.1150,6.995,7.977,6.422,8.959,1.297,13.297,6.934,9.678,4.989,-1.612,12.755,12.781),
                                     date = seq.Date(from = as.Date("2024-04-01"), to = as.Date("2025-04-01"), by = "month")) %>%
  mutate(US = US/100, XUS = XUS/100)

CHINA_YOY_TRADE_GROWTH_GRAPH <- ggplot() + #plotting Chinese PV Exports India
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data= CHINA_YOY_TRADE_GROWTH, aes(x=date,y=XUS,color= "Chinese Exports to Other Countries, % Growth Compared to Last Year"), size = 1.25) +
  geom_line(data= CHINA_YOY_TRADE_GROWTH, aes(x=date,y=US,color= "Chinese Exports to the US, % Growth Compared to Last Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.3, .2), expand = c(0,0)) +
  ylab("Exports, Monthly, Growth From Last Year, %") +
  ggtitle("Chinese Exports to the US Fell in April") +
  theme_apricitas + theme(legend.position = c(.47,.25)) +
  labs(caption = "Graph created by @JosephPolitano using PRC GACC Data",subtitle = "Chinese Exports to the US Dipped More than 20% in April Amidst the Peak of The Trade War") +
  scale_color_manual(name = NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Chinese Exports to the US, % Growth Compared to Last Year","Chinese Exports to Other Countries, % Growth Compared to Last Year")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-04-01")-(.1861*(today()-as.Date("2024-04-01"))), xmax = as.Date("2024-04-01")-(0.049*(today()-as.Date("2024-04-01"))), ymin = -.3-(.3*(.5)), ymax = -.3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_YOY_TRADE_GROWTH_GRAPH, "China Yoy Trade Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



AGG_RECIPROCAL_TARIFF_RATE_CAPS_OLD <- merge(US_IMPORTS_BULK,US_EXPORTS_BULK, by = "CTY_NAME") %>%
  transmute(CTY_NAME, tariff = (as.numeric(GEN_VAL_YR)-as.numeric(ALL_VAL_YR))/as.numeric(GEN_VAL_YR)) %>%
  mutate(tariff = round(tariff,2)/2) %>%
  mutate(tariff = round(tariff,2)) %>%
  mutate(tariff = ifelse(tariff < 0.1, 0.1, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME == "AFGHANISTAN", 0.1, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% c("RUSSIA","BELARUS","CUBA","KOREA, NORTH"), 0, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% EU_27, tariff[CTY_NAME == "EUROPEAN UNION"],tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% c("CHINA","HONG KONG","MACAU"), 1.25, tariff)) %>%
  mutate(tariff = ifelse(!CTY_NAME %in% c("CHINA","HONG KONG","MACAU","MEXICO","CANADA","RUSSIA","BELARUS","CUBA","KOREA, NORTH"), 0.1, tariff)) %>%
  #mutate(tariff = 0) %>%
  select(CTY_NAME, tariff)

AGG_TARIFF_ANALYSIS_OLD <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  mutate(I_COMMODITY = as.character(I_COMMODITY)) %>%
  mutate(I_COMMODITY = if_else(nchar(I_COMMODITY) == 9, paste0("0", I_COMMODITY), I_COMMODITY)) %>%
  left_join(.,AGG_RECIPROCAL_TARIFF_RATE_CAPS_OLD, by = "CTY_NAME") %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = if_else(CTY_NAME %in% c("CHINA","HONG KONG","MACAU"), tariff+.2, tariff)) %>% #Adding China 20% IEEPA Fentanyl Tariffs
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080") & CTY_NAME != "MEXICO" & CTY_NAME != "CANADA", .25, tariff)) %>% #10 digit HS codes for excluded car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c(
    "40111010", "40111050",
    "40112010", "40121940", "40122060",
    "70072151", "73202010", "83021000", "84073100", "84082020",
    "84099110", "84133010", "84133090", "84139110", "84139190",
    "84143080", "84145930", "84145965", "84148005", "84152000",
    "84212300", "84213200", "84254900", "84311000", "84821010",
    "84821050", "84821050", "84822000", "84822000", "84822000",
    "84822000", "84822000", "84822000", "84825000", "84831010",
    "84831030", "85013200", "85013300", "85014000", "85015100",
    "85015200", "85071000", "85079040", "85079080",
    "85111000", "85112000", "85113000", "85113000", "85114000",
    "85115000", "85118020", "85119060", "85119060", "85122020",
    "85122040", "85123000", "85124020", "85124040", "85129020",
    "85129060", "85129070", "85198120", "85269010", "85364100",
    "85391000", "85391000", "85443000", "87060025", "87060005",
    "87060015", "87071000", "87079050", "87079050", "87079050",
    "87081030", "87082100", "87082200", "87082900", "87083000",
    "87084011", "87084070", "87084075", "87085000", "87088000",
    "87089100", "87089360", "87089375", "87089400", "87089580",
    "87089953", "87089955", "87089958", "87089968", "87169050",
    "90292040"
  ) & CTY_NAME != "MEXICO" & CTY_NAME != "CANADA", .25, tariff)) %>% #car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000"), .25, tariff)) %>% #car exclusion
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "UNITED KINGDOM", .1124, tariff)) %>% #car exclusion
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "MEXICO", tariff-.491*tariff, tariff)) %>% #Mexico car content
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "CANADA", tariff -.6654735594*tariff, tariff)) %>% #canada car content
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("05080000", "25041050", "25049000", "25101000", "25102000", "25111010", "25111050", 
                                               "25191000", "25199010", "25199020", "25249000", "25292100", "25292200", "25302010", 
                                               "25302020", "25309010", "25309020", "25309080", "26020000", "26030000", "26050000", 
                                               "26060000", "26080000", "26100000", "26110030", "26110060", "26121000", "26140030", 
                                               "26140060", "26159030", "26159060", "26161000", "26171000", "26203000", "26209950", 
                                               "27011100", "27011200", "27011900", "27012000", "27021000", "27022000", "27030000", 
                                               "27040000", "27050000", "27060000", "27071000", "27072000", "27073000", "27074000", 
                                               "27075000", "27079100", "27079910", "27079920", "27079940", "27079951", "27079955", 
                                               "27079959", "27079990", "27081000", "27082000", "27090010", "27090020", "27101215", 
                                               "27101218", "27101225", "27101245", "27101290", "27101906", "27101911", "27101916", 
                                               "27101924", "27101925", "27101926", "27101930", "27101935", "27101940", "27101945", 
                                               "27101990", "27102005", "27102010", "27102015", "27102025", "27109100", "27109905", 
                                               "27109910", "27109916", "27109921", "27109931", "27109932", "27109939", "27109945", 
                                               "27109990", "27111100", "27111200", "27111300", "27111400", "27111900", "27112100", 
                                               "27112900", "27121000", "27122000", "27129010", "27129020", "27131100", "27131200", 
                                               "27132000", "27139000", "27141000", "27149000", "27150000", "27160000", "28012000", 
                                               "28042900", "28045000", "28046100", "28048000", "28049000", "28051910", "28051920", 
                                               "28051990"), 0, tariff)) %>% #ANNEX 2
  
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("28053000", "28111100", "28111910", "28112910", "28112920", "28121900", "28139010",
                                               "28152000", "28161000", "28164010", "28164020", "28170000", "28181010", "28181020",
                                               "28182000", "28183000", "28201000", "28211000", "28212000", "28220000", "28230000",
                                               "28252000", "28255030", "28256000", "28258000", "28259015", "28259030", "28259090",
                                               "28261200", "28263000", "28269090", "28273100", "28273945", "28273960", "28273990",
                                               "28274100", "28274950", "28275951", "28276010", "28276051", "28332100", "28332500",
                                               "28332700", "28332910", "28332945", "28332951", "28342100", "28342920", "28342951",
                                               "28366000", "28369100", "28369200", "28369910", "28369950", "28418000", "28419020",
                                               "28419040", "28432901", "28433000", "28439000", "28441010", "28441020", "28442000",
                                               "28443020", "28443050", "28444300", "28459001", "28461000", "28469020", "28469040",
                                               "28469080", "28492010", "28492020", "28499030", "28539010", "28539090", "29034510",
                                               "29035990", "29036990", "29037800", "29037990", "29038915", "29038920", "29038970",
                                               "29039200", "29049940", "29052990", "29053990", "29055910", "29055990", "29061950",
                                               "29062960", "29072990", "29081960", "29091918", "29092000", "29093060", "29094910",
                                               "29094915", "29094920", "29094960", "29095040", "29095045", "29095050", "29121950",
                                               "29124926", "29141900", "29144090", "29145030", "29145050", "29146200", "29146921",
                                               "29146990", "29147940", "29152930", "29153931", "29153935", "29153947", "29153990",
                                               "29159010", "29159014", "29159018", "29159020", "29159050", "29161930", "29161950",
                                               "29162050", "29163150", "29163946", "29163979", "29171300", "29171910", "29171970",
                                               "29173401", "29173930", "29181151", "29181350", "29181650", "29181960", "29181990",
                                               "29182210", "29182250", "29182330", "29182350", "29182920", "29182965", "29182975",
                                               "29183025", "29183030", "29183090", "29189930", "29189943", "29189947", "29189950",
                                               "29199030", "29199050", "29209051", "29211911", "29211961", "29212900", "29213010",
                                               "29213050", "29214290", "29214600", "29214938", "29214943", "29214945", "29214950",
                                               "29215980", "29221100", "29221400", "29221909", "29221920", "29221933", "29221960",
                                               "29221970", "29221990", "29221996", "29222927", "29222961", "29222981", "29223100",
                                               "29223925", "29223945", "29223950", "29224100", "29224250", "29224400", "29224910",
                                               "29224926", "29224930", "29224937", "29224949", "29224980", "29225007", "29225010",
                                               "29225011", "29225013", "29225014", "29225017", "29225025", "29225035", "29225040",
                                               "29225050", "29231000", "29232020", "29239001", "29241100", "29241911", "29241980",
                                               "29242116", "29242150", "29242910", "29242962", "29242971", "29242977", "29242995",
                                               "29251200", "29251942", "29251991", "29252100", "29252920", "29252960", "29252990",
                                               "29263010", "29264000", "29269014", "29269043", "29269048", "29270040", "29270050",
                                               "29280025", "29280030", "29280050", "29299020", "29299050", "29302020", "29302090",
                                               "29303060", "29309029", "29309049", "29309092", "29314900", "29315300", "29319022",
                                               "29319090", "29321400", "29321951", "29322020", "29322030", "29322050", "29329961",
                                               "29329970", "29329990", "29331100", "29331935", "29331945", "29331990", "29332100",
                                               "29332920", "29332935", "29332943", "29332945", "29332990", "29333301", "29333400",
                                               "29333500", "29333700", "29333908", "29333910", "29333920", "29333921", "29333923",
                                               "29333925", "29333927", "29333931", "29333941", "29333961", "29333992", "29334100",
                                               "29334908", "29334910", "29334915", "29334917", "29334920", "29334926", "29334930",
                                               "29334960", "29334970", "29335210", "29335290", "29335300", "29335400", "29335910",
                                               "29335915", "29335918", "29335921", "29335922", "29335936", "29335946", "29335953",
                                               "29335959"), 0, tariff)) %>% #ANNEX 2
  
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("29335970", "29335980", "29335985", "29335995", "29336960", "29337200", "29337908",
                                               "29337915", "29337985", "29339100", "29339901", "29339902", "29339905", "29339906",
                                               "29339908", "29339911", "29339912", "29339916", "29339917", "29339922", "29339924",
                                               "29339926", "29339942", "29339946", "29339951", "29339953", "29339955", "29339958",
                                               "29339961", "29339965", "29339970", "29339975", "29339979", "29339982", "29339985",
                                               "29339989", "29339990", "29339997", "29341010", "29341020", "29341090", "29342040",
                                               "29342080", "29343023", "29343027", "29343043", "29343050", "29349100", "29349200",
                                               "29349901", "29349903", "29349905", "29349906", "29349907", "29349908", "29349909",
                                               "29349911", "29349912", "29349915", "29349916", "29349918", "29349920", "29349930",
                                               "29349939", "29349944", "29349947", "29349970", "29349990", "29355000", "29359006",
                                               "29359010", "29359013", "29359015", "29359020", "29359030", "29359032", "29359033",
                                               "29359042", "29359048", "29359060", "29359075", "29359095", "29362100", "29362200",
                                               "29362300", "29362401", "29362500", "29362600", "29362700", "29362800", "29362910",
                                               "29362916", "29362920", "29362950", "29369001", "29371100", "29371200", "29371900",
                                               "29372100", "29372200", "29372310", "29372325", "29372350", "29372910", "29372990",
                                               "29375000", "29379005", "29379010", "29379020", "29379040", "29379045", "29379090",
                                               "29381000", "29389000", "29391100", "29391910", "29391920", "29391950", "29392000",
                                               "29393000", "29394100", "29394200", "29394400", "29394500", "29394903", "29395900",
                                               "29396200", "29396300", "29396900", "29397200", "29397900", "29398000", "29400060",
                                               "29411010", "29411020", "29411030", "29411050", "29412010", "29412050", "29413000",
                                               "29414000", "29415000", "29419010", "29419030", "29419050", "29420005", "29420035",
                                               "29420050", "30012000", "30019001", "30021200", "30021300", "30021400", "30021500",
                                               "30024100", "30024200", "30024900", "30025100", "30025900", "30029010", "30029052",
                                               "30031000", "30032000", "30033910", "30033950", "30034100", "30034200", "30034900",
                                               "30039001", "30041010", "30041050", "30042000", "30043100", "30043200", "30043900",
                                               "30044100", "30044200", "30044900", "30045010", "30045020", "30045030", "30045040",
                                               "30045050", "30046000", "30049010", "30049092", "30063010", "30063050", "30066000",
                                               "30067000", "30069310", "30069320", "30069350", "30069360", "30069380", "31042000",
                                               "31043000", "31049001", "31051000", "31052000", "31056000", "32030080", "32041380",
                                               "32041720", "32041800", "32061100", "32061900", "34024210", "34024220", "34024290",
                                               "36069030", "38089410", "38089450", "38180000", "38249100", "38249929", "38249949",
                                               "38249955", "38249993", "39019090", "39029000", "39046100", "39059110", "39059980",
                                               "39069050", "39071000", "39072100", "39072900", "39073000", "39076100", "39076900",
                                               "39077000", "39079950", "39081000", "39100000", "39119025", "39119091", "39123100",
                                               "39123900", "39129000", "39139020", "39139050", "39140060", "40011000", "40012100",
                                               "40012200", "40012900", "40013000", "44011100", "44011200", "44012100", "44012200",
                                               "44013100", "44013200", "44013942", "44014100", "44014900", "44021000", "44022000",
                                               "44029001", "44031100", "44031200", "44032101", "44032201", "44032301", "44032401",
                                               "44032501", "44032601", "44034200", "44034902", "44039100", "44039301", "44039401",
                                               "44039501", "44039601", "44039700", "44039800", "44039901", "44041000", "44042000",
                                               "44050000", "44061100", "44061200", "44069100", "44069200", "44071100", "44071200",
                                               "44071300", "44071400", "44071900", "44072100", "44072200"), 0, tariff)) %>% #ANNEX 2
  
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("44072301", "44072500", "44072600", "44072700", "44072800", "44072902", "44079100",
                                               "44079200", "44079300", "44079400", "44079500", "44079600", "44079700", "44079902",
                                               "44081001", "44083101", "44083902", "44089001", "44091005", "44091010", "44091020",
                                               "44091040", "44091045", "44091050", "44091060", "44091065", "44091090", "44092105",
                                               "44092190", "44092205", "44092210", "44092225", "44092240", "44092250", "44092260",
                                               "44092265", "44092290", "44092906", "44092911", "44092926", "44092941", "44092951",
                                               "44092961", "44092966", "44092991", "44101100", "44101200", "44101900", "44109000",
                                               "44111210", "44111220", "44111230", "44111260", "44111290", "44111310", "44111320",
                                               "44111330", "44111360", "44111390", "44111410", "44111420", "44111430", "44111460",
                                               "44111490", "44119210", "44119220", "44119230", "44119240", "44119310", "44119320",
                                               "44119330", "44119360", "44119390", "44119400", "44121005", "44121090", "44123106",
                                               "44123126", "44123142", "44123145", "44123148", "44123152", "44123161", "44123192",
                                               "44123306", "44123326", "44123332", "44123357", "44123426", "44123432", "44123457",
                                               "44123910", "44123930", "44123940", "44123950", "44124100", "44124200", "44124900",
                                               "44125110", "44125131", "44125141", "44125151", "44125210", "44125231", "44125241",
                                               "44125251", "44125980", "44125990", "44125995", "44129106", "44129110", "44129131",
                                               "44129141", "44129151", "44129207", "44129211", "44129231", "44129242", "44129252",
                                               "44129958", "44129961", "44129971", "44129981", "44129991", "44129997", "44130000",
                                               "48202000", "49011000", "49019100", "49019900", "49021000", "49029010", "49029020",
                                               "49030000", "49040000", "49052000", "49059020", "49059060", "49060000", "49111000",
                                               "49119960", "49119980", "71069110", "71081210", "71101100", "71101900", "71102100",
                                               "71102900", "71103100", "71103900", "71104100", "71104900", "71129201", "71189000",
                                               "72021110", "72021150", "72021910", "72021950", "72023000", "72024100", "72024910",
                                               "72024950", "72025000", "72028000", "72029100", "72029340", "72029380", "72042100",
                                               "74010000", "74020000", "74031100", "74031200", "74031300", "74031900", "74032100",
                                               "74032200", "74032901", "74040030", "74040060", "74050010", "74050060", "74061000",
                                               "74062000", "74071015", "74071030", "74071050", "74072115", "74072130", "74072150",
                                               "74072170", "74072190", "74072916", "74072934", "74072938", "74072940", "74072950",
                                               "74081130", "74081160", "74081900", "74082100", "74082210", "74082250", "74082910",
                                               "74082950", "74091110", "74091150", "74091910", "74091950", "74091990", "74092100",
                                               "74092900", "74093110", "74093150", "74093190", "74093910", "74093950", "74093990",
                                               "74094000", "74099010", "74099050", "74099090", "74101100", "74101200", "74102130",
                                               "74102160", "74102200", "74111010", "74111050", "74112110", "74112150", "74112200",
                                               "74112910", "74112950", "74121000", "74122000", "74130010", "74130050", "74130090",
                                               "74151000", "74152100", "74152900", "74153305", "74153310", "74153380", "74153900",
                                               "74181000", "74182010", "74182050", "74192000", "74198003", "74198006", "74198009",
                                               "74198015", "74198016", "74198017", "74198030", "74198050", "75089050", "79011100",
                                               "79011210", "79011250", "79012000", "79020000", "79070060", "80011000", "80012000",
                                               "80020000", "80070050", "81011000", "81019700", "81032000", "81033000", "81039100",
                                               "81039900", "81041100", "81041900", "81042000", "81043000", "81049000", "81052030",
                                               "81052060", "81052090", "81053000", "81059000", "81061000", "81069000", "81082000",
                                               "81083000", "81089030", "81089060", "81101000", "81102000", "81109000", "81110047",
                                               "81110049", "81122100", "81122200", "81122900", "81124110", "81124150", "81124900",
                                               "81125900", "81129210", "81129230", "81129240", "81129260", "81129265", "81129910",
                                               "81129991", "85411000", "85412100", "85412900", "85413000", "85414910", "85414970",
                                               "85414980", "85414995", "85415100", "85415900", "85419000", "85423100", "85423200",
                                               "85423300", "85423900", "85429000"), 0, tariff)) %>% #Annex 2
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7601103000, 7601106030, 7601106090, 7601203000, 7601206000, 7601209030,
    7601209045, 7601209060, 7601209075, 7601209080, 7601209085, 7601209095,
    7604101000, 7604103000, 7604105000, 7604210010, 7604210090, 7604291010,
    7604291090, 7604293030, 7604293060, 7604293090, 7604295020, 7604295050,
    7604295090, 7605110000, 7605190000, 7605210000, 7605290000, 7606113030,
    7606113060, 7606116000, 7606123015, 7606123025, 7606123035, 7606123045,
    7606123055, 7606123091, 7606123096, 7606126000, 7606913055, 7606913095,
    7606916055, 7606916095, 7606923025, 7606923035, 7606926055, 7606926095,
    7607113000, 7607116010, 7607116090, 7607119030, 7607119060, 7607119090,
    7607191000, 7607193000, 7607196000, 7607201000, 7607205000, 7608100030,
    7608100090, 7608200030, 7608200090, 7609000000, 7616995160, 7616995170,
    7610100010, 7610100020, 7610100030, 7610900020, 7610900040, 7610900060,
    7610900080, 7615102015, 7615102025, 7615103015, 7615103025, 7615105020,
    7615105040, 7615107125, 7615107130, 7615107155, 7615107180, 7615109100,
    7615200000, 7616109090, 7616991000, 7616995130, 7616995140, 7616995190
  ), .25, tariff)) %>% #ALUMINUM
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7614105000, 7614902030, 7614902060, 7614904000, 7614905000, 8708103030,
    8708292130, 8302103000, 8302106030, 8302106060, 8302106090, 8302200000,
    8302303010, 8302303060, 8302413000, 8302416015, 8302416045, 8302416050,
    8302416080, 8302423010, 8302423015, 8302423065, 8302496035, 8302496045,
    8302496055, 8302496085, 8302500000, 8302603000, 8302609000, 8305100010,
    8305100050, 8306300000, 8414596590, 8415908025, 8415908045, 8415908085,
    8418998005, 8418998050, 8418998060, 8419901000, 8422900680, 8473302000,
    8473305100, 8479899596, 8481909060, 8481909085, 8486900000, 8487900040,
    8487900080, 8503009520, 8508700000, 8513902000, 8515902000, 8516905000,
    8516908050, 8517710000, 8517790000, 8529907300, 8529909760, 8536908585,
    8538100000, 8541900000, 8543908885, 8547900010, 8547900020, 8547900030,
    8547900040, 8708106010, 8708106050, 8708295160, 8708806590, 8708996890,
    8716805010, 8807300015, 8807300030, 8807300060, 9013908000, 9031909195,
    9401999081, 9403100020, 9403100040, 9403200011, 9403200016, 9403200017,
    9403200035, 9403200040, 9403200046, 9403200048, 9403200050, 9403200075,
    9403200078, 9403200082, 9403200086, 9403200090, 9403991040, 9403999010,
    9403999015, 9403999020, 9403999040, 9403999045, 9405992000, 9506114080,
    9506514000, 9506516000, 9506594040, 9506702090, 9506910010, 9506910020,
    9506910030, 9506990510, 9506990520, 9506990530, 9506991500, 9506992000,
    9506992580, 9506992800, 9506995500, 9506996080, 9507302000, 9507304000,
    9507306000, 9507308000, 9507906000, 9603908050, 6603908100
  ) & CTY_NAME != "CHINA", 0, tariff)) %>% #ALUMINUM DERIVATIVES
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7206100000,7206900000,7207110000,7207120010,7207120050,7207190030,7207190090,7207200025,
    7207200045,7207200075,7207200090,7208101500,7208103000,7208106000,7208253000,7208256000,
    7208260030,7208260060,7208270040,7208270045,7208270060,7208360030,7208360060,7208370030,
    7208370060,7208380015,7208380030,7208380090,7208390020,7208390025,7208390030,7208390090,
    7208403030,7208403060,7208406030,7208406060,7208510030,7208510045,7208510060,7208520000,
    7208530000,7208540000,7208900000,7209150000,7209160040,7209160045,7209160060,7209160070,
    7209160091,7209170040,7209170045,7209170060,7209170070,7209170091,7209181530,7209181560,
    7209182520,7209182585,7209186020,7209186090,7209250000,7209260000,7209270000,7209280000,
    7209900000,7210110000,7210120000,7210200000,7210300030,7210300060,7210410000,7210490040,
    7210490045,7210490091,7210490095,7210500020,7210500090,7210610000,7210690000,7210703000,
    7210706030,7210706060,7210706090,7210901000,7210906000,7210909000,7211130000,7211140030,
    7211140045,7211140090,7211191500,7211192000,7211193000,7211194500,7211196000,7211197530,
    7211197560,7211197590,7211231500,7211232000,7211233000,7211234500,7211236030,7211236060,
    7211236090,7211292030,7211292090,7211294500,7211296030,7211296080,7211900000,7212100000,
    7212200000,7212301030,7212301090,7212303000,7212305000,7212401000,7212405000,7212500000,
    7212600000,7213100000,7213200010,7213200080,7213913011,7213913015,7213913020,7213913093,
    7213914500,7213916000,7213990030,7213990060,7213990090,7214100000,7214200000,7214300010,
    7214300080,7214910016,7214910020,7214910060,7214910090,7214990016,7214990021,7214990026,
    7214990031,7214990036,7214990040,7214990045,7214990060,7214990075,7214990090,7215100010,
    7215100080,7215500016,7215500018,7215500020,7215500061,7215500063,7215500065,7215500090,
    7215901000,7215903000,7215905000,7216100010,7216100050,7216210000,7216220000,7216310000,
    7216320000,7216330030,7216330060,7216330090,7216400010,7216400050,7216500000,7216990010,
    7216990090,7217101000,7217102000,7217103000,7217104040,7217104045,7217104090,7217105030,
    7217105090,7217106000,7217107000,7217108010,7217108020,7217108025,7217108030,7217108045,
    7217108060,7217108075,7217108090,7217109000,7217201500,7217203000,7217204510,7217204520,
    7217204530,7217204540,7217204550,7217204560,7217204570,7217204580,7217206000,7217207500,
    7217301530,7217301560,7217303000,7217304504,7217304511,7217304520,7217304530,7217304541,
    7217304550,7217304560,7217304590,7217306000,7217307500,7217901000,7217905030,7217905060,
    7217905090,7218100000,7218910015,7218910030,7218910060,7218990015,7218990030,7218990045,
    7218990060,7218990090,7229200015,7229200090,7229900500,7229901000,7229905006,7229905008,7229905016,7229905031,
    7229905051,7229909000,7301100000,7302101010,7302101015,7302101025,7302101035,7302101045,
    7302101055,7302101065,7302101075,7302105020,7302105040,7302105060,7302400000,7302901000,
    7302909000,7304110020,7304110050,7304110080,7304191020,7304191030,7304191045,7304191060,
    7304191080,7304195020,7304195050,7304195080,7304220030,7304220045,7304220060,7304233000,
    7304236030,7304236045,7304236060,7304243010,7304243020,7304243030,7304243040,7304243045,
    7304243080,7304244010,7304244020,7304244030,7304244040,7304244050,7304244060,7304244080,
    7304246015,7304246030,7304246045,7304246060,7304246075,7304291010,7304291020,7304291030,
    7304291040,7304291050,7304291060,7304291080,7304292010,7304292020,7304292030,7304292040,
    7304292050,7304292060,7304292080,7304293110,7304293120,7304293130,7304293140,7304293150,
    7304293160,7304293180,7304294110,7304294120,7304294130,7304294140,7304294150,7304294160,
    7304294180,7304295015,7304295030,7304295045,7304295060,7304295075,7304296115,7304296130,
    7304296145,7304296160,7304296175,7304313000,7304316010,7304316050,7304390002,7304390004,
    7304390006,7304390008,7304390016,7304390020,7304390024,7304390028,7304390032,7304390036,
    7304390040,7304390044,7304390048,7304390052,7304390056,7304390062,7304390068,7304390072,
    7304390076,7304390080,7304413005,7304413015,7304413045,7304416005,7304416015,7304416045,
    7304490005,7304490015,7304490045,7304490060,7304511000,7304515005,7304515015,7304515045,
    7304515060,7304591000,7304592030,7304592040,7304592045,7304592055,7304592060,7304592070,
    7304592080,7304596000,7304598010,7304598015,7304598020,7304598025,7304598030,7304598035,
    7304598040,7304598045,7304598050,7304598055,7304598060,7304598065,7304598070,7304598080,
    7304901000,7304903000,7304905000,7304907000,7305111030,7305111060,7305115000,7305121030,
    7305121060,7305125000,7305191030,7305191060,7305195000,7305202000,7305204000,7305206000,
    7305208000,7305312000,7305314000,7305316010,7305316090,7305391000,7305395000,7305901000,
    7305905000,7306101010,7306101050,7306105010,7306105050,7306110010,7306110050,7306191010,
    7306191050,7306195110,7306195150,7306213000,7306214000,7306218010,7306218050,7306291030,
    7306291090,7306292000,7306293100,7306294100,7306296010,7306296050,7306298110,7306298150,
    7306301000,7306303000,7306305010,7306305015,7306305020,7306305025,7306305028,7306305032,
    7306305035,7306305040,7306305055,7306305085,7306305090,7306401010,7306401015,7306401090,
    7306405005,7306405015,7306405040,7306405042,7306405044,7306405062,7306405064,7306405080,
    7306405085,7306405090,7306501000,7306503000,7306505010,7306505030,7306505050,7306505070,
    7306611000,7306613000,7306615000,7306617030,7306617060,7306691000,7306693000,7306695000,
    7306697030,7306697060,7306901000,7306905000.7301201000,7301205000,7302300000,7307211000,7307221000,7307225000,7307230030,7307230090,
    7307290030,7307290090,7307911000,7307913000,7307915010,7307915030,7307915050,7307915070,
    7307923010,7307923030,7307929000,7307933010,7307933040,7307936000,7307939010,7307939040,
    7307939060,7307991000,7307993000,7307995015,7307995045,7307995060,7308100000,7308200020,
    7308200090,7308301000,7308305015,7308305025,7308305050,7308400000,7308903000,7308906000,
    7308907000,7308909530,7308909560,7308909590,7309000030,7309000090,7310100005,7310100015,
    7310100090,7310210025,7310210050,7310290020,7310290030,7310290055,7310290065,7311000030,
    7311000060,7311000090,7312100500,7312101030,7312101050,7312101070,7312102000,7312103005,
    7312103010,7312103012,7312103020,7312103045,7312103065,7312103070,7312103074,7312103080,
    7312105000,7312106030,7312106060,7312107000,7312108000,7312109030,7312109060,7312109090,
    7312900000,7313000000,7314121000,7314122000,7314123000,7314126000,7314129000,7314141000,
    7314142000,7314143000,7314146000,7314149000,7314190100,7314200000,7314311000,7314315010,
    7314315080,7314390000,7314410030,7314410040,7314410045,7314410080,7314420030,7314420060,
    7314493000,7314496000,7314500000,7315110005,7315110010,7315110045,7315110060,7315120020,
    7315120040,7315120060,7315120080,7315190000,7315201000,7315205000,7315810000,7315821000,
    7315823000,7315825000,7315827000,7315891000,7315893000,7315895000,7315900000,7316000000,
    7317001000,7317002000,7317003000,7317005501,7317005502,7317005503,7317005505,7317005507,
    7317005508,7317005511,7317005518,7317005519,7317005520,7317005530,7317005540,7317005550,
    7317005560,7317005570,7317005580,7317005590,7317006530,7317006560,7317007500,7318110000,
    7318120000,7318130030,7318130060,7318141030,7318141060,7318145020,7318145080,7318152010,
    7318152020,7318152030,7318152041,7318152046,7318152051,7318152055,7318152061,7318152065,
    7318152091,7318152095,7318154000,7318155030,7318155051,7318155056,7318155090,7318156010,
    7318156040,7318156070,7318156080,7318158020,7318158030,7318158045,7318158055,7318158066,
    7318158069,7318158082,7318158085,7318160015,7318160030,7318160045,7318160060,7318160085,
    7318190000,7318210030,7318210090,7318220000,7318230000,7318240000,7318290000,7319402010,
    7319402050,7319403000,7319405010,7319405050,7319901000,7319909000,7320103000,7320106015,
    7320106060,7320109015,7320109060,7320201000,7320205010,7320205020,7320205045,7320205060,
    7320901000,7320905010,7320905020,7320905060,7321111030,7321111060,7321113010,7321113020,
    7321113050,7321116000,7321120000,7321190020,7321190040,7321190060,7321190080,7321811000,
    7321815000,7321821000,7321825000,7321890010,7321890050,7321901000,7321902000,7321904000,
    7321905000,7321906040,7321906060,7321906090,7326908688
  ), .25, tariff)) %>% #STEEL
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7317003000,7317005503,7317005505,7317005507,7317005560,7317005580,7317006560,
    8708103020,8708292120,8431310020,8431310040,8431310060,8431420000,8431491010,
    8431491060,8431491090,8431499005,8431499010,8431499015,8431499020,8431499025,
    8431499030,8431499035,8431499036,8431499038,8431499044,8431499045,8431499050,
    8431499055,8431499081,8431499084,8431499090,8431499095,8432100020,8432100040,
    8432100060,8432900010,8432900020,8432900040,8432900050,8432900060,8432900081,
    8547900010,8547900020,8547900030,8547900040,9403200011,9403200016,9403200017,
    9403200035,9403200040,9403200046,9403200048,9403200050,9403200075,9403200078,
    9403200082,9403200086,9403200090,9403992040,9403992080,9403994005,9403994010,
    9403994080,9406200000,9406900110,9406900120,9406900130,9406900150,9406900190
  ) & CTY_NAME != "CHINA", 0, tariff)) %>% #Steel Derivatives
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853710", "853720", "870822",
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910") & CTY_NAME != "CHINA", 0.25, tariff)) %>% #Car Parts
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707") & CTY_NAME != "CHINA", 0.25, tariff)) %>% #Car Parts 
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010") & CTY_NAME != "CHINA", 0.25, tariff)) %>% #Car Batteries
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853710", "853720", "870822",
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910") & CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>% #Car Parts
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707") & CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>% #Car Parts 
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010") & CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>% #Car Batteries
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8471","8486","8524","8542") & CTY_NAME != "CHINA", 0, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("847330","851713","851762","852351","852852","854110","854121","854129","854130","854151","854159","854190") & CTY_NAME != "CHINA", 0, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("85414910","85414970","85414980","85414995") & CTY_NAME != "CHINA", 0, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8471","8486","8524","8542") & CTY_NAME == "CHINA", .2, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("847330","851713","851762","852351","852852","854110","854121","854129","854130","854151","854159","854190") & CTY_NAME == "CHINA", .20, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("85414910","85414970","85414980","85414995") & CTY_NAME == "CHINA", .20, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7206100000,7206900000,7207110000,7207120010,7207120050,7207190030,7207190090,7207200025,
    7207200045,7207200075,7207200090,7208101500,7208103000,7208106000,7208253000,7208256000,
    7208260030,7208260060,7208270040,7208270045,7208270060,7208360030,7208360060,7208370030,
    7208370060,7208380015,7208380030,7208380090,7208390020,7208390025,7208390030,7208390090,
    7208403030,7208403060,7208406030,7208406060,7208510030,7208510045,7208510060,7208520000,
    7208530000,7208540000,7208900000,7209150000,7209160040,7209160045,7209160060,7209160070,
    7209160091,7209170040,7209170045,7209170060,7209170070,7209170091,7209181530,7209181560,
    7209182520,7209182585,7209186020,7209186090,7209250000,7209260000,7209270000,7209280000,
    7209900000,7210110000,7210120000,7210200000,7210300030,7210300060,7210410000,7210490040,
    7210490045,7210490091,7210490095,7210500020,7210500090,7210610000,7210690000,7210703000,
    7210706030,7210706060,7210706090,7210901000,7210906000,7210909000,7211130000,7211140030,
    7211140045,7211140090,7211191500,7211192000,7211193000,7211194500,7211196000,7211197530,
    7211197560,7211197590,7211231500,7211232000,7211233000,7211234500,7211236030,7211236060,
    7211236090,7211292030,7211292090,7211294500,7211296030,7211296080,7211900000,7212100000,
    7212200000,7212301030,7212301090,7212303000,7212305000,7212401000,7212405000,7212500000,
    7212600000,7213100000,7213200010,7213200080,7213913011,7213913015,7213913020,7213913093,
    7213914500,7213916000,7213990030,7213990060,7213990090,7214100000,7214200000,7214300010,
    7214300080,7214910016,7214910020,7214910060,7214910090,7214990016,7214990021,7214990026,
    7214990031,7214990036,7214990040,7214990045,7214990060,7214990075,7214990090,7215100010,
    7215100080,7215500016,7215500018,7215500020,7215500061,7215500063,7215500065,7215500090,
    7215901000,7215903000,7215905000,7216100010,7216100050,7216210000,7216220000,7216310000,
    7216320000,7216330030,7216330060,7216330090,7216400010,7216400050,7216500000,7216990010,
    7216990090,7217101000,7217102000,7217103000,7217104040,7217104045,7217104090,7217105030,
    7217105090,7217106000,7217107000,7217108010,7217108020,7217108025,7217108030,7217108045,
    7217108060,7217108075,7217108090,7217109000,7217201500,7217203000,7217204510,7217204520,
    7217204530,7217204540,7217204550,7217204560,7217204570,7217204580,7217206000,7217207500,
    7217301530,7217301560,7217303000,7217304504,7217304511,7217304520,7217304530,7217304541,
    7217304550,7217304560,7217304590,7217306000,7217307500,7217901000,7217905030,7217905060,
    7217905090,7218100000,7218910015,7218910030,7218910060,7218990015,7218990030,7218990045,
    7218990060,7218990090,7229200015,7229200090,7229900500,7229901000,7229905006,7229905008,7229905016,7229905031,
    7229905051,7229909000,7301100000,7302101010,7302101015,7302101025,7302101035,7302101045,
    7302101055,7302101065,7302101075,7302105020,7302105040,7302105060,7302400000,7302901000,
    7302909000,7304110020,7304110050,7304110080,7304191020,7304191030,7304191045,7304191060,
    7304191080,7304195020,7304195050,7304195080,7304220030,7304220045,7304220060,7304233000,
    7304236030,7304236045,7304236060,7304243010,7304243020,7304243030,7304243040,7304243045,
    7304243080,7304244010,7304244020,7304244030,7304244040,7304244050,7304244060,7304244080,
    7304246015,7304246030,7304246045,7304246060,7304246075,7304291010,7304291020,7304291030,
    7304291040,7304291050,7304291060,7304291080,7304292010,7304292020,7304292030,7304292040,
    7304292050,7304292060,7304292080,7304293110,7304293120,7304293130,7304293140,7304293150,
    7304293160,7304293180,7304294110,7304294120,7304294130,7304294140,7304294150,7304294160,
    7304294180,7304295015,7304295030,7304295045,7304295060,7304295075,7304296115,7304296130,
    7304296145,7304296160,7304296175,7304313000,7304316010,7304316050,7304390002,7304390004,
    7304390006,7304390008,7304390016,7304390020,7304390024,7304390028,7304390032,7304390036,
    7304390040,7304390044,7304390048,7304390052,7304390056,7304390062,7304390068,7304390072,
    7304390076,7304390080,7304413005,7304413015,7304413045,7304416005,7304416015,7304416045,
    7304490005,7304490015,7304490045,7304490060,7304511000,7304515005,7304515015,7304515045,
    7304515060,7304591000,7304592030,7304592040,7304592045,7304592055,7304592060,7304592070,
    7304592080,7304596000,7304598010,7304598015,7304598020,7304598025,7304598030,7304598035,
    7304598040,7304598045,7304598050,7304598055,7304598060,7304598065,7304598070,7304598080,
    7304901000,7304903000,7304905000,7304907000,7305111030,7305111060,7305115000,7305121030,
    7305121060,7305125000,7305191030,7305191060,7305195000,7305202000,7305204000,7305206000,
    7305208000,7305312000,7305314000,7305316010,7305316090,7305391000,7305395000,7305901000,
    7305905000,7306101010,7306101050,7306105010,7306105050,7306110010,7306110050,7306191010,
    7306191050,7306195110,7306195150,7306213000,7306214000,7306218010,7306218050,7306291030,
    7306291090,7306292000,7306293100,7306294100,7306296010,7306296050,7306298110,7306298150,
    7306301000,7306303000,7306305010,7306305015,7306305020,7306305025,7306305028,7306305032,
    7306305035,7306305040,7306305055,7306305085,7306305090,7306401010,7306401015,7306401090,
    7306405005,7306405015,7306405040,7306405042,7306405044,7306405062,7306405064,7306405080,
    7306405085,7306405090,7306501000,7306503000,7306505010,7306505030,7306505050,7306505070,
    7306611000,7306613000,7306615000,7306617030,7306617060,7306691000,7306693000,7306695000,
    7306697030,7306697060,7306901000,7306905000.7301201000,7301205000,7302300000,7307211000,7307221000,7307225000,7307230030,7307230090,
    7307290030,7307290090,7307911000,7307913000,7307915010,7307915030,7307915050,7307915070,
    7307923010,7307923030,7307929000,7307933010,7307933040,7307936000,7307939010,7307939040,
    7307939060,7307991000,7307993000,7307995015,7307995045,7307995060,7308100000,7308200020,
    7308200090,7308301000,7308305015,7308305025,7308305050,7308400000,7308903000,7308906000,
    7308907000,7308909530,7308909560,7308909590,7309000030,7309000090,7310100005,7310100015,
    7310100090,7310210025,7310210050,7310290020,7310290030,7310290055,7310290065,7311000030,
    7311000060,7311000090,7312100500,7312101030,7312101050,7312101070,7312102000,7312103005,
    7312103010,7312103012,7312103020,7312103045,7312103065,7312103070,7312103074,7312103080,
    7312105000,7312106030,7312106060,7312107000,7312108000,7312109030,7312109060,7312109090,
    7312900000,7313000000,7314121000,7314122000,7314123000,7314126000,7314129000,7314141000,
    7314142000,7314143000,7314146000,7314149000,7314190100,7314200000,7314311000,7314315010,
    7314315080,7314390000,7314410030,7314410040,7314410045,7314410080,7314420030,7314420060,
    7314493000,7314496000,7314500000,7315110005,7315110010,7315110045,7315110060,7315120020,
    7315120040,7315120060,7315120080,7315190000,7315201000,7315205000,7315810000,7315821000,
    7315823000,7315825000,7315827000,7315891000,7315893000,7315895000,7315900000,7316000000,
    7317001000,7317002000,7317003000,7317005501,7317005502,7317005503,7317005505,7317005507,
    7317005508,7317005511,7317005518,7317005519,7317005520,7317005530,7317005540,7317005550,
    7317005560,7317005570,7317005580,7317005590,7317006530,7317006560,7317007500,7318110000,
    7318120000,7318130030,7318130060,7318141030,7318141060,7318145020,7318145080,7318152010,
    7318152020,7318152030,7318152041,7318152046,7318152051,7318152055,7318152061,7318152065,
    7318152091,7318152095,7318154000,7318155030,7318155051,7318155056,7318155090,7318156010,
    7318156040,7318156070,7318156080,7318158020,7318158030,7318158045,7318158055,7318158066,
    7318158069,7318158082,7318158085,7318160015,7318160030,7318160045,7318160060,7318160085,
    7318190000,7318210030,7318210090,7318220000,7318230000,7318240000,7318290000,7319402010,
    7319402050,7319403000,7319405010,7319405050,7319901000,7319909000,7320103000,7320106015,
    7320106060,7320109015,7320109060,7320201000,7320205010,7320205020,7320205045,7320205060,
    7320901000,7320905010,7320905020,7320905060,7321111030,7321111060,7321113010,7321113020,
    7321113050,7321116000,7321120000,7321190020,7321190040,7321190060,7321190080,7321811000,
    7321815000,7321821000,7321825000,7321890010,7321890050,7321901000,7321902000,7321904000,
    7321905000,7321906040,7321906060,7321906090,7326908688
  ) & CTY_NAME == "CHINA", .25, tariff)) %>% #STEEL
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7601103000, 7601106030, 7601106090, 7601203000, 7601206000, 7601209030,
    7601209045, 7601209060, 7601209075, 7601209080, 7601209085, 7601209095,
    7604101000, 7604103000, 7604105000, 7604210010, 7604210090, 7604291010,
    7604291090, 7604293030, 7604293060, 7604293090, 7604295020, 7604295050,
    7604295090, 7605110000, 7605190000, 7605210000, 7605290000, 7606113030,
    7606113060, 7606116000, 7606123015, 7606123025, 7606123035, 7606123045,
    7606123055, 7606123091, 7606123096, 7606126000, 7606913055, 7606913095,
    7606916055, 7606916095, 7606923025, 7606923035, 7606926055, 7606926095,
    7607113000, 7607116010, 7607116090, 7607119030, 7607119060, 7607119090,
    7607191000, 7607193000, 7607196000, 7607201000, 7607205000, 7608100030,
    7608100090, 7608200030, 7608200090, 7609000000, 7616995160, 7616995170,
    7610100010, 7610100020, 7610100030, 7610900020, 7610900040, 7610900060,
    7610900080, 7615102015, 7615102025, 7615103015, 7615103025, 7615105020,
    7615105040, 7615107125, 7615107130, 7615107155, 7615107180, 7615109100,
    7615200000, 7616109090, 7616991000, 7616995130, 7616995140, 7616995190
  )& CTY_NAME == "CHINA", .25, tariff)) %>% #ALUMINUM
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080") & CTY_NAME == "CHINA", .25, tariff)) %>% #10 digit HS codes for excluded car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c(
    "40111010", "40111050",
    "40112010", "40121940", "40122060",
    "70072151", "73202010", "83021000", "84073100", "84082020",
    "84099110", "84133010", "84133090", "84139110", "84139190",
    "84143080", "84145930", "84145965", "84148005", "84152000",
    "84212300", "84213200", "84254900", "84311000", "84821010",
    "84821050", "84821050", "84822000", "84822000", "84822000",
    "84822000", "84822000", "84822000", "84825000", "84831010",
    "84831030", "85013200", "85013300", "85014000", "85015100",
    "85015200", "85071000", "85079040", "85079080",
    "85111000", "85112000", "85113000", "85113000", "85114000",
    "85115000", "85118020", "85119060", "85119060", "85122020",
    "85122040", "85123000", "85124020", "85124040", "85129020",
    "85129060", "85129070", "85198120", "85269010", "85364100",
    "85391000", "85391000", "85443000", "87060025", "87060005",
    "87060015", "87071000", "87079050", "87079050", "87079050",
    "87081030", "87082100", "87082200", "87082900", "87083000",
    "87084011", "87084070", "87084075", "87085000", "87088000",
    "87089100", "87089360", "87089375", "87089400", "87089580",
    "87089953", "87089955", "87089958", "87089968", "87169050",
    "90292040"
  ) & CTY_NAME == "CHINA", .25, tariff)) %>% #car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "CHINA", .25, tariff)) %>%
  #mutate(tariff = if_else(I_COMMODITY_4 %in% c("8471","8486","8524","8542"), 0, tariff)) %>% #April 11th Electronics Exemption
  #mutate(tariff = if_else(I_COMMODITY_6 %in% c("847330","851713","851762","852351","852852","854110","854121","854129","854130","854151","854159","854190"), 0, tariff)) %>% #April 11th Electronics Exemption
  #mutate(tariff = if_else(I_COMMODITY_8 %in% c("85414910","85414970","85414980","85414995"), 0, tariff)) %>% #April 11th Electronics Exemption
  #mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010"), 0, tariff)) %>% #Car Batteries
  mutate(tariff_val = CON_VAL_YR*tariff) 

AGG_RECIPROCAL_TARIFF_RATE_CAPS_NEW <- merge(US_IMPORTS_BULK,US_EXPORTS_BULK, by = "CTY_NAME") %>%
  transmute(CTY_NAME, tariff = (as.numeric(GEN_VAL_YR)-as.numeric(ALL_VAL_YR))/as.numeric(GEN_VAL_YR)) %>%
  mutate(tariff = round(tariff,2)/2) %>%
  mutate(tariff = round(tariff,2)) %>%
  mutate(tariff = ifelse(tariff < 0.1, 0.1, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME == "AFGHANISTAN", 0.1, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% c("RUSSIA","BELARUS","CUBA","KOREA, NORTH"), 0, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% EU_27, tariff[CTY_NAME == "EUROPEAN UNION"],tariff)) %>%
  mutate(tariff = ifelse(CTY_NAME %in% c("CHINA","HONG KONG","MACAU"), .1, tariff)) %>%
  mutate(tariff = ifelse(!CTY_NAME %in% c("CHINA","HONG KONG","MACAU","MEXICO","CANADA","RUSSIA","BELARUS","CUBA","KOREA, NORTH"), .1, tariff)) %>%
  #mutate(tariff = 0) %>%
  select(CTY_NAME, tariff)

AGG_TARIFF_ANALYSIS_NEW <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  mutate(I_COMMODITY = as.character(I_COMMODITY)) %>%
  mutate(I_COMMODITY = if_else(nchar(I_COMMODITY) == 9, paste0("0", I_COMMODITY), I_COMMODITY)) %>%
  left_join(.,AGG_RECIPROCAL_TARIFF_RATE_CAPS_NEW, by = "CTY_NAME") %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = if_else(CTY_NAME %in% c("CHINA","HONG KONG","MACAU"), tariff+.2, tariff)) %>% #Adding China 20% IEEPA Fentanyl Tariffs
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080") & CTY_NAME != "MEXICO" & CTY_NAME != "CANADA", .25, tariff)) %>% #10 digit HS codes for excluded car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c(
    "40111010", "40111050",
    "40112010", "40121940", "40122060",
    "70072151", "73202010", "83021000", "84073100", "84082020",
    "84099110", "84133010", "84133090", "84139110", "84139190",
    "84143080", "84145930", "84145965", "84148005", "84152000",
    "84212300", "84213200", "84254900", "84311000", "84821010",
    "84821050", "84821050", "84822000", "84822000", "84822000",
    "84822000", "84822000", "84822000", "84825000", "84831010",
    "84831030", "85013200", "85013300", "85014000", "85015100",
    "85015200", "85071000", "85079040", "85079080",
    "85111000", "85112000", "85113000", "85113000", "85114000",
    "85115000", "85118020", "85119060", "85119060", "85122020",
    "85122040", "85123000", "85124020", "85124040", "85129020",
    "85129060", "85129070", "85198120", "85269010", "85364100",
    "85391000", "85391000", "85443000", "87060025", "87060005",
    "87060015", "87071000", "87079050", "87079050", "87079050",
    "87081030", "87082100", "87082200", "87082900", "87083000",
    "87084011", "87084070", "87084075", "87085000", "87088000",
    "87089100", "87089360", "87089375", "87089400", "87089580",
    "87089953", "87089955", "87089958", "87089968", "87169050",
    "90292040"
  ) & CTY_NAME != "MEXICO" & CTY_NAME != "CANADA", .25, tariff)) %>% #car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000"), .25, tariff)) %>% #car exclusion
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "UNITED KINGDOM", .1124, tariff)) %>% #car exclusion
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "MEXICO", tariff-.491*tariff, tariff)) %>% #Mexico car content
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "CANADA", tariff -.6654735594*tariff, tariff)) %>% #canada car content
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("05080000", "25041050", "25049000", "25101000", "25102000", "25111010", "25111050", 
                                               "25191000", "25199010", "25199020", "25249000", "25292100", "25292200", "25302010", 
                                               "25302020", "25309010", "25309020", "25309080", "26020000", "26030000", "26050000", 
                                               "26060000", "26080000", "26100000", "26110030", "26110060", "26121000", "26140030", 
                                               "26140060", "26159030", "26159060", "26161000", "26171000", "26203000", "26209950", 
                                               "27011100", "27011200", "27011900", "27012000", "27021000", "27022000", "27030000", 
                                               "27040000", "27050000", "27060000", "27071000", "27072000", "27073000", "27074000", 
                                               "27075000", "27079100", "27079910", "27079920", "27079940", "27079951", "27079955", 
                                               "27079959", "27079990", "27081000", "27082000", "27090010", "27090020", "27101215", 
                                               "27101218", "27101225", "27101245", "27101290", "27101906", "27101911", "27101916", 
                                               "27101924", "27101925", "27101926", "27101930", "27101935", "27101940", "27101945", 
                                               "27101990", "27102005", "27102010", "27102015", "27102025", "27109100", "27109905", 
                                               "27109910", "27109916", "27109921", "27109931", "27109932", "27109939", "27109945", 
                                               "27109990", "27111100", "27111200", "27111300", "27111400", "27111900", "27112100", 
                                               "27112900", "27121000", "27122000", "27129010", "27129020", "27131100", "27131200", 
                                               "27132000", "27139000", "27141000", "27149000", "27150000", "27160000", "28012000", 
                                               "28042900", "28045000", "28046100", "28048000", "28049000", "28051910", "28051920", 
                                               "28051990"), 0, tariff)) %>% #ANNEX 2
  
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("28053000", "28111100", "28111910", "28112910", "28112920", "28121900", "28139010",
                                               "28152000", "28161000", "28164010", "28164020", "28170000", "28181010", "28181020",
                                               "28182000", "28183000", "28201000", "28211000", "28212000", "28220000", "28230000",
                                               "28252000", "28255030", "28256000", "28258000", "28259015", "28259030", "28259090",
                                               "28261200", "28263000", "28269090", "28273100", "28273945", "28273960", "28273990",
                                               "28274100", "28274950", "28275951", "28276010", "28276051", "28332100", "28332500",
                                               "28332700", "28332910", "28332945", "28332951", "28342100", "28342920", "28342951",
                                               "28366000", "28369100", "28369200", "28369910", "28369950", "28418000", "28419020",
                                               "28419040", "28432901", "28433000", "28439000", "28441010", "28441020", "28442000",
                                               "28443020", "28443050", "28444300", "28459001", "28461000", "28469020", "28469040",
                                               "28469080", "28492010", "28492020", "28499030", "28539010", "28539090", "29034510",
                                               "29035990", "29036990", "29037800", "29037990", "29038915", "29038920", "29038970",
                                               "29039200", "29049940", "29052990", "29053990", "29055910", "29055990", "29061950",
                                               "29062960", "29072990", "29081960", "29091918", "29092000", "29093060", "29094910",
                                               "29094915", "29094920", "29094960", "29095040", "29095045", "29095050", "29121950",
                                               "29124926", "29141900", "29144090", "29145030", "29145050", "29146200", "29146921",
                                               "29146990", "29147940", "29152930", "29153931", "29153935", "29153947", "29153990",
                                               "29159010", "29159014", "29159018", "29159020", "29159050", "29161930", "29161950",
                                               "29162050", "29163150", "29163946", "29163979", "29171300", "29171910", "29171970",
                                               "29173401", "29173930", "29181151", "29181350", "29181650", "29181960", "29181990",
                                               "29182210", "29182250", "29182330", "29182350", "29182920", "29182965", "29182975",
                                               "29183025", "29183030", "29183090", "29189930", "29189943", "29189947", "29189950",
                                               "29199030", "29199050", "29209051", "29211911", "29211961", "29212900", "29213010",
                                               "29213050", "29214290", "29214600", "29214938", "29214943", "29214945", "29214950",
                                               "29215980", "29221100", "29221400", "29221909", "29221920", "29221933", "29221960",
                                               "29221970", "29221990", "29221996", "29222927", "29222961", "29222981", "29223100",
                                               "29223925", "29223945", "29223950", "29224100", "29224250", "29224400", "29224910",
                                               "29224926", "29224930", "29224937", "29224949", "29224980", "29225007", "29225010",
                                               "29225011", "29225013", "29225014", "29225017", "29225025", "29225035", "29225040",
                                               "29225050", "29231000", "29232020", "29239001", "29241100", "29241911", "29241980",
                                               "29242116", "29242150", "29242910", "29242962", "29242971", "29242977", "29242995",
                                               "29251200", "29251942", "29251991", "29252100", "29252920", "29252960", "29252990",
                                               "29263010", "29264000", "29269014", "29269043", "29269048", "29270040", "29270050",
                                               "29280025", "29280030", "29280050", "29299020", "29299050", "29302020", "29302090",
                                               "29303060", "29309029", "29309049", "29309092", "29314900", "29315300", "29319022",
                                               "29319090", "29321400", "29321951", "29322020", "29322030", "29322050", "29329961",
                                               "29329970", "29329990", "29331100", "29331935", "29331945", "29331990", "29332100",
                                               "29332920", "29332935", "29332943", "29332945", "29332990", "29333301", "29333400",
                                               "29333500", "29333700", "29333908", "29333910", "29333920", "29333921", "29333923",
                                               "29333925", "29333927", "29333931", "29333941", "29333961", "29333992", "29334100",
                                               "29334908", "29334910", "29334915", "29334917", "29334920", "29334926", "29334930",
                                               "29334960", "29334970", "29335210", "29335290", "29335300", "29335400", "29335910",
                                               "29335915", "29335918", "29335921", "29335922", "29335936", "29335946", "29335953",
                                               "29335959"), 0, tariff)) %>% #ANNEX 2
  
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("29335970", "29335980", "29335985", "29335995", "29336960", "29337200", "29337908",
                                               "29337915", "29337985", "29339100", "29339901", "29339902", "29339905", "29339906",
                                               "29339908", "29339911", "29339912", "29339916", "29339917", "29339922", "29339924",
                                               "29339926", "29339942", "29339946", "29339951", "29339953", "29339955", "29339958",
                                               "29339961", "29339965", "29339970", "29339975", "29339979", "29339982", "29339985",
                                               "29339989", "29339990", "29339997", "29341010", "29341020", "29341090", "29342040",
                                               "29342080", "29343023", "29343027", "29343043", "29343050", "29349100", "29349200",
                                               "29349901", "29349903", "29349905", "29349906", "29349907", "29349908", "29349909",
                                               "29349911", "29349912", "29349915", "29349916", "29349918", "29349920", "29349930",
                                               "29349939", "29349944", "29349947", "29349970", "29349990", "29355000", "29359006",
                                               "29359010", "29359013", "29359015", "29359020", "29359030", "29359032", "29359033",
                                               "29359042", "29359048", "29359060", "29359075", "29359095", "29362100", "29362200",
                                               "29362300", "29362401", "29362500", "29362600", "29362700", "29362800", "29362910",
                                               "29362916", "29362920", "29362950", "29369001", "29371100", "29371200", "29371900",
                                               "29372100", "29372200", "29372310", "29372325", "29372350", "29372910", "29372990",
                                               "29375000", "29379005", "29379010", "29379020", "29379040", "29379045", "29379090",
                                               "29381000", "29389000", "29391100", "29391910", "29391920", "29391950", "29392000",
                                               "29393000", "29394100", "29394200", "29394400", "29394500", "29394903", "29395900",
                                               "29396200", "29396300", "29396900", "29397200", "29397900", "29398000", "29400060",
                                               "29411010", "29411020", "29411030", "29411050", "29412010", "29412050", "29413000",
                                               "29414000", "29415000", "29419010", "29419030", "29419050", "29420005", "29420035",
                                               "29420050", "30012000", "30019001", "30021200", "30021300", "30021400", "30021500",
                                               "30024100", "30024200", "30024900", "30025100", "30025900", "30029010", "30029052",
                                               "30031000", "30032000", "30033910", "30033950", "30034100", "30034200", "30034900",
                                               "30039001", "30041010", "30041050", "30042000", "30043100", "30043200", "30043900",
                                               "30044100", "30044200", "30044900", "30045010", "30045020", "30045030", "30045040",
                                               "30045050", "30046000", "30049010", "30049092", "30063010", "30063050", "30066000",
                                               "30067000", "30069310", "30069320", "30069350", "30069360", "30069380", "31042000",
                                               "31043000", "31049001", "31051000", "31052000", "31056000", "32030080", "32041380",
                                               "32041720", "32041800", "32061100", "32061900", "34024210", "34024220", "34024290",
                                               "36069030", "38089410", "38089450", "38180000", "38249100", "38249929", "38249949",
                                               "38249955", "38249993", "39019090", "39029000", "39046100", "39059110", "39059980",
                                               "39069050", "39071000", "39072100", "39072900", "39073000", "39076100", "39076900",
                                               "39077000", "39079950", "39081000", "39100000", "39119025", "39119091", "39123100",
                                               "39123900", "39129000", "39139020", "39139050", "39140060", "40011000", "40012100",
                                               "40012200", "40012900", "40013000", "44011100", "44011200", "44012100", "44012200",
                                               "44013100", "44013200", "44013942", "44014100", "44014900", "44021000", "44022000",
                                               "44029001", "44031100", "44031200", "44032101", "44032201", "44032301", "44032401",
                                               "44032501", "44032601", "44034200", "44034902", "44039100", "44039301", "44039401",
                                               "44039501", "44039601", "44039700", "44039800", "44039901", "44041000", "44042000",
                                               "44050000", "44061100", "44061200", "44069100", "44069200", "44071100", "44071200",
                                               "44071300", "44071400", "44071900", "44072100", "44072200"), 0, tariff)) %>% #ANNEX 2
  
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("44072301", "44072500", "44072600", "44072700", "44072800", "44072902", "44079100",
                                               "44079200", "44079300", "44079400", "44079500", "44079600", "44079700", "44079902",
                                               "44081001", "44083101", "44083902", "44089001", "44091005", "44091010", "44091020",
                                               "44091040", "44091045", "44091050", "44091060", "44091065", "44091090", "44092105",
                                               "44092190", "44092205", "44092210", "44092225", "44092240", "44092250", "44092260",
                                               "44092265", "44092290", "44092906", "44092911", "44092926", "44092941", "44092951",
                                               "44092961", "44092966", "44092991", "44101100", "44101200", "44101900", "44109000",
                                               "44111210", "44111220", "44111230", "44111260", "44111290", "44111310", "44111320",
                                               "44111330", "44111360", "44111390", "44111410", "44111420", "44111430", "44111460",
                                               "44111490", "44119210", "44119220", "44119230", "44119240", "44119310", "44119320",
                                               "44119330", "44119360", "44119390", "44119400", "44121005", "44121090", "44123106",
                                               "44123126", "44123142", "44123145", "44123148", "44123152", "44123161", "44123192",
                                               "44123306", "44123326", "44123332", "44123357", "44123426", "44123432", "44123457",
                                               "44123910", "44123930", "44123940", "44123950", "44124100", "44124200", "44124900",
                                               "44125110", "44125131", "44125141", "44125151", "44125210", "44125231", "44125241",
                                               "44125251", "44125980", "44125990", "44125995", "44129106", "44129110", "44129131",
                                               "44129141", "44129151", "44129207", "44129211", "44129231", "44129242", "44129252",
                                               "44129958", "44129961", "44129971", "44129981", "44129991", "44129997", "44130000",
                                               "48202000", "49011000", "49019100", "49019900", "49021000", "49029010", "49029020",
                                               "49030000", "49040000", "49052000", "49059020", "49059060", "49060000", "49111000",
                                               "49119960", "49119980", "71069110", "71081210", "71101100", "71101900", "71102100",
                                               "71102900", "71103100", "71103900", "71104100", "71104900", "71129201", "71189000",
                                               "72021110", "72021150", "72021910", "72021950", "72023000", "72024100", "72024910",
                                               "72024950", "72025000", "72028000", "72029100", "72029340", "72029380", "72042100",
                                               "74010000", "74020000", "74031100", "74031200", "74031300", "74031900", "74032100",
                                               "74032200", "74032901", "74040030", "74040060", "74050010", "74050060", "74061000",
                                               "74062000", "74071015", "74071030", "74071050", "74072115", "74072130", "74072150",
                                               "74072170", "74072190", "74072916", "74072934", "74072938", "74072940", "74072950",
                                               "74081130", "74081160", "74081900", "74082100", "74082210", "74082250", "74082910",
                                               "74082950", "74091110", "74091150", "74091910", "74091950", "74091990", "74092100",
                                               "74092900", "74093110", "74093150", "74093190", "74093910", "74093950", "74093990",
                                               "74094000", "74099010", "74099050", "74099090", "74101100", "74101200", "74102130",
                                               "74102160", "74102200", "74111010", "74111050", "74112110", "74112150", "74112200",
                                               "74112910", "74112950", "74121000", "74122000", "74130010", "74130050", "74130090",
                                               "74151000", "74152100", "74152900", "74153305", "74153310", "74153380", "74153900",
                                               "74181000", "74182010", "74182050", "74192000", "74198003", "74198006", "74198009",
                                               "74198015", "74198016", "74198017", "74198030", "74198050", "75089050", "79011100",
                                               "79011210", "79011250", "79012000", "79020000", "79070060", "80011000", "80012000",
                                               "80020000", "80070050", "81011000", "81019700", "81032000", "81033000", "81039100",
                                               "81039900", "81041100", "81041900", "81042000", "81043000", "81049000", "81052030",
                                               "81052060", "81052090", "81053000", "81059000", "81061000", "81069000", "81082000",
                                               "81083000", "81089030", "81089060", "81101000", "81102000", "81109000", "81110047",
                                               "81110049", "81122100", "81122200", "81122900", "81124110", "81124150", "81124900",
                                               "81125900", "81129210", "81129230", "81129240", "81129260", "81129265", "81129910",
                                               "81129991", "85411000", "85412100", "85412900", "85413000", "85414910", "85414970",
                                               "85414980", "85414995", "85415100", "85415900", "85419000", "85423100", "85423200",
                                               "85423300", "85423900", "85429000"), 0, tariff)) %>% #Annex 2
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7601103000, 7601106030, 7601106090, 7601203000, 7601206000, 7601209030,
    7601209045, 7601209060, 7601209075, 7601209080, 7601209085, 7601209095,
    7604101000, 7604103000, 7604105000, 7604210010, 7604210090, 7604291010,
    7604291090, 7604293030, 7604293060, 7604293090, 7604295020, 7604295050,
    7604295090, 7605110000, 7605190000, 7605210000, 7605290000, 7606113030,
    7606113060, 7606116000, 7606123015, 7606123025, 7606123035, 7606123045,
    7606123055, 7606123091, 7606123096, 7606126000, 7606913055, 7606913095,
    7606916055, 7606916095, 7606923025, 7606923035, 7606926055, 7606926095,
    7607113000, 7607116010, 7607116090, 7607119030, 7607119060, 7607119090,
    7607191000, 7607193000, 7607196000, 7607201000, 7607205000, 7608100030,
    7608100090, 7608200030, 7608200090, 7609000000, 7616995160, 7616995170,
    7610100010, 7610100020, 7610100030, 7610900020, 7610900040, 7610900060,
    7610900080, 7615102015, 7615102025, 7615103015, 7615103025, 7615105020,
    7615105040, 7615107125, 7615107130, 7615107155, 7615107180, 7615109100,
    7615200000, 7616109090, 7616991000, 7616995130, 7616995140, 7616995190
  ), .25, tariff)) %>% #ALUMINUM
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7614105000, 7614902030, 7614902060, 7614904000, 7614905000, 8708103030,
    8708292130, 8302103000, 8302106030, 8302106060, 8302106090, 8302200000,
    8302303010, 8302303060, 8302413000, 8302416015, 8302416045, 8302416050,
    8302416080, 8302423010, 8302423015, 8302423065, 8302496035, 8302496045,
    8302496055, 8302496085, 8302500000, 8302603000, 8302609000, 8305100010,
    8305100050, 8306300000, 8414596590, 8415908025, 8415908045, 8415908085,
    8418998005, 8418998050, 8418998060, 8419901000, 8422900680, 8473302000,
    8473305100, 8479899596, 8481909060, 8481909085, 8486900000, 8487900040,
    8487900080, 8503009520, 8508700000, 8513902000, 8515902000, 8516905000,
    8516908050, 8517710000, 8517790000, 8529907300, 8529909760, 8536908585,
    8538100000, 8541900000, 8543908885, 8547900010, 8547900020, 8547900030,
    8547900040, 8708106010, 8708106050, 8708295160, 8708806590, 8708996890,
    8716805010, 8807300015, 8807300030, 8807300060, 9013908000, 9031909195,
    9401999081, 9403100020, 9403100040, 9403200011, 9403200016, 9403200017,
    9403200035, 9403200040, 9403200046, 9403200048, 9403200050, 9403200075,
    9403200078, 9403200082, 9403200086, 9403200090, 9403991040, 9403999010,
    9403999015, 9403999020, 9403999040, 9403999045, 9405992000, 9506114080,
    9506514000, 9506516000, 9506594040, 9506702090, 9506910010, 9506910020,
    9506910030, 9506990510, 9506990520, 9506990530, 9506991500, 9506992000,
    9506992580, 9506992800, 9506995500, 9506996080, 9507302000, 9507304000,
    9507306000, 9507308000, 9507906000, 9603908050, 6603908100
  ) & CTY_NAME != "CHINA", 0, tariff)) %>% #ALUMINUM DERIVATIVES
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7206100000,7206900000,7207110000,7207120010,7207120050,7207190030,7207190090,7207200025,
    7207200045,7207200075,7207200090,7208101500,7208103000,7208106000,7208253000,7208256000,
    7208260030,7208260060,7208270040,7208270045,7208270060,7208360030,7208360060,7208370030,
    7208370060,7208380015,7208380030,7208380090,7208390020,7208390025,7208390030,7208390090,
    7208403030,7208403060,7208406030,7208406060,7208510030,7208510045,7208510060,7208520000,
    7208530000,7208540000,7208900000,7209150000,7209160040,7209160045,7209160060,7209160070,
    7209160091,7209170040,7209170045,7209170060,7209170070,7209170091,7209181530,7209181560,
    7209182520,7209182585,7209186020,7209186090,7209250000,7209260000,7209270000,7209280000,
    7209900000,7210110000,7210120000,7210200000,7210300030,7210300060,7210410000,7210490040,
    7210490045,7210490091,7210490095,7210500020,7210500090,7210610000,7210690000,7210703000,
    7210706030,7210706060,7210706090,7210901000,7210906000,7210909000,7211130000,7211140030,
    7211140045,7211140090,7211191500,7211192000,7211193000,7211194500,7211196000,7211197530,
    7211197560,7211197590,7211231500,7211232000,7211233000,7211234500,7211236030,7211236060,
    7211236090,7211292030,7211292090,7211294500,7211296030,7211296080,7211900000,7212100000,
    7212200000,7212301030,7212301090,7212303000,7212305000,7212401000,7212405000,7212500000,
    7212600000,7213100000,7213200010,7213200080,7213913011,7213913015,7213913020,7213913093,
    7213914500,7213916000,7213990030,7213990060,7213990090,7214100000,7214200000,7214300010,
    7214300080,7214910016,7214910020,7214910060,7214910090,7214990016,7214990021,7214990026,
    7214990031,7214990036,7214990040,7214990045,7214990060,7214990075,7214990090,7215100010,
    7215100080,7215500016,7215500018,7215500020,7215500061,7215500063,7215500065,7215500090,
    7215901000,7215903000,7215905000,7216100010,7216100050,7216210000,7216220000,7216310000,
    7216320000,7216330030,7216330060,7216330090,7216400010,7216400050,7216500000,7216990010,
    7216990090,7217101000,7217102000,7217103000,7217104040,7217104045,7217104090,7217105030,
    7217105090,7217106000,7217107000,7217108010,7217108020,7217108025,7217108030,7217108045,
    7217108060,7217108075,7217108090,7217109000,7217201500,7217203000,7217204510,7217204520,
    7217204530,7217204540,7217204550,7217204560,7217204570,7217204580,7217206000,7217207500,
    7217301530,7217301560,7217303000,7217304504,7217304511,7217304520,7217304530,7217304541,
    7217304550,7217304560,7217304590,7217306000,7217307500,7217901000,7217905030,7217905060,
    7217905090,7218100000,7218910015,7218910030,7218910060,7218990015,7218990030,7218990045,
    7218990060,7218990090,7229200015,7229200090,7229900500,7229901000,7229905006,7229905008,7229905016,7229905031,
    7229905051,7229909000,7301100000,7302101010,7302101015,7302101025,7302101035,7302101045,
    7302101055,7302101065,7302101075,7302105020,7302105040,7302105060,7302400000,7302901000,
    7302909000,7304110020,7304110050,7304110080,7304191020,7304191030,7304191045,7304191060,
    7304191080,7304195020,7304195050,7304195080,7304220030,7304220045,7304220060,7304233000,
    7304236030,7304236045,7304236060,7304243010,7304243020,7304243030,7304243040,7304243045,
    7304243080,7304244010,7304244020,7304244030,7304244040,7304244050,7304244060,7304244080,
    7304246015,7304246030,7304246045,7304246060,7304246075,7304291010,7304291020,7304291030,
    7304291040,7304291050,7304291060,7304291080,7304292010,7304292020,7304292030,7304292040,
    7304292050,7304292060,7304292080,7304293110,7304293120,7304293130,7304293140,7304293150,
    7304293160,7304293180,7304294110,7304294120,7304294130,7304294140,7304294150,7304294160,
    7304294180,7304295015,7304295030,7304295045,7304295060,7304295075,7304296115,7304296130,
    7304296145,7304296160,7304296175,7304313000,7304316010,7304316050,7304390002,7304390004,
    7304390006,7304390008,7304390016,7304390020,7304390024,7304390028,7304390032,7304390036,
    7304390040,7304390044,7304390048,7304390052,7304390056,7304390062,7304390068,7304390072,
    7304390076,7304390080,7304413005,7304413015,7304413045,7304416005,7304416015,7304416045,
    7304490005,7304490015,7304490045,7304490060,7304511000,7304515005,7304515015,7304515045,
    7304515060,7304591000,7304592030,7304592040,7304592045,7304592055,7304592060,7304592070,
    7304592080,7304596000,7304598010,7304598015,7304598020,7304598025,7304598030,7304598035,
    7304598040,7304598045,7304598050,7304598055,7304598060,7304598065,7304598070,7304598080,
    7304901000,7304903000,7304905000,7304907000,7305111030,7305111060,7305115000,7305121030,
    7305121060,7305125000,7305191030,7305191060,7305195000,7305202000,7305204000,7305206000,
    7305208000,7305312000,7305314000,7305316010,7305316090,7305391000,7305395000,7305901000,
    7305905000,7306101010,7306101050,7306105010,7306105050,7306110010,7306110050,7306191010,
    7306191050,7306195110,7306195150,7306213000,7306214000,7306218010,7306218050,7306291030,
    7306291090,7306292000,7306293100,7306294100,7306296010,7306296050,7306298110,7306298150,
    7306301000,7306303000,7306305010,7306305015,7306305020,7306305025,7306305028,7306305032,
    7306305035,7306305040,7306305055,7306305085,7306305090,7306401010,7306401015,7306401090,
    7306405005,7306405015,7306405040,7306405042,7306405044,7306405062,7306405064,7306405080,
    7306405085,7306405090,7306501000,7306503000,7306505010,7306505030,7306505050,7306505070,
    7306611000,7306613000,7306615000,7306617030,7306617060,7306691000,7306693000,7306695000,
    7306697030,7306697060,7306901000,7306905000.7301201000,7301205000,7302300000,7307211000,7307221000,7307225000,7307230030,7307230090,
    7307290030,7307290090,7307911000,7307913000,7307915010,7307915030,7307915050,7307915070,
    7307923010,7307923030,7307929000,7307933010,7307933040,7307936000,7307939010,7307939040,
    7307939060,7307991000,7307993000,7307995015,7307995045,7307995060,7308100000,7308200020,
    7308200090,7308301000,7308305015,7308305025,7308305050,7308400000,7308903000,7308906000,
    7308907000,7308909530,7308909560,7308909590,7309000030,7309000090,7310100005,7310100015,
    7310100090,7310210025,7310210050,7310290020,7310290030,7310290055,7310290065,7311000030,
    7311000060,7311000090,7312100500,7312101030,7312101050,7312101070,7312102000,7312103005,
    7312103010,7312103012,7312103020,7312103045,7312103065,7312103070,7312103074,7312103080,
    7312105000,7312106030,7312106060,7312107000,7312108000,7312109030,7312109060,7312109090,
    7312900000,7313000000,7314121000,7314122000,7314123000,7314126000,7314129000,7314141000,
    7314142000,7314143000,7314146000,7314149000,7314190100,7314200000,7314311000,7314315010,
    7314315080,7314390000,7314410030,7314410040,7314410045,7314410080,7314420030,7314420060,
    7314493000,7314496000,7314500000,7315110005,7315110010,7315110045,7315110060,7315120020,
    7315120040,7315120060,7315120080,7315190000,7315201000,7315205000,7315810000,7315821000,
    7315823000,7315825000,7315827000,7315891000,7315893000,7315895000,7315900000,7316000000,
    7317001000,7317002000,7317003000,7317005501,7317005502,7317005503,7317005505,7317005507,
    7317005508,7317005511,7317005518,7317005519,7317005520,7317005530,7317005540,7317005550,
    7317005560,7317005570,7317005580,7317005590,7317006530,7317006560,7317007500,7318110000,
    7318120000,7318130030,7318130060,7318141030,7318141060,7318145020,7318145080,7318152010,
    7318152020,7318152030,7318152041,7318152046,7318152051,7318152055,7318152061,7318152065,
    7318152091,7318152095,7318154000,7318155030,7318155051,7318155056,7318155090,7318156010,
    7318156040,7318156070,7318156080,7318158020,7318158030,7318158045,7318158055,7318158066,
    7318158069,7318158082,7318158085,7318160015,7318160030,7318160045,7318160060,7318160085,
    7318190000,7318210030,7318210090,7318220000,7318230000,7318240000,7318290000,7319402010,
    7319402050,7319403000,7319405010,7319405050,7319901000,7319909000,7320103000,7320106015,
    7320106060,7320109015,7320109060,7320201000,7320205010,7320205020,7320205045,7320205060,
    7320901000,7320905010,7320905020,7320905060,7321111030,7321111060,7321113010,7321113020,
    7321113050,7321116000,7321120000,7321190020,7321190040,7321190060,7321190080,7321811000,
    7321815000,7321821000,7321825000,7321890010,7321890050,7321901000,7321902000,7321904000,
    7321905000,7321906040,7321906060,7321906090,7326908688
  ), .25, tariff)) %>% #STEEL
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7317003000,7317005503,7317005505,7317005507,7317005560,7317005580,7317006560,
    8708103020,8708292120,8431310020,8431310040,8431310060,8431420000,8431491010,
    8431491060,8431491090,8431499005,8431499010,8431499015,8431499020,8431499025,
    8431499030,8431499035,8431499036,8431499038,8431499044,8431499045,8431499050,
    8431499055,8431499081,8431499084,8431499090,8431499095,8432100020,8432100040,
    8432100060,8432900010,8432900020,8432900040,8432900050,8432900060,8432900081,
    8547900010,8547900020,8547900030,8547900040,9403200011,9403200016,9403200017,
    9403200035,9403200040,9403200046,9403200048,9403200050,9403200075,9403200078,
    9403200082,9403200086,9403200090,9403992040,9403992080,9403994005,9403994010,
    9403994080,9406200000,9406900110,9406900120,9406900130,9406900150,9406900190
  ) & CTY_NAME != "CHINA", 0, tariff)) %>% #Steel Derivatives
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853710", "853720", "870822",
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910") & CTY_NAME != "CHINA", 0.25, tariff)) %>% #Car Parts
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707") & CTY_NAME != "CHINA", 0.25, tariff)) %>% #Car Parts 
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010") & CTY_NAME != "CHINA", 0.25, tariff)) %>% #Car Batteries
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853710", "853720", "870822",
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910") & CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>% #Car Parts
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707") & CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>% #Car Parts 
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010") & CTY_NAME %in% c("MEXICO","CANADA"), 0, tariff)) %>% #Car Batteries
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8471","8486","8524","8542") & CTY_NAME != "CHINA", 0, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("847330","851713","851762","852351","852852","854110","854121","854129","854130","854151","854159","854190") & CTY_NAME != "CHINA", 0, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("85414910","85414970","85414980","85414995") & CTY_NAME != "CHINA", 0, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8471","8486","8524","8542") & CTY_NAME == "CHINA", .2, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("847330","851713","851762","852351","852852","854110","854121","854129","854130","854151","854159","854190") & CTY_NAME == "CHINA", .20, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("85414910","85414970","85414980","85414995") & CTY_NAME == "CHINA", .20, tariff)) %>% #April 11th Electronics Exemption
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7206100000,7206900000,7207110000,7207120010,7207120050,7207190030,7207190090,7207200025,
    7207200045,7207200075,7207200090,7208101500,7208103000,7208106000,7208253000,7208256000,
    7208260030,7208260060,7208270040,7208270045,7208270060,7208360030,7208360060,7208370030,
    7208370060,7208380015,7208380030,7208380090,7208390020,7208390025,7208390030,7208390090,
    7208403030,7208403060,7208406030,7208406060,7208510030,7208510045,7208510060,7208520000,
    7208530000,7208540000,7208900000,7209150000,7209160040,7209160045,7209160060,7209160070,
    7209160091,7209170040,7209170045,7209170060,7209170070,7209170091,7209181530,7209181560,
    7209182520,7209182585,7209186020,7209186090,7209250000,7209260000,7209270000,7209280000,
    7209900000,7210110000,7210120000,7210200000,7210300030,7210300060,7210410000,7210490040,
    7210490045,7210490091,7210490095,7210500020,7210500090,7210610000,7210690000,7210703000,
    7210706030,7210706060,7210706090,7210901000,7210906000,7210909000,7211130000,7211140030,
    7211140045,7211140090,7211191500,7211192000,7211193000,7211194500,7211196000,7211197530,
    7211197560,7211197590,7211231500,7211232000,7211233000,7211234500,7211236030,7211236060,
    7211236090,7211292030,7211292090,7211294500,7211296030,7211296080,7211900000,7212100000,
    7212200000,7212301030,7212301090,7212303000,7212305000,7212401000,7212405000,7212500000,
    7212600000,7213100000,7213200010,7213200080,7213913011,7213913015,7213913020,7213913093,
    7213914500,7213916000,7213990030,7213990060,7213990090,7214100000,7214200000,7214300010,
    7214300080,7214910016,7214910020,7214910060,7214910090,7214990016,7214990021,7214990026,
    7214990031,7214990036,7214990040,7214990045,7214990060,7214990075,7214990090,7215100010,
    7215100080,7215500016,7215500018,7215500020,7215500061,7215500063,7215500065,7215500090,
    7215901000,7215903000,7215905000,7216100010,7216100050,7216210000,7216220000,7216310000,
    7216320000,7216330030,7216330060,7216330090,7216400010,7216400050,7216500000,7216990010,
    7216990090,7217101000,7217102000,7217103000,7217104040,7217104045,7217104090,7217105030,
    7217105090,7217106000,7217107000,7217108010,7217108020,7217108025,7217108030,7217108045,
    7217108060,7217108075,7217108090,7217109000,7217201500,7217203000,7217204510,7217204520,
    7217204530,7217204540,7217204550,7217204560,7217204570,7217204580,7217206000,7217207500,
    7217301530,7217301560,7217303000,7217304504,7217304511,7217304520,7217304530,7217304541,
    7217304550,7217304560,7217304590,7217306000,7217307500,7217901000,7217905030,7217905060,
    7217905090,7218100000,7218910015,7218910030,7218910060,7218990015,7218990030,7218990045,
    7218990060,7218990090,7229200015,7229200090,7229900500,7229901000,7229905006,7229905008,7229905016,7229905031,
    7229905051,7229909000,7301100000,7302101010,7302101015,7302101025,7302101035,7302101045,
    7302101055,7302101065,7302101075,7302105020,7302105040,7302105060,7302400000,7302901000,
    7302909000,7304110020,7304110050,7304110080,7304191020,7304191030,7304191045,7304191060,
    7304191080,7304195020,7304195050,7304195080,7304220030,7304220045,7304220060,7304233000,
    7304236030,7304236045,7304236060,7304243010,7304243020,7304243030,7304243040,7304243045,
    7304243080,7304244010,7304244020,7304244030,7304244040,7304244050,7304244060,7304244080,
    7304246015,7304246030,7304246045,7304246060,7304246075,7304291010,7304291020,7304291030,
    7304291040,7304291050,7304291060,7304291080,7304292010,7304292020,7304292030,7304292040,
    7304292050,7304292060,7304292080,7304293110,7304293120,7304293130,7304293140,7304293150,
    7304293160,7304293180,7304294110,7304294120,7304294130,7304294140,7304294150,7304294160,
    7304294180,7304295015,7304295030,7304295045,7304295060,7304295075,7304296115,7304296130,
    7304296145,7304296160,7304296175,7304313000,7304316010,7304316050,7304390002,7304390004,
    7304390006,7304390008,7304390016,7304390020,7304390024,7304390028,7304390032,7304390036,
    7304390040,7304390044,7304390048,7304390052,7304390056,7304390062,7304390068,7304390072,
    7304390076,7304390080,7304413005,7304413015,7304413045,7304416005,7304416015,7304416045,
    7304490005,7304490015,7304490045,7304490060,7304511000,7304515005,7304515015,7304515045,
    7304515060,7304591000,7304592030,7304592040,7304592045,7304592055,7304592060,7304592070,
    7304592080,7304596000,7304598010,7304598015,7304598020,7304598025,7304598030,7304598035,
    7304598040,7304598045,7304598050,7304598055,7304598060,7304598065,7304598070,7304598080,
    7304901000,7304903000,7304905000,7304907000,7305111030,7305111060,7305115000,7305121030,
    7305121060,7305125000,7305191030,7305191060,7305195000,7305202000,7305204000,7305206000,
    7305208000,7305312000,7305314000,7305316010,7305316090,7305391000,7305395000,7305901000,
    7305905000,7306101010,7306101050,7306105010,7306105050,7306110010,7306110050,7306191010,
    7306191050,7306195110,7306195150,7306213000,7306214000,7306218010,7306218050,7306291030,
    7306291090,7306292000,7306293100,7306294100,7306296010,7306296050,7306298110,7306298150,
    7306301000,7306303000,7306305010,7306305015,7306305020,7306305025,7306305028,7306305032,
    7306305035,7306305040,7306305055,7306305085,7306305090,7306401010,7306401015,7306401090,
    7306405005,7306405015,7306405040,7306405042,7306405044,7306405062,7306405064,7306405080,
    7306405085,7306405090,7306501000,7306503000,7306505010,7306505030,7306505050,7306505070,
    7306611000,7306613000,7306615000,7306617030,7306617060,7306691000,7306693000,7306695000,
    7306697030,7306697060,7306901000,7306905000.7301201000,7301205000,7302300000,7307211000,7307221000,7307225000,7307230030,7307230090,
    7307290030,7307290090,7307911000,7307913000,7307915010,7307915030,7307915050,7307915070,
    7307923010,7307923030,7307929000,7307933010,7307933040,7307936000,7307939010,7307939040,
    7307939060,7307991000,7307993000,7307995015,7307995045,7307995060,7308100000,7308200020,
    7308200090,7308301000,7308305015,7308305025,7308305050,7308400000,7308903000,7308906000,
    7308907000,7308909530,7308909560,7308909590,7309000030,7309000090,7310100005,7310100015,
    7310100090,7310210025,7310210050,7310290020,7310290030,7310290055,7310290065,7311000030,
    7311000060,7311000090,7312100500,7312101030,7312101050,7312101070,7312102000,7312103005,
    7312103010,7312103012,7312103020,7312103045,7312103065,7312103070,7312103074,7312103080,
    7312105000,7312106030,7312106060,7312107000,7312108000,7312109030,7312109060,7312109090,
    7312900000,7313000000,7314121000,7314122000,7314123000,7314126000,7314129000,7314141000,
    7314142000,7314143000,7314146000,7314149000,7314190100,7314200000,7314311000,7314315010,
    7314315080,7314390000,7314410030,7314410040,7314410045,7314410080,7314420030,7314420060,
    7314493000,7314496000,7314500000,7315110005,7315110010,7315110045,7315110060,7315120020,
    7315120040,7315120060,7315120080,7315190000,7315201000,7315205000,7315810000,7315821000,
    7315823000,7315825000,7315827000,7315891000,7315893000,7315895000,7315900000,7316000000,
    7317001000,7317002000,7317003000,7317005501,7317005502,7317005503,7317005505,7317005507,
    7317005508,7317005511,7317005518,7317005519,7317005520,7317005530,7317005540,7317005550,
    7317005560,7317005570,7317005580,7317005590,7317006530,7317006560,7317007500,7318110000,
    7318120000,7318130030,7318130060,7318141030,7318141060,7318145020,7318145080,7318152010,
    7318152020,7318152030,7318152041,7318152046,7318152051,7318152055,7318152061,7318152065,
    7318152091,7318152095,7318154000,7318155030,7318155051,7318155056,7318155090,7318156010,
    7318156040,7318156070,7318156080,7318158020,7318158030,7318158045,7318158055,7318158066,
    7318158069,7318158082,7318158085,7318160015,7318160030,7318160045,7318160060,7318160085,
    7318190000,7318210030,7318210090,7318220000,7318230000,7318240000,7318290000,7319402010,
    7319402050,7319403000,7319405010,7319405050,7319901000,7319909000,7320103000,7320106015,
    7320106060,7320109015,7320109060,7320201000,7320205010,7320205020,7320205045,7320205060,
    7320901000,7320905010,7320905020,7320905060,7321111030,7321111060,7321113010,7321113020,
    7321113050,7321116000,7321120000,7321190020,7321190040,7321190060,7321190080,7321811000,
    7321815000,7321821000,7321825000,7321890010,7321890050,7321901000,7321902000,7321904000,
    7321905000,7321906040,7321906060,7321906090,7326908688
  ) & CTY_NAME == "CHINA", .25, tariff)) %>% #STEEL
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7601103000, 7601106030, 7601106090, 7601203000, 7601206000, 7601209030,
    7601209045, 7601209060, 7601209075, 7601209080, 7601209085, 7601209095,
    7604101000, 7604103000, 7604105000, 7604210010, 7604210090, 7604291010,
    7604291090, 7604293030, 7604293060, 7604293090, 7604295020, 7604295050,
    7604295090, 7605110000, 7605190000, 7605210000, 7605290000, 7606113030,
    7606113060, 7606116000, 7606123015, 7606123025, 7606123035, 7606123045,
    7606123055, 7606123091, 7606123096, 7606126000, 7606913055, 7606913095,
    7606916055, 7606916095, 7606923025, 7606923035, 7606926055, 7606926095,
    7607113000, 7607116010, 7607116090, 7607119030, 7607119060, 7607119090,
    7607191000, 7607193000, 7607196000, 7607201000, 7607205000, 7608100030,
    7608100090, 7608200030, 7608200090, 7609000000, 7616995160, 7616995170,
    7610100010, 7610100020, 7610100030, 7610900020, 7610900040, 7610900060,
    7610900080, 7615102015, 7615102025, 7615103015, 7615103025, 7615105020,
    7615105040, 7615107125, 7615107130, 7615107155, 7615107180, 7615109100,
    7615200000, 7616109090, 7616991000, 7616995130, 7616995140, 7616995190
  )& CTY_NAME == "CHINA", .25, tariff)) %>% #ALUMINUM
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080") & CTY_NAME == "CHINA", .25, tariff)) %>% #10 digit HS codes for excluded car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c(
    "40111010", "40111050",
    "40112010", "40121940", "40122060",
    "70072151", "73202010", "83021000", "84073100", "84082020",
    "84099110", "84133010", "84133090", "84139110", "84139190",
    "84143080", "84145930", "84145965", "84148005", "84152000",
    "84212300", "84213200", "84254900", "84311000", "84821010",
    "84821050", "84821050", "84822000", "84822000", "84822000",
    "84822000", "84822000", "84822000", "84825000", "84831010",
    "84831030", "85013200", "85013300", "85014000", "85015100",
    "85015200", "85071000", "85079040", "85079080",
    "85111000", "85112000", "85113000", "85113000", "85114000",
    "85115000", "85118020", "85119060", "85119060", "85122020",
    "85122040", "85123000", "85124020", "85124040", "85129020",
    "85129060", "85129070", "85198120", "85269010", "85364100",
    "85391000", "85391000", "85443000", "87060025", "87060005",
    "87060015", "87071000", "87079050", "87079050", "87079050",
    "87081030", "87082100", "87082200", "87082900", "87083000",
    "87084011", "87084070", "87084075", "87085000", "87088000",
    "87089100", "87089360", "87089375", "87089400", "87089580",
    "87089953", "87089955", "87089958", "87089968", "87169050",
    "90292040"
  ) & CTY_NAME == "CHINA", .25, tariff)) %>% #car parts
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000") & CTY_NAME == "CHINA", .25, tariff)) %>%
  #mutate(tariff = if_else(I_COMMODITY_4 %in% c("8471","8486","8524","8542"), 0, tariff)) %>% #April 11th Electronics Exemption
  #mutate(tariff = if_else(I_COMMODITY_6 %in% c("847330","851713","851762","852351","852852","854110","854121","854129","854130","854151","854159","854190"), 0, tariff)) %>% #April 11th Electronics Exemption
  #mutate(tariff = if_else(I_COMMODITY_8 %in% c("85414910","85414970","85414980","85414995"), 0, tariff)) %>% #April 11th Electronics Exemption
  #mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010"), 0, tariff)) %>% #Car Batteries
  mutate(tariff_val = CON_VAL_YR*tariff) 

AGG_TARIFF_BY_CATEGORY_OLD <- AGG_TARIFF_ANALYSIS_OLD %>%
  filter(!(I_COMMODITY_8 %in% c("85411000", "85412100", "85412900", "85413000", "85414910", "85414970",
                                "85414980", "85414995", "85415100", "85415900", "85419000") & CTY_NAME != "CHINA" & CTY_NAME != "HONG KONG" & CTY_NAME != "MACAU")) %>% #excluding semiconductors from 4 digit category to just capture solar panels
  #filter(!(tariff_val == 0 & I_COMMODITY_4 == 8541)) %>% #excluding semiconductors from 4 digit category to just capture solar panels
  group_by(I_COMMODITY_4) %>%
  summarize(value = sum(CON_VAL_YR, na.rm = TRUE),tariff_val = sum(tariff_val, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(effective_tariff = tariff_val/value) %>%
  transmute(I_COMMODITY = as.numeric(I_COMMODITY_4), value, tariff_val, effective_tariff, round_tariff = round(effective_tariff,2)) %>%
  right_join(., HS4_LIST, by = "I_COMMODITY") %>%
  filter(I_COMMODITY <= 9800)

AGG_TARIFF_BY_CATEGORY_NEW <- AGG_TARIFF_ANALYSIS_NEW %>%
  filter(!(I_COMMODITY_8 %in% c("85411000", "85412100", "85412900", "85413000", "85414910", "85414970",
                                "85414980", "85414995", "85415100", "85415900", "85419000") & CTY_NAME != "CHINA" & CTY_NAME != "HONG KONG" & CTY_NAME != "MACAU")) %>% #excluding semiconductors from 4 digit category to just capture solar panels
  #filter(!(tariff_val == 0 & I_COMMODITY_4 == 8541)) %>% #excluding semiconductors from 4 digit category to just capture solar panels
  group_by(I_COMMODITY_4) %>%
  summarize(value = sum(CON_VAL_YR, na.rm = TRUE),tariff_val = sum(tariff_val, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(effective_tariff = tariff_val/value) %>%
  transmute(I_COMMODITY = as.numeric(I_COMMODITY_4), value, tariff_val, effective_tariff, round_tariff = round(effective_tariff,2)) %>%
  right_join(., HS4_LIST, by = "I_COMMODITY") %>%
  filter(I_COMMODITY <= 9800)


AGG_TARIFF_BY_CATEGORY_NEW <- AGG_TARIFF_BY_CATEGORY_NEW %>%
  arrange(desc(tariff_val))

AGG_TARIFF_BY_CATEGORY_OLD <- AGG_TARIFF_BY_CATEGORY_OLD %>%
  arrange(desc(tariff_val))

AGG_TARIFF_BY_CATEGORY_CHANGE <- merge(AGG_TARIFF_BY_CATEGORY_NEW,AGG_TARIFF_BY_CATEGORY_OLD, by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x, value = value.x, tariff_val_change = tariff_val.y-tariff_val.x, old_effective_tariff = round_tariff.y, new_effective_tariff = round_tariff.x) %>%
  arrange(desc(tariff_val_change)) %>%
  slice(1:15) %>%
  mutate(I_COMMODITY_LDESC = c("Batteries","Toys/Puzzles/Etc","Vehicle Parts","Video Game Consoles","Electric Heaters/Dryers/Irons","Seats & Chairs","Misc. Plastic Articles","Plastic Kitchen & Houseware","Misc. Furniture","Audio Equipment","Festive/Party Articles","Excercise Equipment & Pools","Misc. Textiles","Taps, Valves, Etc","Transformers")) %>%
  arrange(desc(new_effective_tariff)) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))


AGG_TARIFF_BY_CATEGORY_CHANGE_GRAPH <- AGG_TARIFF_BY_CATEGORY_CHANGE %>%
  pivot_longer(cols = c(old_effective_tariff, new_effective_tariff),
               names_to = "Tariff_Type",
               values_to = "Tariff") %>%
  ggplot(aes(x = I_COMMODITY_LDESC, y = Tariff)) +
  geom_line(aes(group = I_COMMODITY_LDESC, size = value), color = "grey60") +
  geom_point(aes(color = Tariff_Type, size = value)) +
  xlab(NULL) +
  ggtitle("Imports Getting Tariff Relief in China Pause") +
  ylab("Effective Tariff, 2024 Import Mix") +
  scale_size_continuous(range = c(2, 9), guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1.4), expand = c(0,0)) +
  
  scale_color_manual(name = NULL, values = c("old_effective_tariff" = "#EE6055", "new_effective_tariff" = "#00A99D"),
                     labels = c("Old Effective Tariff", "New Effective Tariff"),
                     breaks = c("old_effective_tariff","new_effective_tariff")) +
  labs(caption = "Graph created by @JosephPolitano using Census Data. Note: Data by 4-Digit HS Code. Tariff Calculated as Weighted Avg of 2024 Import Mix", subtitle = "Tariffs on Toys, Video Game Consoles, Batteries and More Dropped Significantly in Trump's Tariff Pause") +
  theme_apricitas + theme(legend.position = c(.7,.2), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip() +
  theme(plot.title.position = "plot") +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave(dpi = "retina",plot = AGG_TARIFF_BY_CATEGORY_CHANGE_GRAPH, "Tariff By Category Change Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


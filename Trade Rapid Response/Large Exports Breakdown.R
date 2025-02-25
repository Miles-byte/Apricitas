?getCensus
test <- listCensusApis()
sahie_variables <- listCensusMetadata(
  name = "timeseries/intltrade/exports/hs",
  type = "variables")
head(sahie_variables)

US_COUNTRIES_HS4_EXPORTS_BULK <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_YR", "E_COMMODITY","E_COMMODITY_LDESC","CTY_NAME","CTY_CODE","DF"),
  time = "2024-12",
  DF = 1,
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)


US_COUNTRIES_HS4_EXPORTS_TOTAL <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR", "E_COMMODITY","E_COMMODITY_LDESC","CTY_CODE","DF","CTY_NAME","COMM_LVL"),
  time = "2024-12",
  DF = 1,
  COMM_LVL = "HS4",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS4_EXPORTS_TOTAL <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR","E_COMMODITY","CTY_NAME","COMM_LVL"),
  time = "2024-12",
  #I_COMMODITY = "????",
  COMM_LVL = "HS4",
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS4_EXPORTS_TOTAL <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR","E_COMMODITY","CTY_CODE","CTY_NAME","COMM_LVL"),
  time = "2024-12",
  #I_COMMODITY = "????",
  COMM_LVL = "HS4",
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "CHINA",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS4_EXPORTS_1 <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR", "E_COMMODITY","CTY_CODE","DF","CTY_NAME","COMM_LVL"),
  time = "2024-12",
  DF = 1,
  COMM_LVL = "HS4",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS4_EXPORTS_2 <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR", "E_COMMODITY","E_COMMODITY_LDESC","CTY_CODE","DF","CTY_NAME","COMM_LVL"),
  time = "2024-12",
  DF = 1,
  COMM_LVL = "HS4",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS4_EXPORTS_3 <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR", "E_COMMODITY","E_COMMODITY_LDESC","CTY_CODE","DF","CTY_NAME","COMM_LVL"),
  time = "2024-12",
  DF = 1,
  COMM_LVL = "HS4",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  CTY_NAME = Countries[3],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS4_EXPORTS_BULK <- rbind(US_COUNTRIES_HS4_EXPORTS_TOTAL,US_COUNTRIES_HS4_EXPORTS_1) %>%
  rbind(.,US_COUNTRIES_HS4_EXPORTS_2) %>%
  rbind(.,US_COUNTRIES_HS4_EXPORTS_3)


#DOING TOTAL FOR ALL MENTIONED COUNTRIES, SO THAT IF TARIFFS ARE ANNOUNCED ON A BUNCH OF COUNTRIES YOU CAN LOOK AT A SUM
TOP_EXPORT_LEVELS_TOTAL <- US_COUNTRIES_HS4_EXPORTS_BULK %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR)) %>%
  filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
  group_by(E_COMMODITY) %>%
  select(ALL_VAL_YR,E_COMMODITY,E_COMMODITY_LDESC) %>%
  mutate(ALL_VAL_YR = sum(ALL_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  arrange(desc(ALL_VAL_YR)) %>%
  filter(E_COMMODITY != "9880")#excluding electricity

TOP_EXPORT_SHARES_TOTAL <- US_COUNTRIES_HS4_EXPORTS_BULK %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR)) %>%
  filter(CTY_NAME_1 == "TOTAL FOR ALL COUNTRIES") %>%
  group_by(E_COMMODITY) %>%
  select(ALL_VAL_YR,E_COMMODITY,E_COMMODITY_LDESC) %>%
  mutate(ALL_VAL_YR = sum(ALL_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  merge(.,TOP_EXPORT_LEVELS_TOTAL,by = "E_COMMODITY") %>%
  transmute(E_COMMODITY, E_COMMODITY_LDESC = E_COMMODITY_LDESC.x,ALL_VAL_YR = ALL_VAL_YR.y,ALL_VAL_SHARE = ALL_VAL_YR.y/ALL_VAL_YR.x) %>%
  filter(ALL_VAL_YR >= 5000000000) %>%
  filter(E_COMMODITY != "2716") %>% #excluding electricity
  arrange(desc(ALL_VAL_SHARE))





TOP_EXPORT_LEVELS_TOTAL_TOP5 <- TOP_EXPORT_LEVELS_TOTAL %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Gas, Diesel, etc","Vehicle Parts","Cars/Light Trucks","Crude Oil","Soybeans")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))

TOP_EXPORT_SHARES_TOTAL_TOP5 <- TOP_EXPORT_SHARES_TOTAL %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Gasoline ICE Engines","Freight Trucks","Vehicle Parts","Soybeans","Insulated Wires")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))


TOP_EXPORT_LEVELS_TOTAL_GRAPH <- ggplot(data = TOP_EXPORT_LEVELS_TOTAL_TOP5, aes(x = I_COMMODITY_LDESC, y = ALL_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ",I_COMMODITY_LDESC)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ylab("Annual Exports, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(TOP_EXPORT_LEVELS_TOTAL$ALL_VAL_YR/10000000000))*10), expand = c(0,0)) +
  labs(subtitle = "By Dollar Value") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

TOP_EXPORT_SHARE_TOTAL_GRAPH <- ggplot(data = TOP_EXPORT_SHARES_TOTAL_TOP5, aes(x = I_COMMODITY_LDESC, y = ALL_VAL_SHARE)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#EE6055") +
  geom_text(aes(label = paste0(" ",I_COMMODITY_LDESC)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ylab("% of Total US Exports in Category, 2024") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  labs(subtitle = "By % of US Exports") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

Title_Text <- paste(
  "Top US Exports to", 
  if (length(Countries) > 1) {
    if (length(Countries) == 2) {
      paste(stringr::str_to_title(Countries), collapse = " & ")
    } else {
      paste(
        paste(stringr::str_to_title(Countries[1:(length(Countries) - 1)]), collapse = ", "),
        stringr::str_to_title(Countries[length(Countries)]),
        sep = ", & "
      )
    }
  } else {
    stringr::str_to_title(Countries[1])
  }
)

tgrob <- text_grob(paste0(Title_Text),face = "bold",size = 29,color = "white")

# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0.5,0,0.5, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-.15,-0.15,-.15),"cm"))  

Large_Exports <- ggarrange(TOP_EXPORT_LEVELS_TOTAL_GRAPH,TOP_EXPORT_SHARE_TOTAL_GRAPH, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

Large_Exports <- ggarrange(plot_0,Large_Exports, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(.,bottom = text_grob("\nGraph Created by @Josephpolitano Using Census Data\nNOTE: Breakdowns Based on 4-Digit HS Codes. % of Imports Data Only For Goods Above $5B in Exports", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = Large_Exports, "Large Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

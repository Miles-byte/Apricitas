US_COUNTRIES_HS6_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL"), 
  time = "2023-12",
  COMM_LVL = "HS6",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[1],
  CTY_NAME = Countries[2],
  CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

TOP_IMPORT_LEVELS_TOTAL_HS6 <- US_COUNTRIES_HS6_IMPORTS_BULK %>%
  filter(str_detect(I_COMMODITY, "^01|^02|^03|^04|^07|^08|^09|^10|^11|^12|^16|^17|^18|^19|^20|^21|^22|^24")) %>% #filtering for only food items
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  arrange(desc(CON_VAL_YR))

TOP_IMPORT_SHARES_TOTAL_HS6 <- US_COUNTRIES_HS6_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 == "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  merge(.,TOP_IMPORT_LEVELS_TOTAL_HS6,by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x,CON_VAL_YR = CON_VAL_YR.y,CON_VAL_SHARE = CON_VAL_YR.y/CON_VAL_YR.x) %>%
  filter(CON_VAL_YR >= 1000000000) %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  arrange(desc(CON_VAL_SHARE))

TOP_IMPORT_LEVELS_TOTAL_TOP5 <- TOP_IMPORT_LEVELS_TOTAL_HS6 %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Crude Oil","Cars/Light Trucks","Computers","Phones","Vehicle Parts")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))

TOP_IMPORT_SHARES_TOTAL_TOP5 <- TOP_IMPORT_SHARES_TOTAL_HS6 %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Live Cattle","Tomatoes","Canola Oil","Particle Board","Holiday/Party Articles")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))


TOP_IMPORT_LEVELS_TOTAL_GRAPH <- ggplot(data = TOP_IMPORT_LEVELS_TOTAL_TOP5, aes(x = I_COMMODITY_LDESC, y = CON_VAL_YR/1000000000)) +
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
  ylab("Annual Imports, 2023") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(TOP_IMPORT_LEVELS_TOTAL$CON_VAL_YR/10000000000))*10), expand = c(0,0)) +
  labs(subtitle = "By Dollar Value") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

TOP_IMPORT_SHARE_TOTAL_GRAPH <- ggplot(data = TOP_IMPORT_SHARES_TOTAL_TOP5, aes(x = I_COMMODITY_LDESC, y = CON_VAL_SHARE)) +
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
  ylab("% of Total US Imports in Category, 2023") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  labs(subtitle = "By % of US Imports") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

Title_Text <- paste(
  "Top US Imports from", 
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

Large_Imports <- ggarrange(TOP_IMPORT_LEVELS_TOTAL_GRAPH,TOP_IMPORT_SHARE_TOTAL_GRAPH, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

Large_Imports <- ggarrange(plot_0,Large_Imports, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(.,bottom = text_grob("\nGraph Created by @Josephpolitano Using Census Data\nNOTE: Breakdowns Based on 4-Digit HS Codes. % of Imports Data Only For Goods Above $2B in Imports", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = Large_Imports, "Large Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



TOP_IMPORT_SHARES_LARGE_CATEGORIES <- US_COUNTRIES_HS4_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 == "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  merge(.,TOP_IMPORT_LEVELS_TOTAL,by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x,CON_VAL_YR = CON_VAL_YR.y,CON_VAL_SHARE = CON_VAL_YR.y/CON_VAL_YR.x) %>%
  filter(CON_VAL_YR >= 10000000000) %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  arrange(desc(CON_VAL_SHARE))

TOP_IMPORT_SHARES_TOTAL_LARGE_CATEGORIES <- TOP_IMPORT_SHARES_LARGE_CATEGORIES %>%
  slice(1:10) %>%
  mutate(I_COMMODITY_LDESC = c("Freight Trucks","Video Game Consoles","Toys, Puzzles, etc","TVs, Monitors, etc","Natural Gas, Propane, Butane, etc","Air Conditioners", "Crude Oil","Chairs & Seats","Vehicle Parts","Computers")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))


TOP_IMPORT_SHARE_TOTAL_LARGE_CATEGORIES_GRAPH <- ggplot(data = TOP_IMPORT_SHARES_TOTAL_LARGE_CATEGORIES, aes(x = I_COMMODITY_LDESC, y = CON_VAL_SHARE)) +
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
  ggtitle("Major Goods Where a Large % of US Imports\nCome From Canada, Mexico, & China") +
  ylab("% of Total US Imports in Category, 2023") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "By HS4 Code. Large Defined as >$10B in Imports in 2023") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = TOP_IMPORT_SHARE_TOTAL_LARGE_CATEGORIES_GRAPH, "Large Categories Import Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

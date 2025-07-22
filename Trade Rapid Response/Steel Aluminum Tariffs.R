ALUMINUM_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = "2024-12",
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
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

ALUMINUM_IMPORTS <- ALUMINUM_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(CTY_NAME) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN")) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(1:5) %>%
  mutate(CTY_NAME = c("Canada","China","UAE","Mexico","South Korea")) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(CTY_NAME)))


US_IMPORTS_ALUMINUM <- ggplot(data = ALUMINUM_IMPORTS, aes(x = CTY_NAME, y = CON_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#EE6055") +
  geom_text(aes(label = paste0(" ",CTY_NAME)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  #ggtitle("US Imports of Aluminum by Country") +
  ylab("Annual Imports, 2024, Selected HS codes Beginning with 76") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(ALUMINUM_IMPORTS$CON_VAL_YR/10000000000))*10), expand = c(0,0)) +
  labs(subtitle = "Aluminum") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()


STEEL_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = "2024-12",
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
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

STEEL_IMPORTS <- STEEL_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(CTY_NAME) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN")) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(1:5) %>%
  mutate(CTY_NAME = c("Canada","Mexico","Brazil","South Korea","Germany")) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(CTY_NAME)))


US_IMPORTS_STEEL <- ggplot(data = STEEL_IMPORTS, aes(x = CTY_NAME, y = CON_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ",CTY_NAME)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  #ggtitle("US Imports of Aluminum by Country") +
  ylab("Annual Imports, 2024, Selected HS Codes Beginning With 72/73") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(ALUMINUM_IMPORTS$CON_VAL_YR/10000000000))*10), expand = c(0,0)) +
  labs(subtitle = "Steel") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

Title_Text <- paste("Key Sources of US Steel & Aluminum Imports")

tgrob <- text_grob(paste0(Title_Text),face = "bold",size = 29,color = "white")

# Draw the text
plot_title <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0.5,0,0.5, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-.15,-0.15,-.15),"cm"))  

STEEL_ALUMINUM_TARIFFS <- ggarrange(US_IMPORTS_STEEL,US_IMPORTS_ALUMINUM, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

STEEL_ALUMINUM_TARIFFS <- ggarrange(plot_title,STEEL_ALUMINUM_TARIFFS, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(.,bottom = text_grob("Graph Created by @Josephpolitano Using Census Data", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = STEEL_ALUMINUM_TARIFFS, "Steel Aluminum Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

COPPER_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = "2024-12",
  I_COMMODITY = "74",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

COPPER_IMPORTS <- COPPER_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(CTY_NAME) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN")) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(1:5) %>%
  mutate(CTY_NAME = c("Chile","Canada","Mexico","Peru","Germany")) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(CTY_NAME)))

COPPER_IMPORTS <- ggplot(data = COPPER_IMPORTS, aes(x = CTY_NAME, y = CON_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ",CTY_NAME)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("US Imports of Copper (HS:74) by Country") +
  ylab("Annual Imports, 2024, Selected HS Codes Beginning With 72/73") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(COPPER_IMPORTS$CON_VAL_YR/10000000000))*10), expand = c(0,0)) +
  #labs(subtitle = "Copper") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = COPPER_IMPORTS, "Copper Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

WOOD_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = "2024-12",
  I_COMMODITY = "44",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

WOOD_IMPORTS <- WOOD_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(CTY_NAME) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN")) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(1:5) %>%
  mutate(CTY_NAME = c("Canada","China","Brazil","Chile","Vietnam")) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(CTY_NAME)))

WOOD_IMPORTS <- ggplot(data = WOOD_IMPORTS, aes(x = CTY_NAME, y = CON_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ",CTY_NAME)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("US Imports of Wood (HS:44) by Country") +
  ylab("Annual Imports, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,ceiling(max(WOOD_IMPORTS$CON_VAL_YR/10000000000))*10), expand = c(0,0)) +
  #labs(subtitle = "Copper") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = WOOD_IMPORTS, "Wood Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#https://www.butzel.com/alert-list-of-steel-and-aluminum-products-subject-to-section-232-tariff-changes-to-be-released-in-federal-register-tuesday-february-18
#https://www.federalregister.gov/documents/2018/03/15/2018-05477/adjusting-imports-of-aluminum-into-the-united-states
#https://www.federalregister.gov/documents/2018/03/15/2018-05478/adjusting-imports-of-steel-into-the-united-states
pacman::p_load(ggrepel,bea.R,tidyverse,ggpubr,purrr,sf,seasonal,tigris,maps,readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
#pacman::p_load(ggpubr,prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Countries <- c("CANADA","MEXICO")
#List Countries
US_COUNTRIES_HS4_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL"), 
  time = "2024-12",
  COMM_LVL = "HS4",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = Countries[1],
  CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

#DOING TOTAL FOR ALL MENTIONED COUNTRIES, SO THAT IF TARIFFS ARE ANNOUNCED ON A BUNCH OF COUNTRIES YOU CAN LOOK AT A SUM
TOP_IMPORT_LEVELS_TOTAL <- US_COUNTRIES_HS4_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  arrange(desc(CON_VAL_YR))

TOP_IMPORT_SHARES_TOTAL <- US_COUNTRIES_HS4_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 == "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  merge(.,TOP_IMPORT_LEVELS_TOTAL,by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x,CON_VAL_YR = CON_VAL_YR.y,CON_VAL_SHARE = CON_VAL_YR.y/CON_VAL_YR.x) %>%
  filter(CON_VAL_YR >= 2000000000) %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  arrange(desc(CON_VAL_SHARE))

TOP_IMPORT_LEVELS_TOTAL_TOP5 <- TOP_IMPORT_LEVELS_TOTAL %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Crude Oil","Cars/SUVs/Minivans","Computers","Vehicle Parts","Pickup/Delivery Trucks")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))

TOP_IMPORT_SHARES_TOTAL_TOP5 <- TOP_IMPORT_SHARES_TOTAL %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Live Cattle","Tomatoes","Canola Oil","Pickup/Delivery Trucks","Patricle Board")) %>%
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
  ylab("Annual Imports, 2024") +
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
  ylab("% of Total US Imports in Category, 2024") +
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
  annotate_figure(.,bottom = text_grob("\nGraph Created by @Josephpolitano Using Census Data\nNOTE: Breakdowns Based on 4-Digit HS Codes. % of Imports Data Only For Goods Above $5B in Imports", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

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
  filter(CON_VAL_YR >= 1000000000) %>%
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
  ylab("% of Total US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "By HS4 Code. Large Defined as >$10B in Imports in 2024") +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = TOP_IMPORT_SHARE_TOTAL_LARGE_CATEGORIES_GRAPH, "Large Categories Import Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_COUNTRIES_TOTAL_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "CTY_CODE", "CTY_NAME"), 
  time = "2024-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)
US_COUNTRIES_TOTAL_IMPORTS_BREAKDOWN <- US_COUNTRIES_TOTAL_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(CON_VAL_YR,CTY_NAME) %>%
  #mutate(CON_VAL_YR = ifelse(row_number() == 1, CON_VAL_YR - sum(CON_VAL_YR[-1]), CON_VAL_YR)) %>%
  arrange(desc(CON_VAL_YR)) %>%
  filter(!CTY_NAME %in% c("AUSTRALIA AND OCEANIA","AFRICA","CENTRAL AMERICA","AUSTRALIA & OCEANIA","SOUTH AMERICA","NORTH AMERICA","PACIFIC RIM COUNTRIES","CAFTA-DR","NAFTA","TWENTY LATIN AMERICAN REPUBLICS","OECD","NATO","LAFTA","EURO AREA","APEC","ASEAN","CACM","EUROPE","ASIA")) %>%
  filter(!CTY_NAME %in% c("EUROPEAN UNION")) %>%
  mutate(CON_VAL_YR = CON_VAL_YR / first(CON_VAL_YR)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "China",
                              CTY_NAME == "HONG KONG" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  group_by(CTY_NAME) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(-1) %>%
  slice(1:10) %>%
  mutate(TARIFF = c("Tariffs Paused For 30 Days","Tariffed","Tariffs Paused For 30 Days","Not Tariffed","Not Tariffed","Not Tariffed","Not Tariffed","Not Tariffed","Not Tariffed","Not Tariffed")) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "Korea, South" ~ "South Korea",
         TRUE ~ CTY_NAME)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "China" ~ "China incl. Hong Kong",
                              TRUE ~ CTY_NAME)) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = CTY_NAME[order(CON_VAL_YR)]))

US_COUNTRIES_TOTAL_IMPORTS_BREAKDOWN_Graph <- ggplot(data = US_COUNTRIES_TOTAL_IMPORTS_BREAKDOWN, aes(x = CTY_NAME, y = CON_VAL_YR, fill = TARIFF)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ylab("% of Goods Imports") +
  geom_text(aes(label = paste0(" ",CTY_NAME)), 
            position = position_stack(vjust = 0), # Centers text within the bars
            angle = 0, 
            hjust = 0, 
            size = 7,
            color = "white",
            fontface = "bold") +
  xlab(NULL) +
  ggtitle("Trump is Tariffing the Largest US Trade Partners") +
  ylab("% of Total US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.175), expand = c(0,0)) +
  labs(subtitle = "Top Sources of US Goods Imports, %") +
  scale_fill_manual(name= NULL,values = c("#D28E20","#EE6055","#00A99D"), breaks = c("Tariffs Paused For 30 Days","Tariffed","Not Tariffed"), labels = c("Tariffs Paused For 30 Days","Tariffed","Not Yet Tariffed")) +
  theme_apricitas + theme(legend.position = c(.65,.525), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), plot.title = element_text(size =27),plot.subtitle = element_text(size = 20, color = "white", face = "bold"),axis.text.y = element_blank()) +
  coord_flip()
  
ggsave(dpi = "retina",plot = US_COUNTRIES_TOTAL_IMPORTS_BREAKDOWN_Graph, "Impgoods Countries Total Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_COUNTRIES_TOTAL_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR", "CTY_CODE", "CTY_NAME","DF"), 
  time = "2024-12",
  DF = 1,
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[1],
  #CTY_NAME = Countries[2],
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

RECIPROCAL_TARIFF_IMPORTS_BREAKDOWN <- US_COUNTRIES_TOTAL_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(CON_VAL_YR,CTY_NAME) %>%
  #mutate(CON_VAL_YR = ifelse(row_number() == 1, CON_VAL_YR - sum(CON_VAL_YR[-1]), CON_VAL_YR)) %>%
  arrange(desc(CON_VAL_YR)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "CHINA",
                              CTY_NAME == "HONG KONG" ~ "CHINA",
                              TRUE ~ CTY_NAME)) %>%
  filter(CTY_NAME %in% c("ARGENTINA","AUSTRALIA","BRAZIL","CANADA","CHINA","EUROPEAN UNION","INDIA","INDONESIA","JAPAN","KOREA, SOUTH","MALAYSIA","MEXICO","RUSSIA","SAUDI ARABIA","SOUTH AFRICA","SWITZERLAND","TAIWAN","THAILAND","TURKEY","UNITED KINGDOM","VIETNAM")) %>%
  mutate(Imports = CON_VAL_YR) %>%
  group_by(CTY_NAME) %>%
  summarize(Imports = sum(Imports, na.rm = TRUE)) %>%
  arrange(desc(Imports)) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "Korea, South" ~ "South Korea",
                              TRUE ~ CTY_NAME)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "China" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = CTY_NAME[order(Imports)]))


RECIPROCAL_TARIFF_EXPORTS_BREAKDOWN <- US_COUNTRIES_TOTAL_EXPORTS %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR)) %>%
  select(ALL_VAL_YR,CTY_NAME) %>%
  #mutate(CON_VAL_YR = ifelse(row_number() == 1, CON_VAL_YR - sum(CON_VAL_YR[-1]), CON_VAL_YR)) %>%
  arrange(desc(ALL_VAL_YR)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "CHINA" ~ "CHINA",
                              CTY_NAME == "HONG KONG" ~ "CHINA",
                              TRUE ~ CTY_NAME)) %>%
  filter(CTY_NAME %in% c("ARGENTINA","AUSTRALIA","BRAZIL","CANADA","CHINA","EUROPEAN UNION","INDIA","INDONESIA","JAPAN","KOREA, SOUTH","MALAYSIA","MEXICO","RUSSIA","SAUDI ARABIA","SOUTH AFRICA","SWITZERLAND","TAIWAN","THAILAND","TURKEY","UNITED KINGDOM","VIETNAM")) %>%
  mutate(Exports = ALL_VAL_YR) %>%
  group_by(CTY_NAME) %>%
  summarize(Exports = sum(Exports, na.rm = TRUE)) %>%
  arrange(desc(Exports)) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "Korea, South" ~ "South Korea",
                              TRUE ~ CTY_NAME)) %>%
  mutate(CTY_NAME = case_when(CTY_NAME == "China" ~ "China",
                              TRUE ~ CTY_NAME)) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = CTY_NAME[order(RECIPROCAL_TARIFF_IMPORTS_BREAKDOWN$Imports)]))

RECIPROCAL_TARIFF_IMPORTS_EXPORTS <- merge(RECIPROCAL_TARIFF_IMPORTS_BREAKDOWN,RECIPROCAL_TARIFF_EXPORTS_BREAKDOWN, by = "CTY_NAME") %>%
  pivot_longer(-CTY_NAME)

NET_IMPORTS <- merge(RECIPROCAL_TARIFF_IMPORTS_BREAKDOWN,RECIPROCAL_TARIFF_EXPORTS_BREAKDOWN, by = "CTY_NAME") %>%
  mutate(Net_Imports = Imports - Exports)


RECIPROCAL_TARIFFS_graph <- ggplot(data = RECIPROCAL_TARIFF_IMPORTS_EXPORTS, aes(x = CTY_NAME, y = value/1000000000, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("2024 Trade, Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,600), expand = c(0,0)) +
  ggtitle("Countries of 'Particular Interest'\nID'ed for 'Reciprocal' Tariffs") +
  scale_fill_manual(name= "US Trade, 2024",values = c("#00A99D","#EE6055","#A7ACD9","#3083DC","#9A348E","#FFE98F","#FF8E72","#6A4C93"), breaks = c("Imports","Exports")) +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "Trump's 'Reciprocal' Tariffs Could Hit any Country—But USTR ID'ed These as Priorities") +
  theme_apricitas + theme(legend.position = c(.5,.65), axis.text.y = element_text(size = 14), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = RECIPROCAL_TARIFFS_graph, "Reciprocal Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Do A Print-Out Of the Tweet

#DO US-MADE EXPORTS For RETALIATION 


#DO STATE LEVEL EXPORTS AND IMPORTS DATA

#IMPORT SHARE OF EXPORT VALUE ADDED BEA

#MOST IMPACTED INDUSTRIES BEA

#DO USDA IMPORT SHARE  

?getCensus()
  
listCensusMetadata("timeseries/intltrade/imports/hs")
  
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
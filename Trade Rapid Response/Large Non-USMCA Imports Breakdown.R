pacman::p_load(scales,ggrepel,bea.R,tidyverse,ggpubr,purrr,sf,seasonal,tigris,maps,readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
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

US_COUNTRIES_HS4_IMPORTS_USMCA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL","RP"), 
  time = "2024-12",
  COMM_LVL = "HS4",
  RP = "18",
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
TOP_IMPORT_LEVELS_BULK <- US_COUNTRIES_HS4_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  filter(I_COMMODITY != "9801") %>% #excluding repairs
  arrange(desc(CON_VAL_YR))

TOP_IMPORT_LEVELS_USMCA <- US_COUNTRIES_HS4_IMPORTS_USMCA %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  filter(I_COMMODITY != "9801") %>% #excluding repairs
  arrange(desc(CON_VAL_YR))

TOP_IMPORT_LEVELS_TOTAL <- merge(TOP_IMPORT_LEVELS_USMCA,TOP_IMPORT_LEVELS_BULK, by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, CON_VAL_YR = CON_VAL_YR.y - CON_VAL_YR.x,I_COMMODITY_LDESC = I_COMMODITY_LDESC.x) %>%
  arrange(desc(CON_VAL_YR))

TOP_IMPORT_LEVELS_BULK_WORLD <- US_COUNTRIES_HS4_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 == "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  filter(I_COMMODITY != "9801") %>% #excluding repairs
  arrange(desc(CON_VAL_YR))

TOP_IMPORT_SHARES_BULK <- US_COUNTRIES_HS4_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  merge(.,TOP_IMPORT_LEVELS_BULK_WORLD,by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x,CON_VAL_YR = CON_VAL_YR.y,CON_VAL_SHARE = CON_VAL_YR.x/CON_VAL_YR.y) %>%
  filter(CON_VAL_YR >= 2000000000) %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  filter(I_COMMODITY != "9801") %>% #excluding repairs
  arrange(desc(CON_VAL_SHARE))

TOP_IMPORT_SHARES_USMCA <- US_COUNTRIES_HS4_IMPORTS_USMCA %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(CTY_NAME_1 %in% c("CANADA","MEXICO")) %>%
  group_by(I_COMMODITY) %>%
  select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
  mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  merge(.,US_COUNTRIES_HS4_IMPORTS_BULK %>%
          mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
          filter(CTY_NAME_1 != "TOTAL FOR ALL COUNTRIES") %>%
          group_by(I_COMMODITY) %>%
          select(CON_VAL_YR,I_COMMODITY,I_COMMODITY_LDESC) %>%
          mutate(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
          unique() %>%
          ungroup(),by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x,CON_VAL_YR = CON_VAL_YR.y-CON_VAL_YR.x) %>%
  merge(TOP_IMPORT_LEVELS_BULK_WORLD,by = "I_COMMODITY") %>%
  transmute(I_COMMODITY, I_COMMODITY_LDESC = I_COMMODITY_LDESC.x,CON_VAL_YR = CON_VAL_YR.x,CON_VAL_SHARE = CON_VAL_YR.x/CON_VAL_YR.y) %>%
  filter(CON_VAL_YR >= 2000000000) %>%
  filter(I_COMMODITY != "2716") %>% #excluding electricity
  filter(I_COMMODITY != "9801") %>% #excluding repairs
  arrange(desc(CON_VAL_SHARE))


TOP_IMPORT_LEVELS_TOTAL_TOP5 <- TOP_IMPORT_LEVELS_TOTAL %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Crude Oil","Vehicle Parts","Cars/SUVs/Minivans","Aluminum","Industrial/Aircraft Turbines")) %>%
  mutate(I_COMMODITY_LDESC = factor(I_COMMODITY_LDESC, levels = rev(I_COMMODITY_LDESC)))

TOP_IMPORT_SHARES_TOTAL_TOP5 <- TOP_IMPORT_SHARES_USMCA %>%
  slice(1:5) %>%
  mutate(I_COMMODITY_LDESC = c("Aluminum","Baked Goods","Silver","Trailers & Semi-Trailers","Crude Oil")) %>%
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
  "Top Non-USMCA Imports from", 
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

tgrob <- text_grob(paste0(Title_Text),face = "bold",size = 27,color = "white")

# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0.5,0,0.5, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-.15,-0.15,-.15),"cm"))  

Large_Imports <- ggarrange(TOP_IMPORT_LEVELS_TOTAL_GRAPH,TOP_IMPORT_SHARE_TOTAL_GRAPH, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

Large_Imports <- ggarrange(plot_0,Large_Imports, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(.,bottom = text_grob("\nGraph Created by @Josephpolitano Using Census Data\nNOTE: Breakdowns Based on 4-Digit HS Codes. % of Imports Data Only For Goods Above $2B in Imports", color = "grey55",hjust = 1, x = 1, size = 10))+ bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = Large_Imports, "Large Imports Non-USMCA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

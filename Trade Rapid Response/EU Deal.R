US_EU_HS10_IMPORTS_QUANTITY_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL","GEN_QY1_YR"), 
  time = "2024-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "EUROPEAN UNION",
)

US_EU_HS4_IMPORTS_QUANTITY_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL","GEN_QY1_YR"), 
  time = "2024-12",
  COMM_LVL = "HS4",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "EUROPEAN UNION",
)


US_EU_NRG_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_MO", "E_COMMODITY","CTY_CODE", "CTY_NAME","E_COMMODITY_LDESC"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "27",
  #I_COMMODITY = "*",#ALL COuntries
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CANADA",
)

US_EU_NRG_EXPORTS <- US_EU_NRG_EXPORTS_BULK %>%
  #filter(CTY_NAME_1 == "EUROPEAN UNION") %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(year = year(time)) %>%
  group_by(year,CTY_NAME_1) %>%
  filter(n() == 12) %>%
  summarise(ALL_VAL_MO = sum(ALL_VAL_MO)) %>%
  mutate(time = as.Date(paste0(year,"-01-01"))) %>%
  ungroup() %>%
  pivot_wider(names_from = CTY_NAME_1, values_from = ALL_VAL_MO)

EU_NRG_IMPORTS_BULK <- as.data.frame(readSDMX("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-045409/1.0/M.*.*.*.1.VALUE_IN_EUROS?c[reporter]=EU27_2020&c[partner]=EU27_2020_EXTRA&c[product]=27&compress=false")) 


EURO_EXCHANGE_RATE <- fredr("EXUSEU", observation_start = as.Date("2013-01-01")) %>%
  transmute(time = date, value)


EU_NRG_IMPORTS <- EU_NRG_IMPORTS_BULK %>%
  #filter(CTY_NAME_1 == "EUROPEAN UNION") %>%
  mutate(time = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  merge(.,EURO_EXCHANGE_RATE, by = "time") %>%
  mutate(OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  mutate(OBS_VALUE = OBS_VALUE/value) %>%
  mutate(year = year(time)) %>%
  group_by(year) %>%
  filter(n() == 12) %>%
  summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  mutate(time = as.Date(paste0(year,"-01-01"))) %>%
  ungroup()

US_EU_NRG_EXPORTS_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_EU_NRG_EXPORTS, aes(x=time,y= `EUROPEAN UNION`/1000000000,color= "US Energy Exports to the EU"), size = 1.25) + 
  geom_line(data=US_EU_NRG_EXPORTS, aes(x=time,y= `TOTAL FOR ALL COUNTRIES`/1000000000,color= "US Energy Exports to All Nations"), size = 1.25) + 
  geom_line(data=US_EU_NRG_EXPORTS, aes(x=time,y= (`TOTAL FOR ALL COUNTRIES`-MEXICO-CANADA)/1000000000,color= "US Energy Exports to All Overseas Nations"), size = 1.25) + 
  annotate("text", label = "What Trump Claims the EU\nHas Agreed to Buy From the US", x = as.Date("2015-04-25"), y = 260, color = "white", size = 5, hjust = 0, lineheight = 0.8) +
  annotate("hline", y = 0.225, yintercept = 225, color = "white", size = 1.25, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,400),breaks = c(0,50,100,150,200,250,300,350,400), expand = c(0,0)) +
  ylab("Dollars, Annual Rate") +
  ggtitle("Trump's EU 'Deal' is Unrealistic") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Trump Claims the EU will 3x Energy Imports, Which Cannot Realistically Happen") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*(400)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_EU_NRG_EXPORTS_Graph, "US EU NRG Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR","CTY_CODE", "CTY_NAME"), 
  time = "2024-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
)

US_IMPORTS_DATA <- US_IMPORTS_BULK %>%
  filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES","AUSTRALIA AND OCEANIA","AFRICA","CENTRAL AMERICA","AUSTRALIA & OCEANIA","SOUTH AMERICA","NORTH AMERICA","PACIFIC RIM COUNTRIES","CAFTA-DR","USMCA (NAFTA)","NAFTA","TWENTY LATIN AMERICAN REPUBLICS","OECD","NATO","LAFTA","EURO AREA","APEC","ASEAN","CACM","EUROPE","ASIA")) %>%
  filter(!CTY_NAME %in% c(c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECH REPUBLIC",
                            "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
                            "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA",
                            "NETHERLANDS", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA",
                            "SPAIN", "SWEDEN"))) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice_head(n = 19) %>%
  bind_rows(
    US_IMPORTS_BULK %>%
      filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES","AUSTRALIA AND OCEANIA","AFRICA","CENTRAL AMERICA","AUSTRALIA & OCEANIA","SOUTH AMERICA","NORTH AMERICA","PACIFIC RIM COUNTRIES","CAFTA-DR","USMCA (NAFTA)","NAFTA","TWENTY LATIN AMERICAN REPUBLICS","OECD","NATO","LAFTA","EURO AREA","APEC","ASEAN","CACM","EUROPE","ASIA",
                              "AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECH REPUBLIC",
                              "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
                              "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA",
                              "NETHERLANDS", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA",
                              "SPAIN", "SWEDEN")) %>%
      arrange(desc(CON_VAL_YR)) %>%
      slice(-(1:19)) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
      mutate(CTY_NAME = "Other"),
    .) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, South", "South Korea", CTY_NAME)) %>%
  arrange(time) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(CTY_NAME)))


IMPORTS_TOP_20_Graph <- ggplot(data = US_IMPORTS_DATA, aes(x = CTY_NAME, y = CON_VAL_YR/1000000000,fill = "US Imports, 2024")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("Imports, Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,650), expand = c(0,0)) +
  ggtitle("The European Union is \nAmerica's Largest Trade Partner") +
  labs(caption = "Graph created by @JosephPolitano using Census Data") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = IMPORTS_TOP_20_Graph, "Imports Top 20 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")






Ask ChatGPT

  


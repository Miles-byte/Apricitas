pacman::p_load(censusapi,ggpubr,prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


TARIFF_MAP <- map_data("world") %>%
  mutate(region = ifelse(region == "Western Sahara", "Morocco", region)) %>%
  mutate(region = ifelse(region == "Palestine", "Israel", region)) %>% #The US does not recognize Western Sahara or Palestine so they are treated as parts of Morocco and Israel for the purposes of tariffs
  filter(long < 170 & long > -170) %>%
  filter(region != "Antarctica")

LETTERS_DATA <- list(
  #Afghanistan = 0.30,
  Afghanistan = 0.15,
  Japan = 0.15,
  `South Korea` = 0.15,
  #Malaysia = 0.25,
  Malaysia = 0.19,
  Kazakhstan = 0.25,
  `South Africa` = 0.30,
  Laos = 0.40,
  Myanmar = 0.40,
  Tunisia = 0.25,
  `Bosnia and Herzegovina` = 0.30,
  #Indonesia = 0.32,
  Indonesia = 0.19,
  Vietnam = 0.20,
  #Bangladesh = 0.35,
  Bangladesh = 0.20,
  Serbia = 0.35,
  #Cambodia = 0.36,
  Cambodia = 0.19,
  #Thailand = 0.36,
  Thailand = 0.19,
  #Philippines = 0.20,
  Philippines = 0.19,
  Brunei = 0.25,
  Moldova = 0.25,
  Algeria = 0.30,
  #Iraq = 0.30,
  Iraq = 0.35,
  Libya = 0.30,
  #`Sri Lanka` = 0.30,
  `Sri Lanka` = 0.20,
  Brazil = 0.50,
  Canada = 0.35,
  Mexico = 0.25,
  #EU Countries
  # Portugal = 0.30,
  # Spain = 0.30,
  # France = 0.30,
  # Ireland = 0.30,
  # Belgium = 0.30,
  # Netherlands = 0.30,
  # Germany = 0.30,
  # Italy = 0.30,
  # Czechia = 0.30,
  # `Czech Republic` = 0.30,
  # Austria = 0.30,
  # Slovenia = 0.30,
  # Croatia = 0.30,
  # Hungary = 0.30,
  # Romania = 0.30,
  # Slovakia = 0.30,
  # Poland = 0.30,
  # Bulgaria = 0.30,
  # Greece = 0.30,
  # Cyprus = 0.30,
  # Malta = 0.30,
  # Denmark = 0.30,
  # Sweden = 0.30,
  # Finland = 0.30,
  # Estonia = 0.30,
  # Latvia = 0.30,
  # Lithuania = 0.30,
  # Luxembourg = 0.30,
  Portugal = 0.15,
  Spain = 0.15,
  France = 0.15,
  Ireland = 0.15,
  Belgium = 0.15,
  Netherlands = 0.15,
  Germany = 0.15,
  Italy = 0.15,
  #Czechia = 0.15,
  `Czech Republic` = 0.15,
  Austria = 0.15,
  Slovenia = 0.15,
  Croatia = 0.15,
  Hungary = 0.15,
  Romania = 0.15,
  Slovakia = 0.15,
  Poland = 0.15,
  Bulgaria = 0.15,
  Greece = 0.15,
  Cyprus = 0.15,
  Malta = 0.15,
  Denmark = 0.15,
  Sweden = 0.15,
  Finland = 0.15,
  Estonia = 0.15,
  Latvia = 0.15,
  Lithuania = 0.15,
  Luxembourg = 0.15,
  `European Union` = 0.15,
  UK = 0.10,
  #India = 0.25,
  India = 0.5,
  Angola = 0.15,
  Bolivia = 0.15,
  Botswana = 0.15,
  Cameroon = 0.15,
  Chad = 0.15,
  `Costa Rica` = 0.15,
  
  #`Cote d'Ivoire` = 0.15,
  `Ivory Coast` = 0.15,
  
  `Democratic Republic of the Congo` = 0.15,
  `Falkland Islands` = 0.10,
  `North Macedonia` = 0.15,
  `Papua New Guinea` = 0.15,
  Taiwan = 0.20,
  `Trinidad and Tobago` = 0.15,
  
  Ecuador = 0.15,
  `Equatorial Guinea` = 0.15,
  Fiji = 0.15,
  Ghana = 0.15,
  Guyana = 0.15,
  Iceland = 0.15,
  Israel = 0.15,
  Jordan = 0.15,
  Liechtenstein = 0.15,
  Madagascar = 0.15,
  Malawi = 0.15,
  Mauritius = 0.15,
  Mozambique = 0.15,
  Namibia = 0.15,
  Nauru = 0.15,
  `New Zealand` = 0.15,
  Nicaragua = 0.18,
  Nigeria = 0.15,
  Norway = 0.15,
  Pakistan = 0.19,
  Switzerland = 0.39,
  Syria = 0.41,
  Turkey = 0.15,
  Uganda = 0.15,
  Vanuatu = 0.15,
  Venezuela = 0.15,
  Zambia = 0.15,
  Zimbabwe = 0.15,
  Lesotho = 0.15,
  Greenland = 0.15#,
  #China = 0.30
  #Russia = NA,
  #`North Korea` = NA,
  #Cuba = NA
) %>%
  enframe(name = "region", value = "tariff") %>%
  mutate(tariff = unlist(tariff)) %>%
  as_tibble()


TARIFF_MAP <- map_data("world") %>%
  mutate(region = ifelse(region == "Western Sahara", "Morocco", region)) %>%
  mutate(region = ifelse(region == "Palestine", "Israel", region)) %>% #The US does not recognize Western Sahara or Palestine so they are treated as parts of Morocco and Israel for the purposes of tariffs
  full_join(LETTERS_DATA, by = "region") %>%
  mutate(
    tariff = if_else(is.na(tariff), 0.10, tariff),
    tariff = if_else(region %in% c("Cuba", "Russia", "North Korea","USA"), NA, tariff)
  ) %>%
  filter(long < 170 & long > -170) %>%
  filter(region != "Antarctica")


TARIFF_LETTERS_MAP_Graph <- TARIFF_MAP %>% 
  ggplot(aes(fill = tariff, map_id = region)) +
  geom_map(map = TARIFF_MAP, linewidth = 1) +
  expand_limits(x = TARIFF_MAP$long, y = TARIFF_MAP$lat) +
  coord_map("mercator") +
  theme_apricitas + 
  ggtitle(paste("Trump's New Tariff Rates")) +
  scale_y_continuous(limits = c(-50,75)) +
  scale_x_continuous(limits = c(-180,180)) +
  labs(caption = "Graph created by @JosephPolitano using Census data. NOTE: India Tariffs Take Effect 8/27", subtitle = "Trump Is Aggressively Raising Tariff Rates on Imports From Most of the World") +
  labs(fill = NULL) +
  scale_fill_gradientn(name= "Tariff Rate",colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0),limits = c(0,.5)) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0,0.1,-0.1,0.5), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = TARIFF_LETTERS_MAP_Graph, "Tariff Letters Map Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


US_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("GEN_VAL_YR","CTY_CODE", "CTY_NAME"), 
  time = "2024-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR","CTY_CODE", "CTY_NAME"), 
  time = "2024-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

EU_27 <- c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECH REPUBLIC",
           "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
           "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA",
           "NETHERLANDS", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA",
           "SPAIN", "SWEDEN")

RECIPROCAL_RATES <- merge(US_IMPORTS_BULK,US_EXPORTS_BULK, by = "CTY_NAME") %>%
  transmute(CTY_NAME, reciprocal_tariff = (as.numeric(GEN_VAL_YR)-as.numeric(ALL_VAL_YR))/as.numeric(GEN_VAL_YR)) %>%
  mutate(reciprocal_tariff = round(reciprocal_tariff,2)/2) %>%
  mutate(reciprocal_tariff = round(reciprocal_tariff,2)) %>%
  mutate(reciprocal_tariff = ifelse(reciprocal_tariff < 0.1, 0.1, reciprocal_tariff)) %>%
  mutate(reciprocal_tariff = ifelse(CTY_NAME == "AFGHANISTAN", 0.1, reciprocal_tariff)) %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "United Kingdom", "UK", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Democratic Republic Of The Congo", "Democratic Republic of the Congo", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Congo (Brazzaville)", "Republic of Congo", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, South", "South Korea", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, North", "North Korea", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Burma", "Myanmar", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Bosnia And Herzegovina", "Bosnia and Herzegovina", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Trinidad And Tobago", "Trinidad and Tobago", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Cote D'ivoire", "Ivory Coast", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Macedonia", "North Macedonia", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Falkland Islands (Islas Malvinas)", "Falkland Islands", CTY_NAME)) %>%
  select(CTY_NAME, reciprocal_tariff) %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA"))


LETTER_TARIFF_RATE <- US_IMPORTS_BULK %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "United Kingdom", "UK", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Democratic Republic Of The Congo", "Democratic Republic of the Congo", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Congo (Brazzaville)", "Republic of Congo", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, South", "South Korea", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, North", "North Korea", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Burma", "Myanmar", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Bosnia And Herzegovina", "Bosnia and Herzegovina", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Trinidad And Tobago", "Trinidad and Tobago", CTY_NAME)) %>%
  
  mutate(CTY_NAME = ifelse(CTY_NAME == "Cote D'ivoire", "Ivory Coast", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Macedonia", "North Macedonia", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Falkland Islands (Islas Malvinas)", "Falkland Islands", CTY_NAME)) %>%
  #mutate(CTY_NAME = ifelse(CTY_NAME == "Czech Republic", "Czechia", CTY_NAME)) %>%
  full_join(.,LETTERS_DATA, by = c("CTY_NAME" = "region")) %>%
  drop_na()
  
  
LETTER_RECIPROCAL_TARIFF_RATES <- full_join(RECIPROCAL_RATES,LETTER_TARIFF_RATE, by = "CTY_NAME") %>%
  drop_na() %>%
  filter(!CTY_NAME %in% str_to_title(EU_27)) %>%
  filter(!CTY_NAME %in% c("Mexico","Canada")) %>%
  filter(!CTY_NAME %in% c("Brazil")) %>%
  filter(!CTY_NAME %in% c("China"))

#tariff by country calculated in Reciprocal Tariff Code
TARIFF_BY_COUNTRY_EDITED <- TARIFF_BY_COUNTRY %>%
  mutate(CTY_NAME = str_to_title(CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "United Kingdom", "UK", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Democratic Republic Of The Congo", "Democratic Republic of the Congo", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Congo (Brazzaville)", "Republic of Congo", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, South", "South Korea", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Korea, North", "North Korea", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Burma", "Myanmar", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Bosnia And Herzegovina", "Bosnia and Herzegovina", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Trinidad And Tobago", "Trinidad and Tobago", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Cote D'ivoire", "Ivory Coast", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Macedonia", "North Macedonia", CTY_NAME)) %>%
  mutate(CTY_NAME = ifelse(CTY_NAME == "Falkland Islands (Islas Malvinas)", "Falkland Islands", CTY_NAME)) %>%
  select(CTY_NAME, value)

LETTER_RECIPROCAL_TARIFF_RATES <- left_join(LETTER_RECIPROCAL_TARIFF_RATES,TARIFF_BY_COUNTRY_EDITED, by = "CTY_NAME") %>%
  add_row(CTY_NAME = "Brazil",
          reciprocal_tariff = 0.10,
          time = "2024-12",
          GEN_VAL_YR = NA,
          CTY_CODE = NA,
          tariff = 0.50,
          value = 21336234382)



LETTER_VS_RECIPROCAL_TARIFF_RATES_graph <- ggplot(LETTER_RECIPROCAL_TARIFF_RATES, aes(x = reciprocal_tariff, y = tariff, size = value, fill = "Countries", color = "Countries")) +
  geom_point(shape = 21, alpha = 0.6, stroke = 1, color = "#FFE98F") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "white", size = 1.25) +
  geom_text(label = "Line Where Aug & Apr\nTariffs are Equal", x = .42, y = .42, color = "white", size = 4, angle = 25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6), expand = c(0,0), breaks = c(.1,.2,.3,.4,.5,.6)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6), expand = c(0,0), breaks = c(.1,.2,.3,.4,.5,.6)) +
  scale_size(range = c(0, 15)) +
  labs(x = "April 2nd Tariff Rate", y = "August\nTariff Rate") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  theme_apricitas +
  theme(
    axis.title.x = element_text(color = "white",size = 16, face = "bold"),
    axis.title.y = element_text(color = "white",size = 16, face = "bold",angle = 0),
    legend.position = c(.25,.75)
  ) +
  #guides(size = "none")
  ggtitle(paste("Trump's New vs Old Tariff Rates")) +
  labs(caption = "Graph created by @JosephPolitano using Census data. Only for Countries Sent new Tariffs in Aug 1st EO.", subtitle = "Trump's New Tariff Rates are Mostly a Reduction & Rounding of April 2nd Rates") +
  guides(
    size = "none",
    fill = "none",
    #fill = guide_legend(override.aes = list(size = 5, fill = "#FFE98F", colour = "#FFE98F"))
  ) +
  geom_text_repel(
    data = LETTER_RECIPROCAL_TARIFF_RATES %>%
      arrange(desc(value)) %>%
      slice_head(n = 5),
    aes(x = reciprocal_tariff, y = tariff, label = CTY_NAME),
    color = "white",
    segment.color = "white",
    size = 4,
    force = 10,
    box.padding = unit(2, "lines"),
    point.padding = unit(0, "lines")
  ) +
  geom_text_repel(
    data = filter(LETTER_RECIPROCAL_TARIFF_RATES, CTY_NAME == "Brazil"),
    aes(x = reciprocal_tariff, y = tariff, label = CTY_NAME),
    color = "white",
    segment.color = "white",
    size = 4,
    force = 10,
    box.padding = unit(2, "lines"),
    point.padding = unit(0, "lines")
  )


ggsave(dpi = "retina",plot = LETTER_VS_RECIPROCAL_TARIFF_RATES_graph, "Letter vs Reciprocal Tariff Rates Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


AGG_LETTER_TARIFFS <- LETTER_RECIPROCAL_TARIFF_RATES %>%
  #filter(tariff >.10) %>%
  mutate(tariff_val = (tariff-.1)*value) %>%
  summarise(tariff_val = sum(tariff_val))

?geom_text

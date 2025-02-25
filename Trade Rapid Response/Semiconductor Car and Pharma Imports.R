CAR_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8703", #Cars/Light Trucks
  I_COMMODITY = "8708", #Vehicle Parts
  I_COMMODITY = "8704", #Freight Trucks
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "MEXICO",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "JAPAN",
  CTY_NAME = "CANADA",
  CTY_NAME = "KOREA, SOUTH"
)

CAR_IMPORTS <- CAR_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(str_detect(time, "12$")) %>%
  # filter(time == "2024-12") %>%
  # filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CENTRAL AMERICA","CAFTA-DR","CACM")) %>%
  # group_by(CTY_NAME) %>%
  # summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  # ungroup() %>%
  # arrange(desc(CON_VAL_YR))
  select(time, CON_VAL_YR, I_COMMODITY, CTY_NAME) %>%
  mutate(I_COMMODITY = case_when(I_COMMODITY == "8703" ~ "Cars/SUVs/Minivans",
                                 I_COMMODITY == "8704" ~ "Pickup/Delivery Trucks",
                                 I_COMMODITY == "8708" ~ "Vehicle Parts",
                          TRUE ~ I_COMMODITY)) %>%
  pivot_wider(names_from = "CTY_NAME", values_from = CON_VAL_YR) %>%
  mutate(`Other` = `TOTAL FOR ALL COUNTRIES` - rowSums(select(., -`TOTAL FOR ALL COUNTRIES`,-time,-I_COMMODITY), na.rm = TRUE)) %>%
  mutate(date = as.Date(paste0(time,"-01-01"))) %>%
  select(-`TOTAL FOR ALL COUNTRIES`,-time) %>%
  pivot_longer(-c(date,I_COMMODITY)) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = case_when(name == "Korea, South" ~ "South Korea",
                          TRUE ~ name)) %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  mutate(name = factor(name, levels = rev(c("Mexico", "Canada", "European Union", "Japan", "South Korea", "Other")))) %>%
  mutate(I_COMMODITY = factor(I_COMMODITY, levels = c("Cars/SUVs/Minivans","Vehicle Parts","Pickup/Delivery Trucks")))

?facet_wrap

US_CAR_IMPORTS_DOLLAR_BAR_Graph <- ggplot(data = filter(CAR_IMPORTS, date >= as.Date("2013-01-01")), aes(x = date, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  facet_wrap(~ I_COMMODITY, scales = "fixed") +
  xlab("Date") +
  ylab("Billions of Dollars, Annual") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,100,200), limits = c(0,250), expand = c(0,0)) +
  ggtitle("Key US Vehicle Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data. Cars/SUVs/Minivans are HS Code 8703, Pickup/Delivery Trucks 8704, Vehicle Parts 8708", subtitle = "The US Imports Hundreds of Billions in Cars Mostly From Mexico, Canada, the EU, & Japan") +
  theme_apricitas + theme(legend.position = c(.5,.75)) + theme(strip.text = element_text(size = 15, color = "white", face = "bold")) + #theme(legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F","#FF8E72","#6A4C93")) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CAR_IMPORTS_DOLLAR_BAR_Graph, "US Car Imports Dollar Bar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE




SEMICONDUCTOR_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR","CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8542", #Semiconductors
  CTY_NAME = "TAIWAN",
  CTY_NAME = "MALAYSIA",
  CTY_NAME = "ISRAEL",
  CTY_NAME = "KOREA, SOUTH",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES"
)

SEMICONDUCTOR_IMPORTS <- SEMICONDUCTOR_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(str_detect(time, "12$")) %>%
  # filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CENTRAL AMERICA","CAFTA-DR","CACM")) %>%
  # group_by(CTY_NAME) %>%
  # summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  #ungroup() %>%
  #arrange(desc(CON_VAL_YR)) %>%
  select(time, CON_VAL_YR,CTY_NAME) %>%
  pivot_wider(names_from = "CTY_NAME", values_from = CON_VAL_YR) %>%
  mutate(`Other` = `TOTAL FOR ALL COUNTRIES` - rowSums(select(., -`TOTAL FOR ALL COUNTRIES`,-time), na.rm = TRUE)) %>%
  mutate(date = as.Date(paste0(time,"-01-01"))) %>%
  select(-`TOTAL FOR ALL COUNTRIES`,-time) %>%
  pivot_longer(-date) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = case_when(name == "Korea, South" ~ "South Korea",
                                     TRUE ~ name)) %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  mutate(name = factor(name, levels = rev(c("Taiwan", "Malaysia", "Israel", "European Union", "South Korea", "Other"))))

US_CHIP_IMPORTS_DOLLAR_BAR_Graph <- ggplot(data = filter(SEMICONDUCTOR_IMPORTS, date >= as.Date("2013-01-01")), aes(x = date, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Annual") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,5,10,15,20,25,30,35,40,45), limits = c(0,45), expand = c(0,0)) +
  ggtitle("US Semiconductor Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data. Semiconductors are HS Code 8542.", subtitle = "The US Imports Nearly $40B of Chips Each Year, With Taiwan Representing the Largest Source") +
  theme_apricitas + theme(legend.position = c(.175,.85)) + theme(legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F","#FF8E72","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*45), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CHIP_IMPORTS_DOLLAR_BAR_Graph, "US Chip Imports Dollar Bar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


PHARMA_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "3002", #Semiconductors
  I_COMMODITY = "3004", #Packaged Medicine
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "SWITZERLAND",
  CTY_NAME = "SINGAPORE",
  CTY_NAME = "INDIA",
  CTY_NAME = "JAPAN",
)


PHARMA_IMPORTS <- PHARMA_IMPORTS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  filter(str_detect(time, "12$")) %>%
  # filter(!CTY_NAME %in% c("TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CENTRAL AMERICA","CAFTA-DR","CACM")) %>%
  # group_by(CTY_NAME) %>%
  # summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  #ungroup() %>%
  #arrange(desc(CON_VAL_YR)) %>%
  select(time, CON_VAL_YR, I_COMMODITY, CTY_NAME) %>%
  mutate(I_COMMODITY = case_when(I_COMMODITY == "3004" ~ "Packaged Medicine",
                                 I_COMMODITY == "3002" ~ "Vaccines/Antibodies/Blood",
                                 TRUE ~ I_COMMODITY)) %>%
  pivot_wider(names_from = "CTY_NAME", values_from = CON_VAL_YR) %>%
  mutate(`Other` = `TOTAL FOR ALL COUNTRIES` - rowSums(select(., -`TOTAL FOR ALL COUNTRIES`,-time,-I_COMMODITY), na.rm = TRUE)) %>%
  mutate(date = as.Date(paste0(time,"-01-01"))) %>%
  select(-`TOTAL FOR ALL COUNTRIES`,-time) %>%
  pivot_longer(-c(date,I_COMMODITY)) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = case_when(name == "Korea, South" ~ "South Korea",
                          TRUE ~ name)) %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  mutate(name = factor(name, levels = rev(c("European Union","Switzerland","Singapore","India","Japan","Other")))) %>%
  mutate(I_COMMODITY = factor(I_COMMODITY, levels = c("Packaged Medicine","Vaccines/Antibodies/Blood")))

US_PHARMA_IMPORTS_DOLLAR_BAR_Graph <- ggplot(data = filter(PHARMA_IMPORTS, date >= as.Date("2013-01-01")), aes(x = date, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  facet_wrap(~ I_COMMODITY, scales = "fixed") +
  xlab("Date") +
  ylab("Billions of Dollars, Annual") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,50,100,150), limits = c(0,150), expand = c(0,0)) +
  ggtitle("Key US Pharmaceutical Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census data. Packaged Medicine is HS Code 3004, Vaccines/Antibodies/Blood 3002", subtitle = "The US Imports Hundreds of Billions in Pharmaceuticals, Mostly from the European Union") +
  theme_apricitas + theme(legend.position = c(.12,.775)) + theme(strip.text = element_text(size = 17, color = "white", face = "bold")) + #theme(legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F","#FF8E72","#6A4C93")) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_PHARMA_IMPORTS_DOLLAR_BAR_Graph, "US Pharma Imports Dollar Bar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


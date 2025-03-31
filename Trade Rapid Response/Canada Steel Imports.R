ALUMINUM_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR","I_COMMODITY","CTY_CODE","CTY_NAME","I_COMMODITY_LDESC","CON_QY1_YR","UNIT_QY1"),
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "7601103000",
  I_COMMODITY = "7601106030",
  I_COMMODITY = "7601106090",
  I_COMMODITY = "7601203000",
  I_COMMODITY = "7601106000",
  I_COMMODITY = "7601206000",
  I_COMMODITY = "7601209030",
  I_COMMODITY = "7601209045",
  I_COMMODITY = "7601209060",
  I_COMMODITY = "7601209075",
  I_COMMODITY = "7601209080",
  I_COMMODITY = "7601209085",
  I_COMMODITY = "7601209090",
  I_COMMODITY = "7601209095",
  I_COMMODITY = "7604101000",
  I_COMMODITY = "7604103000",
  I_COMMODITY = "7604105000",
  I_COMMODITY = "7604210010",
  I_COMMODITY = "7604210090",
  I_COMMODITY = "7604291000",
  I_COMMODITY = "7604291010",
  I_COMMODITY = "7604291090",
  I_COMMODITY = "7604293010",
  I_COMMODITY = "7604293050",
  I_COMMODITY = "7604293030",
  I_COMMODITY = "7604293060",
  I_COMMODITY = "7604293090",
  I_COMMODITY = "7604295030",
  I_COMMODITY = "7604295060",
  I_COMMODITY = "7606123030",
  I_COMMODITY = "7604295020",
  I_COMMODITY = "7604295050",
  I_COMMODITY = "7604295090",
  I_COMMODITY = "7605110000",
  I_COMMODITY = "7605210000",
  I_COMMODITY = "7605190000",
  I_COMMODITY = "7605290000",
  I_COMMODITY = "7606913030",
  I_COMMODITY = "7606123090",
  I_COMMODITY = "7606123091",
  I_COMMODITY = "7606123096",
  I_COMMODITY = "7606113060",
  I_COMMODITY = "7606116000",
  I_COMMODITY = "7606916095",
  I_COMMODITY = "7606923035",
  I_COMMODITY = "7606926095",
  I_COMMODITY = "7606923060",
  I_COMMODITY = "7606923075",
  I_COMMODITY = "7606923090",
  I_COMMODITY = "7607116000",
  I_COMMODITY = "7606926020",
  I_COMMODITY = "7606926040",
  I_COMMODITY = "7606926060",
  I_COMMODITY = "7606926080",
  I_COMMODITY = "7606913060",
  I_COMMODITY = "7606913075",
  I_COMMODITY = "7606926090",
  I_COMMODITY = "7606916020",
  I_COMMODITY = "7606916040",
  I_COMMODITY = "7606916060",
  I_COMMODITY = "7606916080",
  I_COMMODITY = "7606923030",
  I_COMMODITY = "7606913095",
  I_COMMODITY = "7606126000",
  I_COMMODITY = "7606913055",
  I_COMMODITY = "7606123015",
  I_COMMODITY = "7606123025",
  I_COMMODITY = "7606123035",
  I_COMMODITY = "7606113030",
  I_COMMODITY = "7606916055",
  I_COMMODITY = "7606923025",
  I_COMMODITY = "7606926055",
  I_COMMODITY = "7606123045",
  I_COMMODITY = "7606123055",
  I_COMMODITY = "7607119030",
  I_COMMODITY = "7607119060",
  I_COMMODITY = "7607116010",
  I_COMMODITY = "7607116090",
  I_COMMODITY = "7607119090",
  I_COMMODITY = "7607191000",
  I_COMMODITY = "7607193000",
  I_COMMODITY = "7607196000",
  I_COMMODITY = "7607113000",
  I_COMMODITY = "7607201000",
  I_COMMODITY = "7607205000",
  I_COMMODITY = "7608100030",
  I_COMMODITY = "7608200030",
  I_COMMODITY = "7608100090",
  I_COMMODITY = "7608200090",
  I_COMMODITY = "7609000000",
  I_COMMODITY = "7616995160",
  I_COMMODITY = "7616995170",
  I_COMMODITY = "7616995060",
  I_COMMODITY = "7616995070",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "CANADA",
  CTY_NAME = "CHINA",
  CTY_NAME = "MEXICO",
  CTY_NAME = "RUSSIA",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "KOREA, SOUTH",
  CTY_NAME = "UNITED ARAB EMIRATES",
  CTY_NAME = "BAHRAIN",
)

ALUMINUM_IMPORTS <- ALUMINUM_IMPORTS_BULK %>%
  filter(str_detect(time, "-12$")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR),CON_QY1_YR = as.numeric(CON_QY1_YR)) %>%
  group_by(time,CTY_NAME) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE),CON_QY1_YR = sum(CON_QY1_YR, na.rm = TRUE)) %>%
  select(-CON_VAL_YR) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_QY1_YR) %>%
  mutate(OTHER = `TOTAL FOR ALL COUNTRIES`-`BAHRAIN`-CANADA-CHINA-`EUROPEAN UNION`-`KOREA, SOUTH`-`MEXICO`-RUSSIA-`UNITED ARAB EMIRATES`) %>%
  mutate(date = as.Date(paste0(time, "-01-01"))) %>%
  mutate(date = floor_date(date, "year"))


SELECT_US_ALUMINUM_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= CANADA/1000000000,color= "Canada"), size = 1.25) + 
  geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= CHINA/1000000000,color= "China"), size = 1.25) + 
  #geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= MEXICO/1000000000,color= "Mexico"), size = 1.25) + 
  #geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= `EUROPEAN UNION`/1000000000,color= "EU"), size = 1.25) + 
  #geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= `KOREA, SOUTH`/1000000000,color= "South Korea"), size = 1.25) + 
  geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= `RUSSIA`/1000000000,color= "Russia"), size = 1.25) + 
  #geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= `UNITED ARAB EMIRATES`/1000000000,color= "UAE"), size = 1.25) + 
  #geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= `BAHRAIN`/1000000000,color= "Bahrain"), size = 1.25) + 
  #geom_line(data=ALUMINUM_IMPORTS, aes(x=date,y= `OTHER`/1000000000,color= "Other"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "MMt", accuracy = 1),limits = c(0,3.2), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Millions of Metric Tons") +
  ggtitle("Select US Aluminum Imports") +
  labs(caption = "Graph created by @JosephPolitano using US Census data. Aluminum Imports HS Codes Under ITA Aluminum Import Monitor",subtitle = "Canada is the Largest Source of US Aluminum & has Nearshored Lost Russian/Chinese Imports") +
  theme_apricitas + theme(legend.position = c(.15,.60), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*(3.2)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SELECT_US_ALUMINUM_Graph, "Select US Aluminum Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

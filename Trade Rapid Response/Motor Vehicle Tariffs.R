US_SHARE_TOP_VEHICLES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/aac52b8ccad72b6c588e7e282fc77d2c89340014/Trade%20Rapid%20Response/CAR_US_SHARE.csv") %>%
  mutate(name = factor(name, levels = rev(c("Toyota Camry","Toyota RAV4","Tesla Model Y","Honda Civic","Ram 1500","Honda CR-V","GMC Sierra","Chevy Silverado","Ford F-150","Nissan Rogue"))))

US_SHARE_TOP_VEHICLES <- ggplot(data = US_SHARE_TOP_VEHICLES, aes(x = name, y = percent, fill = "Domestic (US/Canada) Content Share")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("US/Canada Content, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("No Such Thing as an All-American Car") +
  labs(subtitle = "No Car—Even US-Assembled US-Nameplate Models—is Made With 100% US Parts", caption = "Graph created by @Josephpolitano using AALA Data") +
  scale_fill_manual(name= "Top Selling 2024 Models",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  theme_apricitas + theme(legend.position = c(.7,.3), axis.text.y = element_text(size = 16), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 27)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = US_SHARE_TOP_VEHICLES, "US Share Top Vehicles Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAR_IMPORTS_TIMELINE_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO","I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8703220100", 
  I_COMMODITY = "8703220110", 
  I_COMMODITY = "8703220190", 
  I_COMMODITY = "8703230110",
  I_COMMODITY = "8703230120",
  I_COMMODITY = "8703230130",
  I_COMMODITY = "8703230140",
  I_COMMODITY = "8703230160",
  I_COMMODITY = "8703230170",
  I_COMMODITY = "8703230190",
  I_COMMODITY = "8703240110",
  I_COMMODITY = "8703240130",
  I_COMMODITY = "8703240140",
  I_COMMODITY = "8703240150",
  I_COMMODITY = "8703240160",
  I_COMMODITY = "8703240190",
  I_COMMODITY = "8703310100",
  I_COMMODITY = "8703320110",
  I_COMMODITY = "8703320150", 
  I_COMMODITY = "8703330110", 
  I_COMMODITY = "8703330130", 
  I_COMMODITY = "8703330145", 
  I_COMMODITY = "8703330185",
  I_COMMODITY = "870340",
  I_COMMODITY = "870350",
  I_COMMODITY = "870360",
  I_COMMODITY = "870370",
  I_COMMODITY = "870380",
  I_COMMODITY = "8703900100",
  I_COMMODITY = "8704210100",
  I_COMMODITY = "8704310020", 
  I_COMMODITY = "8704310040", 
  I_COMMODITY = "8704310120", 
  I_COMMODITY = "8704310140", 
  I_COMMODITY = "870441", 
  I_COMMODITY = "870451",
  I_COMMODITY = "870460",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "KOREA, SOUTH",
  CTY_NAME = "JAPAN",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CANADA",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
  CTY_NAME = "UNITED KINGDOM",
)

CAR_IMPORTS_TIMELINE <- CAR_IMPORTS_TIMELINE_BULK %>%
  mutate(time = as.Date(paste0(substr(time, 1, 4), "-01-01"))) %>%
  filter(time < as.Date("2025-01-01")) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA", CTY_NAME)) %>%
  group_by(CTY_NAME,time) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(Other = `TOTAL FOR ALL COUNTRIES` - rowSums(select(., where(is.numeric), -`TOTAL FOR ALL COUNTRIES`))) %>%
  select(-`TOTAL FOR ALL COUNTRIES`) %>%
  pivot_longer(-time) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = gsub("Korea, South", "South Korea", name)) %>%
  mutate(category = "Cars")


CAR_PARTS_IMPORTS_TIMELINE_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME"),
  time = paste("from 2017 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "4009120020",
  I_COMMODITY = "4009220020",
  I_COMMODITY = "4009320020",
  I_COMMODITY = "4009420020",
  I_COMMODITY = "4011101000",
  I_COMMODITY = "4011101010",
  I_COMMODITY = "4011101020",
  I_COMMODITY = "4011101030",
  I_COMMODITY = "4011101040",
  I_COMMODITY = "4011101050",
  I_COMMODITY = "4011101060",
  I_COMMODITY = "4011101070",
  I_COMMODITY = "4011105000",
  I_COMMODITY = "4011201005",
  I_COMMODITY = "4011201015",
  I_COMMODITY = "4011201025",
  I_COMMODITY = "4011201035",
  I_COMMODITY = "4012194000",
  I_COMMODITY = "4012206000",
  I_COMMODITY = "4013100010",
  I_COMMODITY = "4013100020",
  I_COMMODITY = "4016996010",
  I_COMMODITY = "7007215100",
  I_COMMODITY = "7320201000",
  I_COMMODITY = "8302103000",
  I_COMMODITY = "8408202000",
  I_COMMODITY = "8409911040",
  I_COMMODITY = "8409991040",
  I_COMMODITY = "8413301000",
  I_COMMODITY = "8413309030",
  I_COMMODITY = "8413309060",
  I_COMMODITY = "8413309090",
  I_COMMODITY = "8413911000",
  I_COMMODITY = "8413919010",
  I_COMMODITY = "8414308030",
  I_COMMODITY = "8414593000",
  I_COMMODITY = "8414596540",
  I_COMMODITY = "8414800500",
  I_COMMODITY = "8431100090",
  I_COMMODITY = "8482101040",
  I_COMMODITY = "8482101080",
  I_COMMODITY = "8482105044",
  I_COMMODITY = "8482105048",
  I_COMMODITY = "8482200020",
  I_COMMODITY = "8482200030",
  I_COMMODITY = "8482200040",
  I_COMMODITY = "8482200061",
  I_COMMODITY = "8482200070",
  I_COMMODITY = "8482200081",
  I_COMMODITY = "8483101030",
  I_COMMODITY = "8483103010",
  I_COMMODITY = "8483103050",
  I_COMMODITY = "8501332040",
  I_COMMODITY = "8501332080",
  I_COMMODITY = "8501333000",
  I_COMMODITY = "8501334040",
  I_COMMODITY = "8501334060",
  I_COMMODITY = "8501336100",
  I_COMMODITY = "8507904000",
  I_COMMODITY = "8507908000",
  I_COMMODITY = "8511300040",
  I_COMMODITY = "8511300080",
  I_COMMODITY = "8511802000",
  I_COMMODITY = "8511906020",
  I_COMMODITY = "8511906040",
  I_COMMODITY = "8512202040",
  I_COMMODITY = "8512202080",
  I_COMMODITY = "8512204040",
  I_COMMODITY = "8512204080",
  I_COMMODITY = "8512402000",
  I_COMMODITY = "8512404000",
  I_COMMODITY = "8512902000",
  I_COMMODITY = "8512906000",
  I_COMMODITY = "8512907000",
  I_COMMODITY = "8519812000",
  I_COMMODITY = "8525601010",
  I_COMMODITY = "8536410005",
  I_COMMODITY = "8539100010",
  I_COMMODITY = "8539100050",
  I_COMMODITY = "8706000520",
  I_COMMODITY = "8706000540",
  I_COMMODITY = "8706000575",
  I_COMMODITY = "8706001520",
  I_COMMODITY = "8706001540",
  I_COMMODITY = "8706002500",
  I_COMMODITY = "8708103020",
  I_COMMODITY = "8708103030",
  I_COMMODITY = "8708103040",
  I_COMMODITY = "8708103050",
  I_COMMODITY = "8708401110",
  I_COMMODITY = "8708401150",
  I_COMMODITY = "8708407000",
  I_COMMODITY = "8708407570",
  I_COMMODITY = "8708407580",
  I_COMMODITY = "8708936000",
  I_COMMODITY = "8708937500",
  I_COMMODITY = "8708995300",
  I_COMMODITY = "8708995500",
  I_COMMODITY = "8708995800",
  I_COMMODITY = "8708996805",
  I_COMMODITY = "8708996810",
  I_COMMODITY = "8708996820",
  I_COMMODITY = "8708996890",
  I_COMMODITY = "8716905010",
  I_COMMODITY = "8716905035",
  I_COMMODITY = "8716905046",
  I_COMMODITY = "8716905047",
  I_COMMODITY = "8716905048",
  I_COMMODITY = "8716905056",
  I_COMMODITY = "8716905059",
  I_COMMODITY = "8716905060",
  I_COMMODITY = "9029204040",
  I_COMMODITY = "9029204080",
  I_COMMODITY = "700910",
  I_COMMODITY = "732010",
  I_COMMODITY = "830120",
  I_COMMODITY = "830230",
  I_COMMODITY = "840731",
  I_COMMODITY = "840732",
  I_COMMODITY = "840733",
  I_COMMODITY = "840734",
  I_COMMODITY = "841520",
  I_COMMODITY = "842123",
  I_COMMODITY = "842132",
  I_COMMODITY = "842549",
  I_COMMODITY = "842691",
  I_COMMODITY = "848240",
  I_COMMODITY = "848250",
  I_COMMODITY = "850132",
  I_COMMODITY = "850134",
  I_COMMODITY = "850140",
  I_COMMODITY = "850151",
  I_COMMODITY = "850152",
  I_COMMODITY = "850710",
  I_COMMODITY = "8507600010",
  I_COMMODITY = "851110",
  I_COMMODITY = "851120",
  I_COMMODITY = "851140",
  I_COMMODITY = "851150",
  I_COMMODITY = "851230",
  I_COMMODITY = "852721",
  I_COMMODITY = "852729",
  #I_COMMODITY = "853710", excluding control panels
  I_COMMODITY = "853720",
  I_COMMODITY = "854430",
  I_COMMODITY = "8707",
  I_COMMODITY = "870821",
  I_COMMODITY = "870822",
  I_COMMODITY = "870829",
  I_COMMODITY = "870830",
  I_COMMODITY = "870850",
  I_COMMODITY = "870870",
  I_COMMODITY = "870880",
  I_COMMODITY = "870891",
  I_COMMODITY = "870894",
  I_COMMODITY = "870895",
  I_COMMODITY = "901510",
  I_COMMODITY = "902910",
  I_COMMODITY = "940120",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  CTY_NAME = "KOREA, SOUTH",
  CTY_NAME = "JAPAN",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CANADA",
  CTY_NAME = "EUROPEAN UNION",
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
  CTY_NAME = "UNITED KINGDOM",
)


CAR_PARTS_IMPORTS_TIMELINE <- CAR_PARTS_IMPORTS_TIMELINE_BULK %>%
  mutate(time = as.Date(paste0(substr(time, 1, 4), "-01-01"))) %>%
  filter(time < as.Date("2025-01-01")) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  mutate(CTY_NAME = if_else(CTY_NAME %in% c("HONG KONG", "MACAU"), "CHINA", CTY_NAME)) %>%
  group_by(CTY_NAME,time) %>%
  summarize(time,CON_VAL_MO = sum(CON_VAL_MO, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(Other = `TOTAL FOR ALL COUNTRIES` - rowSums(select(., where(is.numeric), -`TOTAL FOR ALL COUNTRIES`))) %>%
  select(-`TOTAL FOR ALL COUNTRIES`) %>%
  pivot_longer(-time) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = gsub("Korea, South", "South Korea", name)) %>%
  mutate(category = "Car Parts")

CARS_AND_PARTS_TIMELINE <- rbind(CAR_IMPORTS_TIMELINE,CAR_PARTS_IMPORTS_TIMELINE) %>%
  mutate(name = factor(name, levels = rev(c("Mexico", "Canada", "European Union", "Japan", "South Korea","United Kingdom","China","Other")))) %>%
  mutate(category = factor(category, levels = c("Cars","Car Parts")))

CAR_AND_PARTS_TARGETED_FOR_TARIFFS_GRAPH <- ggplot(data = filter(CARS_AND_PARTS_TIMELINE, time >= as.Date("2013-01-01")), aes(x = time, y = value/1000000000, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  facet_wrap(~ category, scales = "fixed") +
  xlab("Date") +
  ylab("Billions of Dollars, Annual") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "B", prefix = "$"), breaks = c(0,100,200,300), limits = c(0,300), expand = c(0,0)) +
  ggtitle("US Vehicle Imports Targeted for Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "The US Imports Hundreds of Billions in Cars Mostly From Mexico, Canada, the EU, & Japan") +
  theme_apricitas + theme(legend.position = "right") + theme(strip.text = element_text(size = 15, color = "white", face = "bold")) + #theme(legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAR_AND_PARTS_TARGETED_FOR_TARIFFS_GRAPH, "US Car & Parts Targeted For Tariffs Bar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

TOTAL_MOTOR_VEHICLE_SALES <- fredr("ALTSALES", observation_start = as.Date("2017-01-01"))

MOTOR_VEHICLE_SALES_LONG <- ggplot() +
  geom_line(data = TOTAL_MOTOR_VEHICLE_SALES, aes(x=date, y = value, color = "US Total Car Sales"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), limits = c(8,19), breaks = c(8,10,12,14,16,18), expand = c(0,0)) +
  ylab("Total Auto & Light Truck Sales, Seasonally Adjusted Annnual Rate") +
  ggtitle("The Rush to Buy Cars Before Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Vehicle Sales Have Jumped as Consumers Try to Get Cars Before Trump's Tariffs Begin") +
  theme_apricitas + theme(legend.position = c(.32,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 8-(.3*11), ymax = 8) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MOTOR_VEHICLE_SALES_LONG, "Motor Vehicle Sales Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MOTOR_VEHICLE_SALES_SHORT <- ggplot() +
  geom_line(data = filter(TOTAL_MOTOR_VEHICLE_SALES, date >= as.Date("2023-01-01")), aes(x=date, y = value, color = "US Total Car Sales"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"), limits = c(14,18), breaks = c(14,14.5,15,15.5,16,16.5,17,17.5,18), expand = c(0,0)) +
  ylab("Auto & Light Truck Sales, Seasonally Adjusted Annnual Rate") +
  ggtitle("The Rush to Buy Cars Before Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US Vehicle Sales Have Jumped as Consumers Try to Get Cars Before Trump's Tariffs Begin") +
  theme_apricitas + theme(legend.position = c(.32,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = 14-(.3*4), ymax = 14) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MOTOR_VEHICLE_SALES_SHORT, "Motor Vehicle Sales Short Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

US_COUNTRIES_HS10_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL"), 
  time = "2024-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

US_COUNTRIES_HS10_IMPORTS_BULK_USMCA <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL","RP"), 
  time = "2024-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "CANADA",
  CTY_NAME = "MEXICO",
  RP = "18",
  #CTY_NAME = Countries[5],
)


US_COUNTRIES_HS6_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL"), 
  time = "2023-12",
  COMM_LVL = "HS6",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)

CAR_PARTS_IMPORTS_BREAKDOWN <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  #filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080"), 0, tariff)) %>% #10 digit HS codes for excluded car parts
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
  ), 0, tariff)) %>% #car parts
  
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853720", "870822", #"853710", excluding control panels
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910"), 0, tariff)) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010"), 0, tariff)) %>% #only motor vehicle batteries
  #mutate(tariff = if_else(I_COMMODITY_4 %in% c("8404"), 0, tariff)) %>%
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707"), 0, tariff)) %>%
  filter(tariff <.25) %>%
  group_by(I_COMMODITY_6) %>%
  summarize(I_COMMODITY_6,CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  mutate(I_COMMODITY_6 = as.numeric(I_COMMODITY_6)) %>%
  left_join(.,(US_COUNTRIES_HS6_IMPORTS_BULK %>% select(I_COMMODITY, I_COMMODITY_LDESC)), by = c("I_COMMODITY_6" = "I_COMMODITY"))





CAR_PARTS_IMPORTS_BREAKDOWN_USMCA <- US_COUNTRIES_HS10_IMPORTS_BULK_USMCA %>%
  #filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  #filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080"), 0, tariff)) %>% #10 digit HS codes for excluded car parts
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
  ), 0, tariff)) %>% #car parts
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853720", "870822", # "853710" excluding control panels
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910"), 0, tariff)) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010"), 0, tariff)) %>% #only motor vehicle batteries
  #mutate(tariff = if_else(I_COMMODITY_4 %in% c("8404"), 0, tariff)) %>%
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707"), 0, tariff)) %>%
  filter(tariff <.25) %>%
  group_by(I_COMMODITY_6) %>%
  summarize(I_COMMODITY_6,CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  mutate(I_COMMODITY_6 = as.numeric(I_COMMODITY_6)) %>%
  left_join(.,(US_COUNTRIES_HS6_IMPORTS_BULK %>% select(I_COMMODITY, I_COMMODITY_LDESC)), by = c("I_COMMODITY_6" = "I_COMMODITY"))


NON_USMCA_PARTS_IMPORTS_BREAKDOWN <- left_join(CAR_PARTS_IMPORTS_BREAKDOWN,CAR_PARTS_IMPORTS_BREAKDOWN_USMCA, by = "I_COMMODITY_6") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  transmute(I_COMMODITY_6,CON_VAL_YR = CON_VAL_YR.x-CON_VAL_YR.y,I_COMMODITY_LDESC = I_COMMODITY_LDESC.x) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(1:15) %>%
  mutate(name = c("Misc. Body Parts","Rubber Car Tires","Gear Boxes","Ignition Wiring Sets","Rubber Truck Tires","Brakes & Parts","Drive Axles w/Differential","Light & Signal Equipment","Shock Absorbers","Lithium-Ion Batteries","Misc. Parts","Steering Wheels","Engines >1000cc","Catalytic Converters","Liquid Pump Parts")) %>%
  mutate(name = factor(name, levels = rev(c("Misc. Body Parts","Rubber Car Tires","Gear Boxes","Ignition Wiring Sets","Rubber Truck Tires","Brakes & Parts","Drive Axles w/Differential","Light & Signal Equipment","Shock Absorbers","Lithium-Ion Batteries","Misc. Parts","Steering Wheels","Engines >1000cc","Catalytic Converters","Liquid Pump Parts"))))
  

NON_USMCA_IMPORTS_BREAKDOWN_GRAPH <- ggplot(data = NON_USMCA_PARTS_IMPORTS_BREAKDOWN, aes(x = name, y = CON_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ggtitle("Key Non-USMCA Car Parts Hit By Trump's Tariffs") +
  ylab("US Imports By Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,10), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census Data. NOTE: Grouped by 6-Digit HS Code", subtitle = "Trump Has Hit More than $117B in Non-USMCA Car Parts With Tariffs,Including These Major Categories") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white"), plot.title = element_text(size = 26)) +
  coord_flip() +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = NON_USMCA_IMPORTS_BREAKDOWN_GRAPH, "Non-USMCA Imports Breakdown Graph Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAR_IMPORTS_BREAKDOWN <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","USMCA (NAFTA)","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  filter(!CTY_NAME %in% c("AUSTRIA","BELGIUM","BULGARIA","CROATIA","CYPRUS","CZECH REPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HUNGARY","IRELAND","ITALY","LATVIA","LITHUANIA","LUXEMBOURG","MALTA","NETHERLANDS","POLAND","PORTUGAL","ROMANIA","SLOVAKIA","SLOVENIA","SPAIN","SWEDEN")) %>%
  #filter(CTY_NAME == "TOTAL FOR ALL COUNTRIES") %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                             "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                             "87042101", "87043101", "87044100", "87045100", "87046000"), 0, tariff)) %>% #car exclusion
  filter(tariff < .25) %>%
  mutate(I_COMMODITY = case_when(
    I_COMMODITY_6 == "870322" ~ "Gas Cars 1000cc-1500cc",
    I_COMMODITY_6 == "870323" ~ "Gas Cars 1500cc-3000cc",
    I_COMMODITY_6 == "870324" ~ "Gas Cars >3000cc",
    I_COMMODITY_6 == "870333" ~ "Diesel Cars >2500cc",
    I_COMMODITY_6 == "870431" ~ "Gas Pickups",
    I_COMMODITY_6 == "870451" ~ "Hybrid Pickups",
    I_COMMODITY_6 == "870421" ~ "Diesel Pickups",
    I_COMMODITY_6 == "870460" ~ "Electric Pickups",
    I_COMMODITY_6 == "870340" ~ "Hybrid Cars",
    I_COMMODITY_6 == "870380" ~ "Electric Cars",
    I_COMMODITY_6 == "870360" ~ "Plug-in Hybrid Cars",
  )) %>%
  mutate(CTY_NAME = case_when(
    CTY_NAME == "KOREA, SOUTH" ~ "South Korea",
    CTY_NAME == "EUROPEAN UNION" ~ "European Union",
    CTY_NAME == "CHINA" ~ "China",
    CTY_NAME == "HONG KONG" ~ "China",
    CTY_NAME == "MACAU" ~ "China",
    CTY_NAME == "MEXICO" ~ "Mexico",
    CTY_NAME == "JAPAN" ~ "Japan",
    CTY_NAME == "CANADA" ~ "Canada",
    CTY_NAME == "UNITED KINGDOM" ~ "United Kingdom",
    TRUE ~ "Other"
  )) %>%
  drop_na() %>%
  group_by(CTY_NAME,I_COMMODITY) %>%
  summarize(CTY_NAME,I_COMMODITY,CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  mutate(I_COMMODITY = factor(I_COMMODITY, levels = rev(c("Gas Cars 1500cc-3000cc","Gas Cars >3000cc","Hybrid Cars","Gas Cars 1000cc-1500cc","Gas Cars <1000cc","Gas Pickups","Electric Cars","Plug-in Hybrid Cars","Diesel Pickups","Hybrid Pickups","Diesel Cars >2500cc","Electric Pickups")))) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(c("Mexico", "Canada", "European Union", "Japan", "South Korea","United Kingdom","China","Other"))))
  


CAR_IMPORTS_BREAKDOWN_Graph <- ggplot(data = CAR_IMPORTS_BREAKDOWN, aes(x = I_COMMODITY, y = CON_VAL_YR/1000000000, fill = CTY_NAME)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ggtitle("Car Imports Hit By Trump's New Tariffs") +
  ylab("Affected US Imports by Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,100), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  scale_fill_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")),breaks = c("Mexico", "Canada", "European Union", "Japan", "South Korea","United Kingdom","China","Other")) +
  #scale_fill_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F"))) +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "Trump Hit All Non-USMCA Cars the CA/MX Content in USMCA Imports With 25% Tariffs") +
  theme_apricitas + theme(legend.position = c(.7,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 15, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip() +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = CAR_IMPORTS_BREAKDOWN_Graph, "Car Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")





CAR_IMPORTS_BREAKDOWN_USMCA <- US_COUNTRIES_HS10_IMPORTS_BULK_USMCA %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000"), 0, tariff)) %>% #car exclusion
  filter(tariff < .25)


US_CHINA_HS10_IMPORTS_TARIFFS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL","CAL_DUT_YR"), 
  time = "2024-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "CHINA",
  CTY_NAME = "HONG KONG",
  CTY_NAME = "MACAU",
)


US_CHINA_CAR_PARTS_TARIFFS <- US_CHINA_HS10_IMPORTS_TARIFFS_BULK %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  mutate(CAL_DUT_YR = as.numeric(CAL_DUT_YR)) %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c("4009120020", "4009220020", "4009320020", "4009420020", "4013100010", 
                                             "4013100020", "4016996010", "8409911040", "8409991040", "8413919010", 
                                             "8414308030", "8414596540", "8431100090", "8482105044", "8482105048", 
                                             "8482200020", "8482200030", "8482200040", "8482200061", "8482200070", 
                                             "8482200081", "8483101030", "8511100000", "8511300040", "8511300080", 
                                             "8511906020", "8511906040", "8525601010", "8536410005", "8539100010", 
                                             "8539100050", "8707100020", "8707100040", "8707905020", "8707905040", 
                                             "8707905060", "8707905080", "9029204080"), 0, tariff)) %>% #10 digit HS codes for excluded car parts
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
  ), 0, tariff)) %>% #car parts
  mutate(tariff = if_else(I_COMMODITY_6 %in% c("732010", "830230", "840732", "840733", "840734", "850132", "850133", "850134", "850140",
                                               "850151", "850152", "852721", "852729", "853720", "870822", # "853710" excluding control panels
                                               "870829", "870830", "870850", "870870", "870880", "870891", "870894", "870895", "901510",
                                               "902910"), 0, tariff)) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c("8507600010","8507600010"), 0, tariff)) %>% #only motor vehicle batteries
  #mutate(tariff = if_else(I_COMMODITY_4 %in% c("8404"), 0, tariff)) %>%
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8707"), 0, tariff)) %>%
  filter(tariff <.25) %>%
  group_by(I_COMMODITY_6) %>%
  summarize(I_COMMODITY_6,CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE),CAL_DUT_YR = sum(CAL_DUT_YR, na.rm = TRUE)) %>%
  ungroup() %>%
  unique() %>%
  mutate(I_COMMODITY_6 = as.numeric(I_COMMODITY_6)) %>%
  left_join(.,(US_COUNTRIES_HS6_IMPORTS_BULK %>% select(I_COMMODITY, I_COMMODITY_LDESC)), by = c("I_COMMODITY_6" = "I_COMMODITY")) %>%
  mutate(TARIFF_RATE = CAL_DUT_YR/CON_VAL_YR)


US_CHINA_PARTS_BREAKDOWN <- US_CHINA_CAR_PARTS_TARIFFS %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  arrange(desc(CON_VAL_YR)) %>%
  slice(1:15) %>%
  mutate(name = c("Lithium-Ion Batteries","Brakes & Parts","Misc. Body Parts","Car Wheels","Battery Parts","Shock Absorbers","Liquid Pump Parts","Misc. Moutings/Fittings","Drive Axles w/Differential","Fans","Steering Wheels","Trailer Parts","Light & Signal Equipment","Windshields","Misc. Vehicle Parts")) %>%
  mutate(name = factor(name, levels = rev(c("Lithium-Ion Batteries","Brakes & Parts","Misc. Body Parts","Car Wheels","Battery Parts","Shock Absorbers","Liquid Pump Parts","Misc. Moutings/Fittings","Drive Axles w/Differential","Fans","Steering Wheels","Trailer Parts","Light & Signal Equipment","Windshields","Misc. Vehicle Parts"))))


CHINA_CAR_PARTS_IMPORTS_BREAKDOWN_GRAPH <- ggplot(data = US_CHINA_PARTS_BREAKDOWN, aes(x = name, y = CON_VAL_YR/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ggtitle("Key Chinese Car Parts Now Facing 45+% Tariffs") +
  ylab("US Imports By Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "B"), limits = c(0,2.5), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census Data. NOTE: Grouped by 6-Digit HS Code", subtitle = "Trump Has Hit More than $17B in Chinese Car Parts With Tariffs, Meaning they Now Face 45-55% Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white"), plot.title = element_text(size = 26)) +
  coord_flip() +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = CHINA_CAR_PARTS_IMPORTS_BREAKDOWN_GRAPH, "China Car Parts Imports Breakdown Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


RETAIL_IS_MOTOR <- fredr(series_id = "MRTSIR441USS", observation_start = as.Date("2018-01-01"))

VEHICLE_RETAIL_IS_graph <- ggplot() + #plotting components of annual inflation
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 0.5) +
  geom_line(data = RETAIL_IS_MOTOR, aes(x = date, y = value/value[1]*100, color = "Retail Inventory/Sales Ratio\nMotor Vehicles and Parts"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,110), breaks = c(0,25,50,75,100,125,150), expand = c(0,0)) +
  ylab("Index, Jan 2018=100") +
  ggtitle("Car Dealer Inventories Remain Low") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Even Years After the Pandemic, Vehicle & Parts Inventories Remain Below Normal Levels") +
  theme_apricitas + theme(legend.position = c(.25,.5)) +
  scale_color_manual(name= "Index, Jan 2018 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*110), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VEHICLE_RETAIL_IS_graph, "Vehicle Retail IS Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

install_github("keberwein/blscrapeR")
library(blscrapeR)


VEHICLE_ASSEMBLY_YOY <- bls_api("CEU3133610001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-latest) %>%
  rbind(bls_api("CEU3133610001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Vehicle Final Assembly") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

BODY_TRAILER_YOY <- bls_api("CEU3133620001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #software employment
  select(-latest) %>%
  rbind(bls_api("CEU3133620001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Bodies & Trailers") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

ENGINE_YOY <- bls_api("CEU3133631001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133631001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Engines & Related Parts") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

ELECTRICAL_ELECTRONIC_YOY <- bls_api("CEU3133632001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133632001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Electrical & Electronic Components") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TRANSMISSION_POWER_YOY <- bls_api("CEU3133635001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133635001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Transmission & Power Train") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

METAL_STAMPING_YOY <- bls_api("CEU3133637001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133637001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Metal Stamping") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

STEERING_SUSPENSION_YOY <- bls_api("CEU3133639001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #internet employment
  select(-latest) %>%
  rbind(bls_api("CEU3133639001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Steering & Suspension Components") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

VEHICLE_EMPLOY_GROWTH_YOY <- rbind(VEHICLE_ASSEMBLY_YOY,BODY_TRAILER_YOY,ENGINE_YOY,ELECTRICAL_ELECTRONIC_YOY,TRANSMISSION_POWER_YOY,METAL_STAMPING_YOY,STEERING_SUSPENSION_YOY) %>%
  group_by(date) %>%
  filter(n() > 5) %>%
  mutate(series_id = factor(series_id,levels = rev(c("Vehicle Final Assembly","Bodies & Trailers","Engines & Related Parts","Electrical & Electronic Components","Transmission & Power Train","Metal Stamping","Steering & Suspension Components"))))

VEHICLE_EMPLOY_GROWTH_YOYSUM <- VEHICLE_EMPLOY_GROWTH_YOY %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE)) %>%
  mutate(series_id = "Sum YoY Growth")

VEHICLE_EMPLOY_GROWTH_YOY_graph <- ggplot(data = filter(VEHICLE_EMPLOY_GROWTH_YOY, date >= as.Date("2022-01-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(VEHICLE_EMPLOY_GROWTH_YOYSUM, date>= as.Date("2022-01-01")), aes(x=date, y = sum_value, color = "Total Motor Vehicle Manufacturing Job Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-100,-50,0,50,100), limits = c(-50,110), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in Autoworker Jobs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment Has Fallen in the Motor Vehicle Industry Over the Last Year") +
  theme_apricitas + theme(legend.position = c(0.75,0.780), legend.margin=margin(0,0,-6,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#DFB2F4","#EE6055"), breaks = c("Vehicle Final Assembly","Bodies & Trailers","Engines & Related Parts","Electrical & Electronic Components","Transmission & Power Train","Metal Stamping","Steering & Suspension Components")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -50-(.3*160), ymax = -50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VEHICLE_EMPLOY_GROWTH_YOY_graph, "Vehicle Employ Growth YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing





sahie_variables <- listCensusMetadata(
  name = "timeseries/intltrade/exports/statehsexport",
  type = "variables")

US_COUNTRIES_IMPORTS_BULK_STATE_CARS <- getCensus(
  key = Sys.getenv("CENSUS_KEY"),
  name = "timeseries/intltrade/imports/statehs",
  vars = c("CON_VAL_YR","CTY_NAME","CTY_CODE","STATE","I_COMMODITY"),
  time = "2024-12",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  I_COMMODITY = "870322",
  I_COMMODITY = "870323",
  I_COMMODITY = "870324",
  I_COMMODITY = "870331",
  I_COMMODITY = "870332",
  I_COMMODITY = "870333",
  I_COMMODITY = "870340",
  I_COMMODITY = "870350",
  I_COMMODITY = "870360",
  I_COMMODITY = "870370",
  I_COMMODITY = "870380",
  I_COMMODITY = "870390",
  I_COMMODITY = "870421",
  I_COMMODITY = "870431",
  I_COMMODITY = "870441",
  I_COMMODITY = "870451",
  I_COMMODITY = "870460",
  CTY_NAME = "TOTAL FOR ALL COUNTRIES"
)



US_COUNTRIES_IMPORTS_STATE_CARS <- US_COUNTRIES_IMPORTS_BULK_STATE_CARS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(STATE) %>%
  summarise(TOTAL_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  transmute(state = STATE, TOTAL_VAL_YR)

BEA_GDP_STATE_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "SAGDP1", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = 3, # Specify the line code
  "GeoFips" = "STATE", # Specify the geographical level
  "Year" = "2024") # Specify the year

BEA_GDP_STATE <- beaGet(BEA_GDP_STATE_SPECS, iTableStyle = TRUE) %>%
  mutate(state = case_when(
    GeoName == "District of Columbia" ~ "DC",
    TRUE ~ state.abb[match(GeoName, state.name)]
  )
  ) %>%
  drop_na() %>%
  transmute(state,GDP = DataValue_2024)

TRADE_EXPOSURE_GDP_STATE <- merge(US_COUNTRIES_IMPORTS_STATE_CARS,BEA_GDP_STATE, by = "state") %>%
  mutate(imports_GDP_share = (TOTAL_VAL_YR/1000000)/GDP)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(states = state_name)

states <- left_join(states, TRADE_EXPOSURE_GDP_STATE, by = c("state_abbv"="state"))

TRADE_EXPOSURE_GDP_STATE_LABELS <- get_urbn_labels(map = "states") %>%
  left_join(states, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_centroids <- states %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states, .) %>%
  st_centroid()


TRADE_EXPOSURE_GDP_RAINBOW_CHART_CARS <- states %>%
  ggplot(aes(fill = imports_GDP_share)) +
  geom_sf(color = NA) +
  geom_sf(data = states, color = "grey25", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradientn(colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(0,.02,.04,.06,.08,.1), expand = c(0,0)) +
  ggtitle("           Imports of Tariffed Motor Vehicles\n                 As a Share of State GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BEA & Census data") +
  labs(fill = NULL) +
  geom_label_repel(
    data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("NH","VT","MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0 & imports_GDP_share <= .1, "  ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    segment.color = NA,
    hjust = 0.5,
    direction = "y",
    nudge_y = 4000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    box.padding = 0.75,  # Increase box padding
    point.padding = 0.5,
    max.overlaps = 5,
    force = 4,
    force_pull = 1,
    max.iter = 2000000000,
    max.time = 30,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_centroids, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, !state_abbv %in% c("HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","FL","LA","KY","WV","TN","IN","ME","SC","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("FL","TN","IN","ME","SC","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(TRADE_EXPOSURE_GDP_STATE_LABELS, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(imports_GDP_share >= 0, " ", ""), sprintf("%.1f", round(imports_GDP_share * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  #geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(trade_GDP_share*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.15, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = TRADE_EXPOSURE_GDP_STATE_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(trade_GDP_share,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(trade_GDP_share*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = TRADE_EXPOSURE_GDP_RAINBOW_CHART_CARS, "TRADE EXPOSURE GDP CARS STATE RAINBOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

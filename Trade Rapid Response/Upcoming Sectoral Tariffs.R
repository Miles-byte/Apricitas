pacman::p_load(censusapi,ggpubr,prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), plot.title = element_text(size = 28, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


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

US_COUNTRIES_IMPORTS <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  select(-COMM_LVL,-COMM_LVL_1,-CTY_CODE,-time) %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4))

#LUMBER LIST
US_LUMBER_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_8 %in% c(44011100, 44011200, 44012100, 44012200, 44013100, 44013200, 44013942, 44014100, 44014900,
                              44021000, 44022000, 44029001, 44031100, 44031200, 44032101, 44032201, 44032301, 44032401,
                              44032501, 44032601, 44034200, 44034902, 44039100, 44039301, 44039401, 44039501, 44039601,
                              44039700, 44039800, 44039901, 44041000, 44042000, 44050000, 44061100, 44061200, 44069100,
                              44069200, 44071100, 44071200, 44071300, 44071400, 44071900, 44072100, 44072200, 44072301,
                              44072500, 44072600, 44072700, 44072800, 44072902, 44079100, 44079200, 44079300, 44079400,
                              44079500, 44079600, 44079700, 44079902, 44081001, 44083101, 44083902, 44089001, 44091005,
                              44091010, 44091020, 44091040, 44091045, 44091050, 44091060, 44091065, 44091090, 44092105,
                              44092190, 44092205, 44092210, 44092225, 44092240, 44092250, 44092260, 44092265, 44092290,
                              44092906, 44092911, 44092926, 44092941, 44092951, 44092961, 44092966, 44092991, 44101100,
                              44101200, 44101900, 44109000, 44111210, 44111220, 44111230, 44111260, 44111290, 44111310,
                              44111320, 44111330, 44111360, 44111390, 44111410, 44111420, 44111430, 44111460, 44111490,
                              44119210, 44119220, 44119230, 44119240, 44119310, 44119320, 44119330, 44119360, 44119390,
                              44119400, 44121005, 44121090, 44123106, 44123126, 44123142, 44123145, 44123148, 44123152,
                              44123161, 44123192, 44123306, 44123326, 44123332, 44123357, 44123426, 44123432, 44123457,
                              44123910, 44123930, 44123940, 44123950, 44124100, 44124200, 44124900, 44125110, 44125131,
                              44125141, 44125151, 44125210, 44125231, 44125241, 44125251, 44125980, 44125990, 44125995,
                              44129106, 44129110, 44129131, 44129141, 44129151, 44129207, 44129211, 44129231, 44129242,
                              44129252, 44129958, 44129961, 44129971, 44129981, 44129991, 44129997, 44130000, 48202000,
                              49011000, 49019100, 49019900, 49021000, 49029010, 49029020, 49030000, 49040000, 49052000,
                              49059020, 49059060, 49060000, 49111000, 49119960, 49119980, 40011000, 40012100, 40012200,
                              40012900, 40013000))

US_COPPER_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_8 %in% c(
    74010000, 74020000, 74031100, 74031200, 74031300, 74031900, 74032100, 74032200, 74032901,
    74040030, 74040060, 74050010, 74050060, 74061000, 74062000, 74071015, 74071030, 74071050,
    74072115, 74072130, 74072150, 74072170, 74072190, 74072916, 74072934, 74072938, 74072940,
    74072950, 74081130, 74081160, 74081900, 74082100, 74082210, 74082250, 74082910, 74082950,
    74091110, 74091150, 74091910, 74091950, 74091990, 74092100, 74092900, 74093110, 74093150,
    74093190, 74093910, 74093950, 74093990, 74094000, 74099010, 74099050, 74099090, 74101100,
    74101200, 74102130, 74102160, 74102200, 74111010, 74111050, 74112110, 74112150, 74112200,
    74112910, 74112950, 74121000, 74122000, 74130010, 74130050, 74130090, 74151000, 74152100,
    74152900, 74153305, 74153310, 74153380, 74153900, 74181000, 74182010, 74182050, 74192000,
    74198003, 74198006, 74198009, 74198015, 74198016, 74198017, 74198030, 74198050
  ))

US_PHARMA_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_8 %in% c(
    29034510, 29035990, 29036990, 29037800, 29037990, 29038915, 29038920, 29038970, 29039200,
    29049940, 29052990, 29053990, 29055910, 29055990, 29061950, 29062960, 29072990, 29081960,
    29091918, 29092000, 29093060, 29094910, 29094915, 29094920, 29094960, 29095040, 29095045,
    29095050, 29121950, 29124926, 29141900, 29144090, 29145030, 29145050, 29146200, 29146921,
    29146990, 29147940, 29152930, 29153931, 29153935, 29153947, 29153990, 29159010, 29159014,
    29159018, 29159020, 29159050, 29161930, 29161950, 29162050, 29163150, 29163946, 29163979,
    29171300, 29171910, 29171970, 29173401, 29173930, 29181151, 29181350, 29181650, 29181960,
    29181990, 29182210, 29182250, 29182330, 29182350, 29182920, 29182965, 29182975, 29183025,
    29183030, 29183090, 29189930, 29189943, 29189947, 29189950, 29199030, 29199050, 29209051,
    29211911, 29211961, 29212900, 29213010, 29213050, 29214290, 29214600, 29214938, 29214943,
    29214945, 29214950, 29215980, 29221100, 29221400, 29221909, 29221920, 29221933, 29221960,
    29221970, 29221990, 29221996, 29222927, 29222961, 29222981, 29223100, 29223925, 29223945,
    29223950, 29224100, 29224250, 29224400, 29224910, 29224926, 29224930, 29224937, 29224949,
    29224980, 29225007, 29225010, 29225011, 29225013, 29225014, 29225017, 29225025, 29225035,
    29225040, 29225050, 29231000, 29232020, 29239001, 29241100, 29241911, 29241980, 29242116,
    29242150, 29242910, 29242962, 29242971, 29242977, 29242995, 29251200, 29251942, 29251991,
    29252100, 29252920, 29252960, 29252990, 29263010, 29264000, 29269014, 29269043, 29269048,
    29270040, 29270050, 29280025, 29280030, 29280050, 29299020, 29299050, 29302020, 29302090,
    29303060, 29309029, 29309049, 29309092, 29314900, 29315300, 29319022, 29319090, 29321400,
    29321951, 29322020, 29322030, 29322050, 29329961, 29329970, 29329990, 29331100, 29331935,
    29331945, 29331990, 29332100, 29332920, 29332935, 29332943, 29332945, 29332990, 29333301,
    29333400, 29333500, 29333700, 29333908, 29333910, 29333920, 29333921, 29333923, 29333925,
    29333927, 29333931, 29333941, 29333961, 29333992, 29334100, 29334908, 29334910, 29334915,
    29334917, 29334920, 29334926, 29334930, 29334960, 29334970, 29335210, 29335290, 29335300,
    29335400, 29335910, 29335915, 29335918, 29335921, 29335922, 29335936, 29335946, 29335953,
    29335959, 29335970, 29335980, 29335985, 29335995, 29336960, 29337200, 29337908, 29337915,
    29337985, 29339100, 29339901, 29339902, 29339905, 29339906, 29339908, 29339911, 29339912,
    29339916, 29339917, 29339922, 29339924, 29339926, 29339942, 29339946, 29339951, 29339953,
    29339955, 29339958, 29339961, 29339965, 29339970, 29339975, 29339979, 29339982, 29339985,
    29339989, 29339990, 29339997, 29341010, 29341020, 29341090, 29342040, 29342080, 29343023,
    29343027, 29343043, 29343050, 29349100, 29349200, 29349901, 29349903, 29349905, 29349906,
    29349907, 29349908, 29349909, 29349911, 29349912, 29349915, 29349916, 29349918, 29349920,
    29349930, 29349939, 29349944, 29349947, 29349970, 29349990, 29355000, 29359006, 29359010,
    29359013, 29359015, 29359020, 29359030, 29359032, 29359033, 29359042, 29359048, 29359060,
    29359075, 29359095, 29362100, 29362200, 29362300, 29362401, 29362500, 29362600, 29362700,
    29362800, 29362910, 29362916, 29362920, 29362950, 29369001, 29371100, 29371200, 29371900,
    29372100, 29372200, 29372310, 29372325, 29372350, 29372910, 29372990, 29375000, 29379005,
    29379010, 29379020, 29379040, 29379045, 29379090, 29381000, 29389000, 29391100, 29391910,
    29391920, 29391950, 29392000, 29393000, 29394100, 29394200, 29394400, 29394500, 29394903,
    29395900, 29396200, 29396300, 29396900, 29397200, 29397900, 29398000, 29400060, 29411010,
    29411020, 29411030, 29411050, 29412010, 29412050, 29413000, 29414000, 29415000, 29419010,
    29419030, 29419050, 29420005, 29420035, 29420050, 30012000, 30019001, 30021200, 30021300,
    30021400, 30021500, 30024100, 30024200, 30024900, 30025100, 30025900, 30029010, 30029052,
    30031000, 30032000, 30033910, 30033950, 30034100, 30034200, 30034900, 30039001, 30041010,
    30041050, 30042000, 30043100, 30043200, 30043900, 30044100, 30044200, 30044900, 30045010,
    30045020, 30045030, 30045040, 30045050, 30046000, 30049010, 30049092, 30063010, 30063050,
    30066000, 30067000, 30069310, 30069320, 30069350, 30069360, 30069380, 31042000, 31043000,
    31049001, 31051000, 31052000, 31056000, 32030080, 32041380, 32041720, 32041800, 32061100,
    32061900, 34024210, 34024220, 34024290, 36069030, 38089410, 38089450, 38180000, 38249100,
    38249929, 38249949, 38249955, 38249993, 39019090, 39029000, 39046100, 39059110, 39059980,
    39069050, 39071000, 39072100, 39072900, 39073000, 39076100, 39076900, 39077000, 39079950,
    39081000, 39100000, 39119025, 39119091, 39123100, 39123900, 39129000, 39139020, 39139050,
    39140060
  ))

US_CHIP_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_8 %in% c(
    85411000, 85412100, 85412900, 85413000, 85414910, 85414970, 85414980, 85414995,
    85415100, 85415900, 85419000, 85423100, 85423200, 85423300, 85423900, 85429000,
    85411000, 85412100, 85412900, 85413000, 85414910, 85414970, 85414980, 85414995,
    85415100, 85415900, 85419000
  ) | I_COMMODITY_4 == 8542)

US_ELECTRONICS_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_8 %in% c(85171300,85171300,85235100,85285200) | I_COMMODITY_6  == "847330" | I_COMMODITY_4 %in% c(8471,8524))
  
US_CHIPMAKING_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_4 == 8486)
  
US_CRIT_MINERALS_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  filter(I_COMMODITY_8 %in% c(
    71101100, 71101900, 71102100, 71102900, 71103100, 71103900, 71104100, 71104900, 71129201,
    72021110, 72021150, 72021910, 72021950, 72023000, 72024100, 72024910, 72024950, 72025000,
    72028000, 72029100, 72029340, 72029380, 72042100, 75089050, 79011100, 79011210, 79011250,
    79012000, 79020000, 79070060, 80011000, 80012000, 80020000, 80070050, 81011000, 81019700,
    81032000, 81033000, 81039100, 81039900, 81041100, 81041900, 81042000, 81043000, 81049000,
    81052030, 81052060, 81052090, 81053000, 81059000, 81061000, 81069000, 81082000, 81083000,
    81089030, 81089060, 81101000, 81102000, 81109000, 81110047, 81110049, 81122100, 81122200,
    81122900, 81124110, 81124150, 81124900, 81125900, 81129210, 81129230, 81129240, 81129260,
    81129265, 81129910, 81129991, 28012000, 28042900, 28045000, 28046100, 28048000, 28049000,
    28051910, 28051920, 28051990, 28053000, 28111100, 28111910, 28112910, 28112920, 28121900,
    28139010, 28152000, 28161000, 28164010, 28164020, 28170000, 28181010, 28181020, 28182000,
    28183000, 28201000, 28211000, 28212000, 28220000, 28230000, 28252000, 28255030, 28256000,
    28258000, 28259015, 28259030, 28259090, 28261200, 28263000, 28269090, 28273100, 28273945,
    28273960, 28273990, 28274100, 28274950, 28275951, 28276010, 28276051, 28332100, 28332500,
    28332700, 28332910, 28332945, 28332951, 28342100, 28342920, 28342951, 28366000, 28369100,
    28369200, 28369910, 28369950, 28418000, 28419020, 28419040, 28432901, 28433000, 28439000,
    28441010, 28441020, 28442000, 28443020, 28443050, 28444300, 28459001, 28461000, 28469020,
    28469040, 28469080, 28492010, 28492020, 28499030, 28539010, 28539090, 05080000, 25041050,
    25049000, 25101000, 25102000, 25111010, 25111050, 25191000, 25199010, 25199020, 25249000,
    25292100, 25292200, 25302010, 25302020, 25309010, 25309020, 25309080, 26020000, 26030000,
    26050000, 26060000, 26080000, 26100000, 26110030, 26110060, 26121000, 26140030, 26140060,
    26159030, 26159060, 26161000, 26171000, 26203000, 26209950
  ))





US_TOTAL_IMPORTS <- US_COUNTRIES_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Total")

US_TOTAL_PHARMA_IMPORTS <- US_PHARMA_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Pharmaceuticals")

US_TOTAL_CHIP_IMPORTS <- US_CHIP_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Semiconductors")

US_TOTAL_ELECTRONICS_IMPORTS <- US_ELECTRONICS_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Electronics")

US_TOTAL_COPPER_IMPORTS <- US_COPPER_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Copper & Related")

US_TOTAL_LUMBER_IMPORTS <- US_LUMBER_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Lumber & Related")

US_TOTAL_CHIPMAKING_EQUIPMENT_IMPORTS <- US_CHIPMAKING_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Chipmaking Machinery")

US_TOTAL_CRIT_MINERALS_IMPORTS <- US_CRIT_MINERALS_IMPORTS %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  mutate(Category = "Critical Minerals")

US_TARIFF_CATEGORIES <- rbind(US_TOTAL_IMPORTS,US_TOTAL_PHARMA_IMPORTS,US_TOTAL_CHIP_IMPORTS,US_TOTAL_ELECTRONICS_IMPORTS,US_TOTAL_COPPER_IMPORTS,US_TOTAL_LUMBER_IMPORTS,US_TOTAL_CHIPMAKING_EQUIPMENT_IMPORTS,US_TOTAL_CRIT_MINERALS_IMPORTS) %>%
  filter(Category != "Total") %>%
  arrange(desc(Imports)) %>%
  mutate(Category = factor(Category, levels = rev(c("Electronics","Pharmaceuticals","Semiconductors","Critical Minerals","Lumber & Related","Copper & Related","Chipmaking Machinery"))))



UPCOMING_TARIFF_TARGET_BAR_CHART <- ggplot(data = US_TARIFF_CATEGORIES, aes(x = Category, y = Imports/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ggtitle("Major Imports Targeted For\nUpcoming Trump Tariffs") +
  ylab("US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,310), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "Trump is Preparing to Hit Nearly $670B In Imports With New Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = UPCOMING_TARIFF_TARGET_BAR_CHART, "Upcoming Tariff Target Bar Chart Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



EFFECTIVE_TARIFF_PROJ <- data.frame(
  date = seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day"),
  value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-02-04"), TOTAL_IMPORTS$CAL_DUT_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1], TOTAL_IMPORTS$CAL_DUT_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1] + .1*CHINA_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1]), #10% China
  category = "Implemented"
) %>%
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value + .1*CHINA_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% China Again
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value + .25*MEXICO_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% MEXICO
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value + .1*CANADA_SPLIT$CON_VAL_YR[2]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% CANADIAN ENERGY
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value + .25*(CANADA_SPLIT$CON_VAL_YR[1]-CANADA_SPLIT$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% CANADA
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-07"), value, value - .25*MEXICO_TOTAL_USMCA$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #MEXICO USMCA EXCLUSION
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value - .1*CANADA_SPLIT_USMCA$CON_VAL_YR[2]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% CANADIAN ENERGY EXCLUSION
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-07"), value, value - .25*(CANADA_SPLIT_USMCA$CON_VAL_YR[1]-CANADA_SPLIT_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>%#25% CANADA USMCA EXCLUSION
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-12"), value, value + .25*STEEL_TOTAL$CON_VAL_YR[6]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% MEXICO
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-12"), value, value + .25*ALUMINUM_TOTAL$CON_VAL_YR[6]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% MEXICO
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-12"), value, value + 29000000000/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #USING BCG ESTIMATE FOR COST OF TARIFF ON DERIVATIVES
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value - .6654735594*.25*CAR_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Excluding non-canadian content from vehicles
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value - .491*.25*CAR_TOTAL$CON_VAL_YR[5]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Excluding non-Canadian content from vehicles
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-03-04"), value, value + .25*CAR_TOTAL$CON_VAL_YR[6]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Excluding Non-Mexican Content
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-04-03"), value, value + .10*1305214086314/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% RECIPROCAL
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-04-09"), value, value + .1678111*1305214086314/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #REST OF RECIPR0CAL
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-04-09"), value, value + .50*279176209825/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #50% CHINA RECIPROCAL LIST
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-04-10"), value, value - .1474396*1026093911593/TOTAL_IMPORTS$CON_VAL_YR[1])) %>%#10-40% non-China Paused
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-04-10"), value, value + .41*279176209825/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #41% CHINA RECIPROCAL LIST
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-05-03"), value, value + .25*CAR_PARTS_TOTAL$CON_VAL_YR[3]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Vehicle Parts 
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-05-03"), value, value - .25*(CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[1]+CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Temporary Exclusion for USMCA Parts
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-05-03"), value, value + .25*(.530272*CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[1]+0.368127*CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Adding Canadian/Mexican Content for USMCA Parts
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 35, by = "day") < as.Date("2025-05-15"), value, value + .25*666436945144/TOTAL_IMPORTS$CON_VAL_YR[1])) #Adding Canadian/Mexican Content for USMCA Parts



TARIFF_TIMELINE_LINE_RATE_PROJ_GRAPH <- ggplot() +
  annotate("rect", xmin = as.Date("2025-04-18"), xmax = Sys.Date() + 35, ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Projection", x = as.Date("2025-04-10"), y = .375, color = "#EE6055", size = 5, alpha = 0.6) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  #geom_line(data=IMPLEMENTED_SHARE, aes(x=date,y= value,color= "Share of US Imports Hit By New Tariffs"), size = 1.25) + 
  geom_segment(aes(x = as.Date("2025-02-03"), xend = as.Date("2025-02-03"), y = 0, yend = 0.07), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% China",x = as.Date("2025-02-02"), y = .06, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-03-03"), xend = as.Date("2025-03-03"), y = 0, yend = 0.155), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% China\n25% Mexico\n10-25% Canada",x = as.Date("2025-03-02"), y = .125, size = 4, color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-03-06"), xend = as.Date("2025-03-06"), y = 0, yend = 0.185), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "Exclusion for\nUSMCA Goods",x = as.Date("2025-03-07"), y = .16, size = 4,color = "white",hjust = 0, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-03-11"), xend = as.Date("2025-03-11"), y = 0, yend = 0.135), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "25% Steel\n25% Aluminum",x = as.Date("2025-03-12"), y = 0.06, size = 4,color = "white",hjust = 0, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-04-02"), xend = as.Date("2025-04-02"), y = 0, yend = 0.21), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% Most Countries\n25% Automobiles",x = as.Date("2025-04-01"), y = 0.20, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  
  geom_segment(aes(x = as.Date("2025-04-08"), xend = as.Date("2025-04-08"), y = 0, yend = 0.275), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10-40% Most Countries\n74% China",x = as.Date("2025-04-07"), y = 0.25, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  
  geom_segment(aes(x = as.Date("2025-04-09"), xend = as.Date("2025-04-09"), y = 0, yend = 0.35), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "41% China\n90-Day Pause on\n10-40% Other Countries",x = as.Date("2025-04-08"), y = 0.325, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  
  geom_segment(aes(x = as.Date("2025-05-02"), xend = as.Date("2025-05-02"), y = 0, yend = 0.27), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "25% Auto Parts",x = as.Date("2025-05-01"), y = 0.20, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  
  geom_segment(aes(x = as.Date("2025-05-14"), xend = as.Date("2025-05-14"), y = 0, yend = 0.38), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "Example 25%\nPharma, Chips\nLumber, Copper\nElectronics &\nCritical Minerals",x = as.Date("2025-05-13"), y = 0.34, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  
  geom_line(data=EFFECTIVE_TARIFF_PROJ, aes(x=date,y= value,color= "Tariff Rate on 2024 Import Mix"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.40), breaks = c(0,.05,.10,.15,.20,.25,.30,.35,.4,.45,.5), expand = c(0,0)) +
  ylab("Tariff Rate on 2024 Import Mix") +
  ggtitle("A Timeline of Trump's 2nd-Term Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data.",subtitle = "In His 2nd Term, Trump Has Imposed Tariffs On Trillions of Dollars in US Trade") +
  theme_apricitas + theme(legend.position = c(.2,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-20")-(.1861*(today()+35-as.Date("2025-01-20"))), xmax = as.Date("2025-01-20")-(0.049*(today()+35-as.Date("2025-01-20"))), ymin = 0-(.3*(.40)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TARIFF_TIMELINE_LINE_RATE_PROJ_GRAPH, "Tariff Timeline Line Rate Projection Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_HS6_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL"), 
  time = "2024-12",
  COMM_LVL = "HS6",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "TOTAL FOR ALL COUNTRIES",
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
) %>%
  select(I_COMMODITY, I_COMMODITY_LDESC)


US_CATEGORY_ELECTRONICS_IMPORTS <- US_ELECTRONICS_IMPORTS %>%
  group_by(I_COMMODITY_6, CTY_NAME) %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  left_join(.,US_HS6_IMPORTS_BULK, by = c("I_COMMODITY_6" = "I_COMMODITY")) %>%
  filter(I_COMMODITY_6 %in% c("847150","851713","847130","847330")) %>%
  mutate(CTY_NAME = case_when(
    CTY_NAME == "MEXICO" ~ "Mexico",
    CTY_NAME == "CHINA" ~ "China",
    CTY_NAME == "HONG KONG" ~ "China",
    CTY_NAME == "MACAU" ~ "China",
    CTY_NAME == "TAIWAN" ~ "Taiwan",
    CTY_NAME == "VIETNAM" ~ "Vietnam",
    CTY_NAME == "KOREA, SOUTH" ~ "South Korea",
    CTY_NAME == "INDIA" ~ "India",
    CTY_NAME == "MALAYSIA" ~ "Malaysia",
    TRUE ~ "Other"
  )) %>%
  group_by(CTY_NAME,I_COMMODITY_6,I_COMMODITY_LDESC) %>%
  summarise(Imports = sum(Imports)) %>%
  ungroup() %>%
  mutate(I_COMMODITY_6 = case_when(
    I_COMMODITY_6 == "847130" ~ "Laptops",
    I_COMMODITY_6 == "847150" ~ "Desktops",
    I_COMMODITY_6 == "847330" ~ "Computer\nParts",
    I_COMMODITY_6 == "851713" ~ "Smartphones",
  )) %>%
  mutate(I_COMMODITY_6 = factor(I_COMMODITY_6, levels = rev(c("Desktops","Smartphones","Computer\nParts","Laptops")))) %>%
  mutate(CTY_NAME = factor(CTY_NAME, levels = rev(c("China","Mexico","Vietnam","Taiwan","South Korea","India","Malaysia","Other"))))


ELECTRONICS_IMPORTS_BY_COUNTRY_GRAPH <- ggplot(data = US_CATEGORY_ELECTRONICS_IMPORTS, aes(x = I_COMMODITY_6, y = Imports/1000000000, fill = CTY_NAME)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ggtitle("Key Electronics Imports Possibly\nTargeted For Future Trump Tariffs") +
  ylab("Affected US Imports by Country, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,80), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  scale_fill_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("China","Mexico","Vietnam","Taiwan","South Korea","India","Malaysia","Other")) +
  labs(caption = "Graph created by @JosephPolitano using Census Data. Desktops = HS 847150, Smartphones = 851713, Computer Parts = 847330, Laptops = 847130", subtitle = "Trump has Promised Tariffs on Chips and the Whole Electronics Supply Chain,\nIncluding these Goods Which Remain Currently Exempt from Tariffs") +
  theme_apricitas + theme(legend.position = c(.9,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = ELECTRONICS_IMPORTS_BY_COUNTRY_GRAPH, "Electronics Imports By Country Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




US_CHIP_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO", "I_COMMODITY","CTY_CODE", "CTY_NAME"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "85411000",
  I_COMMODITY = "85412100",
  I_COMMODITY = "85412900",
  I_COMMODITY = "85413000",
  I_COMMODITY = "85414910",
  I_COMMODITY = "85414970",
  I_COMMODITY = "85414980",
  I_COMMODITY = "85414995",
  I_COMMODITY = "85415100",
  I_COMMODITY = "85415100",
  I_COMMODITY = "85415900",
  I_COMMODITY = "85419000",
  I_COMMODITY = "85423100",
  I_COMMODITY = "85423200",
  I_COMMODITY = "85423300",
  I_COMMODITY = "85423900",
  I_COMMODITY = "85429000",
  I_COMMODITY = "85415100",
  I_COMMODITY = "85415900",
  I_COMMODITY = "85419000",
  I_COMMODITY = "8542",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  #CTY_NAME = Countries[3],
  #CTY_NAME = Countries[4],
  #CTY_NAME = Countries[5],
)


US_CHIP_IMPORTS <- US_CHIP_IMPORTS_BULK %>%
  filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
  filter(!CTY_NAME %in% c("AUSTRIA","BELGIUM","BULGARIA","CROATIA","CYPRUS","CZECH REPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HUNGARY","IRELAND","ITALY","LATVIA","LITHUANIA","LUXEMBOURG","MALTA","NETHERLANDS","POLAND","PORTUGAL","ROMANIA","SLOVAKIA","SLOVENIA","SPAIN","SWEDEN")) %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO)) %>%
  group_by(CTY_NAME, time) %>%
  summarise(CON_VAL_MO = sum(CON_VAL_MO)) %>%
  pivot_wider(names_from = CTY_NAME, values_from = CON_VAL_MO) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~rollapply(.x, 12, sum, fill = NA, align = "right"))) %>%
  ungroup() %>%
  pivot_longer(-time) %>%
  mutate(time = as.Date(paste0(time,"-01"))) %>%
  mutate(name = case_when(
    name == "MALAYSIA" ~ "Malaysia",
    name == "TAIWAN" ~ "Taiwan",
    name == "ISRAEL" ~ "Israel",
    name == "KOREA, SOUTH" ~ "South Korea",
    name == "EUROPEAN UNION" ~ "EU",
    name == "CHINA" ~ "China",
    name == "HONG KONG" ~ "China",
    name == "MACAU" ~ "China",
    name == "Mexico" ~ "Mexico",
    TRUE ~ "Other"
  )) %>%
  group_by(name,time) %>%
  summarise(value = sum(value)) %>%
  drop_na() %>%
  pivot_wider()


US_CHIP_IMPORTS_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= China/1000000000,color= "China"), size = 1.25) + 
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= EU/1000000000,color= "EU"), size = 1.25) + 
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= Israel/1000000000,color= "Israel"), size = 1.25) + 
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= Malaysia/1000000000,color= "Malaysia"), size = 1.25) + 
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= `South Korea`/1000000000,color= "South Korea"), size = 1.25) + 
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= Taiwan/1000000000,color= "Taiwan"), size = 1.25) + 
  geom_line(data=US_CHIP_IMPORTS, aes(x=time,y= Other/1000000000,color= "Other"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,20), breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Billions of Dollars, 12MMA") +
  ggtitle("Chip Imports Targeted for Trump Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "America Imports Billions in Chips, Mostly From Taiwan, Malaysia, Israel, the EU, & South Korea") +
  theme_apricitas + theme(plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("Taiwan", "Malaysia", "Israel", "EU", "South Korea", "China", "Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*20), ymax = 0) +
  theme_apricitas + theme(legend.position = c(.9,.80), legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0.1, "cm")) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_CHIP_IMPORTS_GRAPH, "US Chip Import Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE





US_CATEGORY_PHARMA_IMPORTS <- US_PHARMA_IMPORTS %>%
  group_by(I_COMMODITY,I_COMMODITY_LDESC) %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  arrange(Imports)

US_CATEGORY_COPPER_IMPORTS <- US_COPPER_IMPORTS %>%
  group_by(I_COMMODITY,I_COMMODITY_LDESC) %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  arrange(desc(Imports))

US_CATEGORY_LUMBER_IMPORTS <- US_LUMBER_IMPORTS %>%
  group_by(I_COMMODITY,I_COMMODITY_LDESC) %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  arrange(desc(Imports))

US_CATEGORY_CRITICAL_MINERALS_IMPORTS <- US_CRIT_MINERALS_IMPORTS %>%
  group_by(I_COMMODITY,I_COMMODITY_LDESC) %>%
  summarise(Imports = sum(CON_VAL_YR)) %>%
  arrange(desc(Imports))
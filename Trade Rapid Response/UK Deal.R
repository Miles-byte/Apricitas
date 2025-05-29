US_UK_HS10_IMPORTS_QUANTITY_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC","COMM_LVL","GEN_QY1_YR"), 
  time = "2024-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)


US_UK_IMPORTS_CARS <- US_UK_HS10_IMPORTS_QUANTITY_BULK %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY_8 %in% c("87032201", "87032301", "87032401", "87033101", "87033201", "87033301", 
                                               "87034000", "87035000", "87036000", "87037000", "87038000", "87039001", 
                                               "87042101", "87043101", "87044100", "87045100", "87046000"), 0, tariff)) %>% #car exclusion
  filter(tariff < .25) %>%
  mutate(GEN_QY1_YR = as.numeric(GEN_QY1_YR), CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  summarize(GEN_QY1_YR = sum(GEN_QY1_YR, na.rm = TRUE),GEN_QY1_YR = sum(GEN_QY1_YR, na.rm = TRUE), CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  mutate(value = (100000/GEN_QY1_YR)*CON_VAL_YR)

US_UK_IMPORTS_JETS <- US_UK_HS10_IMPORTS_QUANTITY_BULK %>%
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY_4 %in% c("8411", "8803"), 0, tariff)) %>% #jets exclusion
  filter(tariff < .25) %>%
  mutate(GEN_QY1_YR = as.numeric(GEN_QY1_YR), CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  summarize(GEN_QY1_YR = sum(GEN_QY1_YR, na.rm = TRUE),GEN_QY1_YR = sum(GEN_QY1_YR, na.rm = TRUE), CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE))


US_UK_IMPORTS_STEEL_ALUMINUM <- US_UK_HS10_IMPORTS_QUANTITY_BULK %>%
  mutate(tariff = .25) %>%
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7601103000, 7601106030, 7601106090, 7601203000, 7601206000, 7601209030,
    7601209045, 7601209060, 7601209075, 7601209080, 7601209085, 7601209095,
    7604101000, 7604103000, 7604105000, 7604210010, 7604210090, 7604291010,
    7604291090, 7604293030, 7604293060, 7604293090, 7604295020, 7604295050,
    7604295090, 7605110000, 7605190000, 7605210000, 7605290000, 7606113030,
    7606113060, 7606116000, 7606123015, 7606123025, 7606123035, 7606123045,
    7606123055, 7606123091, 7606123096, 7606126000, 7606913055, 7606913095,
    7606916055, 7606916095, 7606923025, 7606923035, 7606926055, 7606926095,
    7607113000, 7607116010, 7607116090, 7607119030, 7607119060, 7607119090,
    7607191000, 7607193000, 7607196000, 7607201000, 7607205000, 7608100030,
    7608100090, 7608200030, 7608200090, 7609000000, 7616995160, 7616995170,
    7610100010, 7610100020, 7610100030, 7610900020, 7610900040, 7610900060,
    7610900080, 7615102015, 7615102025, 7615103015, 7615103025, 7615105020,
    7615105040, 7615107125, 7615107130, 7615107155, 7615107180, 7615109100,
    7615200000, 7616109090, 7616991000, 7616995130, 7616995140, 7616995190
  ), 0, tariff)) %>% #ALUMINUM
  mutate(tariff = if_else(I_COMMODITY %in% c(
    7206100000,7206900000,7207110000,7207120010,7207120050,7207190030,7207190090,7207200025,
    7207200045,7207200075,7207200090,7208101500,7208103000,7208106000,7208253000,7208256000,
    7208260030,7208260060,7208270040,7208270045,7208270060,7208360030,7208360060,7208370030,
    7208370060,7208380015,7208380030,7208380090,7208390020,7208390025,7208390030,7208390090,
    7208403030,7208403060,7208406030,7208406060,7208510030,7208510045,7208510060,7208520000,
    7208530000,7208540000,7208900000,7209150000,7209160040,7209160045,7209160060,7209160070,
    7209160091,7209170040,7209170045,7209170060,7209170070,7209170091,7209181530,7209181560,
    7209182520,7209182585,7209186020,7209186090,7209250000,7209260000,7209270000,7209280000,
    7209900000,7210110000,7210120000,7210200000,7210300030,7210300060,7210410000,7210490040,
    7210490045,7210490091,7210490095,7210500020,7210500090,7210610000,7210690000,7210703000,
    7210706030,7210706060,7210706090,7210901000,7210906000,7210909000,7211130000,7211140030,
    7211140045,7211140090,7211191500,7211192000,7211193000,7211194500,7211196000,7211197530,
    7211197560,7211197590,7211231500,7211232000,7211233000,7211234500,7211236030,7211236060,
    7211236090,7211292030,7211292090,7211294500,7211296030,7211296080,7211900000,7212100000,
    7212200000,7212301030,7212301090,7212303000,7212305000,7212401000,7212405000,7212500000,
    7212600000,7213100000,7213200010,7213200080,7213913011,7213913015,7213913020,7213913093,
    7213914500,7213916000,7213990030,7213990060,7213990090,7214100000,7214200000,7214300010,
    7214300080,7214910016,7214910020,7214910060,7214910090,7214990016,7214990021,7214990026,
    7214990031,7214990036,7214990040,7214990045,7214990060,7214990075,7214990090,7215100010,
    7215100080,7215500016,7215500018,7215500020,7215500061,7215500063,7215500065,7215500090,
    7215901000,7215903000,7215905000,7216100010,7216100050,7216210000,7216220000,7216310000,
    7216320000,7216330030,7216330060,7216330090,7216400010,7216400050,7216500000,7216990010,
    7216990090,7217101000,7217102000,7217103000,7217104040,7217104045,7217104090,7217105030,
    7217105090,7217106000,7217107000,7217108010,7217108020,7217108025,7217108030,7217108045,
    7217108060,7217108075,7217108090,7217109000,7217201500,7217203000,7217204510,7217204520,
    7217204530,7217204540,7217204550,7217204560,7217204570,7217204580,7217206000,7217207500,
    7217301530,7217301560,7217303000,7217304504,7217304511,7217304520,7217304530,7217304541,
    7217304550,7217304560,7217304590,7217306000,7217307500,7217901000,7217905030,7217905060,
    7217905090,7218100000,7218910015,7218910030,7218910060,7218990015,7218990030,7218990045,
    7218990060,7218990090,7229200015,7229200090,7229900500,7229901000,7229905006,7229905008,7229905016,7229905031,
    7229905051,7229909000,7301100000,7302101010,7302101015,7302101025,7302101035,7302101045,
    7302101055,7302101065,7302101075,7302105020,7302105040,7302105060,7302400000,7302901000,
    7302909000,7304110020,7304110050,7304110080,7304191020,7304191030,7304191045,7304191060,
    7304191080,7304195020,7304195050,7304195080,7304220030,7304220045,7304220060,7304233000,
    7304236030,7304236045,7304236060,7304243010,7304243020,7304243030,7304243040,7304243045,
    7304243080,7304244010,7304244020,7304244030,7304244040,7304244050,7304244060,7304244080,
    7304246015,7304246030,7304246045,7304246060,7304246075,7304291010,7304291020,7304291030,
    7304291040,7304291050,7304291060,7304291080,7304292010,7304292020,7304292030,7304292040,
    7304292050,7304292060,7304292080,7304293110,7304293120,7304293130,7304293140,7304293150,
    7304293160,7304293180,7304294110,7304294120,7304294130,7304294140,7304294150,7304294160,
    7304294180,7304295015,7304295030,7304295045,7304295060,7304295075,7304296115,7304296130,
    7304296145,7304296160,7304296175,7304313000,7304316010,7304316050,7304390002,7304390004,
    7304390006,7304390008,7304390016,7304390020,7304390024,7304390028,7304390032,7304390036,
    7304390040,7304390044,7304390048,7304390052,7304390056,7304390062,7304390068,7304390072,
    7304390076,7304390080,7304413005,7304413015,7304413045,7304416005,7304416015,7304416045,
    7304490005,7304490015,7304490045,7304490060,7304511000,7304515005,7304515015,7304515045,
    7304515060,7304591000,7304592030,7304592040,7304592045,7304592055,7304592060,7304592070,
    7304592080,7304596000,7304598010,7304598015,7304598020,7304598025,7304598030,7304598035,
    7304598040,7304598045,7304598050,7304598055,7304598060,7304598065,7304598070,7304598080,
    7304901000,7304903000,7304905000,7304907000,7305111030,7305111060,7305115000,7305121030,
    7305121060,7305125000,7305191030,7305191060,7305195000,7305202000,7305204000,7305206000,
    7305208000,7305312000,7305314000,7305316010,7305316090,7305391000,7305395000,7305901000,
    7305905000,7306101010,7306101050,7306105010,7306105050,7306110010,7306110050,7306191010,
    7306191050,7306195110,7306195150,7306213000,7306214000,7306218010,7306218050,7306291030,
    7306291090,7306292000,7306293100,7306294100,7306296010,7306296050,7306298110,7306298150,
    7306301000,7306303000,7306305010,7306305015,7306305020,7306305025,7306305028,7306305032,
    7306305035,7306305040,7306305055,7306305085,7306305090,7306401010,7306401015,7306401090,
    7306405005,7306405015,7306405040,7306405042,7306405044,7306405062,7306405064,7306405080,
    7306405085,7306405090,7306501000,7306503000,7306505010,7306505030,7306505050,7306505070,
    7306611000,7306613000,7306615000,7306617030,7306617060,7306691000,7306693000,7306695000,
    7306697030,7306697060,7306901000,7306905000.7301201000,7301205000,7302300000,7307211000,7307221000,7307225000,7307230030,7307230090,
    7307290030,7307290090,7307911000,7307913000,7307915010,7307915030,7307915050,7307915070,
    7307923010,7307923030,7307929000,7307933010,7307933040,7307936000,7307939010,7307939040,
    7307939060,7307991000,7307993000,7307995015,7307995045,7307995060,7308100000,7308200020,
    7308200090,7308301000,7308305015,7308305025,7308305050,7308400000,7308903000,7308906000,
    7308907000,7308909530,7308909560,7308909590,7309000030,7309000090,7310100005,7310100015,
    7310100090,7310210025,7310210050,7310290020,7310290030,7310290055,7310290065,7311000030,
    7311000060,7311000090,7312100500,7312101030,7312101050,7312101070,7312102000,7312103005,
    7312103010,7312103012,7312103020,7312103045,7312103065,7312103070,7312103074,7312103080,
    7312105000,7312106030,7312106060,7312107000,7312108000,7312109030,7312109060,7312109090,
    7312900000,7313000000,7314121000,7314122000,7314123000,7314126000,7314129000,7314141000,
    7314142000,7314143000,7314146000,7314149000,7314190100,7314200000,7314311000,7314315010,
    7314315080,7314390000,7314410030,7314410040,7314410045,7314410080,7314420030,7314420060,
    7314493000,7314496000,7314500000,7315110005,7315110010,7315110045,7315110060,7315120020,
    7315120040,7315120060,7315120080,7315190000,7315201000,7315205000,7315810000,7315821000,
    7315823000,7315825000,7315827000,7315891000,7315893000,7315895000,7315900000,7316000000,
    7317001000,7317002000,7317003000,7317005501,7317005502,7317005503,7317005505,7317005507,
    7317005508,7317005511,7317005518,7317005519,7317005520,7317005530,7317005540,7317005550,
    7317005560,7317005570,7317005580,7317005590,7317006530,7317006560,7317007500,7318110000,
    7318120000,7318130030,7318130060,7318141030,7318141060,7318145020,7318145080,7318152010,
    7318152020,7318152030,7318152041,7318152046,7318152051,7318152055,7318152061,7318152065,
    7318152091,7318152095,7318154000,7318155030,7318155051,7318155056,7318155090,7318156010,
    7318156040,7318156070,7318156080,7318158020,7318158030,7318158045,7318158055,7318158066,
    7318158069,7318158082,7318158085,7318160015,7318160030,7318160045,7318160060,7318160085,
    7318190000,7318210030,7318210090,7318220000,7318230000,7318240000,7318290000,7319402010,
    7319402050,7319403000,7319405010,7319405050,7319901000,7319909000,7320103000,7320106015,
    7320106060,7320109015,7320109060,7320201000,7320205010,7320205020,7320205045,7320205060,
    7320901000,7320905010,7320905020,7320905060,7321111030,7321111060,7321113010,7321113020,
    7321113050,7321116000,7321120000,7321190020,7321190040,7321190060,7321190080,7321811000,
    7321815000,7321821000,7321825000,7321890010,7321890050,7321901000,7321902000,7321904000,
    7321905000,7321906040,7321906060,7321906090,7326908688
  ), 0, tariff)) %>% #STEEL
  mutate(I_COMMODITY_8 = substr(I_COMMODITY, 1, 8)) %>%
  mutate(I_COMMODITY_6 = substr(I_COMMODITY, 1, 6)) %>%
  mutate(I_COMMODITY_4 = substr(I_COMMODITY, 1, 4)) %>%
  filter(tariff < .25) %>%
  mutate(GEN_QY1_YR = as.numeric(GEN_QY1_YR), CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  summarize(GEN_QY1_YR = sum(GEN_QY1_YR, na.rm = TRUE),GEN_QY1_YR = sum(GEN_QY1_YR, na.rm = TRUE), CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE))




EFFECTIVE_TARIFF <- data.frame(
  date = seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day"),
  value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-02-04"), TOTAL_IMPORTS$CAL_DUT_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1], TOTAL_IMPORTS$CAL_DUT_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1] + .1*CHINA_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1]), #10% China
  category = "Implemented"
) %>%
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + .1*CHINA_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% China Again
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + .25*MEXICO_TOTAL$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% MEXICO
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + .1*CANADA_SPLIT$CON_VAL_YR[2]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% CANADIAN ENERGY
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + .25*(CANADA_SPLIT$CON_VAL_YR[1]-CANADA_SPLIT$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% CANADA
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-07"), value, value - .25*MEXICO_TOTAL_USMCA$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #MEXICO USMCA EXCLUSION
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value - .1*CANADA_SPLIT_USMCA$CON_VAL_YR[2]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% CANADIAN ENERGY EXCLUSION
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-07"), value, value - .25*(CANADA_SPLIT_USMCA$CON_VAL_YR[1]-CANADA_SPLIT_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>%#25% CANADA USMCA EXCLUSION
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-12"), value, value + .25*STEEL_TOTAL$CON_VAL_YR[6]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Steel
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-12"), value, value + .25*ALUMINUM_TOTAL$CON_VAL_YR[6]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Aluminum
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-12"), value, value - .20*(STEEL_TOTAL$CON_VAL_YR[2]+STEEL_TOTAL$CON_VAL_YR[4])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Destacking China Steel
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-12"), value, value - .20*(ALUMINUM_TOTAL$CON_VAL_YR[2]+ALUMINUM_TOTAL$CON_VAL_YR[4])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Destacking China Aluminum
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-12"), value, value + 29000000000/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #USING BCG ESTIMATE FOR COST OF TARIFF ON DERIVATIVES
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + .25*CAR_TOTAL$CON_VAL_YR[6]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Cars
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value - .25*(CAR_TOTAL_USMCA$CON_VAL_YR[1]+CAR_TOTAL_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>%#%>% #Temporary Exclusion for USMCA Cars
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-03"), value, value + .10*1305214086314/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #10% RECIPROCAL
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-09"), value, value + .1678111*1305214086314/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #REST OF RECIPR0CAL
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-09"), value, value + .50*279176209825/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #50% CHINA RECIPROCAL LIST
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-10"), value, value - .1474396*1026093911593/TOTAL_IMPORTS$CON_VAL_YR[1])) %>%#10-40% non-China Paused
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-10"), value, value + .41*279176209825/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #41% CHINA RECIPROCAL LIST
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-03"), value, value + .25*(CAR_PARTS_TOTAL$CON_VAL_YR[6])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #25% Vehicle Parts 
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-03"), value, value - .25*(CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[1]+CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Temporary Exclusion for USMCA Parts
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-03"), value, value - .20*(CAR_PARTS_TOTAL$CON_VAL_YR[2]+CAR_PARTS_TOTAL$CON_VAL_YR[3]+CAR_PARTS_TOTAL$CON_VAL_YR[4])/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Destacking China Car Parts Tariffs
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + (1-.6654735594)*.25*CAR_TOTAL_USMCA$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Excluding non-canadian content from vehicles
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-04"), value, value + (1-.491)*.25*CAR_TOTAL_USMCA$CON_VAL_YR[2]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Excluding non-Canadian content from vehicles
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-08"), value, value - .15*US_UK_IMPORTS_CARS$value/TOTAL_IMPORTS$CON_VAL_YR[1])) #%>% #Excluding non-Canadian content from vehicles  
  #mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-08"), value, value - .10*US_UK_IMPORTS_JETS$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) %>% #Excluding non-Canadian content from vehicles
  #mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-08"), value, value - .25*US_UK_IMPORTS_STEEL_ALUMINUM$CON_VAL_YR[1]/TOTAL_IMPORTS$CON_VAL_YR[1])) #%>% #Excluding non-Canadian content from vehicles
  
#mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-03"), value, value + .25*(.530272*CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[1]+0.368127*CAR_PARTS_TOTAL_USMCA$CON_VAL_YR[2])/TOTAL_IMPORTS$CON_VAL_YR[1])) #Adding Canadian/Mexican Content for USMCA Parts

TARIFF_TIMELINE_LINE_RATE_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  #geom_line(data=IMPLEMENTED_SHARE, aes(x=date,y= value,color= "Share of US Imports Hit By New Tariffs"), size = 1.25) + 
  geom_segment(aes(x = as.Date("2025-02-03"), xend = as.Date("2025-02-03"), y = 0, yend = 0.09), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% China",x = as.Date("2025-02-02"), y = .06, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-03-03"), xend = as.Date("2025-03-03"), y = 0, yend = 0.155), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% China\n25% Mexico\n10-25% Canada",x = as.Date("2025-03-02"), y = .125, size = 4, color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-03-06"), xend = as.Date("2025-03-06"), y = 0, yend = 0.185), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "Exclusion for\nUSMCA Goods",x = as.Date("2025-03-07"), y = .16, size = 4,color = "white",hjust = 0, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-03-11"), xend = as.Date("2025-03-11"), y = 0, yend = 0.135), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "25% Steel\n25% Aluminum",x = as.Date("2025-03-12"), y = 0.06, size = 4,color = "white",hjust = 0, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-04-02"), xend = as.Date("2025-04-02"), y = 0, yend = 0.21), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% Most Countries\n25% Cars",x = as.Date("2025-04-01"), y = 0.20, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-04-08"), xend = as.Date("2025-04-08"), y = 0, yend = 0.275), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10-40% Most Countries\n74% China",x = as.Date("2025-04-07"), y = 0.25, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-04-09"), xend = as.Date("2025-04-09"), y = 0, yend = 0.35), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "41% China\n90-Day Pause on\n10-40% Other Countries",x = as.Date("2025-04-08"), y = 0.325, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-05-02"), xend = as.Date("2025-05-02"), y = 0, yend = 0.27), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "25% Non-USMCA\nCar Parts",x = as.Date("2025-05-01"), y = 0.20, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_line(data=EFFECTIVE_TARIFF, aes(x=date,y= value,color= "Tariff Rate on 2024 Import Mix"), size = 1.25) + 
  geom_segment(aes(x = as.Date("2025-05-08"), xend = as.Date("2025-05-08"), y = 0, yend = 0.35), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = 'UK "Deal"',x = as.Date("2025-05-07"), y = 0.325, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_line(data=EFFECTIVE_TARIFF, aes(x=date,y= value,color= "Tariff Rate on 2024 Import Mix"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.35), breaks = c(0,.05,.10,.15,.20,.25,.30,.35,.4,.45,.5), expand = c(0,0)) +
  ylab("Tariff Rate on 2024 Import Mix") +
  ggtitle("A Timeline of Trump's 2nd-Term Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data.",subtitle = "In His 2nd Term, Trump Has Imposed Tariffs On Trillions of Dollars in US Trade") +
  theme_apricitas + theme(legend.position = c(.20,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-20")-(.1861*(today()-as.Date("2025-01-20"))), xmax = as.Date("2025-01-20")-(0.049*(today()-as.Date("2025-01-20"))), ymin = 0-(.3*(.35)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TARIFF_TIMELINE_LINE_RATE_GRAPH, "Tariff Timeline Line Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")










UK_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR","CTY_CODE", "CTY_NAME","CAL_DUT_YR"), 
  time = "2024-12",
  CTY_NAME = "UNITED KINGDOM",
)

UK_IMPORTS <- UK_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  mutate(CAL_DUT_YR = as.numeric(CAL_DUT_YR)) %>%
  group_by(time) %>%
  summarise(CON_VAL_YR = sum(CON_VAL_YR), CAL_DUT_YR = sum(CAL_DUT_YR))

UK_CAR_PARTS_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY", "CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"),
  time = "2024-12",
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
  CTY_NAME = "UNITED KINGDOM",
  #CTY_NAME = "EUROPEAN UNION"
)

UK_CAR_PARTS_TOTAL <- UK_CAR_PARTS_IMPORTS %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(CTY_NAME) %>%
  summarize(time,CTY_CODE,CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()





EFFECTIVE_TARIFF_UK <- data.frame(
  date = seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day"),
  value = UK_IMPORTS$CAL_DUT_YR[1]/UK_IMPORTS$CON_VAL_YR[1], #10% China
  category = "Implemented"
) %>%
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-03-12"), value, value + .25*US_UK_IMPORTS_STEEL_ALUMINUM$CON_VAL_YR[1]/UK_IMPORTS$CON_VAL_YR[1])) %>% #25% Steel/Aluminum
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-03"), value, value + .25*US_UK_IMPORTS_CARS$CON_VAL_YR[1]/UK_IMPORTS$CON_VAL_YR[1])) %>% #25% Cars
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-04-03"), value, value + .10*37299323080/UK_IMPORTS$CON_VAL_YR[1])) %>% #10% Reciprocal
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-03"), value, value + .25*UK_CAR_PARTS_TOTAL$CON_VAL_YR[1]/UK_IMPORTS$CON_VAL_YR[1])) %>% #25% Car Parts
  mutate(value = ifelse(seq(as.Date("2025-01-20"), Sys.Date() + 3, by = "day") < as.Date("2025-05-08"), value, value - .15*US_UK_IMPORTS_CARS$value/UK_IMPORTS$CON_VAL_YR[1])) #%>% #Excluding non-Canadian content from vehicles  


UK_TARIFF_TIMELINE_LINE_RATE_GRAPH <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  #geom_line(data=IMPLEMENTED_SHARE, aes(x=date,y= value,color= "Share of US Imports Hit By New Tariffs"), size = 1.25) + 
  geom_segment(aes(x = as.Date("2025-03-11"), xend = as.Date("2025-03-11"), y = 0, yend = .035), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "25% Steel\n25% Aluminum",x = as.Date("2025-03-10"), y = 0.026, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-04-02"), xend = as.Date("2025-04-02"), y = 0, yend = .125), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "10% Most Goods\n25% Automobiles",x = as.Date("2025-04-01"), y = .1, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_line(data=EFFECTIVE_TARIFF_UK, aes(x=date,y= value,color= "Tariff Rate on 2024 Import Mix"), size = 1.25) + 
  annotate(geom = "text", label = "10% Most Goods\n25% Automobiles",x = as.Date("2025-04-01"), y = .1, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-05-02"), xend = as.Date("2025-05-02"), y = .1, yend = .15), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = "25% Non-USMCA\nCar Parts",x = as.Date("2025-05-01"), y = 0.125, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_segment(aes(x = as.Date("2025-05-08"), xend = as.Date("2025-05-08"), y = 0, yend = 0.125), color = "white", size = 1, linetype = "dashed") +
  annotate(geom = "text", label = 'UK "Deal"',x = as.Date("2025-05-07"), y = 0.075, size = 4,color = "white",hjust = 1, lineheight = 0.9) +
  geom_line(data=EFFECTIVE_TARIFF_UK, aes(x=date,y= value,color= "Tariff Rate on 2024 Import Mix"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.15), breaks = c(0,.05,.1,.15), expand = c(0,0)) +
  ylab("Tariff Rate on 2024 Import Mix") +
  ggtitle("A Timeline of Trump's 2nd-Term UK Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Census data.",subtitle = "In His 2nd Term, Trump Has Imposed Heavy Tariffs On Most Imports From the UK") +
  theme_apricitas + theme(legend.position = c(.25,.89), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-20")-(.1861*(today()-as.Date("2025-01-20"))), xmax = as.Date("2025-01-20")-(0.049*(today()-as.Date("2025-01-20"))), ymin = 0-(.3*(.150)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_TARIFF_TIMELINE_LINE_RATE_GRAPH, "UK Tariff Timeline Line Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



CAR_IMPORTS_TIMELINE_BULK_UK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_MO","I_COMMODITY","CTY_CODE", "CTY_NAME","GEN_QY1_MO"), 
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
  I_COMMODITY = "8703400005",
  I_COMMODITY = "8703400010",
  I_COMMODITY = "8703400015",
  I_COMMODITY = "8703400020",
  I_COMMODITY = "8703400030",
  I_COMMODITY = "8703400040",
  I_COMMODITY = "8703400045",
  I_COMMODITY = "8703400050",
  I_COMMODITY = "8703400055",
  I_COMMODITY = "8703400060",
  I_COMMODITY = "8703400070",
  I_COMMODITY = "8703400080",
  I_COMMODITY = "8703400090",
  I_COMMODITY = "8703500010",
  I_COMMODITY = "8703500030",
  I_COMMODITY = "8703500050",
  I_COMMODITY = "8703500060",
  I_COMMODITY = "8703500065",
  I_COMMODITY = "8703500070",
  I_COMMODITY = "8703500090",
  I_COMMODITY = "8703600005",
  I_COMMODITY = "8703600010",
  I_COMMODITY = "8703600015",
  I_COMMODITY = "8703600020",
  I_COMMODITY = "8703600030",
  I_COMMODITY = "8703600040",
  I_COMMODITY = "8703600045",
  I_COMMODITY = "8703600050",
  I_COMMODITY = "8703600055",
  I_COMMODITY = "8703600060",
  I_COMMODITY = "8703600070",
  I_COMMODITY = "8703600080",
  I_COMMODITY = "8703600090",
  I_COMMODITY = "8703700010",
  I_COMMODITY = "8703700030",
  I_COMMODITY = "8703700050",
  I_COMMODITY = "8703700060",
  I_COMMODITY = "8703700065",
  I_COMMODITY = "8703700070",
  I_COMMODITY = "8703700090",
  I_COMMODITY = "8703800000",
  I_COMMODITY = "8703800010",
  I_COMMODITY = "8703800020",
  I_COMMODITY = "8703800045",
  I_COMMODITY = "8703800060",
  I_COMMODITY = "8703800080",
  I_COMMODITY = "8703800090",
  I_COMMODITY = "8703900100",
  I_COMMODITY = "8704210100",
  I_COMMODITY = "8704310020", 
  I_COMMODITY = "8704310040", 
  I_COMMODITY = "8704310120", 
  I_COMMODITY = "8704310140", 
  I_COMMODITY = "8704410000", 
  I_COMMODITY = "8704510020",
  I_COMMODITY = "8704510040",
  I_COMMODITY = "8704600000",
  CTY_NAME = "UNITED KINGDOM",
)

CAR_IMPORTS_TIMELINE_UK <- CAR_IMPORTS_TIMELINE_BULK_UK %>%
  mutate(GEN_QY1_MO = as.numeric(GEN_QY1_MO)) %>%
  mutate(time = as.Date(paste0(substr(time, 1, 4), "-01-01"))) %>%
  filter(time < as.Date("2025-01-01")) %>%
  group_by(time) %>%
  summarize(time,GEN_QY1_MO = sum(GEN_QY1_MO, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

CAR_IMPORTS_TIMELINE_UK_GRAPH <- ggplot() + #plotting black-white unemployment graph
  geom_line(data=CAR_IMPORTS_TIMELINE_UK, aes(x=time,y= GEN_QY1_MO/1000,color= "US Car Imports from the UK"), size = 1.25)+ 
  annotate(geom = "hline", y = 100, yintercept = 100, color = "white", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "New US Quota For 10% Tariff Rate", x = as.Date("2018-06-01"), y = 110, color ="white", size = 5) +
  xlab("Date") +
  ylab("Number of Vehicles") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"),limits = c(0,250),breaks = c(0,50,100,150,200,250), expand = c(0,0)) +
  ggtitle("US Vehicle Imports From the UK") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "The US Now Imposes a 10% Tariff on the First 100k British Vehicle Imports Instead of 25%") +
  theme_apricitas + theme(legend.position = c(.5,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*(250)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAR_IMPORTS_TIMELINE_UK_GRAPH, "Car Imports Timeline UK Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




US_UK_HS10_IMPORTS_QUANTITY_BULK_2017 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2017-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2018 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2018-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2019 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2019-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2020 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2020-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2021 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2021-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2022 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2022-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2023 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2023-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)

US_UK_HS10_IMPORTS_QUANTITY_BULK_2024 <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("CON_VAL_YR", "I_COMMODITY","CTY_CODE", "CTY_NAME","COMM_LVL"), 
  time = "2024-12",
  COMM_LVL = "HS10",
  #I_COMMODITY = "????",
  #I_COMMODITY = "*",#ALL COuntries
  #CTY_CODE = "0201", #TOTAL
  CTY_NAME = "UNITED KINGDOM",
)


US_UK_IMPORTS_STEEL_ALUMINUM_MO <- rbind(US_UK_HS10_IMPORTS_QUANTITY_BULK_2017,US_UK_HS10_IMPORTS_QUANTITY_BULK_2018,US_UK_HS10_IMPORTS_QUANTITY_BULK_2019,
                                         US_UK_HS10_IMPORTS_QUANTITY_BULK_2020,US_UK_HS10_IMPORTS_QUANTITY_BULK_2021,US_UK_HS10_IMPORTS_QUANTITY_BULK_2022,
                                         US_UK_HS10_IMPORTS_QUANTITY_BULK_2023,US_UK_HS10_IMPORTS_QUANTITY_BULK_2024)

US_UK_IMPORTS_STEEL_ALUMINUM_MO <- US_UK_IMPORTS_STEEL_ALUMINUM_MO %>%
  mutate(category = "text") %>%
  mutate(category = if_else(I_COMMODITY %in% c(
    7601103000, 7601106030, 7601106090, 7601203000, 7601206000, 7601209030,
    7601209045, 7601209060, 7601209075, 7601209080, 7601209085, 7601209095,
    7604101000, 7604103000, 7604105000, 7604210010, 7604210090, 7604291010,
    7604291090, 7604293030, 7604293060, 7604293090, 7604295020, 7604295050,
    7604295090, 7605110000, 7605190000, 7605210000, 7605290000, 7606113030,
    7606113060, 7606116000, 7606123015, 7606123025, 7606123035, 7606123045,
    7606123055, 7606123091, 7606123096, 7606126000, 7606913055, 7606913095,
    7606916055, 7606916095, 7606923025, 7606923035, 7606926055, 7606926095,
    7607113000, 7607116010, 7607116090, 7607119030, 7607119060, 7607119090,
    7607191000, 7607193000, 7607196000, 7607201000, 7607205000, 7608100030,
    7608100090, 7608200030, 7608200090, 7609000000, 7616995160, 7616995170,
    7610100010, 7610100020, 7610100030, 7610900020, 7610900040, 7610900060,
    7610900080, 7615102015, 7615102025, 7615103015, 7615103025, 7615105020,
    7615105040, 7615107125, 7615107130, 7615107155, 7615107180, 7615109100,
    7615200000, 7616109090, 7616991000, 7616995130, 7616995140, 7616995190
  ), "Aluminum", category)) %>% #ALUMINUM
  mutate(category = if_else(I_COMMODITY %in% c(
    7206100000,7206900000,7207110000,7207120010,7207120050,7207190030,7207190090,7207200025,
    7207200045,7207200075,7207200090,7208101500,7208103000,7208106000,7208253000,7208256000,
    7208260030,7208260060,7208270040,7208270045,7208270060,7208360030,7208360060,7208370030,
    7208370060,7208380015,7208380030,7208380090,7208390020,7208390025,7208390030,7208390090,
    7208403030,7208403060,7208406030,7208406060,7208510030,7208510045,7208510060,7208520000,
    7208530000,7208540000,7208900000,7209150000,7209160040,7209160045,7209160060,7209160070,
    7209160091,7209170040,7209170045,7209170060,7209170070,7209170091,7209181530,7209181560,
    7209182520,7209182585,7209186020,7209186090,7209250000,7209260000,7209270000,7209280000,
    7209900000,7210110000,7210120000,7210200000,7210300030,7210300060,7210410000,7210490040,
    7210490045,7210490091,7210490095,7210500020,7210500090,7210610000,7210690000,7210703000,
    7210706030,7210706060,7210706090,7210901000,7210906000,7210909000,7211130000,7211140030,
    7211140045,7211140090,7211191500,7211192000,7211193000,7211194500,7211196000,7211197530,
    7211197560,7211197590,7211231500,7211232000,7211233000,7211234500,7211236030,7211236060,
    7211236090,7211292030,7211292090,7211294500,7211296030,7211296080,7211900000,7212100000,
    7212200000,7212301030,7212301090,7212303000,7212305000,7212401000,7212405000,7212500000,
    7212600000,7213100000,7213200010,7213200080,7213913011,7213913015,7213913020,7213913093,
    7213914500,7213916000,7213990030,7213990060,7213990090,7214100000,7214200000,7214300010,
    7214300080,7214910016,7214910020,7214910060,7214910090,7214990016,7214990021,7214990026,
    7214990031,7214990036,7214990040,7214990045,7214990060,7214990075,7214990090,7215100010,
    7215100080,7215500016,7215500018,7215500020,7215500061,7215500063,7215500065,7215500090,
    7215901000,7215903000,7215905000,7216100010,7216100050,7216210000,7216220000,7216310000,
    7216320000,7216330030,7216330060,7216330090,7216400010,7216400050,7216500000,7216990010,
    7216990090,7217101000,7217102000,7217103000,7217104040,7217104045,7217104090,7217105030,
    7217105090,7217106000,7217107000,7217108010,7217108020,7217108025,7217108030,7217108045,
    7217108060,7217108075,7217108090,7217109000,7217201500,7217203000,7217204510,7217204520,
    7217204530,7217204540,7217204550,7217204560,7217204570,7217204580,7217206000,7217207500,
    7217301530,7217301560,7217303000,7217304504,7217304511,7217304520,7217304530,7217304541,
    7217304550,7217304560,7217304590,7217306000,7217307500,7217901000,7217905030,7217905060,
    7217905090,7218100000,7218910015,7218910030,7218910060,7218990015,7218990030,7218990045,
    7218990060,7218990090,7229200015,7229200090,7229900500,7229901000,7229905006,7229905008,7229905016,7229905031,
    7229905051,7229909000,7301100000,7302101010,7302101015,7302101025,7302101035,7302101045,
    7302101055,7302101065,7302101075,7302105020,7302105040,7302105060,7302400000,7302901000,
    7302909000,7304110020,7304110050,7304110080,7304191020,7304191030,7304191045,7304191060,
    7304191080,7304195020,7304195050,7304195080,7304220030,7304220045,7304220060,7304233000,
    7304236030,7304236045,7304236060,7304243010,7304243020,7304243030,7304243040,7304243045,
    7304243080,7304244010,7304244020,7304244030,7304244040,7304244050,7304244060,7304244080,
    7304246015,7304246030,7304246045,7304246060,7304246075,7304291010,7304291020,7304291030,
    7304291040,7304291050,7304291060,7304291080,7304292010,7304292020,7304292030,7304292040,
    7304292050,7304292060,7304292080,7304293110,7304293120,7304293130,7304293140,7304293150,
    7304293160,7304293180,7304294110,7304294120,7304294130,7304294140,7304294150,7304294160,
    7304294180,7304295015,7304295030,7304295045,7304295060,7304295075,7304296115,7304296130,
    7304296145,7304296160,7304296175,7304313000,7304316010,7304316050,7304390002,7304390004,
    7304390006,7304390008,7304390016,7304390020,7304390024,7304390028,7304390032,7304390036,
    7304390040,7304390044,7304390048,7304390052,7304390056,7304390062,7304390068,7304390072,
    7304390076,7304390080,7304413005,7304413015,7304413045,7304416005,7304416015,7304416045,
    7304490005,7304490015,7304490045,7304490060,7304511000,7304515005,7304515015,7304515045,
    7304515060,7304591000,7304592030,7304592040,7304592045,7304592055,7304592060,7304592070,
    7304592080,7304596000,7304598010,7304598015,7304598020,7304598025,7304598030,7304598035,
    7304598040,7304598045,7304598050,7304598055,7304598060,7304598065,7304598070,7304598080,
    7304901000,7304903000,7304905000,7304907000,7305111030,7305111060,7305115000,7305121030,
    7305121060,7305125000,7305191030,7305191060,7305195000,7305202000,7305204000,7305206000,
    7305208000,7305312000,7305314000,7305316010,7305316090,7305391000,7305395000,7305901000,
    7305905000,7306101010,7306101050,7306105010,7306105050,7306110010,7306110050,7306191010,
    7306191050,7306195110,7306195150,7306213000,7306214000,7306218010,7306218050,7306291030,
    7306291090,7306292000,7306293100,7306294100,7306296010,7306296050,7306298110,7306298150,
    7306301000,7306303000,7306305010,7306305015,7306305020,7306305025,7306305028,7306305032,
    7306305035,7306305040,7306305055,7306305085,7306305090,7306401010,7306401015,7306401090,
    7306405005,7306405015,7306405040,7306405042,7306405044,7306405062,7306405064,7306405080,
    7306405085,7306405090,7306501000,7306503000,7306505010,7306505030,7306505050,7306505070,
    7306611000,7306613000,7306615000,7306617030,7306617060,7306691000,7306693000,7306695000,
    7306697030,7306697060,7306901000,7306905000.7301201000,7301205000,7302300000,7307211000,7307221000,7307225000,7307230030,7307230090,
    7307290030,7307290090,7307911000,7307913000,7307915010,7307915030,7307915050,7307915070,
    7307923010,7307923030,7307929000,7307933010,7307933040,7307936000,7307939010,7307939040,
    7307939060,7307991000,7307993000,7307995015,7307995045,7307995060,7308100000,7308200020,
    7308200090,7308301000,7308305015,7308305025,7308305050,7308400000,7308903000,7308906000,
    7308907000,7308909530,7308909560,7308909590,7309000030,7309000090,7310100005,7310100015,
    7310100090,7310210025,7310210050,7310290020,7310290030,7310290055,7310290065,7311000030,
    7311000060,7311000090,7312100500,7312101030,7312101050,7312101070,7312102000,7312103005,
    7312103010,7312103012,7312103020,7312103045,7312103065,7312103070,7312103074,7312103080,
    7312105000,7312106030,7312106060,7312107000,7312108000,7312109030,7312109060,7312109090,
    7312900000,7313000000,7314121000,7314122000,7314123000,7314126000,7314129000,7314141000,
    7314142000,7314143000,7314146000,7314149000,7314190100,7314200000,7314311000,7314315010,
    7314315080,7314390000,7314410030,7314410040,7314410045,7314410080,7314420030,7314420060,
    7314493000,7314496000,7314500000,7315110005,7315110010,7315110045,7315110060,7315120020,
    7315120040,7315120060,7315120080,7315190000,7315201000,7315205000,7315810000,7315821000,
    7315823000,7315825000,7315827000,7315891000,7315893000,7315895000,7315900000,7316000000,
    7317001000,7317002000,7317003000,7317005501,7317005502,7317005503,7317005505,7317005507,
    7317005508,7317005511,7317005518,7317005519,7317005520,7317005530,7317005540,7317005550,
    7317005560,7317005570,7317005580,7317005590,7317006530,7317006560,7317007500,7318110000,
    7318120000,7318130030,7318130060,7318141030,7318141060,7318145020,7318145080,7318152010,
    7318152020,7318152030,7318152041,7318152046,7318152051,7318152055,7318152061,7318152065,
    7318152091,7318152095,7318154000,7318155030,7318155051,7318155056,7318155090,7318156010,
    7318156040,7318156070,7318156080,7318158020,7318158030,7318158045,7318158055,7318158066,
    7318158069,7318158082,7318158085,7318160015,7318160030,7318160045,7318160060,7318160085,
    7318190000,7318210030,7318210090,7318220000,7318230000,7318240000,7318290000,7319402010,
    7319402050,7319403000,7319405010,7319405050,7319901000,7319909000,7320103000,7320106015,
    7320106060,7320109015,7320109060,7320201000,7320205010,7320205020,7320205045,7320205060,
    7320901000,7320905010,7320905020,7320905060,7321111030,7321111060,7321113010,7321113020,
    7321113050,7321116000,7321120000,7321190020,7321190040,7321190060,7321190080,7321811000,
    7321815000,7321821000,7321825000,7321890010,7321890050,7321901000,7321902000,7321904000,
    7321905000,7321906040,7321906060,7321906090,7326908688
  ), "Steel", category)) %>% #STEEL
  filter(category %in% c("Steel","Aluminum")) %>%
  mutate(CON_VAL_YR = as.numeric(CON_VAL_YR)) %>%
  group_by(time,category) %>%
  summarize(CON_VAL_YR = sum(CON_VAL_YR, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(substr(time, 1, 4), "-01-01")))
  


KEY_US_METAL_IMPORTS_GRAPH <- ggplot(US_UK_IMPORTS_STEEL_ALUMINUM_MO, aes(fill=category, x=time, y=CON_VAL_YR/1000000000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  ylab("Annual Imports, Dollars") + 
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.25, suffix = "B"), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("Key US Metal Imports From the UK") +
  labs(caption = "Graph created by @JosephPolitano using US Census data", subtitle = "Trump Has Promised to Exempt the Small Amount of UK Steel/Aluminum Exports From Tariffs") +
   theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_fill_manual(name= "Imports Hit by New Tariffs",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#F5B041","#9A348E","#00A99D","#EE6055","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = KEY_US_METAL_IMPORTS_GRAPH, "Key US Metal Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_COUNTRIES_IMPORTS <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(CTY_NAME == "UNITED KINGDOM") %>%
  #filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
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
  mutate(Category = factor(Category, levels = rev(c("Pharmaceuticals","Critical Minerals","Lumber & Related","Electronics","Chipmaking Machinery","Semiconductors","Copper & Related"))))



UPCOMING_TARIFF_TARGET_BAR_CHART_UK <- ggplot(data = US_TARIFF_CATEGORIES, aes(x = Category, y = Imports/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ggtitle("Major UK Exports Targeted For\nUpcoming Trump Tariffs") +
  ylab("US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,10), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "Trump is Preparing to Hit Another $10B in UK Exports With New Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = UPCOMING_TARIFF_TARGET_BAR_CHART_UK, "Upcoming Tariff Target Bar Chart UK Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_COUNTRIES_IMPORTS <- US_COUNTRIES_HS10_IMPORTS_BULK %>%
  filter(CTY_NAME == "UNITED KINGDOM") %>%
  #filter(!CTY_NAME %in% c("CAFTA-DR","CENTRAL AMERICA","AFRICA","TOTAL FOR ALL COUNTRIES", "OECD", "APEC", "NATO","NAFTA","NORTH AMERICA", "TWENTY LATIN AMERICAN REPUBLICS","LAFTA","EUROPE","ASIA","EUROPEAN UNION","PACIFIC RIM COUNTRIES","SOUTH AMERICA","EURO AREA","ASEAN","CACM","AUSTRALIA AND OCEANIA")) %>%
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
  mutate(Category = factor(Category, levels = rev(c("Pharmaceuticals","Critical Minerals","Lumber & Related","Electronics","Chipmaking Machinery","Semiconductors","Copper & Related"))))



UPCOMING_TARIFF_TARGET_BAR_CHART_UK <- ggplot(data = US_TARIFF_CATEGORIES, aes(x = Category, y = Imports/1000000000)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ggtitle("Major UK Exports Targeted For\nUpcoming Trump Tariffs") +
  ylab("US Imports in Category, 2024") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,10), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using Census Data", subtitle = "Trump is Preparing to Hit Another $10B in UK Exports With New Tariffs") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 16, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip()

ggsave(dpi = "retina",plot = UPCOMING_TARIFF_TARGET_BAR_CHART_UK, "Upcoming Tariff Target Bar Chart UK Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


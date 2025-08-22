pacman::p_load(censusapi,ggpubr,prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

Laundry_Equip_Prices <- bls_api("CUSR0000SS30021", startyear = 2013, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(paste0(year,"-",gsub("M","",period),"-01"))) %>%
  arrange(date)

Other_Appliance_Prices <- bls_api("CUSR0000SS30021", startyear = 2006, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(paste0(year,"-",gsub("M","",period),"-01"))) %>%
  arrange(date)

WASHING_MACHINE_PRICES_GRAPH <- ggplot() + #plotting Wage Growth
  geom_line(data=Laundry_Equip_Prices, aes(x=date,y= value/value[49]*100,color= "US Washer/Dryer Prices"), size = 1.25) +
  annotate("vline", x = as.Date("2018-01-01"), xintercept = as.Date("2018-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("vline", x = as.Date("2023-02-01"), xintercept = as.Date("2023-02-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("vline", x = as.Date("2025-02-01"), xintercept = as.Date("2025-02-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Washer\nTariffs\nImposed", x = as.Date("2017-11-01"), hjust = 1, y = 110, color = "white", size = 4, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Washer\nTariffs\nExpire", x = as.Date("2023-04-01"), y = 135, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("text", label = 'Washer &\nDryer\nTariffs\nImposed', x = as.Date("2025-04-01"), y = 133, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,140), breaks = c(70,80,90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Index, Jan 2017 = 100") +
  ggtitle("The Effect of Washing Machine Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS CPI Data",subtitle = "Laundry Machine Prices Spiked as Tariffs Were Implemented and Fell as They Expired") +
  theme_apricitas + theme(legend.position = c(.2,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 70-(.3*70), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WASHING_MACHINE_PRICES_GRAPH, "Washing Machine Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

US_WASHER_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME", "CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "8450",#Washing Machines
  CTY_CODE = "-", #South Korea
)

US_WASHER_IMPORTS <- US_WASHER_IMPORTS_BULK %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO),CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(GEN_VAL_MO,CAL_DUT_MO,date) %>%
  mutate(rollmean = c(rep(NA,11),roll_mean(GEN_VAL_MO,12)), CAL_DUT_MO_rollmean = c(rep(NA,11),roll_mean(CAL_DUT_MO,12)))

US_WASHER_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "8450",
  CTY_CODE = "-", #South Korea
)

US_WASHER_EXPORTS <- US_WASHER_EXPORTS_BULK %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(ALL_VAL_MO,date) %>%
  mutate(rollmean = c(rep(NA,11),roll_mean(ALL_VAL_MO,12)))

US_WASHER_NET_EXPORTS <- merge(US_WASHER_IMPORTS,US_WASHER_EXPORTS, by = "date") %>%
  transmute(date,imports = GEN_VAL_MO, exports = ALL_VAL_MO, duties = CAL_DUT_MO, net_imports = GEN_VAL_MO-ALL_VAL_MO, roll_imports = rollmean.x, roll_exports = rollmean.y, roll_net_imports = rollmean.x-rollmean.y, roll_duties = CAL_DUT_MO_rollmean, tariff = duties/imports, roll_tariff = roll_duties/roll_imports)
  
US_NET_WASHER_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_WASHER_NET_EXPORTS, aes(x=date,y= (net_imports*12)/1000000000,color= "Washing Machine Net Imports"), size = 0.75, alpha = 0.5, linetype = "dashed") + 
  geom_line(data=US_WASHER_NET_EXPORTS, aes(x=date,y= (roll_net_imports*12)/1000000000,color= "Washing Machine Net Imports"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-01-01"), xintercept = as.Date("2018-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("vline", x = as.Date("2023-02-01"), xintercept = as.Date("2023-02-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Washer\nTariffs\nImposed", x = as.Date("2018-03-01"), hjust = 0, y = 2.6, color = "white", size = 4, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Washer\nTariffs\nExpire", x = as.Date("2023-04-01"), y = 2.6, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,3), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("US Washing Machine Imports Amidst Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using Census International Trade data. NOTE: Washing Machines Use HS Code 8450",subtitle = "Tariffs Temporarily Suppressed Washer Imports But Didn't Durably Build Out Industry") +
  theme_apricitas + theme(legend.position = c(.225,.875), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Solid = Rolling 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_NET_WASHER_GRAPH, "US Net Washer Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


US_WASHER_EFFECTIVE_TARIFFS_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_WASHER_NET_EXPORTS, aes(x=date,y= tariff,color= "Effective Tariff Rate"), size = 0.75, alpha = 0.5, linetype = "dashed") + 
  geom_line(data=US_WASHER_NET_EXPORTS, aes(x=date,y= roll_tariff,color= "Effective Tariff Rate"), size = 1.25) + 
  annotate("vline", x = as.Date("2018-01-01"), xintercept = as.Date("2018-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("vline", x = as.Date("2023-02-01"), xintercept = as.Date("2023-02-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Washer\nTariffs\nImposed", x = as.Date("2018-03-01"), hjust = 0, y = 2.6, color = "white", size = 4, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Washer\nTariffs\nExpire", x = as.Date("2023-04-01"), y = 2.6, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.15), breaks = c(0,.05,.1,.15), expand = c(0,0)) +
  ylab("Percent of Gross Imports") +
  ggtitle("US Washer Effective Tariff Rate") +
  labs(caption = "Graph created by @JosephPolitano using Census International Trade data. NOTE: Washing Machines Use HS Code 8450",subtitle = "Tariffs Spiked But Haven't Fallen All the Way Back Down Yet") +
  theme_apricitas + theme(legend.position = c(.225,.875), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Solid = Rolling 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_WASHER_EFFECTIVE_TARIFFS_GRAPH, "US Washer Effective Tariff Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

# beaSets(Sys.getenv("BEA_KEY"))
# 
# beaParams(Sys.getenv("BEA_KEY"),"NIPA")
# 
# test <- beaParamVals(Sys.getenv("BEA_KEY"),"NIPA","TableID")
# 
# test <- test$ParamValue

IMPGOODS_CHINA_ALL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGds',
  'Frequency' = 'QSA',
  'AreaOrCountry' = "China",
  'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

IMPGOODS_CHINA_ALL <- beaGet(IMPGOODS_CHINA_ALL_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q"))) %>%
  select(AreaOrCountry, date, DataValue) %>%
  pivot_wider(names_from = AreaOrCountry, values_from = DataValue)

IMPGOODS_ALLIES_CHINA_ALL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGds',
  'Frequency' = 'A',
  'AreaOrCountry' = "AllCountries,China,Russia,Canada,Mexico,Japan,Australia,NewZealand,SouthKorea,EU,Israel,Brazil,UnitedKingdom,Taiwan,Norway,Iceland,Turkey,Albania,Argentina,Colombia,Morocco,Jordan,Philippines,Thailand,India,HongKong",
  'Year' = paste(seq(from = 2000, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

IMPGOODS_CHINA_ALL <- beaGet(IMPGOODS_ALLIES_CHINA_ALL_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  #mutate(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q"))) %>%
  mutate(AreaOrCountry = case_when(
    AreaOrCountry == "AllCountries" ~ "total",
    AreaOrCountry %in% c("Russia", "China") ~ AreaOrCountry,
    AreaOrCountry == "HongKong" ~ "China",
    TRUE ~ "Close Allies"
  )) %>%
  mutate(date = as.Date(paste0(TimePeriod, "-01-01"))) %>%
  select(AreaOrCountry, date, DataValue) %>%
  group_by(date, AreaOrCountry) %>%
  summarize(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = AreaOrCountry, values_from = DataValue)


IMPGOODS_CHINA_ALLIES_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=IMPGOODS_CHINA_ALL, aes(x=date,y= China/total,color= "China"), size = 1.25) +
  geom_line(data=IMPGOODS_CHINA_ALL, aes(x=date,y= `Close Allies`/total,color= "Close American Allies"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.85), breaks = c(0,.2,.4,.6,.8), expand = c(0,0)) +
  ylab("Share of US Goods Imports, %") +
  ggtitle("Close Allies Make Up Most US Imports") +
  labs(caption = "Graph created by @JosephPolitano using BEA ITA Data\nNOTE: Allies Include NATO, Other Mutual Defense Pacts, Major Non-NATO Allies, EU, Mexico, India, & Taiwan. China Includes HK",subtitle = "The Majority of US Imports Come from Our Close Allies, Not China or Other Geopolitical Rivals") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= "Share of US Goods Imports",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Close American Allies","China")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.85), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IMPGOODS_CHINA_ALLIES_Graph, "Impgoods China Allies Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

IMPGOODS_CHINA_ALL_BAR_BULK <- beaGet(IMPGOODS_ALLIES_CHINA_ALL_SPECS, iTableStyle = FALSE, asWide = FALSE)

IMPGOODS_CHINA_ALL_BAR <- IMPGOODS_CHINA_ALL_BAR_BULK %>%
  #mutate(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q"))) %>%
  mutate(AreaOrCountry = case_when(
    AreaOrCountry %in% c("EU","Canada","Mexico","China","Russia") ~ AreaOrCountry,
    AreaOrCountry == "AllCountries" ~ "Total",
    AreaOrCountry == "HongKong" ~ "China",
    TRUE ~ "Other Close Allies"
  )) %>%
  mutate(date = as.Date(paste0(TimePeriod, "-01-01"))) %>%
  select(AreaOrCountry, date, DataValue) %>%
  group_by(AreaOrCountry, date) %>%
  summarize(across(DataValue, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = AreaOrCountry, values_from = DataValue) %>%
  select(-Russia) %>%
  mutate(across(-date, ~ . / Total)) %>%
  select(-Total) %>%
  pivot_longer(-date) %>%
  mutate(AreaOrCountry_GROUP = case_when(
    name == "China" ~ name,
    TRUE ~ "Close\nAllies"
  )) %>%
  filter(date == max(date)) %>%
  mutate(name = factor(name, levels = rev(c("EU","Mexico","Canada","Other Close Allies","China"))))

IMPGOODS_CHINA_ALL_BAR_Graph <- ggplot(data = IMPGOODS_CHINA_ALL_BAR, aes(x = AreaOrCountry_GROUP, y = value, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ylab("% of Goods Imports") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.8), expand = c(0,0)) +
  ggtitle("US Goods Imports by Source, 2023, %") +
  annotate("text", label = "EU",y = 0.09, x = 2.52, color = "#3083DC", size = 6) +
  annotate("text", label = "Mexico",y = 0.26, x = 2.52, color = "#00A99D", size = 6) +
  annotate("text", label = "Canada",y = 0.41, x = 2.52, color = "#EE6055", size = 6) +
  annotate("text", label = "Other Close Allies",y = 0.595, x = 2.52, color = "#FFE98F", size = 6) +
  scale_fill_manual(name= "Share of US Goods Imports",values = c("#A7ACD9","#FFE98F","#EE6055","#00A99D","#3083DC","#9A348E")) +
  labs(caption = "Graph created by @JosephPolitano using BEA ITA Data\nNOTE: Other Allies Include NATO, Other Mutual Defense Pacts, Major Non-NATO Allies, India, & Taiwan. China Includes HK",subtitle = "The Majority of US Imports Come from Our Close Allies, Not China or Other Geopolitical Rivals") +
  theme_apricitas + theme(legend.position = "none", axis.text.y = element_text(size = 20,color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = IMPGOODS_CHINA_ALL_BAR_Graph, "Impgoods China All Bar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

write.csv(IMPGOODS_CHINA_ALL_BAR %>% select(-date,AreaOrCountry_GROUP),"US_GOODS_IMPORTS_BY_SOURCE_COUNTRY.csv")

#DO US AVERAGE WEIGHTED TARIFF AND AVERAGE WEIGHTED TARIFF ON US EXPORTS

IMPGOODS_ALL_1960_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGds',
  'Frequency' = 'A',
  'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

IMPGOODS_ALL_1960 <- beaGet(IMPGOODS_ALL_1960_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  filter(AreaOrCountry == "AllCountries") %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,imports = DataValue)

CUSTOMS_DUTIES_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'T30200',
  'Frequency' = 'A',
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

CUSTOM_DUTIES <- beaGet(CUSTOMS_DUTIES_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  filter(LineDescription == "Customs duties") %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,Tariffs = DataValue)

WEIGHTED_TARIFFS <- merge(CUSTOM_DUTIES,IMPGOODS_ALL_1960, by = "date") %>%
  mutate(weighted_tariffs = Tariffs/imports)

WEIGHTED_TARIFFS_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=WEIGHTED_TARIFFS, aes(x=date,y= weighted_tariffs,color= "US Average Effective Tariff Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.25), breaks = c(0,.05,.1,.15,.20,.25), expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2025-01-01"), xmax = as.Date("2030-01-01"), ymin = 0.1, ymax = 0.2, fill = "#EE6055", color = NA, alpha = 0.6) +
  annotate("text", label = "Trump\nUniversal\nTariff\nProposal\nRange", x = as.Date("2016-01-01"), y = 0.16, color = "#EE6055", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.8) +
  #annotate("text", label = "China\nTariffs\nCould\nDrive\nThis\nEven\nHigher", x = as.Date("2031-01-01"), y = 0.15, color = "#EE6055", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.8) +
  annotate("text", label = "China\nTariffs\nCould\nDrive\nThis\nEven\nHigher", x = as.Date("2025-01-01"), y = 0.05, color = "#EE6055", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.8) +
  annotate(geom = "segment", x = as.Date("2023-01-01"), xend = as.Date("2025-01-01"), y = WEIGHTED_TARIFFS$Tariffs[nrow(WEIGHTED_TARIFFS)]/WEIGHTED_TARIFFS$imports[nrow(WEIGHTED_TARIFFS)], yend = .20, color = "#EE6055",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2023-01-01"), xend = as.Date("2025-01-01"), y = WEIGHTED_TARIFFS$Tariffs[nrow(WEIGHTED_TARIFFS)]/WEIGHTED_TARIFFS$imports[nrow(WEIGHTED_TARIFFS)], yend = .10, color = "#EE6055",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate(geom = "segment", x = as.Date("2024-12-01"), xend = as.Date("2024-12-01"), y = 0, yend = 115, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate("text", label = "K Line\nLAX\nExtension", x = as.Date("2025-02-01"), y = 105, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  ylab("Tariff Rate, %") +
  ggtitle("Trump is Proposing Massive Tariff Increases") +
  labs(caption = "Graph created by @JosephPolitano using BEA ITA and NIPA Data",subtitle = "Trump's Plans Would Increase US Effective Tariff Rates to Rates Not Seen in the Post-War Era") +
  theme_apricitas + theme(legend.position = c(.3,.88), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*(today()-as.Date("1960-01-01"))), xmax = as.Date("1960-01-01")-(0.049*(today()-as.Date("1960-01-01"))), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WEIGHTED_TARIFFS_Graph, "Weighted Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

WEIGHTED_TARIFFS <- merge(CUSTOM_DUTIES,IMPGOODS_ALL_1960, by = "date") %>%
  mutate(weighted_tariffs = Tariffs/imports)

write.csv(WEIGHTED_TARIFFS, "US_AVERAGE_EFFECTIVE_TARIFF_RATE.csv")

TARIFF_RATE_BY_COUNTRY_BULK <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Universal%20Tariffs/TARIFF_RATE_BY_COUNTRY.csv") %>%
  pivot_longer(-`Country.Name`:-`Country.Code`) %>%
  mutate(name = as.numeric(gsub("X","",name))) %>%
  group_by(`Country.Code`) %>%
  drop_na() %>%
  filter(name == max(name)) %>%
  mutate(category = case_when(
    value < 10 ~ "<10%",
    value >= 10 ~ ">10%",
  )) %>%
  mutate(Country.Name = case_when(
    Country.Name == "United States" ~ "USA",
    Country.Name == "United Kingdom" ~ "UK",
    Country.Name == "Russian Federation" ~ "Russia",
    Country.Name == "Venezuela, RB" ~ "Venezuela",
    Country.Name == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country.Name == "Congo, Rep." ~ "Republic of Congo",
    Country.Name == "Viet Nam" ~ "Vietnam",
    Country.Name == "Greenland" ~ "Denmark",
    Country.Name == "Cote d'Ivoire" ~ "Ivory Coast",
    Country.Name == "Turkiye" ~ "Turkey",
    Country.Name == "Korea, Rep." ~ "South Korea",
    Country.Name == "Korea, Dem. People's Rep." ~ "South Korea",
    Country.Name == "Syrian Arab Republic" ~ "Syria",
    Country.Name == "West Bank and Gaza" ~ "Palestine",
    Country.Name == "Slovak Republic" ~ "Slovakia",
    Country.Name == "Czechia" ~ "Czech Republic",
    Country.Name == "Brunei Darussalam" ~ "Brunei",
    Country.Name == "Lao PDR" ~ "Laos",
    Country.Name == "Eswatini" ~ "Swaziland",
    Country.Name == "Kyrgyz Republic" ~ "Kyrgyzstan",
    TRUE ~ Country.Name
  )) %>%
  mutate(Country.Name = gsub(",.*", "", Country.Name)) %>%
  ungroup() %>%
  add_row(Country.Name = "Greenland",Country.Code = "GRN",name = 2021,value = 0,category = "<10%") %>%
  add_row(Country.Name = "Western Sahara",Country.Code = "GRN",name = 2021,value = 0,category = "<10%") %>%
  add_row(Country.Name = "Puerto Rico",Country.Code = "GRN",name = 2021,value = 0,category = "<10%") %>%
  add_row(Country.Name = "French Guiana",Country.Code = "GRN",name = 2021,value = 0,category = "<10%") %>%
  add_row(Country.Name = "Falkland Islands",Country.Code = "GRN",name = 2021,value = 0,category = "<10%")

  
TARIFF_MAP <- WORLD_MAP %>%
  full_join(TARIFF_RATE_BY_COUNTRY_BULK,by = c("region" = "Country.Name")) %>%
  filter(long < 170 & long > -170) %>%
  filter(region != "Antarctica")


TARIFF_MAP_Graph <- TARIFF_MAP %>% 
  ggplot(aes(fill = category, map_id = region)) +
  geom_map(map = test1, linewidth = 1) +
  expand_limits(x = WORLD_MAP$long, y = WORLD_MAP$lat) +
  coord_map("mercator") +
  theme_apricitas + 
  ggtitle(paste("Countries by Average Effective Tariff Rate")) +
  scale_y_continuous(limits = c(-50,75)) +
  scale_x_continuous(limits = c(-180,180)) +
  labs(caption = "Graph created by @JosephPolitano using World Bank WITS data. As of 2021 or Latest Year", subtitle = "Trump's Tariff Plans Would Be Extremely Aggressive Compared to Most World Economies") +
  labs(fill = NULL) +
  scale_fill_manual(name= "Tariff Rate",values = c("#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("<10%",">10%")) + 
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0,0.1,0,0.5), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 26))

ggsave(dpi = "retina",plot = TARIFF_MAP_Graph, "Tariff Map Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#From: https://www.ers.usda.gov/topics/international-markets-u-s-trade/u-s-agricultural-trade/data/
FOOD_IMPORT_CONSUMPTION_SHARE <- data.frame(category = factor(c("Fruits\n& Nuts","Sweeteners","Vegetables\n& Melons","Grains","Oilseed","Livestock"), levels = rev(c("Fruits\n& Nuts","Sweeteners","Vegetables\n& Melons","Grains","Oilseed","Livestock"))),
                                            value = c(.593,.45,.404,.229,.046,.028))

FOOD_IMPORT_CONSUMPTION_SHARE_Graph <- ggplot(data = FOOD_IMPORT_CONSUMPTION_SHARE, aes(x = category, y = value, fill = "x")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ylab("% of US Consumption") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.7), expand = c(0,0)) +
  ggtitle("US Import Share of Raw Food Consumption, 2022") +
  scale_fill_manual(name= "Share of US Goods Imports",values = c("#FFE98F")) +
  labs(caption = "Graph created by @JosephPolitano using USDA Import Value Share of Consumption Data",subtitle = "Much of the Basic Foods Americans Eat Must be Imported, Especially Fruits & Vegetables") +
  theme_apricitas + theme(plot.title = element_text(size = 23), legend.position = "none", axis.text.y = element_text(size = 17,color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = FOOD_IMPORT_CONSUMPTION_SHARE_Graph, "Food Import Consumption Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

# beaSets(Sys.getenv("BEA_KEY"))
# # 
# beaParams(Sys.getenv("BEA_KEY"),"ITA")
# # 
# test <- beaParamVals(Sys.getenv("BEA_KEY"),"ITA","Indicator")
# # 
# test <- test$ParamValue

ImpGds_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGds',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGds_CATEGORY <- beaGet(ImpGds_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGds = DataValue)

ImpGdsFoodsFeedsAndBevs_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsFoodsFeedsAndBevs',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsFoodsFeedsAndBevs_CATEGORY <- beaGet(ImpGdsFoodsFeedsAndBevs_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGdsAgFoodsFeedsAndBevs = DataValue)


ImpGdsIsm_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsIsm',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsIsm_CATEGORY <- beaGet(ImpGdsIsm_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGdsIsm = DataValue)


ImpGdsCapGoodsExclAuto_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsCapGoodsExclAuto',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsCapGoodsExclAuto_CATEGORY <- beaGet(ImpGdsCapGoodsExclAuto_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGdsCapGoodsExclAuto = DataValue)


ImpGdsAutoVehPartsAndEngines_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsAutoVehPartsAndEngines',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsAutoVehPartsAndEngines_CATEGORY <- beaGet(ImpGdsAutoVehPartsAndEngines_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGdsAutoVehPartsAndEngines = DataValue)


ImpGdsConsGoodsExcFoodAndAuto_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsConsGoodsExcFoodAndAuto',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsConsGoodsExcFoodAndAuto_CATEGORY <- beaGet(ImpGdsConsGoodsExcFoodAndAuto_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGdsConsGoodsExcFoodAndAuto = DataValue)

Imp_Goods_Category_All <- reduce(list(ImpGdsFoodsFeedsAndBevs_CATEGORY,ImpGdsIsm_CATEGORY,ImpGdsCapGoodsExclAuto_CATEGORY,ImpGdsAutoVehPartsAndEngines_CATEGORY,ImpGdsConsGoodsExcFoodAndAuto_CATEGORY,ImpGds_CATEGORY), full_join, by = "date") %>%
  mutate_if(is.numeric, ~ ./ImpGds) %>%
  drop_na()

BREAKING_DOWN_US_TARIFFS_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=Imp_Goods_Category_All, aes(x=date,y= ImpGdsAgFoodsFeedsAndBevs ,color= "Foods, Feeds, & Beverages"), size = 1.25) + 
  geom_line(data=Imp_Goods_Category_All, aes(x=date,y= ImpGdsIsm ,color= "Industrial Supplies & Materials incl. Oil & Energy"), size = 1.25) + 
  geom_line(data=Imp_Goods_Category_All, aes(x=date,y= ImpGdsCapGoodsExclAuto, color= "Capital Goods Ex Auto"), size = 1.25) + 
  geom_line(data=Imp_Goods_Category_All, aes(x=date,y= ImpGdsAutoVehPartsAndEngines,color= "Motor Vehicles, Parts, & Engines"), size = 1.25) + 
  geom_line(data=Imp_Goods_Category_All, aes(x=date,y= ImpGdsConsGoodsExcFoodAndAuto,color= "Consumer Goods Ex Food & Auto"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(.10,.20,.30,.40,.50), expand = c(0,0)) +
  ylab("Percent of Gross Imports") +
  ggtitle("Breaking Down US Imports") +
  labs(caption = "Graph created by @JosephPolitano using BEA ITA data",subtitle = "Capital Goods, Industrial Supplies, Energy, and Food Make Up Large Shares of US Imports") +
  theme_apricitas + theme(legend.position = c(.29,.85), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.55, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13.5)), legend.title=element_text(size=14.5)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "% of US Gross Imports",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Capital Goods Ex Auto","Consumer Goods Ex Food & Auto","Industrial Supplies & Materials incl. Oil & Energy","Motor Vehicles, Parts, & Engines","Foods, Feeds, & Beverages")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BREAKING_DOWN_US_TARIFFS_GRAPH, "Breaking Down US Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

write.csv(Imp_Goods_Category_All %>% select(-ImpGds) %>% setNames(c("date","Foods, Feeds, & Beverages","Industrial Supplies & Materials incl. Oil & Energy","Capital Goods Ex Auto","Motor Vehicles, Parts, & Engines","Consumer Goods Ex Food & Auto")),"BREAKING_DOWN_US_IMPORTS.csv")

ImpGds_CATEGORY <- beaGet(ImpGds_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,ImpGds = DataValue)

ImpGdsFoodsFeedsAndBevs_CATEGORY <- beaGet(ImpGdsFoodsFeedsAndBevs_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,`Foods, Feeds, & Beverages` = DataValue)

ImpGdsEnergyProds_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsEnergyProds',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsEnergyProds_CATEGORY <- beaGet(ImpGdsEnergyProds_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,`Energy Products incl. Oil` = DataValue)

ImpGdsMetalsAndNonmetProds_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsMetalsAndNonmetProds',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsMetalsAndNonmetProds_CATEGORY <- beaGet(ImpGdsMetalsAndNonmetProds_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,`Metals, Gems, & Non-metallic Products` = DataValue)

ImpGdsGemDiamAndOthGem_CATEGORY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'ITA',
  'Indicator' = 'ImpGdsGemDiamAndOthGem',
  'Frequency' = 'A',
  #'AreaOrCountry' = "AllCountries",
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

ImpGdsGemDiamAndOthGem_CATEGORY <- beaGet(ImpGdsGemDiamAndOthGem_CATEGORY_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  mutate(date = as.Date(paste0(TimePeriod,"-01-01"))) %>%
  transmute(date,`Gems` = DataValue)

Imp_Goods_Essential_All <- reduce(list(ImpGdsFoodsFeedsAndBevs_CATEGORY,ImpGdsEnergyProds_CATEGORY,ImpGdsMetalsAndNonmetProds_CATEGORY,ImpGdsGemDiamAndOthGem_CATEGORY,ImpGds_CATEGORY), full_join, by = "date") %>%
  mutate_if(is.numeric, ~ ./ImpGds) %>%
  mutate(`Metals, Gems, & Non-Metallic Mineral Products` = `Metals, Gems, & Non-metallic Products` + Gems) %>%
  select(-Gems,-ImpGds,-`Metals, Gems, & Non-metallic Products`) %>%
  drop_na() %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Foods, Feeds, & Beverages","Metals, Gems, & Non-Metallic Mineral Products","Energy Products incl. Oil"))))

ESSENTIAL_IMPORTS_GRAPH <- ggplot(data = Imp_Goods_Essential_All, aes(x = date, y = value, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ylab("% of Gross Imports") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.52), expand = c(0,0)) +
  ggtitle("Goods With Clear Trade Advantages\nRepresent a Notable Share of US Imports") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_fill_manual(name= "Share of Total US Imports",values = c("#00A99D","#FFE98F","#EE6055"), breaks = c("Foods, Feeds, & Beverages","Metals, Gems, & Non-Metallic Mineral Products","Energy Products incl. Oil")) +
  labs(caption = "Graph created by @JosephPolitano using BEA ITA data",subtitle = "Foods, Minerals, & Energy are Just Some Areas Where the US Obviously Benefit From Trade") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1999-01-01")-(.1861*(today()-as.Date("1999-01-01"))), xmax = as.Date("1999-01-01")-(0.049*(today()-as.Date("1999-01-01"))), ymin = 0-(.3*.40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ESSENTIAL_IMPORTS_GRAPH, "Essential Imports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#From Here:
#https://www.bls.gov/cex/tables/calendar-year/mean-item-share-average-standard-error.htm
#Durables include Appliances & Furnishing, Vehicles
#Nondurables include housekeeping, apparel, drugs, toys, and gasoline


CEX_DATA <- data.frame(category = c("Groceries, Alcohol, & Tobacco","Durable Goods","Nondurable Goods incl. Gasoline"),
                       `Lowest Income` = c(0.266031785,0.158122565,0.199740276),
                       `Second Quintile` = c(0.132173014,0.096575663,0.122695158),
                       `Third Quintile` = c(0.099795814,0.103158875,0.095817194),
                       `Fourth Quintile` = c(0.077324764,0.092655821,0.077869911),
                       `Highest Income` = c(0.051596365,0.081362951,0.053112651)) %>%
  pivot_longer(-category) %>%
  mutate(name = case_when(
    name == "Lowest.Income" ~ "Lowest\nIncome",
    name == "Second.Quintile" ~ "Second\nQuintile",
    name == "Third.Quintile" ~ "Third\nQuintile",
    name == "Fourth.Quintile" ~ "Fourth\nQuintile",
    name == "Highest.Income" ~ "Highest\nIncome",
  )) %>%
  mutate(name = factor(name, levels = c("Lowest\nIncome","Second\nQuintile","Third\nQuintile","Fourth\nQuintile","Highest\nIncome"))) %>%
  mutate(category = factor(category, levels = rev(c("Groceries, Alcohol, & Tobacco","Nondurable Goods incl. Gasoline","Durable Goods"))))

CEX_GOOD_SHARE_OF_INCOME_Graph <- ggplot(data = CEX_DATA, aes(x = name, y = value, fill = category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ylab("% of Household Income") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.7), expand = c(0,0)) +
  ggtitle("Share of Income Spent on Goods\nby Income Quintile") +
  scale_fill_manual(name= "Share of Income Spent on:",values = c("#00A99D","#FFE98F","#EE6055"), breaks = c("Groceries, Alcohol, & Tobacco","Nondurable Goods incl. Gasoline","Durable Goods")) +
  labs(caption = "Graph created by @JosephPolitano using BLS CEX data. NOTE: Quintiles by Consumer Unit. Post-tax Income Used",subtitle = "Lower-Income Households Spend a Higher Share of Their Income on Goods, Including Imports") +
  theme_apricitas + theme(legend.position = c(.75,.75), axis.text.y = element_text(size = 17,color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = CEX_GOOD_SHARE_OF_INCOME_Graph, "CEX Good Share of Income Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

write.csv(CEX_DATA, "QUINTILES_DETAILED_DATA.csv")

#From here:
#https://wits.worldbank.org/CountryProfile/en/Country/WLD/StartYear/1988/EndYear/2021/TradeFlow/Import/Partner/USA/Indicator/AHS-WGHTD-AVRG

RETALIATORY_TARIFF_DATA <- data.frame(date = seq.Date(from = as.Date("1988-01-01"), to = as.Date("2021-01-01"), by = "year"),
                                      value = c(8.988779497,16.04384867,15.0944032,14.69505802,12.42818522,12.40003951,13.44475315,11.43899071,10.5451219,10.52880602,9.565232418,9.706917151,8.073624152,8.071110305,7.595309054,7.638999412,6.300361418,6.317273515,6.249091401,5.786691988,5.667126793,5.471869408,5.863571181,5.949380489,5.673534532,5.921233367,5.481778669,5.746684392,5.793127431,5.212805453,5.423054069,5.274912223,4.963847821,5.059119193
))

RETALIATORY_TARIFF_DATA_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=RETALIATORY_TARIFF_DATA, aes(x=date,y= value/100,color= "Average Effective Tariff Rate\nForeign Countries Place on US Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.30), breaks = c(0,.05,.1,.15,.20,.25,.30), expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2025-01-01"), xmax = as.Date("2030-01-01"), ymin = .10+RETALIATORY_TARIFF_DATA$value[nrow(RETALIATORY_TARIFF_DATA)]/100, ymax = .20+RETALIATORY_TARIFF_DATA$value[nrow(RETALIATORY_TARIFF_DATA)]/100, fill = "#EE6055", color = NA, alpha = 0.6) +
  annotate("text", label = "One-to-One\nRetaliation\nto Trump\nUniversal\nTariff\nProposals", x = as.Date("2017-01-01"), y = 0.2, color = "#EE6055", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.8) +
  #annotate("text", label = "China\nTariffs\nCould\nDrive\nThis\nEven\nHigher", x = as.Date("2031-01-01"), y = 0.15, color = "#EE6055", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.8) +
  #annotate("text", label = "China\nTariffs\nCould\nDrive\nThis\nEven\nHigher", x = as.Date("2025-01-01"), y = 0.05, color = "#EE6055", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.8) +
  annotate(geom = "segment", x = as.Date("2021-01-01"), xend = as.Date("2025-01-01"), y = RETALIATORY_TARIFF_DATA$value[nrow(RETALIATORY_TARIFF_DATA)]/100, yend = .20+RETALIATORY_TARIFF_DATA$value[nrow(RETALIATORY_TARIFF_DATA)]/100, color = "#EE6055",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2021-01-01"), xend = as.Date("2025-01-01"), y = RETALIATORY_TARIFF_DATA$value[nrow(RETALIATORY_TARIFF_DATA)]/100, yend = .10+RETALIATORY_TARIFF_DATA$value[nrow(RETALIATORY_TARIFF_DATA)]/100, color = "#EE6055",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate(geom = "segment", x = as.Date("2024-12-01"), xend = as.Date("2024-12-01"), y = 0, yend = 115, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate("text", label = "K Line\nLAX\nExtension", x = as.Date("2025-02-01"), y = 105, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  ylab("Tariff Rate, %") +
  ggtitle("Retaliation to Trump Tariffs Would be Drastic") +
  labs(caption = "Graph created by @JosephPolitano using World Bank WITS Data",subtitle = "Retaliation to Trump's Univeral Tariffs Could Drive Tariffs Faced by US Exporters Much Higher") +
  theme_apricitas + theme(legend.position = c(.3,.88), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1988-01-01")-(.1861*(today()+1500-as.Date("1988-01-01"))), xmax = as.Date("1988-01-01")-(0.049*(today()+1500-as.Date("1988-01-01"))), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETALIATORY_TARIFF_DATA_Graph, "Retaliatory Tariffs Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


US_TOTAL_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "CON_VAL_MO", "I_COMMODITY", "CTY_CODE", "CTY_NAME", "CAL_DUT_MO"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "-",#ALL COuntries
  CTY_CODE = "-", #TOTAL
)

US_TOTAL_IMPORTS <- US_TOTAL_IMPORTS_BULK %>%
  mutate(CON_VAL_MO = as.numeric(CON_VAL_MO),CAL_DUT_MO = as.numeric(CAL_DUT_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(CON_VAL_MO,CAL_DUT_MO,date) %>%
  mutate(rollmean = c(rep(NA,11),roll_mean(CON_VAL_MO,12)), CAL_DUT_MO_rollmean = c(rep(NA,11),roll_mean(CAL_DUT_MO,12)))

US_TOTAL_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE", "CTY_NAME"), 
  #DF = 1, #excluding reexport
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "-",
  CTY_CODE = "-", #All Countries
)

US_TOTAL_EXPORTS <- US_TOTAL_EXPORTS_BULK %>%
  mutate(ALL_VAL_MO = as.numeric(ALL_VAL_MO)) %>%
  mutate(date = as.Date(paste0(time,"-01"))) %>%
  select(ALL_VAL_MO,date) %>%
  mutate(rollmean = c(rep(NA,11),roll_mean(ALL_VAL_MO,12)))

US_TOTAL_NET_EXPORTS <- merge(US_TOTAL_IMPORTS,US_TOTAL_EXPORTS, by = "date") %>%
  transmute(date,imports = CON_VAL_MO, exports = ALL_VAL_MO, duties = CAL_DUT_MO, net_imports = CON_VAL_MO-ALL_VAL_MO, roll_imports = rollmean.x, roll_exports = rollmean.y, roll_net_imports = rollmean.x-rollmean.y, roll_duties = CAL_DUT_MO_rollmean, tariff = duties/imports, roll_tariff = roll_duties/roll_imports)

US_TOTAL_EFFECTIVE_TARIFFS_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=US_TOTAL_NET_EXPORTS, aes(x=date,y= tariff,color= "Effective Tariff Rate\n(Tariffs Paid as a Share of All Imports)"), size = 0.75, alpha = 0.5, linetype = "dashed") + 
  geom_line(data=US_TOTAL_NET_EXPORTS, aes(x=date,y= roll_tariff,color= "Effective Tariff Rate\n(Tariffs Paid as a Share of All Imports)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.04), breaks = c(0,.01,.02,.03,.04), expand = c(0,0)) +
  ylab("Percent of Gross Imports") +
  ggtitle("US Overall Effective Tariff Rate") +
  labs(caption = "Graph created by @JosephPolitano using Census International Trade data",subtitle = "Tariff Rates Spiked During the Trump Administration and Declined Slightly in the Biden Administration") +
  theme_apricitas + theme(legend.position = c(.28,.875), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Solid = Rolling 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*.04), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_TOTAL_EFFECTIVE_TARIFFS_GRAPH, "US Total Effective Tariff Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
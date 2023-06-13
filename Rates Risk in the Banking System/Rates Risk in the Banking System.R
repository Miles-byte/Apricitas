pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

# GSIB FDIC CERTS
# JP Morgan 628
# Bank of America 3510
# Citi 7213
# HSBC 57890
# Barclays? 57203
# Deutsche Bank 623
# Goldman 33124
# MUFG Union 22826
# BNY Mellon 639
# Morgan Stanley 32992—34221
# State Street 14
# Wells Fargo 3511
# TD Bank 18409
# RBC 26342 17281
# UBS 57565
# Santander 29950
# BNP Paribas 3514
# Mizuho 21843
# Sumitomo Mitsui FG 18618

#NOTE MUST STICH TO REMOVE BANK OF THE WEST AFTER SALE FROM BNP PARIBAS TO BMO

GSIB_INSURED <- read.csv("https://banks.data.fdic.gov/api/financials?filters=CERT%3A%28628%20OR%203510%20OR%207213%20OR%2057890%20OR%2057203%20OR%20623%20OR%2033124%20OR%2022826%20OR%20639%20OR%2032992%20OR%2034221%20OR%2014%20OR%203511%20OR%2018409%20OR%2026342%20OR%2017281%20OR%2057565%20OR%2029950%20OR%203514%20OR%2021843%20OR%2018618%29%20%20AND%20RISDATE%3A%5B20171231%20TO%20%2A%5D&fields=NAME%2CCERT%2CDEP%2CDEPINS%2CRISDATE&limit=10000&agg_by=RISDATE&agg_term_fields=CERT&agg_sum_fields=DEP%2CDEPINS&agg_limit=10000&format=csv&download=true&filename=data_file") %>%
  select(sum_DEP, sum_DEPINS) %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "3 months", length.out = nrow(.))) %>%
  transmute(`Uninsured Deposits in Global Systemically-Important Banks` = sum_DEP-sum_DEPINS, DEPINS_GSIB = sum_DEPINS, date = date)

NONGSIB_INSURED <- read.csv("https://banks.data.fdic.gov/api/financials?filters=%21CERT%3A%28628%20OR%203510%20OR%207213%20OR%2057890%20OR%2057203%20OR%20623%20OR%2033124%20OR%2022826%20OR%20639%20OR%2032992%20OR%2034221%20OR%2014%20OR%203511%20OR%2018409%20OR%2026342%20OR%2017281%20OR%2057565%20OR%2029950%20OR%203514%20OR%2021843%20OR%2018618%29%20%20AND%20RISDATE%3A%5B20171231%20TO%20%2A%5D&fields=NAME%2CCERT%2CDEP%2CDEPINS%2CRISDATE&limit=10000&agg_by=RISDATE&agg_term_fields=CERT&agg_sum_fields=DEP%2CDEPINS&agg_limit=10000&format=csv&download=true&filename=data_file") %>%
  select(sum_DEP, sum_DEPINS) %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "3 months", length.out = nrow(.))) %>%
  transmute(`Uninsured Deposits Outside Global Systemically-Important Banks` = sum_DEP-sum_DEPINS, DEPINS_NONGSIB = sum_DEPINS, date = date)

GSIB_NONGSIB_INSURED <- merge(GSIB_INSURED,NONGSIB_INSURED, by = "date") %>%
  mutate(`FDIC Insured Deposits` = DEPINS_GSIB+DEPINS_NONGSIB) %>%
  select(-DEPINS_GSIB,-DEPINS_NONGSIB) %>%
  pivot_longer(cols = `Uninsured Deposits in Global Systemically-Important Banks`:`FDIC Insured Deposits`) %>%
  mutate(name = factor(name,levels = c("Uninsured Deposits Outside Global Systemically-Important Banks","Uninsured Deposits in Global Systemically-Important Banks","FDIC Insured Deposits")))

GSIB_NONGSIB_INSURED_graph <- ggplot(data = GSIB_NONGSIB_INSURED, aes(x = date, y = value/1000000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10,15,20,25), limits = c(0,27.5), expand = c(0,0)) +
  ggtitle("The Risk of Uninsured Deposits") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Most Uninsured Deposits are Held at 'Too Big To Fail' GSIBs with Implicit State Backing") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits in Global Systemically-Important Banks","Uninsured Deposits Outside Global Systemically-Important Banks")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*27.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GSIB_NONGSIB_INSURED_graph, "GSIB_NONGSIB.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

FAILURES <- read.csv("https://www.fdic.gov/bank/historical/bank/bfb-data.csv") %>%
  `colnames<-`(c("name","pr","date","asset","deposit","note")) %>%
  select(date,asset,deposit) %>%
  mutate(date = as.Date(as.yearmon(date, "%d-%b-%y"))) %>%
  mutate(asset = gsub("\\$","",asset)) %>%
  mutate(asset = as.numeric(gsub(",","", asset))) %>%
  mutate(deposit = gsub("\\$","", deposit)) %>%
  mutate(deposit = as.numeric(gsub(",","",deposit))) %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  group_by(date) %>%
  mutate(deposit = SUM(deposit)) %>%
  mutate(asset = SUM(asset)) %>%
  unique() %>%
  ungroup() %>%
  add_row(date = as.Date("2022-01-01"), asset = 0, deposit = 0) %>%
  add_row(date = as.Date("2021-01-01"), asset = 0, deposit = 0) %>%
  add_row(date = as.Date("2006-01-01"), asset = 0, deposit = 0) %>%
  add_row(date = as.Date("2005-01-01"), asset = 0, deposit = 0)
  
TOTAL_ASSETS <- fredr("QBPBSTAS", observation_start = as.Date("2005-01-01")) %>%
  mutate(date = floor_date(date + 97, "month")) %>%
  transmute(date, total_assets = value)

FAILURES_TOTAL_ASSETS <- merge(FAILURES, TOTAL_ASSETS, by = "date")
  
FAILURES_TOTAL_ASSETS_graph <- ggplot(data = FAILURES_TOTAL_ASSETS, aes(x = date, y = (asset/total_assets), fill = "Failed Banks, Share of All US Banks' Assets")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets of US Commercial Banks") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03), limits = c(0,0.035), expand = c(0,0)) +
  ggtitle("The Banking Crisis") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "The Total Size of Bank Failures So Far in 2023 Have Been the Largest in Several Years") +
  theme_apricitas + theme(legend.position = c(.6,.87)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*.035), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = FAILURES_TOTAL_ASSETS_graph, "Failures as a Share of Total Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

ASSTLTR_TOTAL_BANK <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RISDATE%3A%5B20171231%20TO%20%2A%5D&fields=RISDATE%2CASSET%2CASSTLT&sort_order=DESC&limit=10000&offset=0&agg_by=RISDATE&agg_sum_fields=ASSTLT%2CASSET&format=csv&download=true&filename=data_file") %>%
  mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "3 months", length.out = nrow(.))) %>%
  transmute(`All Other Assets` = sum_ASSET,`Long-Term Assets (5+ Years)` = sum_ASSTLT, date) %>%
  pivot_longer(cols = `All Other Assets`:`Long-Term Assets (5+ Years)`)

ASSTLTR_TOTAL_BANK_graph <- ggplot(data = ASSTLTR_TOTAL_BANK, aes(x = date, y = value/1000000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10,15,20,25,30), limits = c(0,32.5), expand = c(0,0)) +
  ggtitle("US Banks' Long-Term Exposure") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Banks Proportional Exposure to Long-term Assets Did Not Increase Much During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.25,.92)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Long-Term Assets (5+ Years)","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*32.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ASSTLTR_TOTAL_BANK_graph, "Long Term Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

KeyBank <- tq_get("KEY", from = "2023-03-01")
Comerica <- tq_get("CMA", from = "2023-03-01")
PacWest <- tq_get("PACW", from = "2023-03-01")
FirstHorizon <- tq_get("FHN", from = "2023-03-01")
WesternAlliance <- tq_get("WAL", from = "2023-03-01")
ZionBank <- tq_get("ZION", from = "2023-03-01")

AT_RISK_BANKS_graph <- ggplot() +#plotting loan performance data
  geom_line(data=KeyBank, aes(x=date, y= (adjusted-adjusted[1])/adjusted[1],color= "KeyBank"), size = 1.25) +
  geom_line(data=Comerica, aes(x=date, y= (adjusted-adjusted[1])/adjusted[1],color= "Comerica"), size = 1.25) +
  geom_line(data=PacWest, aes(x=date, y= (adjusted-adjusted[1])/adjusted[1],color= "Pacific Western"), size = 1.25) +
  geom_line(data=FirstHorizon, aes(x=date, y= (adjusted-adjusted[1])/adjusted[1],color= "First Horizon"), size = 1.25) +
  geom_line(data=WesternAlliance, aes(x=date, y= (adjusted-adjusted[1])/adjusted[1],color= "Western Alliance"), size = 1.25) +
  geom_line(data=ZionBank, aes(x=date, y= (adjusted-adjusted[1])/adjusted[1],color= "Zions Bank"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Returns Since March 1st") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,-.25,-.50,-.75,-1), limits = c(-1,0.05), expand = c(0,0)) +
  ggtitle("Stock Returns of At-Risk Major Banks") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data with Assistance from @Paulgp", subtitle = "US Regional Bank Stocks Have Been Falling Since SVB's Failure in March") +
  theme_apricitas + theme(legend.position = c(.13,.175), legend.spacing.y = unit(0, 'cm'), legend.key.height = unit(0.35, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#3083DC","#9A348E"), breaks = c("Pacific Western", "Western Alliance", "Zions Bank", "First Horizon", "Comerica", "KeyBank")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-03-01")-(.1861*(today()-as.Date("2023-03-01"))), xmax = as.Date("2023-03-01")-(0.049*(today()-as.Date("2023-03-01"))), ymin = -1-(.3*1.05), ymax = -1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AT_RISK_BANKS_graph, "At Risk Banks Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

AT_RISK_MAJOR_BANKS <- read.csv("https://banks.data.fdic.gov/api/financials?filters=CERT%3A%284977%20OR%2024045%20OR%202270%20OR%20983%20OR%2017534%20OR%2057512%29%20AND%20RISDATE%3A%5B20191231%20TO%20%2A%5D&fields=CERT%2CREPDTE%2CASSET%2CDEP%2CDEPINS%2CNAME%2CASSET%2CASSTLT%2CLNRE&limit=10000&format=csv&download=true&filename=data_file") %>%
  mutate(NAME = str_to_title(NAME)) %>%
  mutate(REPDTE = ymd(REPDTE)) %>%
  mutate(NAME = gsub("Zions Bcorp N A","Zions Bank", NAME)) %>%
  mutate(NAME = gsub("Keybank National Assn","Keybank", NAME))


DEP_DEPINS_MAJOR_BANKS <- AT_RISK_MAJOR_BANKS %>%
  select(DEP, DEPINS, NAME, REPDTE) %>%
  # add_row(DEP = 147388149, DEPINS = 75770590, NAME = "Keybank", REPDTE = as.Date("2023-03-31")) %>%
  # add_row(DEP = 69207568, DEPINS = 37845135, NAME = "Zions Bank", REPDTE = as.Date("2023-03-31")) %>%
  # add_row(DEP = 28527246, DEPINS = 20452549, NAME = "Pacific Western Bank", REPDTE = as.Date("2023-03-31")) %>%
  # add_row(DEP = 62813312, DEPINS = 35038477, NAME = "First Horizon Bank", REPDTE = as.Date("2023-03-31")) %>%
  # add_row(DEP = 47852002, DEPINS = 32353242, NAME = "Western Alliance Bank", REPDTE = as.Date("2023-03-31")) %>%
  # add_row(DEP = 66703000, DEPINS = 31696000, NAME = "Comerica Bank", REPDTE = as.Date("2023-03-31")) %>%
  transmute(`Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS, date = REPDTE, bank = NAME) %>%
  pivot_longer(cols = c(`Uninsured Deposits`, `FDIC Insured Deposits`)) %>%
  mutate(name = factor(name,levels = c("Uninsured Deposits","FDIC Insured Deposits"))) %>%
  mutate(bank = gsub("First Horizon Bank","First Horizon", bank)) %>%
  mutate(bank = gsub("Pacific Western Bank","Pacific Western", bank)) %>%
  mutate(bank = gsub("Keybank","KeyBank", bank)) %>%
  mutate(bank = factor(bank,levels = c("KeyBank","Zions Bank","Comerica Bank", "First Horizon", "Western Alliance Bank", "Pacific Western")))
  
DEP_DEPINS_MAJOR_BANKS_graph <- ggplot(data = subset(DEP_DEPINS_MAJOR_BANKS), aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) + facet_grid(~ bank) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150), limits = c(0,160), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("2020-01-01","2023-01-01")), labels = c("2020","2023")) +
  ggtitle("Uninsured Deposits of At-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Besides Pacific Western and Western Alliance, Most At-Risk Banks Haven't Seen Outflows") +
  theme_apricitas + theme(legend.position = c(.7,.825), panel.spacing.x = unit (0.5, "lines"), strip.text.x = element_text(size = 10)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits")) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DEPINS_MAJOR_BANKS_graph, "At Risk Major Banks Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

LONG_TERM_ASSETS_MAJOR_BANKS <- AT_RISK_MAJOR_BANKS %>%
  transmute(NAME, LT_ASSET_RATIO = ASSTLT/ASSET, date = REPDTE) %>%
  pivot_wider(names_from = "NAME", values_from = LT_ASSET_RATIO)

LONG_TERM_ASSETS_MAJOR_BANKS_graph <- ggplot() + #plotting loan performance data
  geom_line(data=LONG_TERM_ASSETS_MAJOR_BANKS, aes(x=date,y= Keybank,color= "KeyBank"), size = 1.25)+ 
  geom_line(data=LONG_TERM_ASSETS_MAJOR_BANKS, aes(x=date,y= `Zions Bank`,color= "Zions Bank"), size = 1.25)+ 
  geom_line(data=LONG_TERM_ASSETS_MAJOR_BANKS, aes(x=date,y= `Pacific Western Bank`,color= "Pacific Western"), size = 1.25)+ 
  geom_line(data=LONG_TERM_ASSETS_MAJOR_BANKS, aes(x=date,y= `First Horizon Bank`,color= "First Horizon"), size = 1.25)+ 
  geom_line(data=LONG_TERM_ASSETS_MAJOR_BANKS, aes(x=date,y= `Western Alliance Bank`,color= "Western Alliance"), size = 1.25)+ 
  geom_line(data=LONG_TERM_ASSETS_MAJOR_BANKS, aes(x=date,y= `Comerica Bank`,color= "Comerica"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60), limits = c(0,.65), expand = c(0,0)) +
  ggtitle("Long-Term Assets at At-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Long Term Assets Make Up a Large Chunk of Assets in At-Risk Banks") +
  theme_apricitas + theme(legend.position = c(.2,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Pacific Western", "Western Alliance", "Zions Bank", "First Horizon", "Comerica", "KeyBank")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = 0-(.3*.65), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LONG_TERM_ASSETS_MAJOR_BANKS_graph, "Long Term Asset Ratio Major Banks Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FED_EMERGENCY_LOANS <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=a66e338ec176dd641c333de890fd7816&lastobs=100&from=&to=&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","Lending to FDIC Bridge Banks (SVB, Signature, First Republic)","Bank Term Funding Program","Discount Window")) %>%
  mutate(date = as.Date(date)) %>%
  subset(date > as.Date("2023-01-01")) %>%
  pivot_longer(cols = `Lending to FDIC Bridge Banks (SVB, Signature, First Republic)`:`Discount Window`) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(name = factor(name,levels = c("Bank Term Funding Program","Lending to FDIC Bridge Banks (SVB, Signature, First Republic)","Discount Window")))

FED_EMERGENCY_LOANS_graph <- ggplot(data = FED_EMERGENCY_LOANS, aes(x = date, y = value/1000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  #annotate("segment", y = 20, yend = 20, x = as.Date("2023-03-11"), xend = as.Date("2023-03-06"), color = "white", size = 1.25) +
  #annotate("segment", y = 109, yend = 109, x = as.Date("2023-03-11"), xend = as.Date("2023-03-06"), color = "white", size = 1.25) +
  #annotate("segment", y = 109, yend = 20, x = as.Date("2023-03-06"), xend = as.Date("2023-03-06"), color = "white", size = 1.25) +
  #annotate("segment", y = 64.5, yend = 64.5, x = as.Date("2023-03-06"), xend = as.Date("2023-03-04"), color = "white", size = 1.25) +
  #annotate("text", label = "First Republic", y = 104.5, x = as.Date("2023-02-22"), color = "white", size = 5) +
  #annotate("text", label = "Discount Window", y = 84.5, x = as.Date("2023-02-22"), color = "white", size = 5) +
  #annotate("text", label = "Borrowing Range", y = 64.5, x = as.Date("2023-02-22"), color = "white", size = 5) +
  #annotate("text", label = "(Based on Company", y = 44.5, x = as.Date("2023-02-22"), color = "white", size = 5) +
  #annotate("text", label = "Press Release)", y = 24.5, x = as.Date("2023-02-22"), color = "white", size = 5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Wednesday Level") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,100,200,300,400,500), limits = c(0,500), expand = c(0,0)) +
  ggtitle("Fed Emergency Lending") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Fed is Lending Billions to Banks After The Recent Banking Crisis") +
  theme_apricitas + theme(legend.position = c(.47,.86)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Selected Federal Reserve Loans",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Discount Window","Lending to FDIC Bridge Banks (SVB, Signature, First Republic)","Bank Term Funding Program")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = 0-(.3*500), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_EMERGENCY_LOANS_graph, "Fed Emergency Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

SECURITIES_LOSSES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Rates%20Risk%20in%20the%20Banking%20System/losses_securities.csv") %>%
  transmute(date, `Available-For-Sale Securities` = SCAF_MINUS_SCAA,`Held-to-Maturity Securities` = SCHF_MINUS_SCHA) %>%
  pivot_longer(cols = c(`Available-For-Sale Securities`,`Held-to-Maturity Securities`)) %>%
  mutate(date = as.Date(date))

SECURITIES_LOSSES_graph <- ggplot(data = SECURITIES_LOSSES, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-750,-500,-250,0), limits = c(-750,150), expand = c(0,0)) +
  ggtitle("US Banks' Securities Losses") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Banks' Have Large Unrealized Losses in the Wake of Recent Rate Hikes") +
  theme_apricitas + theme(legend.position = c(.25,.7)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Unrealized Losses on Banks' Securities",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -750-(.3*900), ymax = -750) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SECURITIES_LOSSES_graph, "Securities Losses Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FDIC_FTS_DATA_123122 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Rates%20Risk%20in%20the%20Banking%20System/FTS2212.CSV") 
CERT_TICKER_XWALK <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Rates%20Risk%20in%20the%20Banking%20System/ticker_cert_xwalk.csv")

#EXCESS RETURN DATA FROM PAULGP AT RISK BANKS FOLDER

EXCESS_RETURNS_5523 <- bank_data %>%
  subset(date == as.Date("2023-05-05")) %>%
  ungroup() %>%
  transmute(TICKER = ticker, CUMUL_ABNORMAL = cumul_abnormal)
  
FDIC_DATA_TICKER <- FDIC_FTS_DATA_123122 %>%
  left_join(CERT_TICKER_XWALK, by = "CERT") %>%
  #select(CERT,TICKER,ASSET,DEPDOM,DEPUNA) %>%
  drop_na(TICKER) %>%
  unique() %>%
  left_join(EXCESS_RETURNS_5523, by = "TICKER") %>%
  mutate(across(-TICKER, ~as.numeric(gsub(",", "", .)))) %>%
  group_by(TICKER) %>%
  mutate(across(.cols = where(is.numeric) & !CUMUL_ABNORMAL & !CERT,~sum(.))) %>%
  ungroup() %>%
  select(-CERT) %>%
  unique() %>%
  filter(!(TICKER %in% c("SI","SIVB","FRC","SBNY","UMPQ")))

numeric_data <- FDIC_DATA_TICKER %>% mutate(across(everything(), as.numeric)) %>% mutate(across(.cols = everything(), .fns = ~ ./ASSET))
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cumulative_returns_correlations <- cor_matrix["CUMUL_ABNORMAL", ]
sorted_correlations <- cumulative_returns_correlations %>%
  sort(decreasing = TRUE, na.last = TRUE) %>%
  data.frame(Correlation = ., Variable = names(.)) %>%
  arrange(desc(abs(Correlation)))

#EXCLUDING THOSE WITH 0 UNINSURED DEPOSITS WHO DON'T REPORT
UNINSURED_DEP_RETURNS_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=subset(FDIC_DATA_TICKER, DEPUNA != 0), aes(x=DEPUNA/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Uninsured Deposit Reliance", size = ASSET))+
  stat_smooth(data=FDIC_DATA_TICKER,method = "lm", aes(x=DEPUNA/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Uninsured Deposit Reliance"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("Uninsured Domestic Deposits, % of Assets") +
  #geom_text_repel(data = FDIC_DATA_TICKER, aes(y = CUMUL_ABNORMAL, x = DEPUNA/DEPDOM, label = TICKER), hjust=0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  ggtitle("The Risk of Uninsured Deposits") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Banks With More Uninsured Deposits Have Seen Lower Returns Since the Start of the Year") +
  theme_apricitas + theme(legend.position = c(.50,.82)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*1), xmax = 0-(0.049*1), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNINSURED_DEP_RETURNS_graph, "Uninsured Deposits Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ASSET_RETURNS_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=FDIC_DATA_TICKER, aes(x=ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Their Size", size = ASSET))+
  stat_smooth(data=FDIC_DATA_TICKER,method = "loess", aes(x=ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Their Size"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  scale_x_log10(limits = c(600000,4000000000), breaks = c(1000000,10000000,100000000,1000000000), labels = c("$1B","$10B","$100B","$1T")) + 
  ggtitle("Mid-Size Banks Have Fallen The Most") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Since the Start of the Year, It's Mid-Size Banks That Have Fallen Most") +
  theme_apricitas + theme(legend.position = c(.50,.82)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*1), xmax = 0-(0.049*1), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ASSET_RETURNS_graph, "Asset Returns Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REAL_ESTATE_RETURNS_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=FDIC_DATA_TICKER, aes(x=(LNRE)/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Real Estate Loan Asset Share", size = ASSET))+
  stat_smooth(data=FDIC_DATA_TICKER,method = "lm", aes(x=(LNRE)/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Real Estate Loan Asset Share"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("Real Estate Loans, % of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  ggtitle("Banks With Real Estate Loans Fell More") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Since the Start of the Year, Bank Stocks With More Real Estate Exposure Fell More") +
  theme_apricitas + theme(legend.position = c(.50,.82)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*1), xmax = 0-(0.049*1), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_ESTATE_RETURNS_graph, "Real Estate Returns Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CET1_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=subset(FDIC_DATA_TICKER, RWAW != 0), aes(x=(RBCT1C)/RWAW,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. CET1 Capital Ratio", size = ASSET))+
  stat_smooth(data=FDIC_DATA_TICKER,method = "lm", aes(x=(RBCT1C)/RWAW,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. CET1 Capital Ratio"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("CET1 Capital Ratio") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.08,0.3)) +
  ggtitle("Banks With Real Estate Loans Fell More") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Since the Start of the Year, Banks Stocks With More Real Estate Exposure Fell More") +
  theme_apricitas + theme(legend.position = c(.50,.82)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = 0.08-(.1861*0.22), xmax = 0.08-(0.049*0.22), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CET1_graph, "CET1 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HTM_AFS_RETURNS_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=FDIC_DATA_TICKER, aes(x=(SCHF-SCHA + SCAF-SCAA)/RBCT1C,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Securities Losses as a Share of CET1 Capital", size = ASSET))+
  stat_smooth(data=FDIC_DATA_TICKER,method = "lm", aes(x=(SCHF-SCHA + SCAF-SCAA)/RBCT1C,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Securities Losses as a Share of CET1 Capital"), size = 1.25) +
  stat_smooth(data=FDIC_DATA_TICKER,method = "lm", aes(x=(SCHF-SCHA)/RBCT1C,y=CUMUL_ABNORMAL, color= "HTM Losses Only as a Share of CET1 Capital"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("Total Losses on Securities, Share of CET1 Capital") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  #geom_text_repel(data = FDIC_DATA_TICKER, aes(y = CUMUL_ABNORMAL, x = (SCHF-SCHA + SCAF-SCAA)/RBCT1C, label = TICKER), hjust=0) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-1.4,0)) +
  ggtitle("Not (Exactly) About Securities Losses") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Banks With Securities Losses Aren't Necessarily Those With Major Negative Returns") +
  theme_apricitas + theme(legend.position = c(.52,.87)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = -1.4-(.1861*1.4), xmax = -1.4-(0.049*1.4), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")
#C, BK, CASS, FAF ETC HAVE HIGH SHARE UNINSURED BUT ARE OKAY
#CAPITAL RATIOS
ggsave(dpi = "retina",plot = HTM_AFS_RETURNS_graph, "HTM AFS Returns Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

NONINTEREST_DEP_RETURNS_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=subset(FDIC_DATA_TICKER, DEPNI != 0), aes(x=DEPNI/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Noninterest Bearing Deposit Reliance", size = ASSET))+
  stat_smooth(data=FDIC_DATA_TICKER,method = "lm", aes(x=DEPNI/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Noninterest Bearing Deposit Reliance"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("Noninterest Bearing Domestic Deposits, % of Assets") +
  #geom_text_repel(data = FDIC_DATA_TICKER, aes(y = CUMUL_ABNORMAL, x = DEPUNA/DEPDOM, label = TICKER), hjust=0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  ggtitle("Reach For Yield?") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Banks With More Nonnterest Bearing Deposits Haven't Seen Lower Returns") +
  theme_apricitas + theme(legend.position = c(.50,.82)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*1), xmax = 0-(0.049*1), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NONINTEREST_DEP_RETURNS_graph, "Noninterest Deposits Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ASSTLTR_FDIC_API <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RISDATE%3A%2220221231%22&fields=CERT%2CREPDTE%2CASSTLT%2CASSET%2CNAME&limit=10000&format=csv&download=true&filename=data_file")
  
ASSTLTR__DATA_TICKER <- FDIC_FTS_DATA_123122 %>%
  left_join(CERT_TICKER_XWALK, by = "CERT") %>%
  #select(CERT,TICKER,ASSET,DEPDOM,DEPUNA) %>%
  drop_na(TICKER) %>%
  unique() %>%
  left_join(EXCESS_RETURNS_5523, by = "TICKER") %>%
  mutate(across(-TICKER, ~as.numeric(gsub(",", "", .)))) %>%
  select(TICKER, CERT, CUMUL_ABNORMAL) %>%
  left_join(ASSTLTR_FDIC_API, by = "CERT") %>%
  group_by(TICKER) %>%
  mutate(across(.cols = where(is.numeric) & !CUMUL_ABNORMAL & !CERT,~sum(.))) %>%
  ungroup() %>%
  select(CERT,TICKER, ASSET, ASSTLT, CUMUL_ABNORMAL) %>%
  unique() %>%
  filter(!(TICKER %in% c("SI","SIVB","FRC","SBNY","UMPQ")))

LONG_TERM_RETURNS_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=ASSTLTR__DATA_TICKER, aes(x=ASSTLT/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Long-Term Asset (5+ Year) Share", size = ASSET))+
  stat_smooth(data=ASSTLTR__DATA_TICKER,method = "lm", aes(x=ASSTLT/ASSET,y=CUMUL_ABNORMAL, color= "Banks' YTD Excess Returns vs. Long-Term Asset (5+ Year) Share"), size = 1.25) +
  ylab("Excess Returns, 5/5/23, %") +
  xlab("Long-Term Assets, % of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.80,0.275), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  ggtitle("Banks With More Long-Term Assets Fell More") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data with Assistance From @Paulgp", subtitle = "Since the Start of the Year, Banks Stocks With More Long-Term Exposure Fell More") +
  theme_apricitas + theme(legend.position = c(.50,.82)) +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, vjust = 0.5),
        plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*1), xmax = 0-(0.049*1), ymin = -0.80-(.3*1.075), ymax = -0.80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LONG_TERM_RETURNS_graph, "Long Term Returns Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

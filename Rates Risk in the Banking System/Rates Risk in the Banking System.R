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

ggsave(dpi = "retina",plot = GSIB_NONGSIB_INSURED_graph, "GSIB_NONGSIB.png", type = "cairo-png") 

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
  
ggsave(dpi = "retina",plot = FAILURES_TOTAL_ASSETS_graph, "Failures as a Share of Total Assets.png", type = "cairo-png") 

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

ggsave(dpi = "retina",plot = ASSTLTR_TOTAL_BANK_graph, "Long Term Assets.png", type = "cairo-png") 

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

ggsave(dpi = "retina",plot = AT_RISK_BANKS_graph, "At Risk Banks Graph.png", type = "cairo-png") 

AT_RISK_MAJOR_BANKS <- read.csv("https://banks.data.fdic.gov/api/financials?filters=CERT%3A%284977%20OR%2024045%20OR%202270%20OR%20983%20OR%2017534%20OR%2057512%29%20AND%20RISDATE%3A%5B20191231%20TO%20%2A%5D&fields=CERT%2CREPDTE%2CASSET%2CDEP%2CDEPINS%2CNAME%2CASSET%2CASSTLT%2CLNRE&limit=10000&format=csv&download=true&filename=data_file") %>%
  mutate(NAME = str_to_title(NAME)) %>%
  mutate(REPDTE = ymd(REPDTE)) %>%
  mutate(NAME = gsub("Zions Bcorp N A","Zions Bank", NAME)) %>%
  mutate(NAME = gsub("Keybank National Assn","Keybank", NAME))


DEP_DEPINS_MAJOR_BANKS <- AT_RISK_MAJOR_BANKS %>%
  select(DEP, DEPINS, NAME, REPDTE) %>%
  add_row(DEP = 147388149, DEPINS = 75770590, NAME = "Keybank", REPDTE = as.Date("2023-03-31")) %>%
  add_row(DEP = 69207568, DEPINS = 37845135, NAME = "Zions Bank", REPDTE = as.Date("2023-03-31")) %>%
  add_row(DEP = 28527246, DEPINS = 20452549, NAME = "Pacific Western Bank", REPDTE = as.Date("2023-03-31")) %>%
  add_row(DEP = 62813312, DEPINS = 35038477, NAME = "First Horizon Bank", REPDTE = as.Date("2023-03-31")) %>%
  add_row(DEP = 47852002, DEPINS = 32353242, NAME = "Western Alliance Bank", REPDTE = as.Date("2023-03-31")) %>%
  add_row(DEP = 66703000, DEPINS = 31696000, NAME = "Comerica Bank", REPDTE = as.Date("2023-03-31")) %>%
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
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Besides Pacific Western and Western Alliance, Most At-Risk Banks Haven't Seen Deposit Outflows") +
  theme_apricitas + theme(legend.position = c(.7,.825), panel.spacing.x = unit (0.5, "lines"), strip.text.x = element_text(size = 10)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits")) #+
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  #coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DEPINS_MAJOR_BANKS_graph, "At Risk Major Banks Graph.png", type = "cairo-png") 

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
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Long Term Assets Made Up the Outright Majority of SVB's Assets Pre-Run") +
  theme_apricitas + theme(legend.position = c(.2,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Pacific Western", "Western Alliance", "Zions Bank", "First Horizon", "Comerica", "KeyBank")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = 0-(.3*.65), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LONG_TERM_ASSETS_MAJOR_BANKS_graph, "Long Term Asset Ratio Major Banks Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

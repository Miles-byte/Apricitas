pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

DEP_DOM_DATA <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CDEPINS%2CDEPINSR%2CDEP%2CDEPDOM%2CESTINSR&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE) %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  pivot_longer(cols = c(`Uninsured Deposits`,`FDIC Insured Deposits`)) %>%
  subset(date > as.Date("2017-12-01")) %>%
  mutate(name = factor(name,levels = c("Uninsured Deposits","FDIC Insured Deposits")))
  
DEP_DOM_DATA_graph <- ggplot(data = DEP_DOM_DATA, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200), limits = c(0,210), expand = c(0,0)) +
  ggtitle("The Run on Silicon Valley Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "SVB Saw Massive Deposit Growth During the Pandemic—And Most of It Was Uninsured") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DOM_DATA_graph, "Dep Dom Data.png", type = "cairo-png") #cairo gets rid of anti aliasing

DEP_DOM_DATA_PROJ <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CDEPINS%2CDEPINSR%2CDEP%2CDEPDOM%2CESTINSR&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE) %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  pivot_longer(cols = c(`Uninsured Deposits`,`FDIC Insured Deposits`)) %>%
  subset(date > as.Date("2017-12-01")) %>%
  add_row(date = as.Date("2023-10-30"),name = "FDIC Insured Deposits",value = 9990000) %>%
  add_row(date = as.Date("2023-10-30"),name = "Uninsured Deposits",value = 113010000) %>%
  mutate(name = factor(name,levels = c("Uninsured Deposits","FDIC Insured Deposits")))

DEP_DOM_DATA_PROJ_graph <- ggplot(data = DEP_DOM_DATA_PROJ, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("text", label = "Close of Business 3/09", x = as.Date("2023-10-30"), y = 200, color = "white") +
  annotate("text", label = "Hypothetical Scenario", as.Date("2023-10-30"), y = 190, color = "white") +
  annotate("text", label = "If Withdrawls", as.Date("2023-10-30"), y = 170, color = "white") +
  annotate("text", label = "Were All ", as.Date("2023-10-30"), y = 160, color = "white") +
  annotate("text", label = "Successful & of", as.Date("2023-10-30"), y = 150, color = "white") +
  annotate("text", label = "Uninsured Deposits", as.Date("2023-10-30"), y = 140, color = "white") +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200), limits = c(0,210), expand = c(0,0)) +
  ggtitle("The Run on Silicon Valley Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Even Under a Highly Optimistic Scenario, ~90% of Remaining Deposits Would Be Uninsured") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DOM_DATA_PROJ_graph, "Dep Dom Data Proj.png", type = "cairo-png") #cairo gets rid of anti aliasing

DEPINSR_data <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CDEPINS%2CDEPINSR%2CDEP%2CDEPDOM%2CESTINSR&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEPINSR,RISDATE) %>%
  subset(RISDATE > as.Date("2013-03-31"))

DEP_DOM_SHARE <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CDEPINS%2CDEPINSR%2CDEP%2CDEPDOM%2CESTINSR&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE) %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  subset(date > as.Date("2013-03-01"))

DEPINSR_data_graph <- ggplot() + #plotting loan performance data
  geom_line(data=DEPINSR_data, aes(x=RISDATE,y= DEPINSR/100,color= "Insured Deposits, % of Total Assets, Silicon Valley Bank"), size = 1.25)+ 
  geom_line(data=DEP_DOM_SHARE, aes(x=date,y= `FDIC Insured Deposits`/(`Uninsured Deposits`+`FDIC Insured Deposits`),color= "Insured Deposits, % of Total Deposits, Silicon Valley Bank"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.02,0.04,0.06,0.08,0.10,0.12,.14), limits = c(0,.14), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Insured Deposits Made Up a Tiny Share of SVB's Total Assets") +
  theme_apricitas + theme(legend.position = c(.5,.14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-06-01")-(.1861*(today()-as.Date("2013-06-01"))), xmax = as.Date("2013-06-01")-(0.049*(today()-as.Date("2013-06-01"))), ymin = 0-(.3*.14), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEPINSR_data_graph, "Depinsr Data Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

FHLB <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2COTHBFHLB&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  subset(RISDATE > as.Date("1999-12-01"))

FHLB_graph <- ggplot(data = FHLB, aes(x = RISDATE, y = OTHBFHLB/1000000, fill = "Federal Home Loan Bank Advances to Silicon Valley Bank"),) + #FHLB Borrowing
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,5,10,15), limits = c(0,15), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "SVB Started Tapping FHLB Advances Before Its Bankrupcy") +
  theme_apricitas + theme(legend.position = c(.5,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = 0-(.3*15), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FHLB_graph, "FHLB Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

LONG_TERM_ASSET_RATIO <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CASSTLTR&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  subset(RISDATE > as.Date("1999-12-01"))

LONG_TERM_ASSET_RATIO_graph <- ggplot() + #plotting loan performance data
  geom_line(data=LONG_TERM_ASSET_RATIO, aes(x=RISDATE,y= ASSTLTR/100,color= "Silicon Valley Bank, Long-Term Assets (5+ Years) As a Share of Total Assets"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60), limits = c(0,.65), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Long Term Assets Made Up the Outright Majority of SVB's Assets Pre-Run") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = 0-(.3*.65), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LONG_TERM_ASSET_RATIO_graph, "Long Term Asset Ratio Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

HTM_AFS_DATA <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CSCAF%2CSCHA%2CASSET&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(RISDATE,SCAF,SCHA,ASSET) %>%
  transmute(date = RISDATE, `Available For Sale Securities` = SCAF, `Held to Maturity Securities` = SCHA, `All Other Assets`= ASSET-SCAF-SCHA) %>%
  pivot_longer(cols = c(`Available For Sale Securities`,`Held to Maturity Securities`,`All Other Assets`)) %>%
  mutate(name = factor(name,levels = c("All Other Assets","Available For Sale Securities","Held to Maturity Securities"))) %>%
  subset(date > as.Date("2017-12-01"))

HTM_AFS_DATA_graph <- ggplot(data = HTM_AFS_DATA, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200), limits = c(0,225), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "At Peak, Held-To-Maturity Securities Represented Nearly Half of SVB's Total Assets") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Held to Maturity Securities","Available For Sale Securities","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HTM_AFS_DATA_graph, "HTM AFS Data Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

AOCI <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CEQCCOMPI&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  subset(RISDATE > as.Date("2017-12-01"))
  
AOCI_graph <- ggplot(data = AOCI, aes(x = RISDATE, y = EQCCOMPI/1000000, fill = "Accumulated Other Comprehensive Income (incl. Net Unrealized Gains on Available-For-Sale Securities)"),) + #FHLB Borrowing
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-2,-1,0,1), limits = c(-2.1,1), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "SVB Saw the Value of its Securities Portfolio Tank as the Fed Raised Rates") +
  theme_apricitas + theme(legend.position = c(.505,.95)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  theme(legend.text = element_text(size = 11, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -2.1-(.3*3.1), ymax = -2.1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AOCI_graph, "AOCI Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

HTM_FAIR <- data.frame(date = c("12-31-2022","09-30-2022","06-30-2022","03-31-2022","12-31-2021","09-30-2021","06-30-2021","03-31-2021","12-31-2020","09-30-2020","06-30-2020","03-31-2020","12-31-2019","09-30-2019","06-30-2019","03-31-2019","12-31-2018"),
              value = c(76168,77367,84581,91668,97226,81994,60107,41186,17216,13612,13541,14131,14115,14698,15064,14996,15188)) %>%
              mutate(date = as.Date(date, "%m-%d-%Y"))

HTM_AMORT <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CSCHA&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(date = ymd(RISDATE)) %>%
  select(date,SCHA) %>%
  subset(date > as.Date("2018-12-01"))

HTM_MERGE <- merge(HTM_FAIR, HTM_AMORT, by = "date") %>%
  transmute(date, gap = value - SCHA/1000, fair = value, amortized= SCHA/1000)

write.csv(HTM_MERGE, "SVB_HTM_Fair_v_Amortized_Value.csv")

HTM_graph <- ggplot(data = HTM_MERGE, aes(x = date, y = gap/1000, fill = "Difference Between Amortized and Fair Value For SVB Held-to-Maturity Securities")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-15,-10,-5,0), limits = c(-17,3), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "SVB Saw the Value of its Securities Portfolio Tank as the Fed Raised Rates") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -17-(.3*20), ymax = -17) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HTM_graph, "HTM Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

PLEDGED <- read.csv("https://banks.data.fdic.gov/api/financials?filters=NAME%3A%20%22SILICON%20VALLEY%20BANK%22&fields=RISDATE%2CSCPLEDGE%2CSCPLEDGER&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(date = ymd(RISDATE)) %>%
  subset(date > as.Date("2017-12-01"))

PLEDGED_graph <- ggplot(data=PLEDGED, aes(x=date,y= SCPLEDGER/100, fill= "Silicon Valley Bank, Pledged Securities as a Share of Total Assets")) + #plotting loan performance data
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30), limits = c(0,.30), expand = c(0,0)) +
  ggtitle("Silicon Valley Bank, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "A Large Chunk of SVB's Securities Were Pledged in Recent Quarters") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PLEDGED_graph, "Pledged Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

EFFR <- fredr(series_id = "FEDFUNDS", observation_start = as.Date("2021-04-01"))
CHECK <- fredr(series_id = "ICNDR", observation_start = as.Date("2021-04-01"))
SAVING <- fredr(series_id = "SNDR", observation_start = as.Date("2021-04-01"))

BETA_graph <- ggplot() + #plotting loan performance data
  geom_line(data=EFFR, aes(x=date,y= value/100,color= "Federal Funds Rate"), size = 1.25)+ 
  geom_line(data=CHECK, aes(x=date,y= value/100,color= "National Deposit Rate: Interest Checking"), size = 1.25)+ 
  geom_line(data=SAVING, aes(x=date,y= value/100,color= "National Deposit Rate: Savings"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05), limits = c(0,.05), expand = c(0,0)) +
  ggtitle("Deposit Rates Remain Low") +
  labs(caption = "Graph created by @JosephPolitano using FDIC and Fed data", subtitle = "US Bank Deposit Rates Remain Low, Despite Raising Headling Rates—Indicating Low Deposit Betas") +
  theme_apricitas + theme(legend.position = c(.35,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-04-01")-(.1861*(today()-as.Date("2021-04-01"))), xmax = as.Date("2021-04-01")-(0.049*(today()-as.Date("2021-04-01"))), ymin = 0-(.3*.05), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BETA_graph, "BETA Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

HTM_AFS <- read.csv("C:/Users/Joseph/Documents/GitHub/Apricitas/Silicon Valley Bank/Silicon_Valley_Bank.csv") %>%
  mutate(date = as.Date(date))

HTM_AFS_graph <- ggplot() + #plotting HTM Data
  geom_line(data=HTM_AFS, aes(x=date,y= HTM/100,color= "Held to Maturity Securities"), size = 1.25)+ 
  geom_line(data=HTM_AFS, aes(x=date,y= AFS/100,color= "Available for Sale Securities"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10,0.15,0.2), limits = c(0,.20), expand = c(0,0)) +
  ggtitle("HTM On the Rise") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data", subtitle = "Banks Have Increased Their Held to Maturity Securities Levels in the Wake of Rising Rates") +
  theme_apricitas + theme(legend.position = c(.35,.4)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HTM_AFS_graph, "HTM AFS Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing


CONTAGION_5_DEPINS <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE) %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  subset(date > as.Date("2013-03-01"))

CONTAGION_5_DEPINSR <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE,NAME) %>%
  transmute(date = RISDATE, `DEPINS_DIV_DEP` = DEPINS/DEP, NAME = str_to_title(NAME)) %>%
  subset(date == as.Date("2022-12-31")) %>%
  mutate(NAME = factor(NAME,levels = c("Signature Bank","First Republic Bank","Western Alliance Bank","Pacific Western Bank","Customers Bank","First Foundation Bank")))



CONTAGION_5_DEPINSR_graph <- ggplot(data=CONTAGION_5_DEPINSR, aes(x=date,y= DEPINS_DIV_DEP,fill= NAME)) + #plotting dep ins rate contagion
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Date") +
  ylab("Share of Total Deposits") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("FDIC Insurance Among at-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Some At-Risk Banks, Especially Signature Bank, Had Low Deposit Insurance Rates") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Share of Total Deposits That Are FDIC Insured",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONTAGION_5_DEPINSR_graph, "Contagion 5 Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

CONTAGION_5_SCHAR_ASSTLTR <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA%2CASSTLTR&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(ASSTLTR,SCHAR,RISDATE,NAME) %>%
  transmute(date = RISDATE, ASSTLTR, SCHAR, NAME = str_to_title(NAME)) %>%
  subset(date == as.Date("2022-12-31")) %>%
  mutate(NAME = factor(NAME,levels = c("First Republic Bank","First Foundation Bank","Western Alliance Bank","Pacific Western Bank","Signature Bank","Customers Bank")))

CONTAGION_5_SCHAR_graph <- ggplot(data=CONTAGION_5_SCHAR_ASSTLTR, aes(x=date,y= SCHAR/100,fill= NAME)) + #plotting dep ins rate contagion
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Date") +
  ylab("Share of Total Deposits") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("HTM Securities Among at-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Most Banks With Weak Recent Stock Returns, Except Signature, Have Higher Insurance Rates") +
  theme_apricitas + theme(legend.position = c(.60,.75)) +
  scale_fill_manual(name= "Held-to-Maturity Securities as a Share of Total Assets",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONTAGION_5_SCHAR_graph, "Contagion 5 SCHAR.png", type = "cairo-png") #cairo gets rid of anti aliasing


CONTAGION_5_ASSTLTR_graph <- ggplot(data=CONTAGION_5_SCHAR_ASSTLTR, aes(x=date,y= ASSTLTR/100,fill= NAME)) + #plotting dep ins rate contagion
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Date") +
  ylab("Share of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("Long-Term Assets Among at-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Many At-Risk Banks, Especially Signature, Have High Shares of Long-Term Assets") +
  theme_apricitas + theme(legend.position = c(.60,.75)) +
  scale_fill_manual(name= "Long-Term Assets (5+ Years) as a Share of Total Assets",values = c("#00A99D","#3083DC","#EE6055","#9A348E","#FFE98F","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONTAGION_5_ASSTLTR_graph, "Contagion 5 ASSTLTR.png", type = "cairo-png") #cairo gets rid of anti aliasing


CONTAGION_5_ASSTLTR_graph <- ggplot(data=CONTAGION_5_SCHAR_ASSTLTR, aes(x=date,y= SCHAR/100,fill= NAME)) + #plotting dep ins rate contagion
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Date") +
  ylab("Share of Total Deposits") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("FDIC Insurance Among at-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Most Banks With Weak Recent Stock Returns, Except Signature, Have Higher Insurance Rates") +
  theme_apricitas + theme(legend.position = c(.60,.75)) +
  scale_fill_manual(name= "Held-to-Maturity Securities as a Share of Total Assets",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

CONTAGION_FIRST_REPUBLIC_ASSTLTR <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA%2CASSTLTR&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(ASSTLTR,RISDATE,NAME) %>%
  transmute(date = RISDATE, ASSTLTR, NAME = str_to_title(NAME)) %>%
  subset(date > as.Date("1999-12-01")) %>%
  subset(NAME == "First Republic Bank")

FIRST_REPUBLIC_ASSTLTR_graph <- ggplot() + #plotting loan performance data
  geom_line(data=CONTAGION_FIRST_REPUBLIC_ASSTLTR, aes(x=date,y= ASSTLTR/100,color= "First Republic Bank, Long-Term Assets (5+ Years) As a Share of Total Assets"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60), limits = c(0,.65), expand = c(0,0)) +
  ggtitle("Long-Term Assets at First Republic Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Long Term Assets Made Up the Outright Majority of First Republic's Assets") +
  theme_apricitas + theme(legend.position = c(.5,.3)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-06-01")-(.1861*(today()-as.Date("2011-06-01"))), xmax = as.Date("2011-06-01")-(0.049*(today()-as.Date("2011-06-01"))), ymin = 0-(.3*.65), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIRST_REPUBLIC_ASSTLTR_graph, "First Republic ASSTLTR.png", type = "cairo-png") #cairo gets rid of anti aliasing

DEP_DOM_DATA_SIGNATURE <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA%2CASSTLTR&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE,NAME) %>%
  subset(NAME == "SIGNATURE BANK") %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  pivot_longer(cols = c(`Uninsured Deposits`,`FDIC Insured Deposits`)) %>%
  subset(date > as.Date("2017-12-01")) %>%
  mutate(name = factor(name,levels = c("Uninsured Deposits","FDIC Insured Deposits")))

DEP_DOM_DATA_SIGNATURE_graph <- ggplot(data = DEP_DOM_DATA_SIGNATURE, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100), limits = c(0,110), expand = c(0,0)) +
  ggtitle("Uninsured Deposits at Signature Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Signature Saw Growth During the Crypto Boom, But Has Started Shedding Deposits Recently") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*110), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DOM_DATA_SIGNATURE_graph, "Dep Dom Data Signature.png", type = "cairo-png") #cairo gets rid of anti aliasing

CONTAGION_SIGNATURE_BANK_ASSTLTR <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA%2CASSTLTR&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(ASSTLTR,RISDATE,NAME) %>%
  transmute(date = RISDATE, ASSTLTR, NAME = str_to_title(NAME)) %>%
  subset(date > as.Date("1999-12-01")) %>%
  subset(NAME == "Signature Bank")

SIGNATURE_BANK_graph <- ggplot() + #plotting loan performance data
  geom_line(data=CONTAGION_SIGNATURE_BANK_ASSTLTR, aes(x=date,y= ASSTLTR/100,color= "Signature Bank, Long-Term Assets (5+ Years) As a Share of Total Assets"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.10,.20,.30,.40,.50,.60), limits = c(0,.65), expand = c(0,0)) +
  ggtitle("Long-Term Assets at Signature Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Long Term Assets Made Up a Relatively Low Share of Signature's Assets") +
  theme_apricitas + theme(legend.position = c(.5,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-06-30")-(.1861*(today()-as.Date("2001-06-30"))), xmax = as.Date("2001-06-30")-(0.049*(today()-as.Date("2001-06-30"))), ymin = 0-(.3*.65), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SIGNATURE_BANK_graph, "Signature Bank Long Term Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

Discount_Borrowing <- fredr("WLCFLPCL")

Discount_Borrowing_FDIC <- Discount_Borrowing

#Discount_Borrowing_FDIC$value[1057] = Discount_Borrowing_FDIC$value[1057] + 142800

Discount_Borrowing_graph <- ggplot() + #plotting loan performance data
  geom_line(data=Discount_Borrowing_FDIC, aes(x=date,y= value/1000,color= "Discount Window Loans Including to FDIC Bridge Banks"), size = 1.25)+ 
  geom_line(data=Discount_Borrowing, aes(x=date,y= value/1000,color= "Discount Window Loans"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Billions of Dollars, Wednesday Level") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200,250,300), limits = c(0,330), expand = c(0,0)) +
  ggtitle("Banks' Discount Window Borrowing") +
  labs(caption = "Graph created by @JosephPolitano using Fed data", subtitle = "Banks' Discount Window Borrowings are at a Modern Record High") +
  theme_apricitas + theme(legend.position = c(.5,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Discount Window Loans","Discount Window Loans Including to FDIC Bridge Banks")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*330), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Discount_Borrowing_graph, "Discount Window Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

FED_EMERGENCY_LOANS <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=a66e338ec176dd641c333de890fd7816&lastobs=100&from=&to=&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","Lending to FDIC Bridge Banks (SVB, Signature)","Bank Term Funding Program","Discount Window")) %>%
  mutate(date = as.Date(date)) %>%
  subset(date > as.Date("2023-01-01")) %>%
  pivot_longer(cols = `Lending to FDIC Bridge Banks (SVB, Signature)`:`Discount Window`) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(name = factor(name,levels = c("Bank Term Funding Program","Lending to FDIC Bridge Banks (SVB, Signature)","Discount Window")))

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
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200,250,300,350), limits = c(0,350), expand = c(0,0)) +
  ggtitle("Fed Emergency Lending") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Fed is Lending Billions to Banks After SVB's Failure") +
  theme_apricitas + theme(legend.position = c(.4,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Selected Federal Reserve Loans",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Discount Window","Lending to FDIC Bridge Banks (SVB, Signature)","Bank Term Funding Program")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-04")-(.1861*(today()-as.Date("2023-01-04"))), xmax = as.Date("2023-01-04")-(0.049*(today()-as.Date("2023-01-04"))), ymin = 0-(.3*350), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_EMERGENCY_LOANS_graph, "Fed Emergency Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

LOAN_TIMING <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=4676a8c36670ad186310cf1b72c579d8&lastobs=100&from=&to=&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","Within 15 Days","16-90 Days","91 Days to 1 Year","Over 1 Year to 5 Years","Over 5 Years to 10 Years")) %>%
  mutate(date = as.Date(date)) %>%
  subset(date > as.Date("2023-01-01")) %>%
  pivot_longer(cols = `Within 15 Days`:`Over 5 Years to 10 Years`) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(name = factor(name,levels = c("Over 5 Years to 10 Years","Over 1 Year to 5 Years","91 Days to 1 Year","16-90 Days","Within 15 Days")))

LOAN_TIMING_graph <- ggplot(data = LOAN_TIMING, aes(x = date, y = value/1000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200,250,300), limits = c(0,330), expand = c(0,0)) +
  ggtitle("$300B in Fed Emergency Lending") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Most of the Fed's New Lending Came in the Form of Short-Maturity Loans") +
  theme_apricitas + theme(legend.position = c(.5,.775)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Maturity of All Federal Reserve Loans, Wednesday Level",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Within 15 Days","16-90 Days","91 Days to 1 Year","Over 1 Year to 5 Years","Over 5 Years to 10 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-04")-(.1861*(today()-as.Date("2023-01-04"))), xmax = as.Date("2023-01-04")-(0.049*(today()-as.Date("2023-01-04"))), ymin = 0-(.3*330), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LOAN_TIMING_graph, "Loan Timing Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

CHANGE_BY_REGION <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=4d7a458bf10adf46d1f858f4df0c81c1&lastobs=100&from=&to=&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","Boston","New York","Philadelphia","Cleveland","Richmond","Atlanta","Chicago","St. Louis","Minneapolis","Kansas City","Dallas","San Francisco")) %>%
  mutate(date = as.Date(date)) %>%
  subset(date > as.Date("2023-03-07")) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.numeric, funs(c(first(.), (. - first(.))[-1]))) %>%
  .[-1,] %>%
  pivot_longer(cols = `Boston`:`San Francisco`) %>%
  mutate(name = factor(name,levels = c("Atlanta","Cleveland","Chicago","Boston","Minneapolis","Dallas","Richmond","St. Louis","Kansas City","Philadelphia","New York","San Francisco")))

#redo by date once more data released
CHANGE_BY_REGION_graph <- ggplot(data = CHANGE_BY_REGION, aes(x = name, y = value/1000, fill = as.character(date))) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Federal Reserve Bank") +
  ylab("Billions of Dollars, Wednesday Level") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200,250,300), limits = c(-1,275), expand = c(0,0)) +
  ggtitle("$300B in Fed Emergency Lending") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Most of the Fed's New Lending is Going to SF (SVB, FRC) and NY (Signature)") +
  theme_apricitas + theme(legend.position = c(.5,.575)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Change in Loans, Securities, and Repos Since Pre-SVB Collapse",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  coord_flip()

ggsave(dpi = "retina",plot = CHANGE_BY_REGION_graph, "Change by Region Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

#IOER IORB graph

IORB_DPCREDIT_MERGE <- merge(
  rbind(fredr(series_id = "IOER", observation_start = as.Date("2018-01-01")),fredr(series_id = "IORB", observation_start = as.Date("2018-01-01"))),
  fredr(series_id = "DPCREDIT",observation_start = as.Date("2018-01-01")), by = "date")

IORB_DPCREDIT_MERGE_graph <- ggplot() + #plotting loan performance data
  geom_line(data=IORB_DPCREDIT_MERGE, aes(x=date,y= (value.y-value.x)/100,color= "Gap Between Discount Window Rate and Interest Paid on Bank Reserves"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = c(0,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01), limits = c(0,0.01), expand = c(0,0)) +
  ggtitle("Discount Window Destigmatization") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Penalty Rates for the Discount Window Have Fallen Significantly Since the Start of COVID") +
  theme_apricitas + theme(legend.position = c(.5,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.01), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IORB_DPCREDIT_MERGE_graph, "IORB DPCREDIT Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

SFOS_RESPONSES <- data.frame(name = c("If Other Funding Sources Are More Expensive","To Lend at Higher Rates in Money Markets","Only if Funding Becomes Scarce Due to Firm-Specific Strain","Only if Funding Becomes Scarce Due to Market-Wide Conditions"),value = c(8.8,0,17.5,62.5)) %>%
  mutate(name = factor(name,levels = c("To Lend at Higher Rates in Money Markets","If Other Funding Sources Are More Expensive","Only if Funding Becomes Scarce Due to Firm-Specific Strain","Only if Funding Becomes Scarce Due to Market-Wide Conditions")))

SFOS_RESPONSES_graph <- ggplot(data = SFOS_RESPONSES, aes(x = 1, y = value/100, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Answer, Percent of Banks") +
  ylab("Percent of Banks") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.2,0.4,0.6), limits = c(0,0.7), expand = c(0,0)) +
  ggtitle("Discount Window Destigmatization") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "A Majority of Banks Say They Would Use the Discount Window Only in Market Crisis") +
  theme_apricitas + theme(legend.position = c(.6,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Willingness to Borrow from Discount Window, % Of Banks",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Only if Funding Becomes Scarce Due to Market-Wide Conditions","Only if Funding Becomes Scarce Due to Firm-Specific Strain","If Other Funding Sources Are More Expensive","To Lend at Higher Rates in Money Markets")) +
  theme(axis.text.y=element_blank())+
  coord_flip()

ggsave(dpi = "retina",plot = SFOS_RESPONSES_graph, "SFOS Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

TWOYR <- fredr(series_id = "DGS2",observation_start = as.Date("2019-11-04")) #downloading 2yr yields
TWOYR <- drop_na(TWOYR)

TWOYR_Graph <- ggplot() + 
  geom_line(data = TWOYR, aes(x = date, y = value/100, color = "Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(0,.06), breaks = c(0,.01,.02,.03,.04,.05,0.06), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("Higher For Not Quite As Long") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Yield on 2YR Treasury Notes Has Fallen Dramatically in the Wake of SVB's Collapse") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-11-04")-(.1861*(today()-as.Date("2019-11-04"))), xmax = as.Date("2019-11-04")-(0.049*(today()-as.Date("2019-11-04"))), ymin = 0-(.3*.06), ymax = 0) +
  coord_cartesian(clip = "off")

ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)

ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  annotate("hline", y = 0.089, yintercept = 0.089, color = "#EE6055", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2016/2012 Cycle Peak", x = as.Date("2019-01-01"), y = 0.095, color ="#EE6055", size = 5) +
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "High Yield Credit Spreads Jumped Slightly in the Wake of SVB's Collapse") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TWOYR_Graph, "TWOYR Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "ICECCC Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

SWAPS <- fredr("SWPT")

SWAPS <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=b6b21867aa5eb9783a95747538a21cd8&lastobs=&from=01/01/2002&to=12/31/2023&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","value")) %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric)
  
SWAPS_graph <- ggplot() + #plotting loan performance data
  geom_line(data=SWAPS, aes(x=date,y= value/1000,color= "Central Bank Liquidity Swaps"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Billions of Dollars, Wednesday Level") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,100,200,300,400,500,600), limits = c(0,600), expand = c(0,0)) +
  ggtitle("The Fed's International Lending") +
  labs(caption = "Graph created by @JosephPolitano using Fed data", subtitle = "The Fed's International Dollar Lending Rose in the Wake of CS's Demise") +
  theme_apricitas + theme(legend.position = c(.6,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*600), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FOREIGN_REPO <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=6469d791421717c45c635898573f771f&lastobs=&from=01/01/2019&to=12/31/2023&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","value")) %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric)

ggsave(dpi = "retina",plot = SWAPS_graph, "SWAPS Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

FOREIGN_REPO_graph <-  ggplot(data = subset(FOREIGN_REPO, date > as.Date("2020-01-01")), aes(x = date, y = value/1000, fill = "Federal Reserve Foreign Official Repo Agreements")) + #plotting Deposits, Insured and Uninsured
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
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50,60,70), limits = c(0,70), expand = c(0,0)) +
  ggtitle("Fed Foreign Lending") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Fed Lent $60B to Foreign and International Monetary Authorities After SVB's Failure") +
  theme_apricitas + theme(legend.position = c(.4,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = 0-(.3*70), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FOREIGN_REPO_graph, "Foreign Repo Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
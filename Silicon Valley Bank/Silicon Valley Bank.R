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

9990000/(9990000+113010000)

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
  transmute(date = date, gap = value - SCHA/1000)

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




cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
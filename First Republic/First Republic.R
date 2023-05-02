pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)



FED_EMERGENCY_LOANS <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=a66e338ec176dd641c333de890fd7816&lastobs=100&from=&to=&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","Lending to FDIC Bridge Banks (SVB, Signature)","Bank Term Funding Program","Discount Window")) %>%
  mutate(date = as.Date(date)) %>%
  subset(date > as.Date("2022-12-01")) %>%
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
  theme_apricitas + theme(legend.position = c(.31,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Selected Federal Reserve Loans",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Discount Window","Lending to FDIC Bridge Banks (SVB, Signature)","Bank Term Funding Program")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-12-04")-(.1861*(today()-as.Date("2022-12-04"))), xmax = as.Date("2022-12-04")-(0.049*(today()-as.Date("2022-12-04"))), ymin = 0-(.3*350), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_EMERGENCY_LOANS_graph, "Fed Emergency Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FOREIGN_REPO <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=6469d791421717c45c635898573f771f&lastobs=&from=01/01/2019&to=12/31/2023&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","value")) %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric)

FOREIGN_REPO_graph <-  ggplot(data = subset(FOREIGN_REPO, date > as.Date("2020-01-01")), aes(x = date, y = value/1000, fill = "Federal Reserve Foreign Official Repo Agreements")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars, Wednesday Level") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50,60,70), limits = c(0,70), expand = c(0,0)) +
  ggtitle("Fed Foreign Lending") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Fed Lent $60B to Foreign and International Monetary Authorities After CS's Takeover") +
  theme_apricitas + theme(legend.position = c(.4,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = 0-(.3*70), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FOREIGN_REPO_graph, "Foreign Repo Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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

ggsave(dpi = "retina",plot = FIRST_REPUBLIC_ASSTLTR_graph, "First Republic ASSTLTR.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

BORROWINGS_LARGE <- fredr(series_id = "H8B3094NLGA",observation_start = as.Date("2019-01-01"))

BORROWINGS_SMALL <- fredr(series_id = "H8B3094NSMA",observation_start = as.Date("2019-01-01"))

BORROWINGS_SMALL_LARGE_graph <- ggplot() + #plotting loan performance data
  geom_line(data=BORROWINGS_LARGE, aes(x=date,y= value/1000,color= "Borrowings: Large Domestic Banks (incl. First Republic)"), size = 1.25)+ 
  geom_line(data=BORROWINGS_SMALL, aes(x=date,y= value/1000,color= "Borrowings: Small Domestic Banks"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Billions of Dollars, Wednesday Level") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,250,500,750,1000), limits = c(0,1000), expand = c(0,0)) +
  ggtitle("Banks' Borrowing Binge") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Banks are Borrowing Hundreds of Billions More in the Aftermathe of SVB's Failure") +
  theme_apricitas + theme(legend.position = c(.6,.14)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*1000), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BORROWINGS_SMALL_LARGE_graph, "Borrowings Small Large Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

DEP_DOM_DATA_PROJ_FRC <- read.csv("https://banks.data.fdic.gov/api/financials?filters=CERT%3A59017&fields=CERT%2CRISDATE%2CDEP%2CDEPINS&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE) %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  pivot_longer(cols = c(`Uninsured Deposits`,`FDIC Insured Deposits`)) %>%
  subset(date > as.Date("2017-12-01")) %>%
  add_row(date = as.Date("2023-03-31"),name = "FDIC Insured Deposits",value = 54651000) %>%
  add_row(date = as.Date("2023-03-31"),name = "Uninsured Deposits",value = 19765000) %>%
  add_row(date = as.Date("2023-03-31"),name = "Deposit Infusion From Large US Banks",value = 30000000) %>%
  mutate(name = factor(name,levels = c("Deposit Infusion From Large US Banks","Uninsured Deposits","FDIC Insured Deposits")))

DEP_DOM_DATA_PROJ_FRC_graph <- ggplot(data = DEP_DOM_DATA_PROJ_FRC, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200), limits = c(0,210), expand = c(0,0)) +
  ggtitle("The Run on First Republic Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Nearly $100B in Uninsured Deposits Fled First Republic by the End of March") +
  theme_apricitas + theme(legend.position = c(.37,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Uninsured Deposits","FDIC Insured Deposits","Deposit Infusion From Large US Banks")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DOM_DATA_PROJ_FRC_graph, "Dep Dom Data Proj FRC.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

DEMAND_DEP <- fredr(series_id = "DEMDEPSL", observation_start = as.Date("2022-01-01")) %>%
  mutate(value = value-value[1])
OTHER_DEPOSITS <- fredr(series_id = "MDLM", observation_start = as.Date("2022-01-01")) %>%
  mutate(value = value-value[1])
RETAIL_MONEY <- fredr(series_id = "RMFSL", observation_start = as.Date("2022-01-01")) %>%
  mutate(value = value-value[1])
TIME_DEPOSITS <- fredr(series_id = "STDSL", observation_start = as.Date("2022-01-01")) %>%
  mutate(value = value-value[1])

CHANGE_MONEY_SUPPLY_graph <- ggplot() + #plotting loan performance data
  geom_line(data=DEMAND_DEP, aes(x=date,y= value/1000,color= "Demand Deposits"), size = 1.25)+ 
  geom_line(data=OTHER_DEPOSITS, aes(x=date,y= value/1000,color= "Savings Deposits, Money Market Deposit Accounts, and Other Deposits"), size = 1.25)+ 
  geom_line(data=RETAIL_MONEY, aes(x=date,y= value/1000,color= "Retail Money Market Funds"), size = 1.25)+ 
  geom_line(data=TIME_DEPOSITS, aes(x=date,y= value/1000,color= "Small-Denomination CDs and Time Deposits"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Billions of Dollars, Change Since Jan 2022") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "T"), breaks = c(-2,-1.5,-1,-0.5,0,0.5), limits = c(-2.1,0.7), expand = c(0,0)) +
  ggtitle("The Effects of Rate Hikes") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Trillions Have Left Savings Accounts While CDs & Money Market Funds Rise") +
  theme_apricitas + theme(legend.position = c(.44,.19)) +
  scale_color_manual(name= "Change in M2 Components Since Jan 2022",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Demand Deposits","Savings Deposits, Money Market Deposit Accounts, and Other Deposits","Small-Denomination CDs and Time Deposits","Retail Money Market Funds")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-30-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-30-as.Date("2022-01-01"))), ymin = -2.1-(.3*2.8), ymax = -2.1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHANGE_MONEY_SUPPLY_graph, "Change in Money Supply.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

DEPOSITS_LARGE <- fredr(series_id = "ODSLCBW027SBOG", observation_start = as.Date("2022-01-01")) %>%
  mutate(value = value-value[1])
DEPOSITS_SMALL <- fredr(series_id = "ODSSCBW027SBOG", observation_start = as.Date("2022-01-01")) %>%
  mutate(value = value-value[1])

CHANGE_LARGE_SMALL_DEPOSITS_graph <- ggplot() + #plotting loan performance data
  geom_line(data=DEPOSITS_LARGE, aes(x=date,y= value/1000,color= "Deposits ex-Large Time Deposits, Large (top 25, incl. First Republic) Domestic Banks"), size = 1.25)+ 
  geom_line(data=DEPOSITS_SMALL, aes(x=date,y= value/1000,color= "Deposits ex-Large Time Deposits, Small Domestic Banks"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Billions of Dollars, Change Since Jan 2022") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.25, suffix = "T"), breaks = c(0.25,0,-0.25,-0.5,-0.75,-1,-1.25), limits = c(-1.25,0.25), expand = c(0,0)) +
  ggtitle("Deposit Flows") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Deposits are Slowly Trickling Out of Banks as Rates Rise and QT Continues") +
  theme_apricitas + theme(legend.position = c(.53,.19)) +
  scale_color_manual(name= "Change Since Jan 2022",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Deposits ex-Large Time Deposits, Large (top 25, incl. First Republic) Domestic Banks","Deposits ex-Large Time Deposits, Small Domestic Banks")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-30-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-30-as.Date("2022-01-01"))), ymin = -1.25-(.3*1.5), ymax = -1.25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHANGE_LARGE_SMALL_DEPOSITS_graph, "Change Large Small Deposits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

TENYR_MINUS_FFR <- fredr(series_id = "T10YFF", observation_start = as.Date("2018-01-01")) %>%
  drop_na()
FIVEYR_MINUS_FFR <- fredr(series_id = "T5YFF", observation_start = as.Date("2018-01-01")) %>%
  drop_na()

YIELD_INVERSION_GRAPH <- ggplot() + #plotting loan performance data
  geom_line(data=TENYR_MINUS_FFR, aes(x=date,y= value/100,color= "Ten Year Treasury Yield Minus Federal Funds Rate"), size = 1.25)+ 
  geom_line(data=FIVEYR_MINUS_FFR, aes(x=date,y= value/100,color= "Five Year Treasury Yield Minus Federal Funds Rate"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), breaks = c(-0.02,-0.01,0,0.01,0.02), limits = c(-0.02,0.0275), expand = c(0,0)) +
  ggtitle("Borrow Long Lend Short") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Yield Curves are Extremely Inverted—Which Might Make it Harder for Banks to Operate") +
  theme_apricitas + theme(legend.position = c(.4,.17)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.02-(.3*.0475), ymax = -0.02) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = YIELD_INVERSION_GRAPH, "Yield Inversion.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FRC <- tq_get("FRC", from = "2018-01-01")

FRC_TOTAL_RETURN_graph <- ggplot() + #plotting loan performance data
  geom_line(data=FRC, aes(x=date,y= (adjusted/adjusted[1]*100)/100-1,color= "First Republic Bank (FRC), Total Return Since January 2018"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Total Return, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-1,-0.5,0,0.5,1,1.5), limits = c(-1.00,1.75), expand = c(0,0)) +
  ggtitle("The Decline of First Republic") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "After a Massive Runup During the Pandemic, First Republic's Value Has Now Collapsed") +
  theme_apricitas + theme(legend.position = c(.5,.23)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -1-(.3*2.75), ymax = -1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FRC_TOTAL_RETURN_graph, "FRC Total Return Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

BORROW_FRC <- read.csv("https://banks.data.fdic.gov/api/financials?filters=CERT%3A59017&fields=CERT%2CRISDATE%2COTHBOR%2COTHBFHLB&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(OTHBOR,RISDATE) %>%
  subset(RISDATE > as.Date("2017-12-01")) %>%
  add_row(RISDATE = as.Date("2023-03-31"),OTHBOR = 105890000)
  
BORROW_FRC_graph <- ggplot(data = BORROW_FRC, aes(x = RISDATE, y = OTHBOR/1000000, fill = "Borrowings by First Republic Bank")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,25,50,75,100,125), limits = c(0,125), expand = c(0,0)) +
  ggtitle("The Run on First Republic Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Fleeing Depositors Have Forced First Republic to Borrow More Than $100B at Higher Rates") +
  theme_apricitas + theme(legend.position = c(.37,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*125), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BORROW_FRC_graph, "Borrow FRC.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

GAP_FRC <- data.frame(date = as.Date(c("2018-01-01","2019-01-01","2020-01-01","2021-01-01","2022-01-01")), value = c(-2341,-1746,-1316,-3132,-19273))

GAP_graph <- ggplot(data = GAP_FRC, aes(x = date, y = value/1000, fill = "Difference Between Carrying and Fair Value For First Republic Mortgages")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-20,-15,-10,-5,0), limits = c(-22,1), expand = c(0,0)) +
  ggtitle("First Republic, Before The Run") +
  labs(caption = "Graph created by @JosephPolitano using First Republic data", subtitle = "SVB Saw the Value of its Mortgage Portfolio Tank as the Fed Raised Rates") +
  theme_apricitas + theme(legend.position = c(.52,.07)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-09-01")-(.1861*(today()-365-as.Date("2017-09-01"))), xmax = as.Date("2017-09-01")-(0.049*(today()-365-as.Date("2017-09-01"))), ymin = -22-(.3*23), ymax = -22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GAP_graph, "GAP FRC.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RE_ASSETS <- read.csv("https://banks.data.fdic.gov/api/financials?filters=CERT%3A59017&fields=CERT%2CRISDATE%2CLNRE%2CASSET%2CSCMTGBK&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(RISDATE,LNRE,SCMTGBK,ASSET) %>%
  transmute(date = RISDATE, `All Other Assets` = ASSET-LNRE-SCMTGBK, `Loans Secured By Real Estate` = LNRE, "Mortgage-Backed Securities" = SCMTGBK) %>%
  pivot_longer(cols = c(`All Other Assets`,`Loans Secured By Real Estate`,`Mortgage-Backed Securities`)) %>%
  subset(date > as.Date("2017-12-01")) %>%
  mutate(name = factor(name,levels = c("All Other Assets","Mortgage-Backed Securities","Loans Secured By Real Estate")))

RE_ASSETS_Graph <- ggplot(data = RE_ASSETS, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100,150,200), limits = c(0,220), expand = c(0,0)) +
  ggtitle("First Republic, Before the Run") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Most of First Republic's Assets Were in Real Estate Loans, Leaving it Vulnerable to Rising Rates") +
  theme_apricitas + theme(legend.position = c(.37,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Loans Secured By Real Estate","Mortgage-Backed Securities","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*210), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RE_ASSETS_Graph, "RE Assets Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

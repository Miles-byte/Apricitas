pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Graphing Fed Balance Sheet
TREASURY_ASSETS <- fredr(series_id = "WSHOTSL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Treasury Securities")
MBS_ASSETS <- fredr(series_id = "WSHOMCB",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Mortgage Backed Securities")
ALL_ASSETS <- fredr(series_id = "WALCL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Assets")

FED_ASSETS <- rbind(TREASURY_ASSETS,MBS_ASSETS,ALL_ASSETS) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`All Other Assets` = `All Assets`-`Treasury Securities`-`Mortgage Backed Securities`) %>%
  select(-`All Assets`) %>%
  pivot_longer(cols = `Treasury Securities`:`All Other Assets`)

CURRENCY_LIABILITIES <- fredr(series_id = "WLFN",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Physical Currency")
RESERVES_LIABILITIES <- fredr(series_id = "WLDLCL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Bank Reserves")
REVERSE_REPO_LIABILITIES <- fredr(series_id = "WLRRAL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Reverse Repo")
ALL_LIABILITIES <- fredr(series_id = "WLTLECL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Liabilities")

FED_LIABILITIES <- rbind(CURRENCY_LIABILITIES,RESERVES_LIABILITIES,REVERSE_REPO_LIABILITIES,ALL_LIABILITIES) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`All Other Liabilities` = `All Liabilities`-`Reverse Repo`-`Bank Reserves`-`Physical Currency`) %>%
  select(-`All Liabilities`) %>%
  pivot_longer(cols = `Physical Currency`:`All Other Liabilities`) %>%
  mutate(name = factor(name,levels = c("All Other Liabilities","Reverse Repo","Physical Currency","Bank Reserves")))


FED_ASSETS_Graph <- ggplot(data = FED_ASSETS, aes(x = date, y = value/1000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("The Start of Quantitative Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Federal Reserve's Balance Sheet is Shrinking Again") +
  theme_apricitas + theme(legend.position = c(.43,.85)) +
  scale_fill_manual(name= "Federal Reserve Assets",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#00A99D","#A7ACD9","#3083DC"), breaks = c("Treasury Securities","Mortgage Backed Securities","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-12-18")-(.1861*(today()-as.Date("2002-12-18"))), xmax = as.Date("2002-12-18")-(0.049*(today()-as.Date("2002-12-18"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FED_LIABILITIES_Graph <- ggplot(data = FED_LIABILITIES, aes(x = date, y = value/1000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("The Start of Quantitative Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Federal Reserve's Balance Sheet is Shrinking Again") +
  theme_apricitas + theme(legend.position = c(.43,.8)) +
  scale_fill_manual(name= "Federal Reserve Liabilities",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#00A99D","#A7ACD9","#3083DC"), breaks = c("Bank Reserves","Physical Currency","Reverse Repo","All Other Liabilities")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-12-18")-(.1861*(today()-as.Date("2002-12-18"))), xmax = as.Date("2002-12-18")-(0.049*(today()-as.Date("2002-12-18"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_ASSETS_Graph, "Federal Reserve Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = FED_LIABILITIES_Graph, "Federal Reserve Liabilities.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Commercial Bank H.8 Data

SECURITIES_ASSETS <- fredr(series_id = "SBCACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Treasury, MBS, and Other Securities")
LOANS_ASSETS <- fredr(series_id = "TOTLL",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Loans and Leases")
CASH_ASSETS <- fredr(series_id = "CASACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Bank Reserves and Other Cash Assets")
ALL_ASSETS <- fredr(series_id = "TLAACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Assets")
CAPITAL_ASSETS <- fredr(series_id = "RALACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Bank Capital (Assets Net Liabilities)")

BANK_ASSETS <- rbind(SECURITIES_ASSETS,LOANS_ASSETS,CASH_ASSETS,ALL_ASSETS) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`All Other Assets` = `All Assets`-`Treasury, MBS, and Other Securities`-`Loans and Leases`-`Bank Reserves and Other Cash Assets`) %>%
  select(-`All Assets`) %>%
  pivot_longer(cols = `Treasury, MBS, and Other Securities`:`All Other Assets`) %>%
  mutate(name = factor(name,levels = c("All Other Assets","Loans and Leases", "Treasury, MBS, and Other Securities","Bank Reserves and Other Cash Assets")))

BANK_ASSETS_Graph <- ggplot(data = BANK_ASSETS, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10,15,20,25), limits = c(0,25.5), expand = c(0,0)) +
  ggtitle("The Start of Quantitative Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Commercial Bank Asset Levels are Dropping, Though Loans and Leases are Increasing") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Assets of Commercial Banks in the United States",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Bank Reserves and Other Cash Assets","Treasury, MBS, and Other Securities","Loans and Leases","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-12-18")-(.1861*(today()-as.Date("2002-12-18"))), xmax = as.Date("2002-12-18")-(0.049*(today()-as.Date("2002-12-18"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BANK_ASSETS_Graph, "Bank Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#HQLA Assets
AGENCY_MBS_ASSETS <- fredr(series_id = "TMBACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Agency MBS")
TREASURIES_ASSETS <- fredr(series_id = "TNMACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Treasury Securities")

HQLA_ASSETS <- rbind(CASH_ASSETS,AGENCY_MBS_ASSETS,TREASURIES_ASSETS,ALL_ASSETS) %>%
  subset(date > as.Date("2009-07-01")) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`Agency MBS` = `Agency MBS`/`All Assets`) %>%
  mutate(`Bank Reserves and Other Cash Assets` = `Bank Reserves and Other Cash Assets`/`All Assets`) %>%
  mutate(`Treasury Securities` = `Treasury Securities`/`All Assets`) %>%
  select(-`All Assets`) %>%
  pivot_longer(cols = `Bank Reserves and Other Cash Assets`:`Treasury Securities`) %>%
  mutate(name = factor(name,levels = c("Agency MBS","Treasury Securities","Bank Reserves and Other Cash Assets")))

HQLA_ASSETS_Graph <- ggplot(data = HQLA_ASSETS, aes(x = date, y = value, fill = name)) + #plotting HQLA asssets
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Commercial Bank Assets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.1,0.2,0.3,0.4), limits = c(0,.40), expand = c(0,0)) +
  ggtitle("Quantity of Quality") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "High Quality Liquid Assets are Declining as a Share of Bank Assets") +
  theme_apricitas + theme(legend.position = c(.33,.85)) +
  scale_fill_manual(name= "HQLA as a Share of US Commercial Bank Assets",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Bank Reserves and Other Cash Assets","Treasury Securities","Agency MBS")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2009-07-01")-(.1861*(today()-as.Date("2009-07-01"))), xmax = as.Date("2009-07-01")-(0.049*(today()-as.Date("2009-07-01"))), ymin = 0-(.3*.40), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HQLA_ASSETS_Graph, "High Quality Liquid Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#graphing residential, commercial, and other real estate assets
RES_RE_ASSETS <- fredr(series_id = "RREACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Residential Real Estate Loans")
COM_RE_ASSETS <- fredr(series_id = "CREACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Commercial Real Estate Loans")
CC_CON_ASSETS <- fredr(series_id = "CCLACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Credit Cards and Other Revolving Loans")
AUTO_CON_ASSETS <- fredr(series_id = "CARACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Automobile Loans")
OTHER_CON_ASSETS <- fredr(series_id = "AOCACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Other Consumer Loans")
LOANS_FIN_ASSETS <- fredr(series_id = "LNFACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Loans to Nondepository Financial Institutions")
ALL_OTHER_LOANS <- fredr(series_id = "OLNACBW027SBOG",observation_start = as.Date("2015-01-07"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Other Loans")

CONSUMER_LOAN_ASSETS <- rbind(CC_CON_ASSETS,AUTO_CON_ASSETS,OTHER_CON_ASSETS) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  #pivot_wider() %>% 
  #mutate(`All Other Assets` = `All Assets`-`Treasury, MBS, and Other Securities`-`Loans and Leases`-`Bank Reserves and Other Cash Assets`) %>%
  #select(-`All Assets`) %>%
  #pivot_longer(cols = `Treasury, MBS, and Other Securities`:`All Other Assets`) %>%
  mutate(name = factor(name,levels = c("Other Consumer Loans","Automobile Loans", "Credit Cards and Other Revolving Loans")))

BANK_CONSUMER_LOAN_ASSETS_Graph <- ggplot(data = CONSUMER_LOAN_ASSETS, aes(x = date, y = value/1000, fill = name)) + #plotting bank consumer loan data
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "T"), breaks = c(0,0.5,1,1.5,2,2.5), limits = c(0,2.5), expand = c(0,0)) +
  ggtitle("Back to Borrowing") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Consumer Loan Levels are Still Increasing") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_fill_manual(name= "Consumer Loan Assets of Commercial Banks in the United States",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Credit Cards and Other Revolving Loans","Automobile Loans","Other Consumer Loans")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-07")-(.1861*(today()-as.Date("2015-01-07"))), xmax = as.Date("2015-01-07")-(0.049*(today()-as.Date("2015-01-07"))), ymin = 0-(.3*2.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BANK_CONSUMER_LOAN_ASSETS_Graph, "Bank Consumer Loan Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Loan Loss Ratios
LOAN_LOSS_ASSETS <- fredr(series_id = "ALLACBW027SBOG",observation_start = as.Date("2009-07-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Allowance for Loan and Lease Losses")
LOANS_LEASES_ASSETS <- fredr(series_id = "TOTLL",observation_start = as.Date("2009-07-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Loans and Leases")

LOAN_LOSS_RATIO <- rbind(LOAN_LOSS_ASSETS,LOANS_LEASES_ASSETS) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>%
  mutate(`Allowance for Loan and Lease Losses` =`Allowance for Loan and Lease Losses`/`Loans and Leases`) %>%
  select(-`Loans and Leases`)
  
LOAN_LOSS_RATIO_Graph <- ggplot() + #plotting loan loss ratio
  geom_line(data=subset(LOAN_LOSS_RATIO, date > as.Date("2019-01-01")), aes(x=date,y= `Allowance for Loan and Lease Losses`,color= "Allowance for Loan and Lease Losses as a Share of Loans and Leases"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), breaks = c(0,0.005,0.01,0.015,0.02,0.025), limits = c(0,.025), expand = c(0,0)) +
  ggtitle("ALLL In") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Loan Loss Reserve Ratios Remain Elevated and are Increasing Again") +
  theme_apricitas + theme(legend.position = c(.53,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*.025), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LOAN_LOSS_RATIO_Graph, "Loan Loss Ratio.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Putting nonperformance data
THIRTY_NINETY_PAST_DUE <- fredr(series_id = "QBPLNTLN3089DUR",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
NONCURRENT_RATE <- fredr(series_id = "QBPLNTLNNCUR",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
NET_CHARGE_OFF_RATE <- fredr(series_id = "QBPLNTLNNTCGOFFR",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

LOAN_PERFORMANCE_Graph <- ggplot() + #plotting loan performance data
  geom_line(data=THIRTY_NINETY_PAST_DUE, aes(x=date,y= value/100,color= "30-89 Day Past Due Rate"), size = 1.25)+ 
  geom_line(data=NONCURRENT_RATE, aes(x=date,y= value/100,color= "Noncurrent Rate"), size = 1.25)+ 
  geom_line(data=NET_CHARGE_OFF_RATE, aes(x=date,y= value/100,color= "Net Charge-Off Rate"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06), limits = c(0,.06), expand = c(0,0)) +
  ggtitle("Performing Pretty Well") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Charge-Off Rates and Past-due Rates Remain Near Historic Lows") +
  theme_apricitas + theme(legend.position = c(.25,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("30-89 Day Past Due Rate","Noncurrent Rate","Net Charge-Off Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*.06), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LOAN_PERFORMANCE_Graph, "Loan Performance Ratio.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Lending Graphs

CI_LARGE_MIDDLE <- fredr(series_id = "DRTSCILM",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
CI_SMALL <- fredr(series_id = "DRTSCIS",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)


CI_LOAN_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=CI_LARGE_MIDDLE, aes(x=date,y= value/100,color= "To Large and Midsize Firms"), size = 1.25)+ 
  geom_line(data=CI_SMALL, aes(x=date,y= value/100,color= "To Small Firms"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.40,-.20,0,.20,.40,.60,.80,1), limits = c(-.40,1), expand = c(0,0)) +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Banks are Dramatically Tightening Lending Standards for Businesses") +
  theme_apricitas + theme(legend.position = c(.465,.85)) +
  scale_color_manual(name= "Net Percentage of Domestic Banks Tightening, Commercial & Industrial Loans",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.40-(.3*1.40), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CI_LOAN_Graph, "CI Loan Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CON_CC <- fredr(series_id = "DRTSCLCC",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
CON_AUTO <- fredr(series_id = "STDSAUTO",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
CON_EX_AUTO_CC <- fredr(series_id = "STDSOTHCONS",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

CON_LOAN_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=CON_CC, aes(x=date,y= value/100,color= "Credit Card Loans"), size = 1.25)+ 
  geom_line(data=CON_AUTO, aes(x=date,y= value/100,color= "Auto Loans"), size = 1.25)+ 
  geom_line(data=CON_EX_AUTO_CC, aes(x=date,y= value/100,color= "Other Consumer Loans"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.40,-.20,0,.20,.40,.60,.80,1), limits = c(-.40,1), expand = c(0,0)) +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Banks are Slowly Tightening Lending Standards for Consumers") +
  theme_apricitas + theme(legend.position = c(.475,.84)) +
  scale_color_manual(name= "Net Percentage of Domestic Banks Tightening Standards, Consumer Loans",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.40-(.3*1.40), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CON_LOAN_Graph, "CON Loan Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

COM_MF_RES <- fredr(series_id = "SUBLPDRCSM",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
COM_NF_NON_RES <- fredr(series_id = "SUBLPDRCSN",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
COM_CON_LAND <- fredr(series_id = "SUBLPDRCSC",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

COM_LOAN_Graph <- ggplot() + #plotting net tightening data
  geom_line(data=COM_MF_RES, aes(x=date,y= value/100,color= "Secured by Multifamily Residential Structures"), size = 1.25)+ 
  geom_line(data=COM_NF_NON_RES, aes(x=date,y= value/100,color= "Secured by Nonfarm Nonresidential Structures"), size = 1.25)+ 
  geom_line(data=COM_CON_LAND, aes(x=date,y= value/100,color= "With Construction and Land Development Purposes"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.40,-.20,0,.20,.40,.60,.80,1), limits = c(-.40,1), expand = c(0,0)) +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Banks are Rapidly Tightening Their Lending Standards for Commercial Real Estate") +
  theme_apricitas + theme(legend.position = c(.5,.84)) +
  scale_color_manual(name= "Net Percentage of Domestic Banks Tightening Standards, Commercial Real Estate",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-10-01")-(.1861*(today()-as.Date("2013-10-01"))), xmax = as.Date("2013-10-01")-(0.049*(today()-as.Date("2013-10-01"))), ymin = -0.40-(.3*1.40), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COM_LOAN_Graph, "COM Loan Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Commercial and Industrial Loans
CI <- fredr(series_id = "TOTCI",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
CI_QUARTERLY <- fredr(series_id = "QBPBSTASCOMLN",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)
PPP <- fredr(series_id = "BOGZ1FL313170103Q",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

CI_LESS_PPP <- merge(CI_QUARTERLY,PPP,by = "date")
  
CI_LOANS_PPP_Graph <- ggplot() + #plotting bank consumer loan data
  geom_line(data=CI, aes(x=date,y= value/1000,color= "Commercial and Industrial Loans"), size = 1.25) + 
  geom_line(data=CI_LESS_PPP, aes(x=date+45,y= (value.x-value.y)/1000000,color= "Commercial and Industrial Loans, Excluding PPP, Quarterly"), size = 1.25) + 
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, suffix = "T"), breaks = c(0,0.5,1,1.5,2,2.5,3,3.5), limits = c(0,3.5), expand = c(0,0)) +
  ggtitle("Back to Borrowing") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and FDIC data", subtitle = "Commercial and Industrial Loan Levels are Rapidly Increasing") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= "US Commercial Bank Assets",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-07")-(.1861*(today()-as.Date("2000-01-07"))), xmax = as.Date("2000-01-07")-(0.049*(today()-as.Date("2000-01-07"))), ymin = 0-(.3*3.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CI_LOANS_PPP_Graph, "CI Loans ex PPP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#LOOK HERE FOR DATA ON DELINQUENCY RATES FOR ALL BANKS
#https://fred.stlouisfed.org/release/tables?rid=231&eid=148470#snid=148477

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
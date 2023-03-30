pacman::p_load(dplyr,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CS <- tq_get("CS", from = "2007-04-25")

CS_TOTAL_RETURN_graph <- ggplot() + #plotting loan performance data
  geom_line(data=CS, aes(x=date,y= (adjusted/adjusted[1]*100)/100-1,color= "Credit Suisse (CS), Total Return Since April 2007"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Total Return, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,-.25,-.5,-.75,-1.00), limits = c(-1.00,0), expand = c(0,0)) +
  ggtitle("The Death of Credit Suisse") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "Before Being Sold to UBS, Credit Suisse's Financial Struggles Had Been Going On for Years") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-04-25")-(.1861*(today()-as.Date("2007-04-25"))), xmax = as.Date("2007-04-25")-(0.049*(today()-as.Date("2007-04-25"))), ymin = -1-(.3*1), ymax = -1.00) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CS_TOTAL_RETURN_graph, "CS Total Return Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

AT1_ETF <- tq_get("AT1.L", from = "2023-03-01")

AT1_ETF_TOTAL_RETURN_graph <- ggplot() + #plotting loan performance data
  geom_line(data=AT1_ETF, aes(x=date,y= (adjusted/adjusted[1]*100)/100-1,color= "Invesco European AT1 Capital USD Bond ETF"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Total Return, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,-0.05,-0.10,-0.15,-.20), limits = c(-.20,0.01), expand = c(0,0)) +
  ggtitle("Additional Problems") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "Investors Have Soured on European AT1 Capital Bonds in the Wake of Credit Suisse's Fall") +
  theme_apricitas + theme(legend.position = c(.4,.10)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-03-01")-(.1861*(today()-as.Date("2023-03-01"))), xmax = as.Date("2023-03-01")-(0.049*(today()-as.Date("2023-03-01"))), ymin = -.20-(.3*0.21), ymax = -.20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AT1_ETF_TOTAL_RETURN_graph, "AT1 Total Return Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

FOREIGN_REPO <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=6469d791421717c45c635898573f771f&lastobs=&from=01/01/2019&to=12/31/2023&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","value")) %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric)

ggsave(dpi = "retina",plot = SWAPS_graph, "SWAPS Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

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

ggsave(dpi = "retina",plot = FOREIGN_REPO_graph, "Foreign Repo Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

CS_FINANCIALS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Credit%20Suisse/CS_FINANCIALS.csv") %>%
  mutate(Pretax_Net_Income = as.numeric(gsub(",","",Pretax_Net_Income))) %>%
  mutate(Customer_Deposits = as.numeric(gsub(",","",Customer_Deposits))) %>%
  mutate(Customer_Deposits_Change = Customer_Deposits - lag(Customer_Deposits)) %>%
  mutate(AUM = as.numeric(gsub(",","",AUM))) %>%
  mutate(Date = as.Date(Date))

CS_Pretax_Net_Income_graph <- ggplot(data = CS_FINANCIALS, aes(x = Date, y = Pretax_Net_Income/1000, fill = "Credit Suisse Pretax Net Income, Quarterly")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Swiss Francs") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.5, prefix = "CHF ", suffix = "B"), breaks = c(-1.50,-1,-0.5,0,0.5,1,1.5), limits = c(-1.75,1.75), expand = c(0,0)) +
  ggtitle("Debit Suisse") +
  labs(caption = "Graph created by @JosephPolitano using Credit Suisse Regulatory Filings", subtitle = "Credit Suisse Has Been Losing Money For Most of 2021 and 2022") +
  theme_apricitas + theme(legend.position = c(.4,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -1.75-(.3*3.5), ymax = -1.75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CS_Pretax_Net_Income_graph, "CS Pretax Income Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

CS_NET_NEW_ASSETS_graph <- ggplot(data = CS_FINANCIALS, aes(x = Date, y = NET_NEW_ASSETS, fill = "Credit Suisse Net New Assets Under Management, Quarterly")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Swiss Francs") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "CHF ", suffix = "B"), breaks = c(-120,-80,-40,0,40), limits = c(-140,40), expand = c(0,0)) +
  ggtitle("Debit Suisse") +
  labs(caption = "Graph created by @JosephPolitano using Credit Suisse Regulatory Filings", subtitle = "Credit Suisse Lost More Than CHF 110B in AUM to Withdrawls in Q4 2022") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -140-(.3*180), ymax = -140) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CS_NET_NEW_ASSETS_graph, "CS Net New Assets Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

CS_NET_CUSTOMER_DEPOSITS_graph <- ggplot(data = CS_FINANCIALS, aes(x = Date, y = Customer_Deposits_Change/1000, fill = "Credit Suisse Net Change in Customer Deposits, Quarterly")) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Swiss Francs") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "CHF ", suffix = "B"), breaks = c(-120,-80,-40,0,40), limits = c(-140,40), expand = c(0,0)) +
  ggtitle("Debit Suisse") +
  labs(caption = "Graph created by @JosephPolitano using Credit Suisse Regulatory Filings", subtitle = "Credit Suisse Lost Almost CHF 140B in Deposits to Withdrawls in Q4 2022") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -140-(.3*180), ymax = -140) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CS_NET_CUSTOMER_DEPOSITS_graph, "CS Net Customer Deposits Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

SNB_FOREIGN_BIG_DEP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Credit%20Suisse/FOREIGN_DEP_SNB_DATA.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

SNB_FOREIGN_BIG_DEP_graph <- ggplot() + #plotting Deposits, Insured and Uninsured
  geom_line(data = SNB_FOREIGN_BIG_DEP, aes(x = date, y = Foreign_BIG_Dep_CHF/1000000, color = "Foreign Deposits at Large Banks in Switzerland"), size = 1.25) +
  geom_line(data = SNB_FOREIGN_BIG_DEP, aes(x = date, y = Dollar_Dep_Dom_For/1000000, color = "Dollar Deposits at Large Banks in Switzerland"), size = 1.25) +
  xlab("Date") +
  ylab("Billions of Swiss Francs") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, prefix = "CHF ", suffix = "B"), breaks = c(0,50,100,150), limits = c(0,150), expand = c(0,0)) +
  ggtitle("Debit Suisse") +
  labs(caption = "Graph created by @JosephPolitano using SNB Data", subtitle = "Foreign Depositors Withdrew CHF 58B From Major Swiss Banks Over the Last 9 Months") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Foreign Deposits at Large Banks in Switzerland","Dollar Deposits at Large Banks in Switzerland")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*150), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SNB_FOREIGN_BIG_DEP_graph, "SNB Customer Deposits Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

GSIB_data <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Credit%20Suisse/BIS_DATA.csv")

GSIB_data_graph <- ggplot() + #plotting Deposits, Insured and Uninsured
  geom_point(data = GSIB_data, aes(x = Cross_Jur, y = Intra_fin, color = category, size = size)) +
  xlab("Cross Jurisdictional Assets & Liabilities / Total Exposures") +
  ylab("Intra-Financial System Assets & Liabilities / Total Exposures") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("Over Exposed") +
  labs(caption = "Graph created by @JosephPolitano using BIS Data", subtitle = "Among G-SIBs, Credit Suisse and UBS Have More International and Intrafinancial Exposures") +
  theme_apricitas + theme(legend.position = c(.85,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Credit Suisse","UBS","G-SIBs")) +
  scale_size(guide = "none") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*150), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GSIB_data_graph, "GSIB data Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

FOREIGN_REPO <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H41&series=6469d791421717c45c635898573f771f&lastobs=&from=01/01/2019&to=12/31/2023&filetype=csv&label=omit&layout=seriescolumn") %>%
  .[-1,] %>%
  `colnames<-`(c("date","value")) %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric)

ggsave(dpi = "retina",plot = SWAPS_graph, "SWAPS Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

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

ggsave(dpi = "retina",plot = FOREIGN_REPO_graph, "Foreign Repo Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
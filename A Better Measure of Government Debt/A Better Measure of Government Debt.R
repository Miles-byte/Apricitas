pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes)

govdebt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/govdebt.csv")
fyoint <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/FYOINT.csv")
DAMonthly <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/Debt+Assets_Monthly.csv")
DAQuarterly <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/Debt+Assets_Quarterly.csv")
TotalAssets <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/TotalGovAssets.csv")
FedNet <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/FGNETWQ027S.csv")

JPNNGDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/JPNNGDP.csv")
JPNDebtQE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/Japan_Debt_QE.csv")

EnglandDebt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/England_Debt.csv")

EU_Debt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/EU_Debt_Data.csv")

EU_Debt$DATE <- as.Date(EU_Debt$DATE, "%Y-%d-%m") #forcing date
EU_Debt$DATE <- as.IDate(EU_Debt$DATE) #forcing date
EU_Debt[,2:6] <- sapply(EU_Debt[,2:6],as.numeric) #forcing numeric
EU_Debt$CPMNACSCAB1GQIT <- EU_Debt$CPMNACSCAB1GQIT*4 #annualizing quarterly gdp
EU_Debt$CPMNACSCAB1GQDE <- EU_Debt$CPMNACSCAB1GQDE*4

EnglandDebt$Date <- as.Date(EnglandDebt$Date, "%d/%m/%Y") #forcing date
EnglandDebt$Date <- as.IDate(EnglandDebt$Date)
EnglandDebt[,2:10] <- sapply(EnglandDebt[,2:10],as.numeric)

JPNNGDP$DATE <- as.Date(JPNNGDP$DATE) #forcing date
JPNNGDP$DATE <- as.IDate(JPNNGDP$DATE)

JPNDebtQE$Name.of.time.series <- paste0("01/",JPNDebtQE$Name.of.time.series) #adding day
JPNDebtQE$Name.of.time.series <- as.Date(JPNDebtQE$Name.of.time.series, "%d/%Y/%m") #forcing date
JPNDebtQE$DATE <- as.IDate(JPNDebtQE$Name.of.time.series)


DAQuarterly$DATE <- as.Date(DAQuarterly$DATE, "%m/%d/%Y") #forcing date
DAQuarterly$DATE <- as.IDate(DAQuarterly$DATE)

DAQuarterly$DATE <- as.Date(DAQuarterly$DATE, "%m/%d/%Y") #forcing date
DAQuarterly$DATE <- as.IDate(DAQuarterly$DATE)
DAQuarterly$BOGZ1FL892090005Q <- as.numeric(DAQuarterly$BOGZ1FL892090005Q) 



FedNet$DATE <- as.Date(FedNet$DATE, "%m/%d/%Y") #forcing date
FedNet$DATE <- as.IDate(FedNet$DATE)
FedNet$FGNETWQ027S <- as.numeric(FedNet$FGNETWQ027S) 

TotalAssets$DATE <- as.Date(TotalAssets$DATE, "%m/%d/%Y") #forcing date
TotalAssets$DATE <- as.IDate(TotalAssets$DATE)
TotalAssets$FGTNILQ027S <- as.numeric(TotalAssets$FGTNILQ027S) 

DAMonthly$DATE <- as.Date(DAMonthly$DATE) #forcing date
DAMonthly$DATE <- as.IDate(DAMonthly$DATE)

govdebt$X <- as.Date(govdebt$X, "%m/%d/%Y") #forcing date
govdebt$X <- as.IDate(govdebt$X)

fyoint$FRBREMIT <- gsub(",","",fyoint$FRBREMIT)#removing commas from FRBREMIT

fyoint$FRBREMIT <- as.numeric(fyoint$FRBREMIT)#forcing Fed remittances and GDP numbers to be numeric instead of char
fyoint$GDP <- as.numeric(fyoint$GDP)
fyoint$DATE <- as.Date(fyoint$DATE, "%m/%d/%Y") #forcing date
fyoint$DATE <- as.IDate(fyoint$DATE)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

InterestPct <- ggplot() + #plotting interest as a %of GDP
  geom_line(data=fyoint, aes(x=DATE,y=OINT/GDP,color= "Interest"), size = 1.25) +
  geom_line(data=fyoint, aes(x=DATE,y=(OINT-FRBREMIT)/GDP,color= "Interest Less Fed Remittances"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1947-01-01"),as.IDate("2020-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), expand = c(0,0),limits = c(0,0.035)) +
  ylab("Percent of GDP") +
  ggtitle("The Simplest and Best Way to Measure the National Debt") +
  labs(caption = "Graph created by @JosephPolitano using OMB and BEA data",subtitle = "Net Interest Expenses to GDP is the Clearest Measure of Consolidated Government Liabilities") +
  theme_apricitas + theme(legend.position = c(.85,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))


fyoint$DATE <- FLOOR_YEAR(fyoint$DATE) #here I am converting the dates to Jan 1 of each year to make the merge works. FYOINT is by FY but govdebt is by calendar yr
fyoint$DATE <- as.IDate(fyoint$DATE)

fyoint[nrow(fyoint), 1] = "2021-01-01" #2021 data broke for this csv, manually adding it here
fyoint[nrow(fyoint), 4] = as.numeric("22061503")

MergedDebtGDP <- merge(govdebt, fyoint, by.x = "X", by.y = "DATE") #merging the dallas govdebt df with the interest df to calculate debt/gdp measures
MergedDebtGDPRes <- merge(MergedDebtGDP, DAMonthly, by.x = "X", by.y = "DATE") #merging for reserves
MergedDebtGDPRes <- merge(MergedDebtGDPRes, TotalAssets, by.x = "X", by.y = "DATE") #merging with FEDNET Liabilities 
MergedDebtGDPRes <- merge(MergedDebtGDPRes, DAQuarterly, by.x = "X", by.y = "DATE") #merging with US wealth
MergedDebtAssets <- merge(MergedDebtGDP, TotalAssets, by.x = "X", by.y = "DATE") #merging with total federal assets for fed assets/liabilities graph
MergedDebtAssets <- merge(MergedDebtAssets, FedNet, by.x = "X", by.y = "DATE") #merging with FEDNET Liabilities 


DebtGDP <- ggplot() + #plotting interest as a %of GDP
  geom_line(data=MergedDebtGDP, aes(x=X,y=Gross.federal.debt.par*1000/GDP,color= "Gross Federal Debt/GDP"), size = 1.25) +
  geom_line(data=MergedDebtGDP, aes(x=X,y=Marketable.Treasury.debt.par*1000/GDP,color= "Marketable Debt/GDP"), size = 1.25) +
  geom_line(data=MergedDebtGDP, aes(x=X,y=Privately.held.gross.federal.debt.par*1000/GDP,color= "Privately Held Debt/GDP"), size = 1.25) +
  geom_line(data=MergedDebtGDPRes, aes(x=X,y=(Privately.held.gross.federal.debt.par+TOTRESNS)*1000/GDP.x,color= "Privately Held Debt & Reserves/GDP"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1947-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.25,1.35),  breaks = c(-.25,0,0.25,0.50,0.75,1,1.25), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Properly Counting the National Debt") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed data",subtitle = "Excluding Intragovernmental Debt and Fed Holdings Changes the Picture") +
  theme_apricitas + theme(legend.position = c(.60,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))

DebtNetAssets <- ggplot() + #plotting federal assets net liabilities
  geom_line(data=MergedDebtAssets, aes(x=X,y=(((Gross.federal.debt.par*1000)-FGTNILQ027S)/GDP),color= "Gross Federal Debt Net Federal Assets/GDP"), size = 1.25) +
  geom_line(data=MergedDebtAssets, aes(x=X,y=((Marketable.Treasury.debt.par*1000)-FGTNILQ027S)/GDP,color= "Marketable Debt Net Federal Assets/GDP"), size = 1.25) +
  geom_line(data=MergedDebtAssets, aes(x=X,y=((Privately.held.gross.federal.debt.par*1000)-FGTNILQ027S)/GDP,color= "Privately Held Debt Net Federal Assets/GDP"), size = 1.25) +
  geom_line(data=MergedDebtGDPRes, aes(x=X,y=(((Privately.held.gross.federal.debt.par+TOTRESNS))*1000-FGTNILQ027S)/GDP.x,color= "Privately Held Debt & Reserves Net Federal Assets/GDP"), size = 1.25) +
  geom_line(data=MergedDebtAssets, aes(x=X,y=(-FGNETWQ027S)/GDP,color= "Gross Net Federal Liabilities/GDP"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1952-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.25,1.35), breaks = c(-.25,0,0.25,0.50,0.75,1,1.25), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Properly Counting the National Debt") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "Accounting for the Federal Government's Assets Provides Important Context") +
  theme_apricitas + theme(legend.position = c(.50,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#9A348E","#00A99D","#EE6055","#A7ACD9"))


MergedDebtWealth <- merge(MergedDebtAssets, DAQuarterly, by.x = "X", by.y = "DATE") #merging with FEDNET Liabilities 


DebtNetAssetsStock <- ggplot() + #plotting interest as a %of GDP
  geom_line(data=MergedDebtWealth, aes(x=X,y=(((Gross.federal.debt.par*1000)-FGTNILQ027S)/(BOGZ1FL892090005Q*1000)),color= "Gross Federal Debt Net Federal Assets/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtWealth, aes(x=X,y=((Marketable.Treasury.debt.par*1000)-FGTNILQ027S)/(BOGZ1FL892090005Q*1000),color= "Marketable Debt Net Federal Assets/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtWealth, aes(x=X,y=((Privately.held.gross.federal.debt.par*1000)-FGTNILQ027S)/(BOGZ1FL892090005Q*1000),color= "Privately Held Debt Net Federal Assets/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtGDPRes, aes(x=X,y=(((Privately.held.gross.federal.debt.par+TOTRESNS))*1000-FGTNILQ027S)/(BOGZ1FL892090005Q*1000),color= "Privately Held Debt & Reserves Net Federal Assets/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtWealth, aes(x=X,y=(-FGNETWQ027S)/(BOGZ1FL892090005Q*1000),color= "Gross Net Federal Liabilities/Wealth"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1952-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.1,.3), breaks = c(-.1,0,.1,.2,.3), expand = c(0,0)) +
  ylab("Percent of National Wealth") +
  ggtitle("Comparing National Debt to National Wealth") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "A Stock-to-Stock Measure of the National Debt, Adjusted for the Federal Government's Assets") +
  theme_apricitas + theme(legend.position = c(.3,.83)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#9A348E","#00A99D","#EE6055","#A7ACD9"))

DebtWealth <- ggplot() + #plotting interest as a %of wealth
  geom_line(data=MergedDebtWealth, aes(x=X,y=(((Gross.federal.debt.par*1000))/(BOGZ1FL892090005Q*1000)),color= "Gross Federal Debt/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtWealth, aes(x=X,y=((Marketable.Treasury.debt.par*1000))/(BOGZ1FL892090005Q*1000),color= "Marketable Debt/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtWealth, aes(x=X,y=((Privately.held.gross.federal.debt.par*1000))/(BOGZ1FL892090005Q*1000),color= "Privately Held Debt/Wealth"), size = 1.25) +
  geom_line(data=MergedDebtGDPRes, aes(x=X,y=(((Privately.held.gross.federal.debt.par+TOTRESNS))*1000)/(BOGZ1FL892090005Q*1000),color= "Privately Held Debt & Reserves/Wealth"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1952-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.1,.3), breaks = c(-.1,0,.1,.2,.3), expand = c(0,0)) +
  ylab("Percent of National Wealth") +
  ggtitle("Comparing National Debt to National Wealth") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "A Stock-to-Stock Measure of the National Debt") +
  theme_apricitas + theme(legend.position = c(.3,.83)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"))


JPNDebtGDP <- merge(JPNDebtQE, JPNNGDP, by.x = "Name.of.time.series", by.y = "DATE") #merging with FEDNET Liabilities 


JapanEUDebtGDP <- ggplot() + #plotting privately held debt for US,UK,JPN,GER,ITA
  geom_line(data=EnglandDebt, aes(x=Date+365,y=DebtGdp - BoEGDP,color= "UK"), size = 1.25) + #adding 365 because England data is EOY
  geom_line(data=JPNDebtGDP, aes(x=DATE,y=(National.Government.Debt.Total-X_By.Holder.and.Lender.Bank.of.Japan)/(JPNNGDP*10),color= "Japan"), size = 1.25) +
  geom_line(data=MergedDebtGDP, aes(x=X,y=Privately.held.gross.federal.debt.par*1000/GDP,color= "USA"), size = 1.25) +
  geom_line(data=EU_Debt, aes(x=DATE+365, y=GGGDTADEA188N/100-((ECBASSETSW*.2636)/(CPMNACSCAB1GQDE)), color = "Germany"), size = 1.25) + #=365 normalizes the date to EOY.Debt is divided by 100 to normalize to percent Multiplying by .26/.16 gives their share of ECB assets
  geom_line(data=EU_Debt, aes(x=DATE+365, y=GGGDTAITA188N/100-((ECBASSETSW*.1698)/(CPMNACSCAB1GQIT)), color = "Italy"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1990-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,2),  breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Debt Minus Central Bank Assets/GDP for Various Countries") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed, BoJ, and BoE data",subtitle = "Accounting for Central Bank Holdings Changes the Picture") +
  theme_apricitas + theme(legend.position = c(.90,.85)) +
  scale_color_manual(name= NULL,values = c("#9A348E","#A7ACD9","#EE6055","#00A99D","#FFE98F"))


USUKDebtGDP <- ggplot() + #plotting US and UK privately held Debt/GDP
  geom_line(data=MergedDebtGDP, aes(x=X,y=Privately.held.gross.federal.debt.par*1000/GDP,color= "USA"), size = 1.25) +
  geom_line(data=EnglandDebt, aes(x=Date+365,y=DebtGdp - BoEGDP,color= "UK"), size = 1.25) + #adding 365 because England data is EOY
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1952-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,2),  breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Privately Held Debt/GDP in the US and UK") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed and BoE data",subtitle = "Excluding Central Bank Holdings Changes the Picture") +
  theme_apricitas + theme(legend.position = c(.60,.70)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))

USUKInterestGDP <- ggplot() + #plotting US and UK privately held Debt/GDP
  geom_line(data=fyoint, aes(x=DATE,y=(OINT-FRBREMIT)/GDP,color= "USA"), size = 1.25) +
  geom_line(data=EnglandDebt, aes(x=Date+365,y=IntAPFGDP,color= "UK"), size = 1.25) + #adding 365 because England data is EOY
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1947-01-01"),as.IDate("2020-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.045),  breaks = c(0,.01,.02,.03,0.04,0.05), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Interest Expenses Net Central Bank Remittances as a % of GDP") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed and BoE data",subtitle = "Comparing the Cost to Service the Debt") +
  theme_apricitas +#+ theme(legend.position = c(.60,.70)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))


ggsave(dpi = "retina",plot = InterestPct, "InterestPct.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = DebtGDP, "DebtGDP.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = DebtNetAssets, "DebtNetAssets.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = DebtWealth, "DebtWealth.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = DebtNetAssetsStock, "DebtNetAssetsStock.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = JapanEUDebtGDP, "JPNEUDebtGDP.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = USUKDebtGDP, "USUKDebtGDP.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = USUKInterestGDP, "USUKInterestGDP.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

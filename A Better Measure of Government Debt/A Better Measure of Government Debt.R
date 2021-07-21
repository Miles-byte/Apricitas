pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes)

govdebt <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/govdebt.csv")
fyoint <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/FYOINT.csv")
DAMonthly <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/Debt+Assets_Monthly.csv")
DAQuarterly <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/A%20Better%20Measure%20of%20Government%20Debt/Debt+Assets_Quarterly.csv")

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
  ggtitle("Interest as a Percent of GDP Was at Historical Lows") +
  labs(caption = "Graph created by @JosephPolitano using OMB and BEA data",subtitle = "When Accounting for Fed Remittances, Interest Costs on the Federal Debt Bottomed Out in 2017") +
  theme_apricitas + theme(legend.position = c(.85,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))


fyoint$DATE <- FLOOR_YEAR(fyoint$DATE) #here I am converting the dates to Jan 1 of each year to make the merge works. FYOINT is by FY but govdebt is by calendar yr
fyoint$DATE <- as.IDate(fyoint$DATE)

fyoint[nrow(fyoint), 1] = "2021-01-01" #2021 data broke for this csv, manually adding it here
fyoint[nrow(fyoint), 4] = as.numeric("22061503")


MergedDebtGDP <- merge(govdebt, fyoint, by.x = "X", by.y = "DATE") #merging the dallas govdebt df with the interest df to calculate debt/gdp measures
DoubleMergedDebtGDP <- merge(MergedDebtGDP, DAMonthly, by.x = "X", by.y = "DATE")



DebtGDP <- ggplot() + #plotting interest as a %of GDP
  geom_line(data=MergedDebtGDP, aes(x=X,y=Gross.federal.debt.par*1000/GDP,color= "Gross Federal Debt/GDP"), size = 1.25) +
  geom_line(data=MergedDebtGDP, aes(x=X,y=Marketable.Treasury.debt.par*1000/GDP,color= "Marketable Debt/GDP"), size = 1.25) +
  geom_line(data=MergedDebtGDP, aes(x=X,y=Privately.held.gross.federal.debt.par*1000/GDP,color= "Privately Held Debt/GDP"), size = 1.25) +
  geom_line(data=DoubleMergedDebtGDP, aes(x=X,y=(Privately.held.gross.federal.debt.par+TOTRESNS)*1000/GDP,color= "Privately Held Debt & Reserves/GDP"), size = 1.25) +
  xlab("Date") +
  scale_x_date(limits = c(as.IDate("1947-01-01"),as.IDate("2021-01-01"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1.35), breaks = c(0,0.25,0.50,0.75,1,1.25), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Properly Counting the National Debt") +
  labs(caption = "Graph created by @JosephPolitano using Dallas Fed data",subtitle = "Excluding Intergovernmental Debt and Fed Holdings Changes the Picture") +
  theme_apricitas + theme(legend.position = c(.63,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))

ggsave(dpi = "retina",plot = InterestPct, "InterestPct.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = DebtGDP, "DebtGDP.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

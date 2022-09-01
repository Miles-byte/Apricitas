pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(DSPI$date, n=1), "months")), trend = 16622.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(DSPI$date, n=1), by = 'month')) - 1))) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(POUT$date, n=1), "months")), trend = 15328.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(POUT$date, n=1), by = 'month')) - 1)))

Corporate_Savings <- fredr(series_id = "B057RC1Q027SBEA",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
PSAVert <- fredr(series_id = "PSAVert",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays

DFA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/dfa-income-levels-detail.csv")

DFA$Date <- as.Date(as.yearqtr(DFA$Date, format = "%Y:Q%q")) #converting DFA date
DFA$Category <- gsub("pct99to100", "Top 1%", DFA$Category)
DFA$Category <- gsub("pct80to99", "80-99%", DFA$Category)
DFA$Category <- gsub("pct60to80", "60-80%", DFA$Category)
DFA$Category <- gsub("pct40to60", "40-60%", DFA$Category)
DFA$Category <- gsub("pct20to40", "20-40%", DFA$Category)
DFA$Category <- gsub("pct00to20", "Bottom 20%", DFA$Category)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

DSPImerge <- merge(DSPI, DSPITrend, by = "date")
POUTmerge <- merge(POUT, POUTTrend, by = "date")
CUMSUMDSPImerge <- DSPImerge
CUMSUMPOUTmerge <- POUTmerge
CUMSUMDSPImerge$value <- cumsum(CUMSUMDSPImerge$value/12)
CUMSUMDSPImerge$trend <- cumsum(CUMSUMDSPImerge$trend/12)
CUMSUMPOUTmerge$value <- cumsum(CUMSUMPOUTmerge$value/12)
CUMSUMPOUTmerge$trend <- cumsum(CUMSUMPOUTmerge$trend/12)

Taxes <- fredr(series_id = "W055RC1",observation_start = as.Date("2018-01-01")) 
#3.50% annual growth rate trend
TaxesTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 2251.3*1.002871^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
Taxesmerge <- merge(Taxes, TaxesTrend, by = "date")
Taxesmerge$value <- cumsum(Taxesmerge$value/12)
Taxesmerge$trend <- cumsum(Taxesmerge$trend/12)
Taxesmerge <- data.frame(date = Taxesmerge$date, savings = Taxesmerge$value-Taxesmerge$trend)

GovInsurance <- fredr(series_id = "A061RC1",observation_start = as.Date("2018-01-01"))
#4.50% annual growth rate trend
GovInsuranceTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 1474.9*1.003675^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
GovInsurancemerge <- merge(GovInsurance, GovInsuranceTrend, by = "date")
GovInsurancemerge$value <- cumsum(GovInsurancemerge$value/12)
GovInsurancemerge$trend <- cumsum(GovInsurancemerge$trend/12)
GovInsurancemerge <- data.frame(date = GovInsurancemerge$date, savings = GovInsurancemerge$value-GovInsurancemerge$trend)

TaxesGovInsurancemerge <- data.frame(date = GovInsurancemerge$date, savings = Taxesmerge$savings + GovInsurancemerge$savings)

Compensation <- fredr(series_id = "W209RC1",observation_start = as.Date("2018-01-01"))
#4.4% annual growth rate trend
CompensationTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 11790.9*1.003595^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
Compensationmerge <- merge(Compensation, CompensationTrend, by = "date")
Compensationmerge$value <- cumsum(Compensationmerge$value/12)
Compensationmerge$trend <- cumsum(Compensationmerge$trend/12)
Compensationmerge <- data.frame(date = Compensationmerge$date, savings = Compensationmerge$value-Compensationmerge$trend)


Proprietor <- fredr(series_id = "A041RC1",observation_start = as.Date("2018-01-01"))
#2.6% annual growth rate trend
ProprietorTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 1656.3*1.002141^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
Proprietormerge <- merge(Proprietor, ProprietorTrend, by = "date")
Proprietormerge$value <- cumsum(Proprietormerge$value/12)
Proprietormerge$trend <- cumsum(Proprietormerge$trend/12)
Proprietormerge <- data.frame(date = Proprietormerge$date, savings = Proprietormerge$value-Proprietormerge$trend)

Int_Dividend <- fredr(series_id = "PIROA",observation_start = as.Date("2018-01-01"))
#4.3% annual growth rate trend
Int_DividendTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 2984.6*1.003515^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
Int_Dividendmerge <- merge(Int_Dividend, Int_DividendTrend, by = "date")
Int_Dividendmerge$value <- cumsum(Int_Dividendmerge$value/12)
Int_Dividendmerge$trend <- cumsum(Int_Dividendmerge$trend/12)
Int_Dividendmerge <- data.frame(date = Int_Dividendmerge$date, savings = Int_Dividendmerge$value-Int_Dividendmerge$trend)

ProprietorInt_Dividendmerge <- data.frame(date = GovInsurancemerge$date, savings = Int_Dividendmerge$savings + Proprietormerge$savings)


TransferReceived <- fredr(series_id = "PCTR",observation_start = as.Date("2018-01-01"))
#4.00% annual growth rate trend
TransferReceivedTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 3208.8*1.003274^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
TransferReceivedmerge <- merge(TransferReceived, TransferReceivedTrend, by = "date")
TransferReceivedmerge$value <- cumsum(TransferReceivedmerge$value/12)
TransferReceivedmerge$trend <- cumsum(TransferReceivedmerge$trend/12)
TransferReceivedmerge <- data.frame(date = TransferReceivedmerge$date, savings = TransferReceivedmerge$value-TransferReceivedmerge$trend)


PCE <- fredr(series_id = "PCE",observation_start = as.Date("2018-01-01")) 
#4.2% annual growth rate trend
PCETrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 14769.9*1.003434^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
PCEmerge <- merge(PCE, PCETrend, by = "date")
PCEmerge$value <- cumsum(PCEmerge$value/12)
PCEmerge$trend <- cumsum(PCEmerge$trend/12)
PCEmerge <- data.frame(date = PCEmerge$date, savings = PCEmerge$value-PCEmerge$trend)


InterestPaid <- fredr(series_id = "B069RC1",observation_start = as.Date("2018-01-01"))
#6.3% annual growth rate trend
InterestPaidTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 346.3*1.005104^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
InterestPaidmerge <- merge(InterestPaid, InterestPaidTrend, by = "date")
InterestPaidmerge$value <- cumsum(InterestPaidmerge$value/12)
InterestPaidmerge$trend <- cumsum(InterestPaidmerge$trend/12)
InterestPaidmerge <- data.frame(date = InterestPaidmerge$date, savings = InterestPaidmerge$value-InterestPaidmerge$trend)


TransferPaid <- fredr(series_id = "W211RC1",observation_start = as.Date("2018-01-01")) 
#3.4% annual growth rate trend
TransferPaidTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 212.6*1.002790^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
TransferPaidmerge <- merge(TransferPaid, TransferPaidTrend, by = "date")
TransferPaidmerge$value <- cumsum(TransferPaidmerge$value/12)
TransferPaidmerge$trend <- cumsum(TransferPaidmerge$trend/12)
TransferPaidmerge <- data.frame(date = TransferPaidmerge$date, savings = TransferPaidmerge$value-TransferPaidmerge$trend)

Savings_Component <- data.frame(date = TransferPaidmerge$date,
                                Compensation = Compensationmerge$savings,
                                PCE = -PCEmerge$savings,
                                TransferReceived = TransferReceivedmerge$savings,
                                TaxesGovInsurance = -TaxesGovInsurancemerge$savings,
                                ProprietorInt_Dividend = ProprietorInt_Dividendmerge$savings,
                                InterestPaid = -InterestPaidmerge$savings)#excluding transferpaid because it is so small

Savings_Component <- pivot_longer(Savings_Component, cols = Compensation:InterestPaid)

Savings_Component_Graph <- ggplot() + #plotting components of excess savings
  geom_area(data = Savings_Component, aes(x = date, y = value/1000, fill = name), color = NA, size = 0) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(-1,3.5), breaks = c(-1,0,1,2,3), expand = c(0,0)) +
  ylab("Contribution to Excess Savings, Trillions of Dollars") +
  ggtitle("A Penny Saved...") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Transfers and Spending Cutbacks Buoyed Savings Despite Lower Labor and Capital Income") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED"), breaks = c("TransferReceived","PCE","Compensation","ProprietorInt_Dividend","InterestPaid","TaxesGovInsurance"), labels = c("Government Transfers","Personal Consumption Expenditures","Labor Income","Capital/Proprietors' Income","Interest Payments","Taxes")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*720), xmax = as.Date("2020-01-01")-(0.049*720), ymin = -1-(.3*4.5), ymax = -1) +
  coord_cartesian(clip = "off")

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Personal Income is on Trend, But Consumers Are Spending Down Their Excess Savings") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  scale_fill_manual(name = NULL, values = c("#EE6055","#A7ACD9")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

CUMSUMDSPImerge$total <- (CUMSUMDSPImerge$value-CUMSUMDSPImerge$trend)-(CUMSUMPOUTmerge$value-CUMSUMPOUTmerge$trend) #graphing total excess savings

Cumulative_Savings_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_area(data = CUMSUMDSPImerge, aes(x = date, y = (value-trend)/1000, fill = "Increased Income", color = NULL), size = 0) +
  geom_area(data = CUMSUMPOUTmerge, aes(x = date, y = (value-trend)/1000, fill = "Decreased Spending", color = NULL), size = 0) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(-1.5,1.75), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5), expand = c(0,0)) +
  ylab("Cumulative Deviation from Trend, Trillions of Dollars") +
  ggtitle("A Penny Saved...") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "With Less Spending and More Income, Americans Have Saved Trillions of Dollars") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"), breaks = c("Increased Income", "Decreased Spending")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -1.5-(.3*3.25), ymax = -1.5) +
  coord_cartesian(clip = "off")

Total_Excess_Savings_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = CUMSUMDSPImerge, aes(x = date, y = total/1000, color = "Estimated Excess Savings"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(0,2.5), breaks = c(0.5,1,1.5,2,2.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("Breaking the (Piggy) Bank") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Excess Savings are down $250 Billion as Americans Spend More Money") +
  theme_apricitas + theme(legend.position = c(.25,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = 0-(.3*2.5), ymax = 0) +
  coord_cartesian(clip = "off")

Undistributed_Profits_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = Corporate_Savings, aes(x = date, y = value/1000, color = "Domestic Business: Undistributed Corporate Profits"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(0,1.5), breaks = c(0.5,1,1.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("Rainy Day Funds") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Corporate America is Saving Money Too, Given Pandemic-Induced Risks") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 0-(.3*1.5), ymax = 0) +
  coord_cartesian(clip = "off")

PSAVert_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = PSAVert, aes(x = date, y = value/100, color = "Personal Saving Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.40), breaks = c(0,0.1,.2,.3,.4), expand = c(0,0)) +
  ylab("Personal Saving Rate, %") +
  ggtitle("A Penny Saved...") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Americans' Saving Rate Jumped During the Pandemic") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 0-(.3*.40), ymax = 0) +
  coord_cartesian(clip = "off")

DFAsubset15 <- subset(DFA, Date > as.Date("2015-7-01"))
DFAsubset19 <- subset(DFA, Date > as.Date("2019-7-01"))
DFAsubset19$liquidassets <- DFAsubset19$Checkable.deposts.and.currency + DFAsubset19$Money.market.fund.shares
DFAliquidassets <- select(DFAsubset19,c("Date", "Category", "liquidassets"))
DFAliquidassets <- pivot_wider(DFAliquidassets, names_from = Category, values_from = liquidassets)
DFAconsumercredit <- select(DFAsubset15,c("Date", "Category", "Consumer.credit"))
DFAconsumercredit <- pivot_wider(DFAconsumercredit, names_from = Category, values_from = Consumer.credit)


LiquidAssets_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DFAliquidassets, aes(x = Date, y = `Top 1%`/6793.13, color = "Top 1%"), size = 1.25) +
  geom_line(data = DFAliquidassets, aes(x = Date, y = `80-99%`/15303.94, color = "80-99%"), size = 1.25) +
  geom_line(data = DFAliquidassets, aes(x = Date, y = `60-80%`/4346.91, color = "60-80%"), size = 1.25) +
  geom_line(data = DFAliquidassets, aes(x = Date, y = `40-60%`/2122.56, color = "40-60%"), size = 1.25) +
  geom_line(data = DFAliquidassets, aes(x = Date, y = `20-40%`/1285.36, color = "20-40%"), size = 1.25) +
  geom_line(data = DFAliquidassets, aes(x = Date, y = `Bottom 20%`/1141.88, color = "Bottom 20%"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(90,260), breaks = c(100,150,200,250), expand = c(0,0)) +
  ylab("index, Q4 2019 = 100") +
  ggtitle("The Growing Piggy Bank") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Americans of All Incomes Have More Cash, but Especially the Top 1%") +
  theme_apricitas + theme(legend.position = c(.30,.60)) +
  scale_color_manual(name= "Liquid Financial Assets, Q4 2019 = 100",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC"), breaks = c("Top 1%","80-99%","60-80%","40-60%","20-40%","Bottom 20%")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-10-15")-(.1861*720), xmax = as.Date("2019-10-15")-(0.049*720), ymin = 90-(.3*170), ymax = 90) +
  coord_cartesian(clip = "off")

LiquidAssets_Barplot_Data <- data.frame(incomelvl = c("Top 1%","80-99%","60-80%","40-60%","20-40%","Bottom 20%"),`Change in Liquid Financial Assets` = c(1043172,1105954,487322,283102,114646,15725))
LiquidAssets_Barplot_Data$incomelvl <- factor(LiquidAssets_Barplot_Data$incomelvl,levels = c("Top 1%","80-99%","60-80%","40-60%","20-40%","Bottom 20%"))

LiquidAssets_Barplot_Graph <- ggplot(data = LiquidAssets_Barplot_Data, aes( x = incomelvl, y = Change.in.Liquid.Financial.Assets/1000000, color = NULL)) + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_bar(stat = "identity", fill = "#FFE98F") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.25),limits = c(0,1.25), breaks = c(0.25,.5,.75,1,1.25), expand = c(0,0)) +
  ylab("Excess Liquid Financial Assets, Trillions of Dollars") +
  ggtitle("Savings for Whom?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Vast Majority of Excess Liquid Savings Are Held by the Top 20%") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_x_discrete(name= NULL) +
  annotation_custom(apricitas_logo_rast, xmin = -.40, xmax = .40, ymin = -.30, ymax = 0) +
  coord_cartesian(clip = "off")

DFAconsumercredit_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DFAconsumercredit, aes(x = Date, y = `80-99%`/11574.35, color = "80-99%"), size = 1.25) +
  geom_line(data = DFAconsumercredit, aes(x = Date, y = `60-80%`/11552.69, color = "60-80%"), size = 1.25) +
  geom_line(data = DFAconsumercredit, aes(x = Date, y = `40-60%`/9027.55, color = "40-60%"), size = 1.25) +
  geom_line(data = DFAconsumercredit, aes(x = Date, y = `20-40%`/6106.77, color = "20-40%"), size = 1.25) +
  geom_line(data = DFAconsumercredit, aes(x = Date, y = `Bottom 20%`/3204.06, color = "Bottom 20%"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(),limits = c(70,110), breaks = c(70,80,90,100,110), expand = c(0,0)) +
  ylab("index, Q4 2019 = 100") +
  ggtitle("Cutting Cards") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Consumer Credit Growth Has Slowed Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.80,.40)) +
  scale_color_manual(name= "Consumer Credit, Q4 2019 = 100",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC"), breaks = c("Top 1%","80-99%","60-80%","40-60%","20-40%","Bottom 20%")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 70-(.3*40), ymax = 70) +
  annotate("text", label ="*note: Top 1% excluded due to volatility", x = as.Date("2020-01-01"), y = 75)+
  coord_cartesian(clip = "off")





SHED_2020 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202020.csv")
SHED_2019 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202019.csv")
SHED_2021 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202021.csv")


SHED_2019$ppincimp <- gsub("Less than \\$5,000","<$10k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$5,000 to \\$7,499","<$10k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$7,500 to \\$9,999","<$10k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$10,000 to \\$12,499","$10k-$25k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$12,500 to \\$14,999","$10k-$25k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$15,000 to \\$19,999","$10k-$25k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$20,000 to \\$24,999","$10k-$25k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$25,000 to \\$29,999","$25k-$50k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$30,000 to \\$34,999","$25k-$50k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$35,000 to \\$39,999","$25k-$50k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$40,000 to \\$49,999","$25k-$50k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$50,000 to \\$59,999","$50k-$75k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$60,000 to \\$74,999","$50k-$75k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$75,000 to \\$84,999","$75k-$100k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$85,000 to \\$99,999","$75k-$100k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$100,000 to \\$124,999","$100k-$150k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$125,000 to \\$149,999","$100k-$150k",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$150,000 to \\$174,999","$150k+",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$175,000 to \\$199,999","$150k+",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$200,000 to \\$249,999","$150k+",SHED_2019$ppincimp)
SHED_2019$ppincimp <- gsub("\\$250,000 or more","$150k+",SHED_2019$ppincimp)

SHED_2020$ppincimp <- gsub("Less than \\$5,000","<$10k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$5,000 to \\$7,499","<$10k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$7,500 to \\$9,999","<$10k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$10,000 to \\$12,499","$10k-$25k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$12,500 to \\$14,999","$10k-$25k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$15,000 to \\$19,999","$10k-$25k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$20,000 to \\$24,999","$10k-$25k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$25,000 to \\$29,999","$25k-$50k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$30,000 to \\$34,999","$25k-$50k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$35,000 to \\$39,999","$25k-$50k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$40,000 to \\$49,999","$25k-$50k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$50,000 to \\$59,999","$50k-$75k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$60,000 to \\$74,999","$50k-$75k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$75,000 to \\$84,999","$75k-$100k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$85,000 to \\$99,999","$75k-$100k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$100,000 to \\$124,999","$100k-$150k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$125,000 to \\$149,999","$100k-$150k",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$150,000 to \\$174,999","$150k+",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$175,000 to \\$199,999","$150k+",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$200,000 to \\$249,999","$150k+",SHED_2020$ppincimp)
SHED_2020$ppincimp <- gsub("\\$250,000 or more","$150k+",SHED_2020$ppincimp)


SHED_2019 <- select(SHED_2019, ppincimp, EF1, weight)
SHED_2020 <- select(SHED_2020, ppincimp, EF1, weight)

Emergency_Expenses21 <- crosstab(df = SHED_2021, x = ppinc7, y = EF1, weight = weight,format = "long") #taking crosstabs of EF1 "do you have an emergency fund of 3 months worth of expenses" and ppinc7 "household income" (the codebook was changed for 2021)

Emergency_Expenses20 <- crosstab(df = SHED_2020, x = ppincimp, y = EF1, weight = weight,format = "long") #taking crosstabs of EF1 "do you have an emergency fund of 3 months worth of expenses" and ppinc "household income"

Emergency_Expenses19 <- crosstab(df = SHED_2019, x = ppincimp, y = EF1, weight = weight,format = "long") #taking crosstabs of EF1 "do you have an emergency fund of 3 months worth of expenses" and ppinc "household income"


levels(Emergency_Expenses21$ppinc7) <- list("<$10k" ="Less than $10,000", #renaming household income so it fits on the chart
                                            "$10k-$25k" = "$10,000 to $24,999",
                                            "$25k-$50k" = "$25,000 to $49,999",
                                            "$50k-$75k" = "$50,000 to $74,999",
                                            "$75k-$100k" = "$75,000 to $99,999",
                                            "$100k-$150k" = "$100,000 to $149,999",
                                            "$150k+" = "$150,000 or more")


levels(Emergency_Expenses21$EF1) <- list("No" = "No","Yes"="Yes") #renaming and reordering the answers


levels(Emergency_Expenses20$ppincimp) <- list( "<$10k"="<$10k", #renaming household income so it fits on the chart
                                               "$10k-$25k"="$10k-$25k",
                                               "$25k-$50k"="$25k-$50k",
                                               "$50k-$75k"="$50k-$75k",
                                               "$75k-$100k"="$75k-$100k",
                                               "$100k-$150k"="$100k-$150k",
                                               "$150k+"="$150k+")


levels(Emergency_Expenses20$EF1) <- list( "Refused to Answer" = "Refused","No" = "No","Yes"="Yes" #renaming and reordering the answers
)
levels(Emergency_Expenses19$ppincimp) <- list( "<$10k"="<$10k", #renaming household income so it fits on the chart
                                               "$10k-$25k"="$10k-$25k",
                                               "$25k-$50k"="$25k-$50k",
                                               "$50k-$75k"="$50k-$75k",
                                               "$75k-$100k"="$75k-$100k",
                                               "$100k-$150k"="$100k-$150k",
                                               "$150k+"="$150k+")


levels(Emergency_Expenses19$EF1) <- list( "Refused to Answer" = "Refused","No" = "No","Yes"="Yes" #renaming and reordering the answers
)

Emergency_Expenses_2020_Graph <- ggplot(Emergency_Expenses20, aes(x = ppincimp,pct/100, fill = EF1))+
  geom_bar(stat = "identity", color = NA) +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Do You Have Emergency Funds That Could Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2020") +
  theme_economist() +
  scale_fill_economist(name="")

Emergency_Expenses_2019_Graph <- ggplot(Emergency_Expenses19, aes(x = ppincimp,pct/100, fill = EF1))+
  geom_bar(stat = "identity", color = NA) +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Do You Have Emergency Funds That Could Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2019") +
  theme_economist() +
  scale_fill_economist(name="")

Emergency_Expenses_2021_Graph <- ggplot(Emergency_Expenses21, aes(x = ppinc7,pct/100, fill = EF1))+
  geom_bar(stat = "identity", color = NA) +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Do You Have Emergency Funds That Could Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2021") +
  theme_economist() +
  scale_fill_economist(name="")

Emergency_Expenses19 <- subset(Emergency_Expenses19, EF1 == "Yes")
Emergency_Expenses20 <- subset(Emergency_Expenses20, EF1 == "Yes")
Emergency_Expenses21 <- subset(Emergency_Expenses21, EF1 == "Yes")

Merge1920 <- merge(Emergency_Expenses19,Emergency_Expenses20, by = "ppincimp")
Merge1920 <- select(Merge1920, c(ppincimp, pct.x, pct.y))
colnames(Emergency_Expenses21) <- c("ppincimp", "EF1", "pct", "n")
colnames(Merge1920) <- c("ppincimp", "2019", "2020")
Merge192021 <- merge(Merge1920,Emergency_Expenses21, by = "ppincimp")
Merge192021 <- select(Merge192021, c(ppincimp, "2019", "2020", pct))
colnames(Merge192021) <- c("ppincimp", "2019", "2020", "2021")

Merge192021 <- pivot_longer(Merge192021, cols = c("2019","2020","2021"), names_to = "Year")

Emergency_Expenses_Merge_192021 <- ggplot(data = Merge192021, aes(x = ppincimp, y = value/100, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab("Household Income Bin") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), breaks = c(0,.25,.5,.75,1), expand = c(0,0)) +
  ylab("Percentage") +
  ggtitle("Share of Households With a 3 Month Emergency Fund") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Aggregate Savings Have Improved Financial Security-Marginally") +
  theme_apricitas + theme(legend.position = c(.20,.60), plot.title = element_text(size = 20, color = "white")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")
  
Credit_Cards <- fredr(series_id = "REVOLSL",observation_start = as.Date("2018-01-01")) #downloading credit card balances

FedDeficit <- fredr(series_id = "MTSDS133FMS",observation_start = as.Date("2017-01-01")) #downloading credit card balances
FedDeficit$rollmean <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(FedDeficit$value, 12))

Credit_Cards_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = Credit_Cards, aes(x = date, y = value/1000, color = "Revolving Consumer Credit Owned and Securitized"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.005),limits = c(.960,1.125), breaks = c(.975,1,1.025,1.05,1.075,1.1,1.125), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("Swipe Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Credit Card Balances are Returning to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = .960-(.3*0.160), ymax = .960) +
  coord_cartesian(clip = "off")

FedDeficit <- subset(FedDeficit, date > as.Date("2017-12-31"))

Fed_Deficit_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = FedDeficit, aes(x = date, y = value/1000000, color = "Monthly Federal Deficit/Surplus"), size = 1.25) +
  geom_line(data = FedDeficit, aes(x = date, y = rollmean/1000000, color = "Monthly Federal Deficit/Surplus (12M Moving Average)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.25),limits = c(-1,0.5), breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("Tax and Spend") +
  labs(caption = "Graph created by @JosephPolitano using Treasury data",subtitle = "The Federal Deficit is Rapidly Shrinking") +
  theme_apricitas + theme(legend.position = c(.45,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = -1-(.3*1.5), ymax = -1) +
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Cumulative_Savings_Graph, "Cumulative Savings.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Undistributed_Profits_Graph, "Undistributed Profits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = LiquidAssets_Graph, "Liquid Assets Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = DFAconsumercredit_Graph, "Consumer Credit.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = LiquidAssets_Barplot_Graph, "Liquid Assets.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Savings_Component_Graph, "Savings Component.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PSAVert_Graph, "Personal Savings.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Total_Excess_Savings_Graph, "Total Excess Savings.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Emergency_Expenses_Merge_192021, "Emergency Expenses Merge.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Credit_Cards_Graph, "Credit Card Spending.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Fed_Deficit_Graph , "Fed Deficit.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
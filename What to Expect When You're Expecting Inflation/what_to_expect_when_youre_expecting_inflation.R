#this code is to graph and calculate several metrics of inflation expectations and their accuracy for an article titled "what to expect when you're expecting (inflation)"

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,scale,data.table,lubridate,forecast)

fiveyrbreakeven <-import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Five Year TIPS Breakevens.csv")

tenyrbreakeven <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/10 Year TIPS Breakevens.csv")

fiveyrfiveyrbreakeven <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/5 Yr 5 Yr forward Tips Breakevens.csv")

DKWModel <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/DKW Model.csv")

ClevelandFedModel <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Cleveland Fed Inflation Expectations Model.csv")

Livingston <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Livingston Survey.csv")

SurveyofProfessionals <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Survey of Professional Forecasters.csv")

AruobaModel <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Aruoba Model of Inflation Expectations.csv")

UMich <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/UMich Survey.csv")

CPINSA <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/CPI - Not Seasonally Adjusted.csv")

fiveyrbreakeven$T5YIE <- as.numeric(fiveyrbreakeven$T5YIE) #for some reason FRED data is a character, forcing it to number here

tenyrbreakeven$T10YIE <- as.numeric(tenyrbreakeven$T10YIE) 

fiveyrfiveyrbreakeven$T5YIFR <- as.numeric(fiveyrfiveyrbreakeven$T5YIFR)

ClevelandFedModel$`Model Output Date` <-as.Date(ClevelandFedModel$`Model Output Date`,"%m/%d/%Y") #forcing date on the Cleveland Fed Model - Capital Y calls four digit year, lowercase calls two digit
ClevelandFedModel$`Model Output Date` <-as.IDate(ClevelandFedModel$`Model Output Date`) #forcing integer data now

#adjusting all the survey data here
Livingston <- data.frame("Date"= tail(Livingston$Date,-90),"L10YIE"=tail(Livingston$CPI_10Y,-90)) #livingston didn't start collecting 10yr inflation expectations data until 1990, so I cut out everything until then
Livingston$Date <-as.Date(Livingston$Date,"%m/%d/%Y")#forcing date on the Livingston Data using lubridate
Livingston$Date <-as.IDate(Livingston$Date)#forcing Idate on the Livingston data

SurveyofProfessionals <- data.frame("Date"= tail(SurveyofProfessionals$Date,-87),"PF10YIE"=tail(SurveyofProfessionals$INFCPI10YR,-87))
SurveyofProfessionals$Date <- as.Date(SurveyofProfessionals$Date,"%m/%d/%Y") #forcing date
SurveyofProfessionals$Date <- as.IDate(SurveyofProfessionals$Date) #forcing IDate

UMich <- data.frame("Date"= UMich$Date,"UM5YR"=UMich$Median)
UMich$Date <- as.Date(UMich$Date,"%m/%d/%Y") #forcing date
UMich$Date <- as.IDate(UMich$Date)
#here I'm pulling out the 5yr percentage inflation rate and 10yr percentage inflation rate
CPINSA5yr <- data.frame("DATE" = tail(CPINSA$DATE,-60), "CPI5YRpct" = tail((((CPINSA$CPIAUCNS/lag(CPINSA$CPIAUCNS,n=60))^0.2)-1)*100,-60))# this one is pretty complex - essentially, it is formatting the CPI data to be 5yr annualized inflation rates where the year is the 5yr period that it starts, so that this is directly comparable to the 5yr breakevens

CPINSA10yr <- data.frame("DATE" = tail(CPINSA$DATE,-120), "CPI10YRpct" = tail((((CPINSA$CPIAUCNS/lag(CPINSA$CPIAUCNS,n=120))^0.1)-1)*100,-120))

CPINSA1yr <- data.frame("DATE" = tail(CPINSA$DATE,-12), "CPI1YRpct" = tail((((CPINSA$CPIAUCNS/lag(CPINSA$CPIAUCNS,n=12)))-1)*100,-12))

ggAcf(CPINSA5yr$CPI5YRpct,lag.max = 240) + theme_economist() + xlim (60,250) + xlab("Lag (months)") + ylab("Autocorrelation") + ggtitle("Autocorrelation of 5yr Annualized Inflation")
ggAcf(CPINSA10yr$CPI10YRpct,lag.max = 240) + theme_economist() + xlim (120,250) + xlab("Lag (months)") + ylab("Autocorrelation") + ggtitle("Autocorrelation of 10yr Annualized Inflation")
ggAcf(CPINSA1yr$CPI1YRpct,lag.max = 240) + theme_economist() + xlim (12,250) + xlab("Lag (months)") + ylab("Autocorrelation") + ggtitle("Autocorrelation of 1yr Annualized Inflation")
#Here I'm just defining the start and end dates for the TIPS graphs
Start.end5yr <- c(as.Date("2008-01-01"),as.Date("2021-04-01"))
Start.end5yr5yrfwd <- c(as.Date("2013-01-01"),as.Date("2021-04-01"))
Start.end10yr <- c(as.Date("2013-01-01"),as.Date("2021-04-01"))

#Plotting TIPS inflation expectations against real inflation

ggplot() + 
  geom_line(data=fiveyrbreakeven, aes(x=DATE+1826,y=T5YIE,color= "5yr Breakeven Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.end5yr) +
  ylim(-3,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()
  # Essentially, this is graphing 5year inflation expectations against real 5yr inflation, anchored to when the real measurement was taken. double check that this is matching up as intended..... I believe that it is but still
  #You have to put colors as titles within aes to get the scale legends to work

ggplot() + 
  geom_line(data=fiveyrfiveyrbreakeven, aes(x=DATE+3652,y=T5YIFR,color= "5yr, 5yr Forward Breakeven Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.end10yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

ggplot() + 
  geom_line(data=tenyrbreakeven, aes(x=DATE+3652,y=T10YIE,color= "10yr Breakeven Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.end5yr5yrfwd) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

#Now that the TIPS graphs are over, its time to go to the Cleveland Fed and DKW Models!

#DKW first!

Start.EndDKW5yr <- c(as.Date("2002-01-01"),as.Date("2021-04-01"))
Start.EndDKW10yr <- c(as.Date("2007-01-01"),as.Date("2021-04-01"))

ggplot() + 
  geom_line(data=DKWModel, aes(x=date+1826,y=exp.inflation.5 ,color= "5yr DKW Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndDKW5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

ggplot() + 
  geom_line(data=DKWModel, aes(x=date+3652,y=exp.inflation.10 ,color= "10yr DKW Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndDKW10yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

#Cleveland Fed Now!

ggplot() + 
  geom_line(data=ClevelandFedModel, aes(x=`Model Output Date`+1826,y= `5 year Expected Inflation`*100 ,color= "5yr Cleveland Fed Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndDKW5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()
#note - inflation is expressed as "0.06" instead of "6" for the cleveland Fed Model, hence why I multiply by 100

ggplot() + 
  geom_line(data=ClevelandFedModel, aes(x=`Model Output Date`+3652, y= `10 year Expected Inflation`*100 ,color= "10yr Cleveland Fed Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndDKW10yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

#Now for the survey models!

Start.EndAruoba10yr <- c(as.Date("2008-01-01"),as.Date("2021-04-01"))
Start.EndAruoba5yr <- c(as.Date("2003-01-01"),as.Date("2021-04-01"))

ggplot() + 
  geom_line(data=Livingston, aes(x=Date,y=L10YIE ,color= "10yr Livingston Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndDKW5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

ggplot() + 
  geom_line(data=SurveyofProfessionals, aes(x=Date+3652,y=PF10YIE ,color= "10yr Survey of Professional Forecasters Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndDKW5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

ggplot() + 
  geom_line(data=UMich, aes(x=Date+1862,y=UM5YR ,color= "5yr University of Michigan Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndAruoba5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

ggplot() + 
  geom_line(data=AruobaModel, aes(x=`_date_`+1826,y=infexp60 ,color= "5yr Aruoba Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndAruoba5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

ggplot() + 
  geom_line(data=AruobaModel, aes(x=`_date_`+3652,y=infexp120 ,color= "10yr Aruoba Inflation Expectations"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr Annualized Inflation"),size = 1.25) + 
  scale_x_date(limits = Start.EndAruoba10yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()

#Finally, the multi-model graphical comparisons


#10yr 
ggplot() + 
  geom_line(data=AruobaModel, aes(x=`_date_`+3652,y=infexp120 ,color= "10yr Aruoba"), size = 1.25) +
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr"),size = 2) + 
  geom_line(data=SurveyofProfessionals, aes(x=Date+3652,y=PF10YIE ,color= "10yr Survey of Professional Forecasters"), size = 1.25) +
  geom_line(data=Livingston, aes(x=Date+3652,y=L10YIE ,color= "10yr Livingston"), size = 1.25) + 
  scale_x_date(limits = Start.EndDKW5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","light blue","yellow","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","light blue","yellow","black")) +
  theme_economist() +
  scale_color_economist(name="")

#TIPS Models!
ggplot() + 
  geom_line(data=fiveyrbreakeven, aes(x=DATE+1826,y=T5YIE,color= "5yr Breakeven"), size = 1.25) + 
  geom_line(data=DKWModel, aes(x=date+1826,y=exp.inflation.5 ,color= "5yr DKW"), size = 1.25) + 
  geom_line(data=ClevelandFedModel, aes(x=`Model Output Date`+1826,y= `5 year Expected Inflation`*100 ,color= "5yr Cleveland Fed"), size = 1.25) + 
  geom_line(data=CPINSA5yr, aes(x=DATE, y=CPI5YRpct,color = "Actual 5yr"),size = 2) + 
  scale_x_date(limits = Start.EndDKW5yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("5 Year Real/Expected Inflation Rate (%)") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist() +
  scale_color_economist(name="")

ggplot() + 
  geom_line(data=tenyrbreakeven, aes(x=DATE+3652,y=T10YIE,color= "10yr Breakeven"), size = 1.25) + 
  geom_line(data=DKWModel, aes(x=date+3652,y=exp.inflation.10 ,color= "10yr DKW"), size = 1.25) + 
  geom_line(data=ClevelandFedModel, aes(x=`Model Output Date`+3652, y= `10 year Expected Inflation`*100 ,color= "10yr Cleveland Fed"), size = 1.25) + 
  geom_line(data=CPINSA10yr, aes(x=DATE, y=CPI10YRpct,color = "Actual 10yr"),size = 2) + 
  scale_x_date(limits = Start.EndDKW10yr) +
  ylim(0,4) +
  xlab("Date of Realized Expectations") +
  ylab("10 Year Real/Expected Inflation Rate (%)") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist() +
  scale_color_economist(name="")


# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

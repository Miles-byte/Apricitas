if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,tidyr,zoo)

CPIHousing <-import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/CPI Shelter and Ex Shelter.csv")
#Importing Data
Ratios <- import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/Ratio of Shelter Ex Shelter Price Indexes.csv")

RatiosMSA <- import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/By MSAs.csv")

RatiosMSA2000 <- import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/By MSAs - 2000.csv")

HousingPerCapita <- import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/New Housing Starts Per Capita.csv")

Residential_Investment <- import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/Private Residential Investment as a Share of GDP.csv")

Vacancy_Rate <- import("C:/Users/Joseph/Documents/SubStack/Fix Inflation - With Housing/Rental and Homeowner Vacancy Rate.csv")

CPIHousing$Date <-as.Date(CPIHousing$Date,"%m/%d/%Y")#forcing date on the CPI Data using lubridate
CPIHousing$Date <-as.IDate(CPIHousing$Date)#forcing Idate on the CPI data

Vacancy_Rate$DATE <-as.Date(Vacancy_Rate$DATE,"%m/%d/%Y")#forcing date on the CPI Data using lubridate
Vacancy_Rate$DATE <-as.IDate(Vacancy_Rate$DATE)#forcing Idate on the CPI data

#calculating yearly housing and shelter inflation
CPIHousing$CPIHousingINF <- CPIHousing$Housing/lag(CPIHousing$Housing)-1 

CPIHousing$CPIShelterINF <- CPIHousing$Shelter/lag(CPIHousing$Shelter)-1
#Calculating housing and shelter inflation's contribution to overall inflation
CPIHousing$CPIHousingCONT <- CPIHousing$CPIHousingINF*(CPIHousing$`Housing Weights`/100)

CPIHousing$CPIShelterCONT <- CPIHousing$CPIShelterINF*(CPIHousing$`Shelter Weights`/100)
#creating an index for Housing and Shelter's Contribution to Inflation
CPIHousing$CPIHousingCONTIndex <- CPIHousing$CPIHousingCONT*lag(CPIHousing$CPI)

CPIHousing$CPIHousingCONTIndex[1] <- 100 #have to set the first number to 100

CPIHousing$CPIHousingCONTIndex <- cumsum(CPIHousing$CPIHousingCONTIndex) #cumsum adds together all the previous numbers


CPIHousing$CPIShelterCONTIndex <- CPIHousing$CPIShelterCONT*lag(CPIHousing$CPI)

CPIHousing$CPIShelterCONTIndex[1] <- 100

CPIHousing$CPIShelterCONTIndex <- cumsum(CPIHousing$CPIShelterCONTIndex)



#creating area plots of inflation due to shelter and housing
ggplot() + 
  geom_area(data=CPIHousing, aes(x=Date,y=CPI-100,fill = "CPI Inflation")) +
  geom_area(data=CPIHousing, aes(x=Date,y=CPIHousingCONTIndex-100,fill = "Housing's Contribution to CPI Inflation")) + 
  scale_y_continuous(limits=c(0,60)) +
  xlab("Year") +
  ylab("CPI Index (Jan 2000 = 0)") +
  theme_economist() +
  scale_fill_economist(name="")

ggplot() + 
  geom_area(data=CPIHousing, aes(x=Date,y=CPI-100,fill = "CPI Inflation")) +
  geom_area(data=CPIHousing, aes(x=Date,y=CPIShelterCONTIndex-100,fill = "Shelter's Contribution to CPI Inflation")) + 
  scale_y_continuous(limits=c(0,60)) +
  xlab("Year") +
  ylab("CPI Index (Jan 2000 = 0)") +
  theme_economist() +# +
  scale_fill_economist(name="")

Start.end <- c(as.Date("2000-01-01"),as.Date("2021-04-01"))

ggplot() + 
  geom_line(data=Ratios, aes(x=DATE,y=CUSR0000SAH1_CUUR0000SA0L2,color= "Ratio of 'Shelter' and 'All Items Less Shelter' Price Indexes"), size = 1.25) + 
  #scale_x_date(limits = Start.end) +
  #ylim(-3,4) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Ratio") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()+
  scale_color_economist(name="")

ggplot() + 
  geom_line(data=Ratios, aes(x=DATE,y=CUSR0000SAH1_CUUR0000SA0L2,color= "Ratio of 'Shelter' and 'All Items Less Shelter' Price Indexes"), size = 1.25) + 
  scale_x_date(limits = Start.end) +
  ylim(1,1.4) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Ratio") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()+
  scale_color_economist(name="")

ggplot() + 
  geom_line(data=HousingPerCapita, aes(x=observation_date,y=Housing_Starts_Per_Capita,color= "Monthly Privately-Owned Housing Starts Per Capita"), size = 1.25) + 
  #scale_x_date(limits = Start.end) +
  ylim(0,0.0125) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Housing Starts Per Capita") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()+
  scale_color_economist(name="")

ggplot() + 
  geom_line(data=Residential_Investment, aes(x=observation_date,y=PRFI_GDP*100,color= "Private Residential Investment as a Share of GDP"), size = 1.25) + 
  #scale_x_date(limits = Start.end) +
  #ylim(0,0.0125) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("% of GDP") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()+
  scale_color_economist(name="")

ggplot() + 
  geom_line(data=Vacancy_Rate, aes(x=DATE,y=RHVRUSQ156N,color= "Homeowner Vacancy Rate"), size = 1.25) +
  geom_line(data=Vacancy_Rate, aes(x=DATE,y=RRVRUSQ156N,color= "Rental Vacancy Rate"), size = 1.25) +
  #scale_x_date(limits = Start.end) +
  ylim(0,12) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Vacancy Rate (%)") +
  ggtitle("US Residential Vacancy Rates") +
  labs(caption = "Graph created by @JosephPolitano using CPS data") + 
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist() +
  scale_color_economist(name="")

Start.endMSA <- c(as.Date("1985-01-01"),as.Date("2020-07-01"))
Start.endMSA2000 <- c(as.Date("2000-01-01"),as.Date("2020-07-01"))

ggplot() + 
  geom_line(data=RatiosMSA, aes(x=date_monthly,y=SF,color= "San Francisco"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_monthly,y=LA,color= "Los Angeles"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_monthly,y=USA,color= "USA"), size = 2) +
  geom_line(data=RatiosMSA, aes(x=date_semi,y=NYC,color= "New York City"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_semi,y=Houston,color= "Houston"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_semi,y=Chicago,color= "Chicago"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_semi,y=Seattle,color= "Seattle"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_semi,y=Atlanta,color= "Atlanta"), size = 1.25) +
  geom_line(data=RatiosMSA, aes(x=date_yearly,y=Boston,color= "Boston"), size = 1.25) +
  ggtitle("Ratio of Shelter Price Indexes to the CPI All Items Less Shelter Price Index") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  #labs(caption = "testas") +
  #ggmain("Ratio of Shelter Price Indexes to CPI All-Items Less Shelter Price Index")
  scale_x_date(limits = Start.endMSA) +
  ylim(0.8,1.75) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Ratio") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()+
  scale_color_economist(name="")

ggplot() + 
  geom_line(data=RatiosMSA2000, aes(x=date_monthly,y=SF,color= "San Francisco"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_monthly,y=LA,color= "Los Angeles"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_monthly,y=USA,color= "USA"), size = 2) +
  geom_line(data=RatiosMSA2000, aes(x=date_semi,y=NYC,color= "New York City"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_semi,y=Houston,color= "Houston"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_semi,y=Chicago,color= "Chicago"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_semi,y=Seattle,color= "Seattle"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_semi,y=Atlanta,color= "Atlanta"), size = 1.25) +
  geom_line(data=RatiosMSA2000, aes(x=date_yearly,y=Boston,color= "Boston"), size = 1.25) +
  ggtitle("Ratio of Shelter Price Indexes to the CPI All Items Less Shelter Price Index") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  #labs(caption = "testas") +
  #ggmain("Ratio of Shelter Price Indexes to CPI All-Items Less Shelter Price Index")
  scale_x_date(limits = Start.endMSA2000) +
  #ylim(0.8,1.75) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Ratio") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist()+
  scale_color_economist(name="")

#creating a plot of the ratios of the shelter and non-shelter price indexes

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

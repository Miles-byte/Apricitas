pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,tidyr,zoo,RCurl,Cairo)


Yearly_NIPA_Aggregates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/Yearly%20Aggregates.csv")
# create data
Yearly_NIPA_Aggregates <- t(Yearly_NIPA_Aggregates)#transposing data

colnames(Yearly_NIPA_Aggregates) <- Yearly_NIPA_Aggregates[1,] #moving text names to headers
Yearly_NIPA_Aggregates <-Yearly_NIPA_Aggregates[-1,] #removing text names

Yearly_NIPA_Aggregates <- as.data.frame(Yearly_NIPA_Aggregates) #forcing data frame

names(Yearly_NIPA_Aggregates) <- gsub(" ", "", names(Yearly_NIPA_Aggregates))
names(Yearly_NIPA_Aggregates) <- make.names(names(Yearly_NIPA_Aggregates), unique=TRUE)

Yearly_NIPA_Aggregates <- data.frame(lapply(Yearly_NIPA_Aggregates,as.numeric)) # making everything numeric. Lapply really messed with the variable names here though

Yearly_NIPA_Aggregates$Year <- as.IDate(c("2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01"))

Yearly_NIPA_Aggregates$PandemicPrograms <- 
  Yearly_NIPA_Aggregates$Economicimpactpayments5 + 
  Yearly_NIPA_Aggregates$CoronavirusFoodAssistanceProgram1 + 
  Yearly_NIPA_Aggregates$PaycheckProtectionProgramloanstobusinesses2 + 
  Yearly_NIPA_Aggregates$PaycheckProtectionProgramloanstobusinesses2.1 + 
  Yearly_NIPA_Aggregates$IncreaseinMedicarereimbursementrates3 + 
  Yearly_NIPA_Aggregates$ExtendedUnemploymentBenefits + 
  Yearly_NIPA_Aggregates$PandemicEmergencyUnemploymentCompensation + 
  Yearly_NIPA_Aggregates$PandemicUnemploymentAssistance + 
  Yearly_NIPA_Aggregates$PandemicUnemploymentCompensationPayments + 
  Yearly_NIPA_Aggregates$Lostwagessupplementalpayments6 + 
  Yearly_NIPA_Aggregates$PaycheckProtectionProgramloanstoNPISH2 + 
  Yearly_NIPA_Aggregates$Studentloanforbearance8*-1 #summming all the pandemic assistance programs. theres a lot of em

Yearly_NIPA_Aggregates$PandemicPrograms[is.na(Yearly_NIPA_Aggregates$PandemicPrograms)] <- 0 #converting na to 0

personal_income <- ggplot() + 
  geom_line(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome,color= "Nominal Personal Income, Billions"), size = 1.25) + 
  #scale_x_date(limits = Start.end) +
  scale_y_continuous(labels = scales::comma,limits = c(15000,20000)) +
  #xlim(2000,2021) +
  xlab("Year") +
  ylab("Personal Income, Nominal $, Billions") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist() +
  scale_color_economist(name="")

test <- Yearly_NIPA_Aggregates$Personalincome - Yearly_NIPA_Aggregates$PandemicPrograms - Yearly_NIPA_Aggregates$Personalcurrenttransferreceipts
test
Yearly_NIPA_Aggregates$Personalincome

stacked_area <- 
  ggplot() + 
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome,fill= "COVID Programs")) +
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome - PandemicPrograms,fill= "Regular UI")) +
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome - PandemicPrograms - Unemploymentinsurance,fill= "Prior Gov't Transfers")) +
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome - PandemicPrograms - Personalcurrenttransferreceipts,fill= "Nominal Private Income")) +
  scale_x_date(limits = c(as.IDate("2018-01-01"),as.IDate("2020-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  #ylim(labels = scales::comma)
  scale_y_continuous(labels = scales::dollar) +
  #xlim(2000,2021) +
  xlab("Year") +
  ylab("Nominal $,Billions") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist() +
  scale_fill_economist(name="")

  
ggsave(dpi = "retina",plot = stacked_area, "Stacked Personal Income.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = personal_income, "Personal Income.png",w = 5, h = 5.53, type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#test <- c(rbind(Yearly_NIPA_Aggregates$Personalincome,Yearly_NIPA_Aggregates$Economicimpactpayments5,Yearly_NIPA_Aggregates$CoronavirusFoodAssistanceProgram1,Yearly_NIPA_Aggregates$PaycheckProtectionProgramloanstobusinesses2))
#trying to cbind for a new stacked graph
#ime <- as.numeric(rep(seq(2015,2020),each=7))  # x axis = 7 to include personal income, pre existing transfers, traditional unemployment, expanded unemployment, checks, ppp, and other new programs
#value <- runif(49, 10, 100)               # y Axis
#group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
#data <- data.frame(time, value, group)

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())


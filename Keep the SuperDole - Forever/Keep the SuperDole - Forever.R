pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,tidyr,zoo,RCurl,Cairo,datetime,stringr,pollster)

Yearly_NIPA_Aggregates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/Yearly%20Aggregates.csv")
# create data

PI_PCE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/Personal%20Income%20and%20Consumption%20Expenditures.csv")

SHED_2020 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/SHED%20Data%202020.csv")

SHED_2019_Update <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/SHED%202019%20Supplement.csv")

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

PI_PCE08 <- PI_PCE[PI_PCE$DATE > "2007-10-01", ] #trimming PCE and PI data to the recessions and indexing them to months before/after the recession begins
PI_PCE20 <- PI_PCE[PI_PCE$DATE > "2019-12-01", ]
PI_PCE08$MonthsRecession <- -1:161
PI_PCE20$MonthsRecession <- -1:15

#Taking SHED data and converting it to numbers representing "yes" to "lower than, higher than, same as" responses to if they received more money on unemployment
#This is so I can apply weighting

lower_than_weighted <- str_replace_all(SHED_2019_Update$CV17,"Income from unemployment insurance is lower than prior income","1")
lower_than_weighted <- as.numeric(lower_than_weighted)
lower_than_weighted <- lower_than_weighted*SHED_2019_Update$weight

greater_than_weighted <- str_replace_all(SHED_2019_Update$CV17,"Income from unemployment insurance is higher than prior income","1")
greater_than_weighted <- as.numeric(greater_than_weighted)
greater_than_weighted <- greater_than_weighted*SHED_2019_Update$weight

about_same_weighted <- str_replace_all(SHED_2019_Update$CV17,"About the same","1")
about_same_weighted <- as.numeric(about_same_weighted)
about_same_weighted <- about_same_weighted*SHED_2019_Update$weight

#data frame of percent of ppl who received ui more/less/equal to work income
unemployedincome <- data.frame(
  group = c("Higher","Lower","About the same"),
  value = c(sum(greater_than_weighted, na.rm=TRUE),sum(lower_than_weighted, na.rm=TRUE),sum(about_same_weighted, na.rm = TRUE)))

UI_Income_Weighted_Crosstabs <- crosstab(df = SHED_2019_Update, x = ppincimp, y = CV16_a, weight = weight,format = "long")

#taking crosstabs of the UI data to make the UI by income graph

UI_Income_Weighted_Crosstabs$ppincimp <- factor(UI_Income_Weighted_Crosstabs$ppincimp, levels = c("Less than $5,000","$5,000 to $7,499","$7,500 to $9,999","$10,000 to $12,499","$12,500 to $14,999","$15,000 to $19,999","$20,000 to $24,999","$25,000 to $29,999","$30,000 to $34,999","$35,000 to $39,999","$40,000 to $49,999","$50,000 to $59,999","$60,000 to $74,999","$75,000 to $84,999","$85,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999","$150,000 to $174,999","$175,000 to $199,999","$200,000 to $249,999","$250,000 or more"))
UI_Income_Weighted_Crosstabs$CV16_a <- factor(UI_Income_Weighted_Crosstabs$CV16_a, levels = c("Refused","Did not apply for and did not receive","Applied for but not received","Received"))
#reordering factors

#renaming factors
levels(UI_Income_Weighted_Crosstabs$ppincimp) <- list("<5k" ="Less than $5,000",
                                                      "7k" = "$5,000 to $7,499",
                                                      "10k" = "$7,500 to $9,999",
                                                      "12.5k" = "$10,000 to $12,499",
                                                      "15k" = "$12,500 to $14,999",
                                                      "20k" = "$15,000 to $19,999",
                                                      "25k" = "$20,000 to $24,999",
                                                      "30k" = "$25,000 to $29,999",
                                                      "35k" = "$30,000 to $34,999",
                                                      "40k" = "$35,000 to $39,999",
                                                      "50k" = "$40,000 to $49,999",
                                                      "60k" = "$50,000 to $59,999",
                                                      "75k" = "$60,000 to $74,999",
                                                      "85k" = "$75,000 to $84,999",
                                                      "100k" = "$85,000 to $99,999",
                                                      "125k" = "$100,000 to $124,999",
                                                      "150k" = "$125,000 to $149,999",
                                                      "175k" = "$150,000 to $174,999",
                                                      "200k" = "$175,000 to $199,999",
                                                      "250k" = "$200,000 to $249,999",
                                                      ">250k" = "$250,000 or more")

levels(UI_Income_Weighted_Crosstabs$CV16_a) <- list("Refused" = "Refused",
                                                    "Did not apply" = "Did not apply for and did not receive" ,
                                                    "Applied for but not received" = "Applied for but not received",
                                                    "Received" = "Received")
                                                    
  
#graphing ui outcomes by hh income
UI_Outcomes_Income <- ggplot(UI_Income_Weighted_Crosstabs, aes(x = ppincimp,pct/100, fill = CV16_a))+
  geom_bar(stat = "identity") +
  xlab("Household Income bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Weighted UI Outcomes by Household Income Groups") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_economist() +
  scale_fill_economist(name="")

#graphing comparison of unemployment income to prior work income
Unemployed_income_graph <- ggplot(unemployedincome, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  xlab("") +
  ylab("") +
  theme_economist() +
  ggtitle("UI Compared to Prior Work Income") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank()) +
  scale_fill_economist(name="")

#graphing personal income by recession
personal_income_rec <- ggplot() + 
  geom_line(data=PI_PCE08, aes(x=MonthsRecession,y=(PI/12162)*100,color= "2008 Personal Income"), size = 1.25) +
  geom_line(data=PI_PCE20, aes(x=MonthsRecession,y=(PI/18973)*100,color= "2020 Personal Income"), size = 1.25) +
  ylim(80,130) +
  xlim(-1,30) +
  xlab("Months Since Start of Recession") +
  ylab("Index - 1 Month Before Recession = 100") +
  ggtitle("A Tale of Two Recessions: Seasonally Adjusted Nominal Personal Income") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  theme_economist() +
  scale_color_economist(name="")

#graphing personal consumption by recession
personal_consumption_rec <- ggplot() + 
  geom_line(data=PI_PCE08, aes(x=MonthsRecession,y=(PCE/9898)*100,color= "2008 Personal Consumption Expenditures"), size = 1.25) +
  geom_line(data=PI_PCE20, aes(x=MonthsRecession,y=(PCE/14880)*100,color= "2020 Personal Consumption Expenditures"), size = 1.25) +
  ylim(80,130) +
  xlim(-1,30) +
  xlab("Months Since Start of Recession") +
  ylab("Index - 1 Month Before Recession = 100") +
  ggtitle("A Tale of Two Recessions: Seasonally Adjusted Nominal PCE") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") +
  theme_economist() +
  scale_color_economist(name="")

#graphing nominal personal income
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

#graphing composition of personal income
stacked_area <- 
  ggplot() + 
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome,fill= "COVID Programs")) +
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome - PandemicPrograms,fill= "Regular UI")) +
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome - PandemicPrograms - Unemploymentinsurance,fill= "Prior Gov't Transfers")) +
  geom_area(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personalincome - PandemicPrograms - Personalcurrenttransferreceipts,fill= "Nominal Private Income")) +
  scale_x_date(limits = c(as.IDate("2018-01-01"),as.IDate("2020-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  #ylim(labels = scales::comma)
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Breakdown of Annual Nominal Personal Income") +
  #xlim(2000,2021) +
  xlab("Year") +
  ylab("Nominal $,Billions") +
  labs(caption = "Graph created by @JosephPolitano with help from Michael Farquharson using BEA data") +
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  theme_economist() +
  scale_fill_economist(name="")

#printing charts
ggsave(dpi = "retina",plot = stacked_area, "Stacked Personal Income.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = personal_income, "Personal Income.png",w = 5, h = 5.53, type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = personal_income_rec, "Personal Income Recession.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = personal_consumption_rec, "Personal Consumption Recession.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Unemployed_income_graph, "UI Income.png",w = 4.5, h = 5.5, type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = UI_Outcomes_Income, "UI Outcomes by Income.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())


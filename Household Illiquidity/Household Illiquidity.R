pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster)

SHED_2020 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202020.csv")
SHED_2019 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202019.csv")
Delinquencies <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/Delinquency.csv")

Emergency_Expenses20 <- crosstab(df = SHED_2020, x = ppincimp, y = EF1, weight = weight,format = "long") #taking crosstabs of EF1 "do you have an emergency fund of 3 months worth of expenses" and ppinc "household income"
Emergency_Expenses_2020

Emergency_Expenses19 <- crosstab(df = SHED_2019, x = ppincimp, y = EF1, weight = weight,format = "long") #taking crosstabs of EF1 "do you have an emergency fund of 3 months worth of expenses" and ppinc "household income"

levels(Emergency_Expenses20$ppincimp) <- list("<5k" ="Less than $5,000", #renaming household income so it fits on the chart
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


levels(Emergency_Expenses20$EF1) <- list( "Refused to Answer" = "Refused","No" = "No","Yes"="Yes" #renaming and reordering the answers
)
levels(Emergency_Expenses19$ppincimp) <- list("<5k" ="Less than $5,000", #renaming household income so it fits on the chart
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


levels(Emergency_Expenses19$EF1) <- list( "Refused to Answer" = "Refused","No" = "No","Yes"="Yes" #renaming and reordering the answers
)

Emergency_Expenses_2020_Graph <- ggplot(Emergency_Expenses20, aes(x = ppincimp,pct/100, fill = EF1))+
  geom_bar(stat = "identity") +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Do You Have Emergency Funds That Could Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2020") +
  theme_economist() +
  scale_fill_economist(name="")

Emergency_Expenses_2019_Graph <- ggplot(Emergency_Expenses19, aes(x = ppincimp,pct/100, fill = EF1))+
  geom_bar(stat = "identity") +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Do You Have Emergency Funds That Could Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2019") +
  theme_economist() +
  scale_fill_economist(name="")


Emergency_Expenses_19_Borrow <- crosstab(df = SHED_2019, x = ppincimp, y = EF2, weight = weight,format = "long") #taking crosstabs of EF1 "could you borrow or sell enough to cover 3 months of expenses" and ppinc "household income"

levels(Emergency_Expenses_19_Borrow$ppincimp) <- list("<5k" ="Less than $5,000", #renaming household income so it fits on the chart
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


levels(Emergency_Expenses_19_Borrow$EF2) <- list( "Refused to Answer" = "Refused","No" = "No","Yes"="Yes" #renaming and reordering the answers
)

Emergency_Expenses_19_Borrow_No<- Emergency_Expenses_19_Borrow[Emergency_Expenses_19_Borrow$EF2 == "No", ] #filtering only those who could not afford 3 months expenses with emergency fund (from EF1) with those who could not cover thru selling/borrowing (EF2)

Emergency_Expenses_19_Borrow_Graph_No <- ggplot(Emergency_Expenses_19_Borrow_No, aes(x = ppincimp,pct/100, fill = EF2))+
  geom_bar(stat = "identity") +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  ggtitle("Could Not Cover 3 Months' Expenses with Savings, Borrowing, or Selling") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2019") +
  theme_economist() +
  scale_fill_economist(name="")+
  theme(legend.position = "none")


Emergency_Expenses_19_Borrow_Graph <- ggplot(Emergency_Expenses_19_Borrow, aes(x = ppincimp,pct/100, fill = EF2))+
  geom_bar(stat = "identity") +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  ggtitle("Could You Borrow,Use Savings, or Sell Assets to Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Only Those Who Did Not Have 3 Month Emergency Fund, Weighted, 2019") +
  theme_economist() +
  scale_fill_economist(name="")


SHED2019Non_Retired <- SHED_2019[SHED_2019$ppwork != "Not working retired", ] #pulling only SHED data of those not retired so that I can graph the percent of them who could not cover 3 months' expenses

SHED2019Non_Retired_Crosstabs <- crosstab(df = SHED2019Non_Retired, x = ppincimp, y = EF2, weight = weight,format = "long") #taking crosstabs of EF2 "could you cover a 3 month expense with borrowing/selling" and ppinc "household income"

levels(SHED2019Non_Retired_Crosstabs$ppincimp) <- list("<5k" ="Less than $5,000", #renaming household income so it fits on the chart
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


levels(SHED2019Non_Retired_Crosstabs$EF2) <- list( "Refused to Answer" = "Refused","No" = "No","Yes"="Yes" #renaming and reordering the answers
)

SHED2019Non_Retired_Crosstabs<- SHED2019Non_Retired_Crosstabs[SHED2019Non_Retired_Crosstabs$EF2 == "No", ] #filtering only those who could not afford 3 months expenses with emergency fund (from EF1) with those who could not cover thru selling/borrowing (EF2)

Emergency_Expenses_19_Borrow_Not_Retired <- ggplot(SHED2019Non_Retired_Crosstabs, aes(x = ppincimp,pct/100, fill = EF2))+
  geom_bar(stat = "identity") +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  ggtitle("Could Not Cover 3 Months' Expenses with Savings, Borrowing, or Selling") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Of Those Not Retired, Responses by Household Income, Weighted, 2019") +
  theme_economist() +
  scale_fill_economist(name="")+
  theme(legend.position = "none")


Surprise_Expense <- select(SHED_2019,weight,ppincimp,EF3_a:EF3_h) #organizing the data for answers to the question "how would you pay for a $400 emergency expense" by income
Surprise_Expense <- gather(Surprise_Expense, Answer, Unexpected_400, EF3_a:EF3_h) #gathering answers to $400 emergency question into one variable

Surprise_Expense_Crosstab <- crosstab_3way(df = Surprise_Expense, x = ppincimp, y = Answer, z = Unexpected_400, weight = weight) #taking a 3=way crosstab of the answers to the unexpected $400. because of a quirk with gather, the columns are split into yes/no 

Surprise_Expense_Crosstab <- Surprise_Expense_Crosstab[Surprise_Expense_Crosstab$Unexpected_400 == "Yes", ] #filtering out the "no" answers. Because of the structure of the questions, you are only allowed to answer "yes" to one questions, so the no answers are residuals

Surprise_Expense_Crosstab <- gather(Surprise_Expense_Crosstab,payment_type, pct,EF3_a:EF3_h) #re-gathering the answers as the crosstab function separated them

levels(Surprise_Expense_Crosstab$ppincimp) <- list("<5k" ="Less than $5,000", #renaming household income so it fits on the chart
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
Surprise_Expense_Crosstab$payment_type <- as.factor(Surprise_Expense_Crosstab$payment_type)

levels(Surprise_Expense_Crosstab$payment_type) <- list( "Credit Card, Next Statement" = "EF3_a",
                                                  "Credit Card, Over Time" = "EF3_b",
                                                  "Cash Equivalent"="EF3_c",
                                                  "Bank Loan"="EF3_d",
                                                  "Borrow, Friends/Family"="EF3_e",
                                                  "Payday Loan"="EF3_f",
                                                  "Selling Something"="EF3_g",
                                                  "Could Not Pay"="EF3_h"
                                                  ) #renaming and reordering the answers



Surprise_Expense_Crosstab_Graph <- ggplot(Surprise_Expense_Crosstab, aes(x = ppincimp,pct/100, fill = payment_type))+
  geom_bar(stat = "identity") +
  xlab("Household Income Bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("How Would You Cover a $400 Emergency Expense?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Responses by Household Income, Weighted, 2019") +
  theme_economist() +
  scale_fill_economist(name="")


Delinquencies08 <- Delinquencies[Delinquencies$DATE > "2007-9-01", ] #trimming Credit Card and SFH Mortgage data to the recessions and indexing them to Quarter before/after the recession begins
Delinquencies20 <- Delinquencies[Delinquencies$DATE > "2019-12-01", ]
Delinquencies08$QuartersRecession <- -1:51
Delinquencies20$QuartersRecession <- -1:3

Delinquencies_Graph <- ggplot() + 
  geom_line(data=Delinquencies08, aes(x=QuartersRecession,y=(DRCCLACBN/4.8)*100,color= "2008 Credit Card"), size = 1.25) +
  geom_line(data=Delinquencies20, aes(x=QuartersRecession,y=(DRCCLACBN/2.76)*100,color= "2020 Credit Card"), size = 1.25) +
  geom_line(data=Delinquencies08, aes(x=QuartersRecession,y=(DRSFRMACBS/3.68)*100,color= "2008 Mortgage"), size = 1.25) +
  geom_line(data=Delinquencies20, aes(x=QuartersRecession,y=(DRSFRMACBS/2.39)*100,color= "2020 Mortgage"), size = 1.25) +
  ylim(70,350) +
  xlim(-1,12) +
  xlab("Quarters Since Start of Recession") +
  ylab("Index - 1 Quarter Before Recession = 100") +
  ggtitle("A Tale of Two Recessions: Delinquency Rates") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_economist() +
  scale_color_economist(name="")

ggsave(dpi = "retina",plot = Emergency_Expenses_2019_Graph, "2019 SHED Emergency Funds.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Emergency_Expenses_2020_Graph, "2020 SHED Emergency Funds.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Emergency_Expenses_19_Borrow_Graph, "2019 SHED Emergency Funds Borrowing.png",height = 5.76, width = 9.25, type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Emergency_Expenses_19_Borrow_Graph_No, "2019 SHED Emergency Funds Borrowing pctNo.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Surprise_Expense_Crosstab_Graph, "$400 Emergency Expense Crosstab Graph.png",height = 5.76, width = 9.5, type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Delinquencies_Graph, "Delinquencies.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Emergency_Expenses_19_Borrow_Not_Retired, "Emergency Expenses Borrow Not Retired.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

rlang::last_error()
# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

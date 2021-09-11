pacman::p_load(usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

Interest_Rates <- data.frame(Year = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021), 
                             Subsidized = c(0.068,0.068,0.06,0.056,0.045,0.034,0.034,0.0386,0.0466,0.0429,0.0376,0.0445,0.05045,0.04529,0.02750,0.0373), 
                             Unsubsidized = c(0.068,0.068,0.068,0.068,0.068,0.068,0.068,0.0386,0.0466,0.0429,0.0376,0.0445,0.05045,0.04529,0.02750,0.0373),
                             Grad = c(0.068,0.068,0.068,0.068,0.068,0.068,0.068,0.0541,0.0621,0.0584,0.0531,0.06,0.06595,0.06079,0.043,0.0528),
                             PLUS = c(0.079,0.079,0.079,0.079,0.079,0.079,0.079,0.0641,0.0721,0.0684,0.0631,0.07,0.07595,0.07079,0.053,0.0628))



Loan_Debt_Grad <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Student%20Loans%20are%20Bad%20Taxes/Average_Loan_Balance_Grad.csv")

Marginal_Tax_Rates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Student%20Loans%20are%20Bad%20Taxes/Student_Loan_Marginal_Tax_Rates.csv")

Loans_Outstanding <- fredr(series_id = c("SLOAS")) #downloading outstanding student loan balance from FRED
Net_Assets <- fredr(series_id = c("BOGZ1FL892090005Q")) #downloading net wealth

Loans_Outstanding <- merge(Loans_Outstanding,Net_Assets, by = "date")#merging in

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

Interest_Rates_Graph <- ggplot() + #plotting Student Loan Rates
  geom_line(data=Interest_Rates, aes(x=Year,y= Subsidized,color= "Subsidized Undergraduate"), size = 1.25)+ 
  geom_line(data=Interest_Rates, aes(x=Year,y= Unsubsidized,color= "Unsubsidized Undergraduate"), size = 1.25)+ 
  geom_line(data=Interest_Rates, aes(x=Year,y= Grad,color= "Graduate"), size = 1.25)+ 
  geom_line(data=Interest_Rates, aes(x=Year,y= PLUS,color= "Parent and Graduate PLUS"), size = 1.25)+ 
  xlab("School Year") +
  ylab("Interest Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.105), expand = c(0,0)) +
  ggtitle("Student Loan Interest Rates Have Been Shrinking") +
  labs(caption = "Graph created by @JosephPolitano using Department of Education data", subtitle = "Since Student Loan Rates are Mostly Based on Bond Yields, They Have Been Declining") +
  theme_apricitas + theme(legend.position = c(.75,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Parent and Graduate PLUS","Graduate","Unsubsidized Undergraduate","Subsidized Undergraduate")) +
  annotate(geom = "vline", x = 2012, xintercept = 2012, color = "white", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Bipartisan Student Loan Certainty Act", x = 2015, y = 0.02825, color ="white")

Loans_Outstanding_Graph <- ggplot() + #plotting Student Loan Rates
  geom_line(data=Loans_Outstanding, aes(x=date,y= (value.x*1000)/value.y,color= "Student Loans Outstanding as a % of Total US Wealth"), size = 1.25)+ 
  xlab("Date") +
  ylab("Dollars, as a % of Total US Wealth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25),limits = c(0,0.02), expand = c(0,0)) +
  ggtitle("Back to School Blues") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Student Loan Balances Rose as a Share Of Wealth in the Aftermath of the 2008 Recession") +
  theme_apricitas + theme(legend.position = c(.70,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))

Loan_Debt_Grad_Graph <- ggplot() + #plotting Student debt at graduation by year
  geom_line(data=Loan_Debt_Grad, aes(x=Year,y=Real_2020_Balance,color= "Average Student Loan Debt At Graduation, 2020 Dollars"), size = 1.25)+ 
  xlab("Graduation Year") +
  ylab("2020 Dollars") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(10000,40000), expand = c(0,0)) +
  ggtitle("Payback Time") +
  labs(caption = "Graph created by @JosephPolitano using educationdata.org data", subtitle = "After Years of Constant Increases, Debt Levels for Graduates are Stabilizing") +
  theme_apricitas + theme(legend.position = c(.70,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"))

Marginal_Tax_Rates_Marginal_Graph <- ggplot() + #plotting Marginal Student Loan Tax Rates
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=Standard_Marginal.,color= "Standard Repayment Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=IBR_Marginal.,color= "Income-Based Repayment Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=REPAYE_Marginal.,color= "REPAYE Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=PAYE_Marginal.,color= "PAYE Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=ICR_Marginal.,color= "Income-Contingent Repayment Plan"), size = 1.25)+
  xlab("Income") +
  ylab("Marginal Tax Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,.30), expand = c(0,0)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  ggtitle("Student Loans are Bad Taxes") +
  labs(caption = "Graph created by @JosephPolitano", subtitle = "Marginal Tax Rates for Student Loan Repayment Plans are High and Uneven") +
  theme_apricitas + theme(legend.position = c(.70,.82)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))

Marginal_Tax_Rates_Average_Graph <- ggplot() + #plotting Average Student Loan Tax Rates
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=Standard_Average.,color= "Standard Repayment Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=IBR_Average.,color= "Income-Based Repayment Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=REPAYE_Average.,color= "REPAYE Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=PAYE_Average.,color= "PAYE Plan"), size = 1.25)+ 
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=ICR_Average.,color= "Income-Contingent Repayment Plan"), size = 1.25)+
  xlab("Income") +
  ylab("Average Tax Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,.30), expand = c(0,0)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  ggtitle("Student Loans are Bad Taxes") +
  labs(caption = "Graph created by @JosephPolitano", subtitle = "Average Tax Rates Show that Student Loans are Regressive") +
  theme_apricitas + theme(legend.position = c(.70,.82)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))


Marginal_Tax_Rates_Mnthly_Graph <- ggplot() + #plotting Student Loan Marginal and Average Tax Rates for Lowest Monthly Payment
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=LowestMnthly_Marginal.,color= "Marginal Tax Rate for Lowest Monthly Payment"), size = 1.25)+
  geom_line(data=Marginal_Tax_Rates, aes(x=Income,y=LowestMnthly_Average.,color= "Average Tax Rate for Lowest Monthly Payment"), size = 1.25)+
  xlab("Income") +
  ylab("Tax Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,.30), expand = c(0,0)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  ggtitle("Student Loans are Bad Taxes") +
  labs(caption = "Graph created by @JosephPolitano", subtitle = "Single Borrowers Making $13,000-$45,000 Experience High Marginal Tax Rates") +
  theme_apricitas + theme(legend.position = c(.70,.43)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#6A4C93"))

  
ggsave(dpi = "retina",plot = Interest_Rates_Graph, "Student Loan Interest Rates.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Loans_Outstanding_Graph, "Student Loan Outstanding.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Loan_Debt_Grad_Graph, "Loan Debt by Graduation Year.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Marginal_Tax_Rates_Marginal_Graph, "Marginal Tax Rates.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Marginal_Tax_Rates_Average_Graph, "Average Tax Rates.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Marginal_Tax_Rates_Mnthly_Graph, "Tax Rates for Lowest Monthly Payment.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
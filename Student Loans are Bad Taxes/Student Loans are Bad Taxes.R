pacman::p_load(usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

Interest_Rates <- data.frame(Year = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021), 
                             Subsidized = c(0.068,0.068,0.06,0.056,0.045,0.034,0.034,0.0386,0.0466,0.0429,0.0376,0.0445,0.05045,0.04529,0.02750,0.0373), 
                             Unsubsidized = c(0.068,0.068,0.068,0.068,0.068,0.068,0.068,0.0386,0.0466,0.0429,0.0376,0.0445,0.05045,0.04529,0.02750,0.0373),
                             Grad = c(0.068,0.068,0.068,0.068,0.068,0.068,0.068,0.0541,0.0621,0.0584,0.0531,0.06,0.06595,0.06079,0.043,0.0528),
                             PLUS = c(0.079,0.079,0.079,0.079,0.079,0.079,0.079,0.0641,0.0721,0.0684,0.0631,0.07,0.07595,0.07079,0.053,0.0628))



Loan_Debt_Grad <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Student%20Loans%20are%20Bad%20Taxes/Avg_Loan_Balance_Grad.csv")

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



ggsave(dpi = "retina",plot = Interest_Rates_Graph, "Student Loan Interest Rates.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Loans_Outstanding_Graph, "Student Loan Outstanding.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
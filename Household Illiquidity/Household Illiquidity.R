pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster)

SHED_2020 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202020.csv")
SHED_2019 <- read.csv()

Emergency_Expenses <- crosstab(df = SHED_2019, x = ppincimp, y = EF1, weight = weight,format = "long") #taking crosstabs of EF1 "do you have an emergency fund of 3 months worth of expenses" and ppinc "household income"
Emergency_Expenses_2019

levels(Emergency_Expenses$ppincimp) <- list("<5k" ="Less than $5,000", #renaming household income so it fits on the chart
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


levels(Emergency_Expenses$EF1) <- list( "Refused to Answer" = "Refused", "Yes" = "Yes","No" = "No", #renaming and reordering the answers
                                        )


Emergency_Expenses_2019_Graph <- ggplot(Emergency_Expenses, aes(x = ppincimp,pct/100, fill = EF1))+
  geom_bar(stat = "identity") +
  xlab("Household Income bin") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Do You Have Emergency Funds That Could Cover 3 Months' Expenses?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Weighted Responses by Household Income, 2020") +
  theme_economist() +
  scale_fill_economist(name="")

ggsave(dpi = "retina",plot = Emergency_Expenses_2019_Graph, "2020 SHED Emergency Funds.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

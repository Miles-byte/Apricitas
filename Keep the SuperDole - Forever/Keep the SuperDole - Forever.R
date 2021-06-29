pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,tidyr,zoo,RCurl)


Yearly_NIPA_Aggregates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/Yearly%20Aggregates.csv")
# create data
Yearly_NIPA_Aggregates <- t(Yearly_NIPA_Aggregates)#transposing data

colnames(Yearly_NIPA_Aggregates) <- Yearly_NIPA_Aggregates[1,] #moving text names to headers
Yearly_NIPA_Aggregates <-Yearly_NIPA_Aggregates[-1,] #removing text names

Yearly_NIPA_Aggregates <- as.data.frame(Yearly_NIPA_Aggregates) #forcing data frame

Yearly_NIPA_Aggregates <- data.frame(lapply(Yearly_NIPA_Aggregates,as.numeric)) # making everything numeric. Lapply really messed with the variable names here though

Yearly_NIPA_Aggregates <- transform(Yearly_NIPA_Aggregates, `Personal income` = as.numeric(`Personal income`))

Yearly_NIPA_Aggregates$Year <- as.IDate(c("2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01"))


ggplot() + 
  geom_line(data=Yearly_NIPA_Aggregates, aes(x=Year,y=Personal.income,color= "Nominal Personal Income, Billions"), size = 1.25) + 
  #scale_x_date(limits = Start.end) +
  scale_y_continuous(labels = scales::comma,limits = c(15000,20000)) +
  #xlim(2000,2021) +
  xlab("Date") +
  ylab("Personal Income, Nominal $, Billions") +
  labs(caption = "Graph created by @JosephPolitano using BEA data") + 
  #scale_color_manual(name="", values = c("clrscheme" = "red","black")) +
  #scale_fill_manual(name="", values=c("clrscheme"= "red","black")) +
  
  theme_economist()+
  scale_color_economist(name="")


time <- as.numeric(rep(seq(2015,2020),each=7))  # x axis = 7 to include personal income, pre existing transfers, traditional unemployment, expanded unemployment, checks, ppp, and other new programs
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)

?rep()
# stacked area chart
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())


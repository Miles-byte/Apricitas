pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,tidyr,zoo,RCurl)

Yearly_NIPA_Aggregates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Keep%20the%20SuperDole%20-%20Forever/Yearly%20Aggregates.csv")
# create data
Yearly_NIPA_Aggregates <- t(Yearly_NIPA_Aggregates)#transposing data

colnames(Yearly_NIPA_Aggregates) <- Yearly_NIPA_Aggregates[1,] #moving text names to headers
Yearly_NIPA_Aggregates <-Yearly_NIPA_Aggregates[-1,]

?reshape()
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


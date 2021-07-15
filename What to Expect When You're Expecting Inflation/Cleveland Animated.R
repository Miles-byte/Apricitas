if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,scale,data.table,lubridate,forecast,gifski,av,tidyr,gganimate)

ClevelandFedModel <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Cleveland Fed Inflation Expectations Model.csv")

ClevelandFedModel2 <- import("C:/Users/Joseph/Documents/SubStack/What to Expect when you're expecting inflation/Cleveland Fed Attempt 2.csv")

summary(ClevelandFedModel)

summary(ClevelandFedModel2)

head(ClevelandFedModel)

gathered_Cleveland_Fed2 <- gather(ClevelandFedModel2, Period, Exp_Inf,`1 year Expected Inflation`:`30 year Expected Inflation`)

gathered_Cleveland_Fed2 <- gather(ClevelandFedModel2, Period, Exp_Inf,`1 year Expected Inflation`:`30 year Expected Inflation`)


gathered_Cleveland_Fed <- gather(ClevelandFedModel, Period, Exp_Inf,`1 year Expected Inflation`:`30 year Expected Inflation`)

gathered_Cleveland_Fed$`Model Output Date` <-as.Date(gathered_Cleveland_Fed$`Model Output Date`,"%m/%d/%Y")

gathered_Cleveland_Fed2$`Model Output Date` <-as.Date(gathered_Cleveland_Fed2$`Model Output Date`,"%m/%d/%Y")

gathered_Cleveland_Fed2$Years <- as.numeric(rep(seq(1,30),each=474))

p <- ggplot(gathered_Cleveland_Fed2, aes(x = Years,y = Exp_Inf,fill = Period)) +
  geom_point(size = 2) +
  theme_clean() +
  theme_economist() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Horizon (years)") +
  ylab("Expected Inflation") +
  theme_economist() +
  ggtitle("Cleveland Fed Model of Inflation Expectations: Term Structure Over Time") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data                    apricitas.substack.com") +
  theme(legend.position = "none")
  
animated_graph <- p + transition_time(`Model Output Date`) + labs(subtitle = 'Date of Expectations: {frame_time}',size = 2)
attempt2 <- p + transition_states(`Model Output Date`, transition_length = 0.1, state_length = 0.1) + labs(subtitle = 'Date of Expectations: {closest_state}')
?transition_states()
?transition_time()
animate(animated_graph, height = 570, width = 912, fps = 60, duration = 20, end_pause = 360, res = 100)
animate(attempt2, height = 570, width = 912, fps = 60, duration = 25, end_pause = 360, res = 100)

anim_save("IE over time month.gif")

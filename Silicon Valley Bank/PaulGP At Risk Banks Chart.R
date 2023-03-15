p_load(ggrepel,readxl,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

library(dplyr,readxl)
# load data


full_data = read_excel("C:/Users/Joseph/Downloads/bank_data (1).xlsx") %>%
  gather(date, close_price, -ticker) %>%
  group_by(ticker) %>%
  mutate(date = as.Date(date),
         close_price = as.numeric(close_price)) %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  filter(ticker != "ABBB") %>%
  mutate(daily_return = (close_price/lag(close_price)) - 1) %>%
  mutate(daily_return_ann = (1+daily_return)^{365} - 1) %>%
  filter(!is.na(daily_return ))

# bank_balance_sheet = read_excel("~/Downloads/bank_balance_sheets.xlsx")  %>%
#   mutate(date = as.Date(`Data Date`)) %>%
#   select(date, ticker = `Ticker Symbol`,
#          deposit = `Deposits - Total`,
#          assets = `Assets - Total`,
#          liabilities = `Liabilities - Total`,
#          fhlb = `Advances from FHLB`) %>%
#   mutate(fhlb = replace_na(fhlb, 0)) %>%
#   filter(date == as.Date("2022-12-31")) %>%
#   select(-date)

bank_data = full_data %>% filter(ticker != "s&p")
sp_data = full_data %>% filter(ticker == "s&p") %>%
  ungroup() %>%
  select(daily_return_mkt = daily_return, date)

bank_data = bank_data %>% left_join(sp_data) %>%
  mutate(abnormal = daily_return - daily_return_mkt) %>%
  mutate(cumul_abnormal = exp(cumsum(log(1+abnormal)))-1) %>%
  filter(ticker != "SI")  

bank_cumul_ret = bank_data %>% filter(date == as.Date("2023-03-10")) %>%
  arrange(cumul_abnormal) 
ggplot(data= bank_cumul_ret[1:20,]) +
  geom_col(aes(y = cumul_abnormal, x= reorder(ticker, cumul_abnormal))) + 
  coord_flip() +
  theme_minimal() +
  labs(y = "Cumulative Return Excess of Market", x = "Bank Ticker") +
  scale_y_continuous(labels = scales::percent_format()) 

ggplot(data= bank_cumul_ret) +
  geom_density(aes(x = cumul_abnormal)) + 
  theme_minimal() +
  labs(y = "Percent of Banks", x = "Cumulative Return Excess of Market") +
  scale_x_continuous(labels = scales::percent_format()) 
# 
# ggplot(data= bank_cumul_ret %>% left_join(bank_balance_sheet) %>% 
#          filter(!is.na(deposit))) +
#   geom_point(aes(x = deposit, y = cumul_abnormal )) + 
#   geom_smooth(aes(x = deposit, y = cumul_abnormal )) + 
#   theme_minimal() +
#   labs(x = "Total Deposits (millions)", y = "Cumulative Return Excess of Market") +
#   scale_x_log10(labels = scales::dollar_format()) +
#   scale_y_continuous(labels = scales::percent_format())

bank_list = c( bank_cumul_ret[1:5,] %>% pull(ticker), "JPM", "C", "BAC", "WFC")

bank_list = c( bank_cumul_ret[1:7,] %>% pull(ticker))
ggplot(data = bank_data %>% 
         filter(ticker %in% bank_list)) + 
  geom_line(aes(y = cumul_abnormal, x = date, color = ticker)) +
  geom_point(aes(y = cumul_abnormal, x = date, color = ticker)) +
  geom_text_repel(data = bank_data %>% 
                    filter(ticker %in% bank_list & date == as.Date("2023-03-10")), 
                  aes(y = cumul_abnormal, x = date, label = ticker, color = ticker),
                  hjust=0
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(as.Date("2023-03-02"), as.Date("2023-03-11")))+
  labs(y = "Cumulative Return Excess of Market",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "none")

?geom_point

geom_text_repel(data = bank_data %>% 
                  filter(ticker %in% bank_list & date == as.Date("2023-03-10") & ticker != "SIVB"), 
                aes(y = cumul_abnormal, x = date, label = ticker, color = ticker),
                hjust=0
)

AT_RISK_BANKS_graph <- #plotting loan performance data
  ggplot(data = subset(bank_data, ticker != "SIVB") %>% 
           filter(ticker %in% bank_list) %>%
            mutate(ticker = gsub("SBNY", "Signature Bank", ticker)) %>%
           mutate(ticker = gsub("FRC", "First Republic Bank", ticker)) %>%
           mutate(ticker = gsub("WAL", "Western Alliance Bank", ticker))%>%
           mutate(ticker = gsub("PACW", "Pacific Western Bank", ticker))%>%
           mutate(ticker = gsub("FFWM", "First Foundation Bank", ticker))%>%
           mutate(ticker = gsub("CUBI", "Customers Bank", ticker)) %>%
           mutate(ticker = factor(ticker,levels = c("Pacific Western Bank","Signature Bank","Western Alliance Bank","First Republic Bank","Customers Bank","First Foundation Bank"))))+ 
  geom_line(aes(y = cumul_abnormal, x = date, color = ticker),size = 1.25) +
  geom_point(aes(y = cumul_abnormal, x = date, color = ticker), size = 2) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Cumulative Return in Excess of Markets") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,-.10,-.20,-.30,-.40,-.50,-.60), limits = c(-.60,0), expand = c(0,0)) +
  ggtitle("The Most At-Risk Banks") +
  labs(caption = "Graph created by @JosephPolitano using @Paulgp data", subtitle = "Of the Six Banks With the Worst Recent Returns, Most are CA or Tech-Related") +
  theme_apricitas + theme(legend.position = c(.5,.44)) +
  scale_color_manual(name= "Cumulative Return in Excess of Markets",values = c("#9A348E","#FFE98F","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-06-01")-(.1861*(today()-as.Date("2013-06-01"))), xmax = as.Date("2013-06-01")-(0.049*(today()-as.Date("2013-06-01"))), ymin = 0-(.3*.14), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AT_RISK_BANKS_graph, "At Risk Banks.png", type = "cairo-png") #cairo gets rid of anti aliasing

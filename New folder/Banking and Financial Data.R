pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Graphing Fed Balance Sheet
TREASURY_ASSETS <- fredr(series_id = "WSHOTSL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Treasury Securities")
MBS_ASSETS <- fredr(series_id = "WSHOMCB",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Mortgage Backed Securities")
ALL_ASSETS <- fredr(series_id = "WALCL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Assets")

FED_ASSETS <- rbind(TREASURY_ASSETS,MBS_ASSETS,ALL_ASSETS) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`All Other Assets` = `All Assets`-`Treasury Securities`-`Mortgage Backed Securities`) %>%
  select(-`All Assets`) %>%
  pivot_longer(cols = `Treasury Securities`:`All Other Assets`)

CURRENCY_LIABILITIES <- fredr(series_id = "WLFN",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Physical Currency")
RESERVES_LIABILITIES <- fredr(series_id = "WLDLCL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Bank Reserves")
REVERSE_REPO_LIABILITIES <- fredr(series_id = "WLRRAL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Reverse Repo")
ALL_LIABILITIES <- fredr(series_id = "WLTLECL",observation_start = as.Date("1995-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Liabilities")

FED_LIABILITIES <- rbind(CURRENCY_LIABILITIES,RESERVES_LIABILITIES,REVERSE_REPO_LIABILITIES,ALL_LIABILITIES) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`All Other Liabilities` = `All Liabilities`-`Reverse Repo`-`Bank Reserves`-`Physical Currency`) %>%
  select(-`All Liabilities`) %>%
  pivot_longer(cols = `Physical Currency`:`All Other Liabilities`) %>%
  mutate(name = factor(name,levels = c("All Other Liabilities","Reverse Repo","Physical Currency","Bank Reserves")))


FED_ASSETS_Graph <- ggplot(data = FED_ASSETS, aes(x = date, y = value/1000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("The Start of Quantitative Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Federal Reserve's Balance Sheet is Shrinking Again") +
  theme_apricitas + theme(legend.position = c(.43,.85)) +
  scale_fill_manual(name= "Federal Reserve Assets",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#00A99D","#A7ACD9","#3083DC"), breaks = c("Treasury Securities","Mortgage Backed Securities","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-12-18")-(.1861*(today()-as.Date("2002-12-18"))), xmax = as.Date("2002-12-18")-(0.049*(today()-as.Date("2002-12-18"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FED_LIABILITIES_Graph <- ggplot(data = FED_LIABILITIES, aes(x = date, y = value/1000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("The Start of Quantitative Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "The Federal Reserve's Balance Sheet is Shrinking Again") +
  theme_apricitas + theme(legend.position = c(.43,.8)) +
  scale_fill_manual(name= "Federal Reserve Liabilities",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#00A99D","#A7ACD9","#3083DC"), breaks = c("Bank Reserves","Physical Currency","Reverse Repo","All Other Liabilities")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-12-18")-(.1861*(today()-as.Date("2002-12-18"))), xmax = as.Date("2002-12-18")-(0.049*(today()-as.Date("2002-12-18"))), ymin = 0-(.3*10), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_ASSETS_Graph, "Federal Reserve Assets.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = FED_LIABILITIES_Graph, "Federal Reserve Liabilities.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Commercial Bank H.8 Data

SECURITIES_ASSETS <- fredr(series_id = "SBCACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Treasury, MBS, and Other Securities")
LOANS_ASSETS <- fredr(series_id = "TOTLL",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Loans and Leases")
CASH_ASSETS <- fredr(series_id = "CASACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Bank Reserves and Other Cash Assets")
ALL_ASSETS <- fredr(series_id = "TLAACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "All Assets")
CAPITAL_ASSETS <- fredr(series_id = "RALACBW027SBOG",observation_start = as.Date("2002-12-18"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(name = "Bank Capital (Assets Net Liabilities)")

BANK_ASSETS <- rbind(SECURITIES_ASSETS,LOANS_ASSETS,CASH_ASSETS,ALL_ASSETS,CAPITAL_ASSETS) %>%
  select(-realtime_start,-realtime_end,-series_id) %>%
  pivot_wider() %>% 
  mutate(`All Other Assets` = `All Assets`-`Treasury, MBS, and Other Securities`-`Loans and Leases`-`Bank Reserves and Other Cash Assets`) %>%
  select(-`All Assets`) %>%
  pivot_longer(cols = `Treasury, MBS, and Other Securities`:`All Other Assets`) %>%
  mutate(name = factor(name,levels = c("All Other Assets","Loans and Leases", "Treasury, MBS, and Other Securities","Bank Reserves and Other Cash Assets","Bank Capital (Assets Net Liabilities)")))

BANK_ASSETS_Graph <- ggplot(data = BANK_ASSETS, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "T"), breaks = c(0,5,10,15,20,25), limits = c(0,25.5), expand = c(0,0)) +
  ggtitle("The Start of Quantitative Tightening") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Commercial Bank Asset Levels are Dropping, Though Loans and Leases are Increasing") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Assets of Commercial Banks in the United States",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Bank Capital (Assets Net Liabilities)","Bank Reserves and Other Cash Assets","Treasury, MBS, and Other Securities","Loans and Leases","All Other Assets")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-12-18")-(.1861*(today()-as.Date("2002-12-18"))), xmax = as.Date("2002-12-18")-(0.049*(today()-as.Date("2002-12-18"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BANK_ASSETS_Graph, "Bank Assets.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
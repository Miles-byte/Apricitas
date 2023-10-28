pacman::p_load(ggpubr,convey,survey,mitools,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

remotes::install_github("ajdamico/lodown")
library(lodown)

#NOTE: THIS LINE DOWNLOADS A MASSIVE AMOUNT OF DATA FROM THE SURVEY OF CONSUMER FINANCES
#Commented out So you don't accidentally run it unless you need to

#lodown("scf", output_dir = file.path( path.expand( "~" ) , "SCF" ))

#DOUBLE CHECK THAT IT IS INFLATION ADJUSTED

AGE_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_agecl_median.csv") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

EDU_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_edcl_median.csv")  %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

FAM_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_famstruct_median.csv")  %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

RAC_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_racecl4_median.csv")  %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

HOU_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_housecl_median.csv")  %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

INC_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_inccat_median.csv")  %>%
  mutate(year = as.Date(paste0(year, "-01-01")))
#

AGE_HAVE_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_agecl_have.csv") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

RACE_HAVE_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_racecl4_have.csv") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

HOU_HAVE_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/ddf12d6c47eda47dd24622614b151928450efda1/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_housecl_have.csv") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

#UNREALIZED CAPITAL GAINS UP A TON

ALL_MEDIAN_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_all_median.csv") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

ALL_HAVE_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/SCF%20Data/interactive_bulletin_charts_all_have.csv") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

ALL_HAVE_DATA_ASSETS_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Transaction_Accounts/100, color = "Checking and Other Transaction Accounts"), size = 1.25) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Owned_Vehicles/100, color = "Owned Vehicles"), size = 1.25) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Primary_Residence/100, color = "Primary Residence"), size = 1.25) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Stock_Holdings/100, color = "Direct and Indirect Stock Holdings"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,.25,.5,.75,1), expand = c(0,0)) +
  ylab("Share of Families") +
  ggtitle("Percent of Families With:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Median US Family Has Bank accounts, Cars, a Home, and a Small Amount of Stock Exposure") +
  theme_apricitas + theme(legend.position = c(.75,.20)) +
  scale_color_manual(name= "Share of Families With Select Assets",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Checking and Other Transaction Accounts","Owned Vehicles","Primary Residence","Direct and Indirect Stock Holdings")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_HAVE_DATA_ASSETS_GRAPH, "All Have Data Assets SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MEDIAN_NET_WORTH_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = ALL_MEDIAN_DATA, aes(x = year, y = Net_Worth, color = "Median Real Family Net Worth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"),limits = c(0,200), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("2022 Dollars") +
  ggtitle("US Median Wealth Has hit a Record High") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Real Net Worth of the Median American Family Rose 37% Since 2019 to a Record High") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEDIAN_NET_WORTH_GRAPH, "Median Net Worth SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RENT_OWN_MEDIAN_NET_WORTH_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = filter(HOU_MEDIAN_DATA, Category == "Owner"), aes(x = year, y = Net_Worth, color = "Median Homeowner's Real Family Net Worth"), size = 1.25) +
  geom_line(data = filter(HOU_MEDIAN_DATA, Category == "Owner"), aes(x = year, y = Primary_Residence, color = "Median Homeowner's Real Home Value"), size = 1.25) +
  geom_line(data = filter(HOU_MEDIAN_DATA, Category == "Renter or other"), aes(x = year, y = Net_Worth, color = "Median Renter's Real Family Net Worth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"),limits = c(0,400), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ylab("2022 Dollars") +
  ggtitle("Home Prices Drive Median Wealth Growth") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Homeowners' Real Net Worth Surged Since 2019, Largely Thanks to a Boom in Home Values") +
  theme_apricitas + theme(legend.position = c(.65,.425)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*400), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RENT_OWN_MEDIAN_NET_WORTH_GRAPH, "Rent Own Median Net Worth SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MEDIAN_HH_INC_MEDIAN_HOME_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = ALL_MEDIAN_DATA, aes(x = year, y = Primary_Residence/Before_Tax_Income, color = "Ratio of Median Home Value to Median Pretax Household Income"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(2,5), breaks = c(2,3,4,5), expand = c(0,0)) +
  ylab("Ratio") +
  ggtitle("Home Afforability Worsened During COVID") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Growth in Home Prices Have Outpaced Income Gains Since 2019") +
  theme_apricitas + theme(legend.position = c(.45,.825)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 2-(.3*3), ymax = 2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MEDIAN_HH_INC_MEDIAN_HOME_GRAPH, "Median Income to Median Home Value SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


DIRECT_STOCK_OWNERSHIP <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  #geom_line(data = filter(RACE_HAVE_DATA, Category == "Black, non-Hispanic") , aes(x = year, y = Directly_Held_Stocks, color = "Black"), size = 1.25) +
  #geom_line(data = filter(RACE_HAVE_DATA, Category == "Hispanic") , aes(x = year, y = Directly_Held_Stocks, color = "Hispanic"), size = 1.25) +
  geom_line(data = filter(AGE_HAVE_DATA, Catetory == "Less than 35"), aes(x = year, y = Directly_Held_Stocks/100, color = "Direct Individual Stock Ownership Rate Among Under-35s"), size = 1.25) +
  #geom_line(data = filter(AGE_HAVE_DATA, Catetory == "Less than 35"), aes(x = year, y = Stock_Holdings/100, color = "Diret & Indirect (e.g. via Retirement Accounts) Stock Ownership Rate Among Under-35s"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.25), breaks = c(0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Percent of Families") +
  ggtitle("The Boom in Direct Stock Ownership") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Share of Young Families Directly Invested in Stocks has Surged to Record Highs") +
  theme_apricitas + theme(legend.position = c(.5,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DIRECT_STOCK_OWNERSHIP, "Direct Stock Ownership Graph SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STOCK_OWNERSHIP_CATEGORIES_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Stock_Holdings/100, color = "Direct and Indirect Stock Holdings"), size = 1.25) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Retirement_Accounts/100, color = "Retirement Accounts"), size = 1.25) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Pooled_Investment_Funds/100, color = "Direct Pooled Investment Fund (ETF, Mutual Fund, etc.) Holdings"), size = 1.25) +
  geom_line(data = ALL_HAVE_DATA, aes(x = year, y = Directly_Held_Stocks/100, color = "Direct Individual Stock Holdings"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.60), breaks = c(0,.20,.40,.60), expand = c(0,0)) +
  ylab("Share of Families") +
  ggtitle("Percent of Families With:") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Stock Ownership Rates Have Risen Throughout the Pandemic") +
  theme_apricitas + theme(legend.position = c(.65,.55)) +
  scale_color_manual(name= "Share of Families With Select Assets",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Direct and Indirect Stock Holdings","Retirement Accounts","Direct Individual Stock Holdings","Direct Pooled Investment Fund (ETF, Mutual Fund, etc.) Holdings")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*.6), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STOCK_OWNERSHIP_CATEGORIES_GRAPH, "Stock Ownership Have Data Assets SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

DIRECT_STOCK_OWNERSHIP_CATEGORIES_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = filter(AGE_HAVE_DATA, Catetory == "Less than 35"), aes(x = year, y = Directly_Held_Stocks/100, color = "Under-35 Families"), size = 1.25) +
  geom_line(data = filter(RACE_HAVE_DATA, Category == "Black, non-Hispanic"), aes(x = year, y = Directly_Held_Stocks/100, color = "Black Families"), size = 1.25) +
  geom_line(data = filter(RACE_HAVE_DATA, Category == "Hispanic"), aes(x = year, y = Directly_Held_Stocks/100, color = "Hispanic Families"), size = 1.25) +
  geom_line(data = filter(HOU_HAVE_DATA, Category == "Renter or other"), aes(x = year, y = Directly_Held_Stocks/100, color = "Renter Families"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.25), breaks = c(0,0.05,.1,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Share of Families") +
  ggtitle("Percent of Families Owning Individual Stocks") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Record Shares of Young, Renter, Black, and Hispanic Families Now Own Stocks") +
  theme_apricitas + theme(legend.position = c(.35,.825), plot.title = element_text(size = 26)) +
  scale_color_manual(name= "Share of Families With Direct Individual Stock Ownership",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Under-35 Families","Renter Families","Black Families","Hispanic Families")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DIRECT_STOCK_OWNERSHIP_CATEGORIES_GRAPH, "Direct Stock Ownership Categories SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

DIRECT_BIZ_OWNERSHIP_CATEGORIES_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = filter(AGE_HAVE_DATA, Catetory == "Less than 35"), aes(x = year, y = Business_Equity/100, color = "Under-35 Families"), size = 1.25) +
  geom_line(data = filter(HOU_HAVE_DATA, Category == "Renter or other"), aes(x = year, y = Business_Equity/100, color = "Renter Families"), size = 1.25) +
  geom_line(data = filter(RACE_HAVE_DATA, Category == "Hispanic"), aes(x = year, y = Business_Equity/100, color = "Hispanic Families"), size = 1.25) +
  geom_line(data = filter(RACE_HAVE_DATA, Category == "Black, non-Hispanic"), aes(x = year, y = Business_Equity/100, color = "Black Families"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.12), breaks = c(0,0.05,.1,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Share of Families") +
  ggtitle("Percent of Families Owning a Business") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Record Shares of Young, Renter, Black, and Hispanic Families Now Own Their Own Businesses") +
  theme_apricitas + theme(legend.position = c(.67,.175), plot.title = element_text(size = 26)) +
  scale_color_manual(name= "Share of Families With Direct Business Equity",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Under-35 Families","Renter Families","Black Families","Hispanic Families")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*.125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DIRECT_BIZ_OWNERSHIP_CATEGORIES_GRAPH, "Direct Biz Ownership Categories SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BANK_BALANCE_INCOME_PCT <- ggplot() + #plotting ownership share over time
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = filter(INC_MEDIAN_DATA, Category == "Less than 20" & year >= as.Date("2019-01-01")), aes(x = year, y = Transaction_Accounts/Transaction_Accounts[1]-1, color = "Less Than 20"), size = 1.25) +
  geom_line(data = filter(INC_MEDIAN_DATA, Category == "20-39.9" & year >= as.Date("2019-01-01")), aes(x = year, y = Transaction_Accounts/Transaction_Accounts[1]-1, color = "20-39.9"), size = 1.25) +
  geom_line(data = filter(INC_MEDIAN_DATA, Category == "40-59.9" & year >= as.Date("2019-01-01")), aes(x = year, y = Transaction_Accounts/Transaction_Accounts[1]-1, color = "40-59.9"), size = 1.25) +
  geom_line(data = filter(INC_MEDIAN_DATA, Category == "60-79.9" & year >= as.Date("2019-01-01")), aes(x = year, y = Transaction_Accounts/Transaction_Accounts[1]-1, color = "60-79.9"), size = 1.25) +
  geom_line(data = filter(INC_MEDIAN_DATA, Category == "80-89.9" & year >= as.Date("2019-01-01")), aes(x = year, y = Transaction_Accounts/Transaction_Accounts[1]-1, color = "80-89.9"), size = 1.25) +
  geom_line(data = filter(INC_MEDIAN_DATA, Category == "90-100" & year >= as.Date("2019-01-01")), aes(x = year, y = Transaction_Accounts/Transaction_Accounts[1]-1, color = "Above 90"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.05,.50), breaks = c(0,.10,.20,.30,.40,.50), expand = c(0,0)) +
  ylab("Percent Growth From 2019") +
  ggtitle("The Boom in Direct Stock Ownership") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Share of Young Families Directly Invested in Stocks has Surged to Record Highs") +
  theme_apricitas + theme(legend.position = c(.5,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Less Than 20","20-39.9","40-59.9","60-79.9","80-89.9","Above 90")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1989-01-01")-(.1861*(today()-as.Date("1989-01-01"))), xmax = as.Date("1989-01-01")-(0.049*(today()-as.Date("1989-01-01"))), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

INC_MEDIAN_DATA_PCT_GROWTH <- INC_MEDIAN_DATA %>%
  filter(year >= as.Date("2019-01-01")) %>%
  select(year, Category, Transaction_Accounts) %>%
  pivot_wider(names_from = year, values_from = Transaction_Accounts) %>%
  transmute(Category, pct_change_median = (`2022-01-01`-`2019-01-01`)/`2019-01-01`) %>%
  mutate(Category = factor(Category, levels = c("Less than 20","20-39.9","40-59.9","60-79.9","80-89.9","90-100")))

INC_MEAN_DATA_RAW_GROWTH <- INC_MEDIAN_DATA %>%
  filter(year >= as.Date("2019-01-01")) %>%
  select(year, Category, Transaction_Accounts) %>%
  pivot_wider(names_from = year, values_from = Transaction_Accounts) %>%
  transmute(Category, dol_change_median = (`2022-01-01`-`2019-01-01`))  %>%
  mutate(Category = factor(Category, levels = c("Less than 20","20-39.9","40-59.9","60-79.9","80-89.9","90-100")))

BANK_BALANCE_INCOME_PCT <- ggplot(data = INC_MEDIAN_DATA_PCT_GROWTH, aes(x = Category, y = pct_change_median, fill = Category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_area(data = Savings_Component_Net_Fiscal_Longer, aes(x = date, y = value/1000, fill = name), color = NA, size = 0) +
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  xlab("Percentile of Family Income") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-0.05,0.5), breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ylab("Percent Change in Median Real Cash Balances") +
  ggtitle("Percent Change") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Higher Spending and Lower Net Government Transfers Have Reduced 'Excess' Savings") +
  theme_apricitas + theme(plot.title = element_text(size = 23), legend.position = "left", axis.text.x = element_blank()) +
  scale_fill_manual(name= "Percentile of Family Income",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#EE6055","RED"))

BANK_BALANCE_INCOME_RAW <- ggplot(data = INC_MEAN_DATA_RAW_GROWTH, aes(x = Category, y = dol_change_median, fill = Category)) +
  #geom_area(data = Savings_Component_Net_Fiscal_Longer, aes(x = date, y = value/1000, fill = name), color = NA, size = 0) +
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  xlab("Percentile of Family Income") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "k"),limits = c(-0.05,32.5), breaks = c(0,10,20,30), expand = c(0,0)) +
  ylab("Dollar Change in Median Real Cash Balances") +
  ggtitle("Dollar Change") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Higher Spending and Lower Net Government Transfers Have Reduced 'Excess' Savings") +
  theme_apricitas + theme(plot.title = element_text(size = 23), legend.position = "none", axis.text.x = element_blank()) +
  scale_fill_manual(name= "Percentile of Family Income",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#EE6055","RED"))

BANK_BALANCE_COMBINED_PLOT <- ggarrange(BANK_BALANCE_INCOME_PCT, BANK_BALANCE_INCOME_RAW, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

BANK_BALANCE_COMBINED_PLOT <- annotate_figure(BANK_BALANCE_COMBINED_PLOT, 
                                 top = text_grob("Change in Median Real Cash Balances 2019-22\n By Family Income Percentiles", 
                                                 size = 28, 
                                                 face = "bold", 
                                                 hjust = 0.5,
                                                 color = "white")
) + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = BANK_BALANCE_COMBINED_PLOT, "Bank Balance Combined SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


INC_MEDIAN_CC_DATA_PCT_GROWTH <- INC_MEDIAN_DATA %>%
  filter(year >= as.Date("2019-01-01")) %>%
  select(year, Category, Credit_Card_Balances) %>%
  pivot_wider(names_from = year, values_from = Credit_Card_Balances) %>%
  transmute(Category, pct_change_median = (`2022-01-01`-`2019-01-01`)/`2019-01-01`) %>%
  mutate(Category = factor(Category, levels = c("Less than 20","20-39.9","40-59.9","60-79.9","80-89.9","90-100")))

INC_MEAN_CC_DATA_RAW_GROWTH <- INC_MEDIAN_DATA %>%
  filter(year >= as.Date("2019-01-01")) %>%
  select(year, Category, Credit_Card_Balances) %>%
  pivot_wider(names_from = year, values_from = Credit_Card_Balances) %>%
  transmute(Category, dol_change_median = (`2022-01-01`-`2019-01-01`))  %>%
  mutate(Category = factor(Category, levels = c("Less than 20","20-39.9","40-59.9","60-79.9","80-89.9","90-100")))


CC_INCOME_PCT <- ggplot(data = INC_MEDIAN_CC_DATA_PCT_GROWTH, aes(x = Category, y = pct_change_median, fill = Category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_area(data = Savings_Component_Net_Fiscal_Longer, aes(x = date, y = value/1000, fill = name), color = NA, size = 0) +
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  xlab("Percentile of Family Income") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-0.3,0.1), breaks = c(-.3,-.2,-.1,0,.1), expand = c(0,0)) +
  ylab("Percent Change in Median Real Credit Card Debt") +
  ggtitle("Percent Change") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Higher Spending and Lower Net Government Transfers Have Reduced 'Excess' Savings") +
  theme_apricitas + theme(plot.title = element_text(size = 23), legend.position = "left", axis.text.x = element_blank()) +
  scale_fill_manual(name= "Percentile of Family Income",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#EE6055","RED"))

CC_INCOME_RAW <- ggplot(data = INC_MEAN_CC_DATA_RAW_GROWTH, aes(x = Category, y = dol_change_median*1000, fill = Category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #geom_area(data = Savings_Component_Net_Fiscal_Longer, aes(x = date, y = value/1000, fill = name), color = NA, size = 0) +
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  xlab("Percentile of Family Income") +
  scale_y_continuous(labels = scales::dollar_format(),limits = c(-1000,250), breaks = c(-1000,-750,-500,-250,0,250), expand = c(0,0)) +
  ylab("Dollar Change in Median Real Credit Card Debt") +
  ggtitle("Dollar Change") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Higher Spending and Lower Net Government Transfers Have Reduced 'Excess' Savings") +
  theme_apricitas + theme(plot.title = element_text(size = 23), legend.position = "none", axis.text.x = element_blank()) +
  scale_fill_manual(name= "Percentile of Family Income",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#EE6055","RED"))

CC_COMBINED_PLOT <- ggarrange(CC_INCOME_PCT, CC_INCOME_RAW, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

CC_COMBINED_PLOT <- annotate_figure(CC_COMBINED_PLOT, 
                                              top = text_grob("Change in Median Real Credit Card Debt 2019-22\n By Family Income Percentiles", 
                                                              size = 28, 
                                                              face = "bold", 
                                                              hjust = 0.5,
                                                              color = "white")
) + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = CC_COMBINED_PLOT, "CC Combined SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Importing downloaded SCF Relative Weights and microdata for 2019 and 2022
scf_imp_2019 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2019.rds" ) )

scf_rw_2019 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2019 rw.rds" ) )

scf_imp_2022 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2022.rds" ) )

scf_rw_2022 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2022 rw.rds" ) )

scf_design_2019 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2019[ , -1 ] , 
    data = imputationList(scf_imp_2019) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
)

scf_design_2022 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2022[ , -1 ] , 
    data = imputationList(scf_imp_2022) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
)

Networth_distribution_2019 <- scf_MIcombine( with( scf_design_2019 ,
                     svyquantile(
                       ~ networth ,
                       seq(0, 1, by = 0.01) , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

Networth_distribution_2022 <- scf_MIcombine( with( scf_design_2022,
                                                   svyquantile(
                                                     ~ networth ,
                                                     seq(0, 1, by = 0.01) , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                                                   ) ) )

Networth_distribution_2022_df <- as.data.frame(Networth_distribution_2022$coefficients) %>%
  tibble::remove_rownames() %>%
  transmute(percentile = seq(0, 1, by = 0.01), networth = Networth_distribution_2022$coefficients) %>%
  filter(!(percentile %in% c(0,1)))

Networth_distribution_2019_df <- as.data.frame(Networth_distribution_2019$coefficients) %>%
  tibble::remove_rownames() %>%
  transmute(percentile = seq(0, 1, by = 0.01), networth = Networth_distribution_2019$coefficients) %>%
  filter(!(percentile %in% c(0,1)))

REAL_NET_WORTH_DISTRIBUTION_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  annotate("text",label = "NOTE: Graph Only Goes Up To\nTop 1% Wealth Percentile Threshold", x = .78, y = 9, color = "white", size = 4) +
  geom_line(data = Networth_distribution_2019_df, aes(x = percentile, y = networth/1000000, color = "2019"), size = 1.25) +
  geom_line(data = Networth_distribution_2022_df, aes(x = percentile, y = networth/1000000, color = "2022"), size = 1.25) +
  xlab("Percentile of Net Worth Distribution") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "M"),limits = c(-.1,15), breaks = c(0,5,10,15), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(.25,.5,.75)) +
  ylab("2022 Dollars") +
  ggtitle("Wealth Distribution Remains Unequal") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Americans Got Richer and Wealth Inequality Eased—But Was Not Fundamentally Reshaped") +
  theme_apricitas + theme(legend.position = c(.7,.8)) +
  scale_color_manual(name= "Real Net Worth Distribution by Year",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*(1)), xmax = 0-(0.049*(1)), ymin = 0-(.3*15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_NET_WORTH_DISTRIBUTION_GRAPH, "Real Net Worth Distribution SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


Bank_distribution_2019 <- scf_MIcombine( with( scf_design_2019 ,
                                                   svyquantile(
                                                     ~ liq ,
                                                     seq(0, 1, by = 0.01) , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                                                   ) ) )

Bank_distribution_2022 <- scf_MIcombine( with( scf_design_2022,
                                                   svyquantile(
                                                     ~ liq ,
                                                     seq(0, 1, by = 0.01) , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                                                   ) ) )

Bank_distribution_2022_df <- as.data.frame(Bank_distribution_2022$coefficients) %>%
  tibble::remove_rownames() %>%
  transmute(percentile = seq(0, 1, by = 0.01), liq = Bank_distribution_2022$coefficients) %>%
  filter(!(percentile %in% c(0,1)))

Bank_distribution_2019_df <- as.data.frame(Bank_distribution_2019$coefficients) %>%
  tibble::remove_rownames() %>%
  transmute(percentile = seq(0, 1, by = 0.01), liq = Bank_distribution_2019$coefficients) %>%
  filter(!(percentile %in% c(0,1)))

REAL_BANK_DISTRIBUTION_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  annotate("text",label = "NOTE: Graph Only Goes Up To\nTop 1% Wealth Percentile", x = .8, y = 9, color = "white", size = 4) +
  geom_line(data = Bank_distribution_2019_df, aes(x = percentile, y = liq/1000, color = "2019"), size = 1.25) +
  geom_line(data = Bank_distribution_2022_df, aes(x = percentile, y = liq/1000, color = "2022"), size = 1.25) +
  xlab("Percentile of Net Worth Distribution") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"),limits = c(0,20), breaks = c(0,5,10,15), expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.6), breaks = c(.25,.5,.75)) +
  ylab("2022 Dollars") +
  ggtitle("Wealth Distribution Remains Unequal") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Americans Got Richer and Wealth Inequality Eased—But Was Not Fundamentally Reshaped") +
  theme_apricitas + theme(legend.position = c(.7,.8)) +
  scale_color_manual(name= "Real Net Worth Distribution by Year",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*(1)), xmax = 0-(0.049*(1)), ymin = 0-(.3*15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_BANK_DISTRIBUTION_GRAPH, "Real Bank Balance Distribution SCF.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

scf_design_2022_gini <- scf_design_2022
scf_design_2022_gini$designs <- lapply(scf_design_2022$designs,convey_prep)

GINI_2022 <- scf_MIcombine( with( scf_design_2022_gini , svygini( ~ networth ) ) )

scf_design_2019_gini <- scf_design_2019
scf_design_2019_gini$designs <- lapply(scf_design_2019$designs,convey_prep)

GINI_2019 <- scf_MIcombine( with( scf_design_2019_gini , svygini( ~ networth ) ) )


#Yes, I know it's horrendous coding practice to not use a loop here, but the process to set up the survey designs and use scf_MI_combine is so resource intensive that loops crash most of the time. This way allows me to go year-by-year and leaves me the option of easily interrupting if something goes wrong

scf_imp_2016 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2016.rds" ) )

scf_rw_2016 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2016 rw.rds" ) )

scf_design_2016 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2016[ , -1 ] , 
    data = imputationList(scf_imp_2016) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_2016_gini <- scf_design_2016
scf_design_2016_gini$designs <- lapply(scf_design_2016$designs,convey_prep)

GINI_2016 <- scf_MIcombine( with( scf_design_2016_gini , svygini( ~ networth ) ) )

#2013

scf_imp_2013 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2013.rds" ) )

scf_rw_2013 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2013 rw.rds" ) )

scf_design_2013 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2013[ , -1 ] , 
    data = imputationList(scf_imp_2013) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_2013_gini <- scf_design_2013
scf_design_2013_gini$designs <- lapply(scf_design_2013$designs,convey_prep)

GINI_2013 <- scf_MIcombine( with( scf_design_2013_gini , svygini( ~ networth ) ) )

#2010

scf_imp_2010 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2010.rds" ) )

scf_rw_2010 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2010 rw.rds" ) )

scf_design_2010 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2010[ , -1 ] , 
    data = imputationList(scf_imp_2010) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_2010_gini <- scf_design_2010
scf_design_2010_gini$designs <- lapply(scf_design_2010$designs,convey_prep)

GINI_2010 <- scf_MIcombine( with( scf_design_2010_gini , svygini( ~ networth ) ) )

#2007

scf_imp_2007 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2007.rds" ) )

scf_rw_2007 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2007 rw.rds" ) )

scf_design_2007 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2007[ , -1 ] , 
    data = imputationList(scf_imp_2007) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_2007_gini <- scf_design_2007
scf_design_2007_gini$designs <- lapply(scf_design_2007$designs,convey_prep)

GINI_2007 <- scf_MIcombine( with( scf_design_2007_gini , svygini( ~ networth ) ) )

#2004

scf_imp_2004 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2004.rds" ) )

scf_rw_2004 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2004 rw.rds" ) )

scf_design_2004 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2004[ , -1 ] , 
    data = imputationList(scf_imp_2004) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_2004_gini <- scf_design_2004
scf_design_2004_gini$designs <- lapply(scf_design_2004$designs,convey_prep)

GINI_2004 <- scf_MIcombine( with( scf_design_2004_gini , svygini( ~ networth ) ) )


#2001

scf_imp_2001 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2001.rds" ) )

scf_rw_2001 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2001 rw.rds" ) )

scf_design_2001 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_2001[ , -1 ] , 
    data = imputationList(scf_imp_2001) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_2001_gini <- scf_design_2001
scf_design_2001_gini$designs <- lapply(scf_design_2001$designs,convey_prep)

GINI_2001 <- scf_MIcombine( with( scf_design_2001_gini , svygini( ~ networth ) ) )

#1998

scf_imp_1998 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1998.rds" ) )

scf_rw_1998 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1998 rw.rds" ) )

scf_design_1998 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_1998[ , -1 ] , 
    data = imputationList(scf_imp_1998) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_1998_gini <- scf_design_1998
scf_design_1998_gini$designs <- lapply(scf_design_1998$designs,convey_prep)

GINI_1998 <- scf_MIcombine( with( scf_design_1998_gini , svygini( ~ networth ) ) )

#1995

scf_imp_1995 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1995.rds" ) )

scf_rw_1995 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1995 rw.rds" ) )

scf_design_1995 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_1995[ , -1 ] , 
    data = imputationList(scf_imp_1995) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_1995_gini <- scf_design_1995
scf_design_1995_gini$designs <- lapply(scf_design_1995$designs,convey_prep)

GINI_1995 <- scf_MIcombine( with( scf_design_1995_gini , svygini( ~ networth ) ) )

#1992

scf_imp_1992 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1992.rds" ) )

scf_rw_1992 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1992 rw.rds" ) )

scf_design_1992 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_1992[ , -1 ] , 
    data = imputationList(scf_imp_1992) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_1992_gini <- scf_design_1992
scf_design_1992_gini$designs <- lapply(scf_design_1992$designs,convey_prep)

GINI_1992 <- scf_MIcombine( with( scf_design_1992_gini , svygini( ~ networth ) ) )

#1989

scf_imp_1989 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1989.rds" ) )

scf_rw_1989 <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 1989 rw.rds" ) )

scf_design_1989 <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw_1989[ , -1 ] , 
    data = imputationList(scf_imp_1989) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design_1989_gini <- scf_design_1989
scf_design_1989_gini$designs <- lapply(scf_design_1989$designs,convey_prep)

GINI_1989 <- scf_MIcombine( with( scf_design_1989_gini , svygini( ~ networth ) ) )

FINAL_GINI <- tibble()

for (year in seq(1989, 2022, by = 3)) {
  TEMP_GINI <- as.data.frame(get(paste0("GINI_", year))$coefficients) %>%
    tibble::remove_rownames() %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  FINAL_GINI <- bind_rows(FINAL_GINI, TEMP_GINI)
}

FINAL_GINI <- FINAL_GINI %>% as.data.frame() %>%
  setnames(c("gini","date"))

FINAL_GINI_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = FINAL_GINI, aes(x = date, y = gini*100, color = "Gini Coefficient of Family Net Worth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(78,87), breaks = c(80,85,90), expand = c(0,0)) +
  ylab("Gini Coefficient") +
  ggtitle("Wealth Inequality is High But Fell to a Post-08 Low") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "US Wealth Inequality Eased During the Pandemic But Remains Very High") +
  theme_apricitas + theme(legend.position = c(.3,.8), plot.title = element_text(size = 23)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 78-(.3*9), ymax = 78) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FINAL_GINI_GRAPH, "Final GINI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Measuring rent-inclusive payment to income ratio

#2022
scf_design_2022_2 <- subset(scf_design_2022, income != 0)

scf_design_2022_2 <- update( 
  scf_design_2022_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2022 <- scf_MIcombine( with( scf_design_2022_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2019
scf_design_2019_2 <- subset(scf_design_2019, income != 0)

scf_design_2019_2 <- update( 
  scf_design_2019_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2019 <- scf_MIcombine( with( scf_design_2019_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2016
scf_design_2016_2 <- subset(scf_design_2016, income != 0)

scf_design_2016_2 <- update( 
  scf_design_2016_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2016 <- scf_MIcombine( with( scf_design_2016_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2013
scf_design_2013_2 <- subset(scf_design_2013, income != 0)

scf_design_2013_2 <- update( 
  scf_design_2013_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2013 <- scf_MIcombine( with( scf_design_2013_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2010
scf_design_2010_2 <- subset(scf_design_2010, income != 0)

scf_design_2010_2 <- update( 
  scf_design_2010_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2010 <- scf_MIcombine( with( scf_design_2010_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2007
scf_design_2007_2 <- subset(scf_design_2007, income != 0)

scf_design_2007_2 <- update( 
  scf_design_2007_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2007 <- scf_MIcombine( with( scf_design_2007_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2004
scf_design_2004_2 <- subset(scf_design_2004, income != 0)

scf_design_2004_2 <- update( 
  scf_design_2004_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2004 <- scf_MIcombine( with( scf_design_2004_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#2001
scf_design_2001_2 <- subset(scf_design_2001, income != 0)

scf_design_2001_2 <- update( 
  scf_design_2001_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112+x8428)/(income/12)
)

RIPIR_2001 <- scf_MIcombine( with( scf_design_2001_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#1998
scf_design_1998_2 <- subset(scf_design_1998, income != 0)

scf_design_1998_2 <- update( 
  scf_design_1998_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112)/(income/12)
)

RIPIR_1998 <- scf_MIcombine( with( scf_design_1998_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#1995
scf_design_1995_2 <- subset(scf_design_1995, income != 0)

scf_design_1995_2 <- update( 
  scf_design_1995_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112)/(income/12)
)

RIPIR_1995 <- scf_MIcombine( with( scf_design_1995_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#1992
scf_design_1992_2 <- subset(scf_design_1992, income != 0)

scf_design_1992_2 <- update( 
  scf_design_1992_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112)/(income/12)
)

RIPIR_1992 <- scf_MIcombine( with( scf_design_1992_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

#1989
scf_design_1989_2 <- subset(scf_design_1989, income != 0)

scf_design_1989_2 <- update( 
  scf_design_1989_2 , 
  
  RIPIR = (tpay+rent+x2105+x2112)/(income/12)
)

RIPIR_1989 <- scf_MIcombine( with( scf_design_1989_2 ,
                     svyquantile(
                       ~ RIPIR ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )


FINAL_RIPIR <- tibble()

for (year in seq(1989, 2022, by = 3)) {
  TEMP_RIPIR <- as.data.frame(get(paste0("RIPIR_", year))$coefficients) %>%
    tibble::remove_rownames() %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  FINAL_RIPIR <- bind_rows(FINAL_RIPIR, TEMP_RIPIR)
}

FINAL_RIPIR <- FINAL_RIPIR %>% as.data.frame() %>%
  setnames(c("RIPIR","date"))

FINAL_RIPIR_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = FINAL_RIPIR, aes(x = date, y = RIPIR, color = "Median Rent-Inclusive Payment-to-Income Ratio"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.19,.24), breaks = c(.19,.20,.21,.22,.23,.24), expand = c(0,0)) +
  ylab("Percent of Pretax Income") +
  ggtitle("Payment Burdens Have Fallen Significantly") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Median Household Is Spending Less of Their Income on Rent, Mortgages, and Other Debt") +
  theme_apricitas + theme(legend.position = c(.5,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .19-(.3*.05), ymax = .19) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FINAL_RIPIR_GRAPH, "Final RIPIR Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

test <- subset(scf_design_2022, nstocks != 0)
test_2019 <- subset(scf_design_2019, ntrad != 0)

scf_MIcombine( with( scf_design_2022, svymean( ~ ntrad ) ) )
scf_MIcombine( with( scf_design_2019, svymean( ~ ntrad ) ) )

FINAL_NTRAD <- tibble()

for (year in seq(2022, 1989, by = -3)) {
  TEMP_RESULT <- as.data.frame(scf_MIcombine(with(get(paste0("scf_design_", year)), svymean(~ ntrad)))$coefficients) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  FINAL_NTRAD <- bind_rows(FINAL_NTRAD, TEMP_RESULT)
}

FINAL_NTRAD_MEDIAN <- tibble()
FINAL_NTRAD_MEAN <- tibble()

for (year in seq(2022, 1989, by = -3)) {
  TEMP_NTRAD_MEAN <- scf_MIcombine(with(subset(get(paste0("scf_design_", year)), ntrad != 0), svymean(~ ntrad)))
  
  TEMP_NTRAD_MEDIAN <- scf_MIcombine(with(subset(get(paste0("scf_design_", year)), ntrad != 0), svyquantile(
    ~ ntrad ,
    0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
  ) ) )
  
  TEMP_NTRAD_MEAN <- as.data.frame(TEMP_NTRAD_MEAN$coefficients) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  TEMP_NTRAD_MEDIAN <- as.data.frame(TEMP_NTRAD_MEDIAN$coefficients) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  FINAL_NTRAD_MEAN <- bind_rows(FINAL_NTRAD_MEAN, TEMP_NTRAD_MEAN)
  FINAL_NTRAD_MEDIAN <- bind_rows(FINAL_NTRAD_MEDIAN, TEMP_NTRAD_MEDIAN)
}



FINAL_NSTOCKS_MEAN <- tibble()
FINAL_NSTOCKS_MEDIAN <- tibble()


for (year in seq(2022, 1989, by = -3)) {
  TEMP_NSTOCKS_MEAN <- scf_MIcombine(with(subset(get(paste0("scf_design_", year)), nstocks != 0), svymean(~ nstocks)))
  
  TEMP_NSTOCKS_MEDIAN <- scf_MIcombine(with(subset(get(paste0("scf_design_", year)), nstocks != 0), svyquantile(
    ~ nstocks ,
    0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
  ) ) )
  
  TEMP_NSTOCKS_MEAN <- as.data.frame(TEMP_NSTOCKS_MEAN$coefficients) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  TEMP_NSTOCKS_MEDIAN <- as.data.frame(TEMP_NSTOCKS_MEDIAN$coefficients) %>%
    mutate(date = as.Date(paste0(year, "-01-01")))
  
  FINAL_NSTOCKS_MEAN <- bind_rows(FINAL_NSTOCKS_MEAN, TEMP_NSTOCKS_MEAN)
  FINAL_NSTOCKS_MEDIAN <- bind_rows(FINAL_NSTOCKS_MEDIAN, TEMP_NSTOCKS_MEDIAN)
}

STOCK_MEAN_GRAPH <- ggplot() + #plotting ownership share over time
  #annotate("hline", y = .50, yintercept = .50, color = "white", size = 0.5) +
  geom_line(data = FINAL_NSTOCKS_MEAN, aes(x = date, y = `TEMP_NSTOCKS_MEAN$coefficients`, color = "Average Number of Stocks Held by Families With Direct Individual Stock Ownership"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,11), breaks = c(0,5,10), expand = c(0,0)) +
  ylab("Number if Individual Stocks") +
  ggtitle("Americans are Buying More Stocks") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Families are Increasing the Range of their Holdings As Direct Involvement In the Stock Market Rises") +
  theme_apricitas + theme(legend.position = c(.51,.2)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*11), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STOCK_MEAN_GRAPH, "Stock Mean Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



years <- seq(2022, 1992, by = -3)
result_list <- list()

for(y in years) {
  design_var <- paste0("scf_design_", y)
  scf_design <- get(design_var)
  scf_design <- update(scf_design, wsaved_factor = factor(wsaved))
  results <- scf_MIcombine(with(scf_design, svytotal(~wsaved_factor)))
  total_weight <- sum(as.data.frame(results$coefficients))
  results_perc <- as.data.frame(results$coefficients) / total_weight * 100
  results_perc$Year <- ymd(paste0(y, "-01-01"))
  result_list[[as.character(y)]] <- results_perc
}

spend_save_final_result <- bind_rows(result_list) %>%
  as.data.frame() %>%
  setNames(c("value","date")) %>%
  mutate(category = rep(c("Spending Exceeded Income", 
                          "Spending Equaled Income", 
                          "Spending Less Than Income"), 
                        length.out = n())) %>%
  mutate(category = factor(category, levels = rev(c("Spending Exceeded Income","Spending Equaled Income","Spending Less Than Income"))))


Spend_Save_Final_Result_Graph <- ggplot(data = spend_save_final_result, aes(x = date, y = value/100, fill = category)) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,.25,.5,.75,1), expand = c(0,0)) +
  ylab("Percent of Families") +
  ggtitle("Spending Down Their Savings?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Only a Slightly Higher Share of Households are Spending More Than They're Saving") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*20), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Spend_Save_Final_Result_Graph, "Spend Save Final Result Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


scf_MIcombine( with( test ,
                     svyquantile(
                       ~ ntrad ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )

scf_MIcombine( with( test_2019 ,
                     svyquantile(
                       ~ ntrad ,
                       0.5 , se = TRUE , method = 'constant' , interval.type = 'quantile' 
                     ) ) )


?scf_MIcombine()
scf_MIcombine( with( scf_design_2019, svymean( ~ ntrad ) ) )


#HBROK: Have Brokerage
#NSTOCKS: Number of Different Companies With Stock in
#NWCAT: Net Worth Percentiles
#HTRADE AND NTRADE VARIABLES: HAVE TRADED IN LAST YEAR AND NUMBER OF TRADES

#Growth in Nonhousing Wealth

#The SCF is a detailed triennial survey of U.S. family finances that, on its own, is not particularly well-suited to capture between-survey dynamics at the unprecedented scale of the pandemic. In anticipation of a need to better understand these dynamics, the 2022 SCF added questions to capture families' pandemic experiences ("COVID-19 questions").3 These questions included a longer lookback period than is typical for the SCF—asking families to consider their experiences since "the onset of the pandemic in early 2020"—and probed families' health and employment status, relief on required payments, experiences of financial hardship, receipt of early stimulus benefits, and child-related responsibilities.

#DISTRIBUTION IN INCREASE ON TRANSACTION ACCOUNTS BY CATEGORY
#NET WORTH RAW AND PERCENTILE INCREASE IN WEALTH DISTRIBUTION

#GROWTH IN REAL SAVINGS TOP 1% TOP 0.1% TOP 10% BOTTOM 90%  
#SECTION J ATTITUDES ABOUT SPENDING AND INVESTING

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
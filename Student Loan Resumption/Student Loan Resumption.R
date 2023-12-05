pacman::p_load(Bea.R,ggpattern,jsonlite,fs,bea.R,cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FEDERAL_STUDENT_LOANS <- fredr(series_id = "FGCCSAQ027S", observation_start = as.Date("2006-01-01")) %>%
  mutate(series_id = "Federal")
ALL_STUDENT_LOANS <- fredr(series_id = "BOGZ1FL153166220Q", observation_start = as.Date("2006-01-01")) %>%
  mutate(series_id = "Other")
DSPI <- fredr(series_id = "DSPI", observation_start = as.Date("2006-01-01"), frequency = "q", aggregation_method = "avg") 

STUDENT_LOANS <- rbind(ALL_STUDENT_LOANS,FEDERAL_STUDENT_LOANS,DSPI) %>%
  pivot_wider(names_from = series_id) %>%
  transmute(date, Other = (Other-Federal)/(DSPI*1000), Federal = Federal/(DSPI*1000)) %>%
  drop_na() %>%
  pivot_longer(cols = c(Other, Federal)) %>%
  mutate(name = factor(name,levels = c("Other", "Federal")))

STUDENT_LOANS_DSPI_graph <- ggplot(data = STUDENT_LOANS, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("% of Disposable Income") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.05,0.1), limits = c(0,.105), expand = c(0,0)) +
  ggtitle("All Student Loans, % of Aggregate Disposable Income") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and BEA data", subtitle = "The Pandemic-era Forebearance Caused the Aggregate Burden of Student Loans To Fall") +
  theme_apricitas + theme(legend.position = c(.12,.82)) +
  theme(plot.title = element_text(size = 21)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("Federal", "Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*.105), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STUDENT_LOANS_DSPI_graph, "Student Loans DSPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

url1 <- 'https://studentaid.gov/sites/default/files/fsawg/' |>
paste0('datacenter/library/Portfolio-by-Debt-Size.xls')

UA <- 'Mozilla/5.0 (Windows NT 10.0; Win64;' |> 
  paste('x64; rv:109.0) Gecko/20100101 Firefox/114.0')

httr::GET(url1, httr::user_agent(UA), 
          httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

LOAN_BY_DEBT_SIZE <- read_xls(tf) %>%
  slice(-(1:5), -((n()-1):n())) %>%
  setNames(c("year","quarter","dol_<5k","num_<5k","dol_5-10k","num_5-10k","dol_10-20k","num_10-20k","dol_20-40k","num_20-40k","dol_40-60k","num_40-60k","dol_60-80k","num_60-80k","dol_80-100k","num_80-100k","dol_100-200k","num_100-200k","dol_200k+","num_200k+")) %>%
  mutate(date = as.Date(as.yearqtr(paste(year, quarter), format = "%Y Q%q"))) %>%
  select(-year,-quarter) %>%
  mutate(across(where(is.character), ~ round(as.numeric(.), 1))) #%>%
  #mutate(dol_sum = rowSums(select(., starts_with("dol")), na.rm = TRUE),
         #num_sum = rowSums(select(., starts_with("num")), na.rm = TRUE)) #%>%
  #mutate(across(starts_with("dol"), ~ . / dol_sum)) %>%
  #mutate(across(starts_with("num"), ~ . / num_sum))

DOL_LOAN_BY_DEBT_SIZE <- LOAN_BY_DEBT_SIZE %>%
  select(date, starts_with("dol")) %>%
  rename_with(~ sub("dol_", "", .), starts_with("dol")) %>%
  rename_with(~ sub("(\\d)", "\\$\\1", .)) %>%
  pivot_longer(cols = `<$5k`:`$200k+`) %>%
  mutate(name = factor(name, levels = rev(c("<$5k","$5-10k","$10-20k","$20-40k","$40-60k","$60-80k","$80-100k","$100-200k","$200k+"))))


NUM_LOAN_BY_DEBT_SIZE <- LOAN_BY_DEBT_SIZE %>%
  select(date, starts_with("num")) %>%
  rename_with(~ sub("num_", "", .), starts_with("num")) %>%
  rename_with(~ sub("(\\d)", "\\$\\1", .)) %>%
  pivot_longer(cols = `<$5k`:`$200k+`) %>%
  mutate(name = factor(name, levels = rev(c("<$5k","$5-10k","$10-20k","$20-40k","$40-60k","$60-80k","$80-100k","$100-200k","$200k+"))))

color_func <- colorRampPalette(c("#ea3c2e","#d12215","#a21b10","#74130c","#460b07","#170402"))

# Use the color gradient function to generate 10 colors
colors <- color_func(9)

NUM_LOAN_BY_DEBT_SIZE_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = NUM_LOAN_BY_DEBT_SIZE, aes(x = date, y = value, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,50), breaks = c(0,10,20,30,40,50), expand = c(0,0)) +
  ylab("Number") +
  ggtitle("Number of Student Loan Debtors by Balance") +
  labs(caption = "Graph created by @JosephPolitano using Department of Education data",subtitle = "Most Debtors Do Not Owe That Much, With 75% Having Less Than $40k in Remaining Debt") +
  theme_apricitas + theme(legend.position = "right", plot.title = element_text(size = 20)) +
  scale_fill_manual(name = NULL, values = colors, breaks = c(c("<$5k","$5-10k","$10-20k","$20-40k","$40-60k","$60-80k","$80-100k","$100-200k","$200k+"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-04-01")-(.1861*(today()-as.Date("2017-04-01"))), xmax = as.Date("2017-04-01")-(0.049*(today()-as.Date("2017-04-01"))), ymin = 0-(.3*50), ymax = 0) +
  coord_cartesian(clip = "off")

DOL_LOAN_BY_DEBT_SIZE_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = DOL_LOAN_BY_DEBT_SIZE, aes(x = date, y = value/1000, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.25, suffix = "T"),limits = c(0,1.75), breaks = c(0,.250,.500,.750,1,1.250,1.5,1.75), expand = c(0,0)) +
  ylab("Number") +
  ggtitle("Dollars Outstanding of Student Loan Debtors by Balance") +
  labs(caption = "Graph created by @JosephPolitano using Department of Education data",subtitle = "Most Outstanding Loans Belong to High-Balance Debtors, With 60% From Those with $60k+") +
  theme_apricitas + theme(legend.position = "right", plot.title = element_text(size = 20)) +
  scale_fill_manual(name = NULL, values = colors, breaks = c(c("<$5k","$5-10k","$10-20k","$20-40k","$40-60k","$60-80k","$80-100k","$100-200k","$200k+"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-04-01")-(.1861*(today()-as.Date("2017-04-01"))), xmax = as.Date("2017-04-01")-(0.049*(today()-as.Date("2017-04-01"))), ymin = 0-(.3*1.7), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DOL_LOAN_BY_DEBT_SIZE_GRAPH, "DOL Loan by Size.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = NUM_LOAN_BY_DEBT_SIZE_GRAPH, "NUM Loan by Size.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#NOTE: NAME OF SPREADSHEET HAS TO BE UPDATED AS NEW SHEETS COME OUT
FRBNY_CONSUMER_CREDIT <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/householdcredit/data/xls/HHD_C_Report_2023Q1.xlsx?sc_lang=en", sheet = "Page 12 Data") %>%
  slice(-(1:3)) %>%
  setnames(c("date","Mortgage","HELOC","Auto","Credit Card","Student Loan","Other","All","X")) %>%
  select(-Other,-All,-X) %>%
  mutate(date = seq.Date(from = as.Date("2003-01-01"), by = "3 months", length = nrow(.))) %>%
  mutate(across(where(is.character), ~ as.numeric(.))) %>%
  pivot_longer(cols = Mortgage:`Student Loan`)

FRBNY_CONSUMER_CREDIT_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = FRBNY_CONSUMER_CREDIT, aes(x = date, y = value/100, color = name), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.25),limits = c(0,.15), breaks = c(0,.0250,.0500,.0750,.1,.125,.15), expand = c(0,0)) +
  ylab("% of Total Balance") +
  ggtitle("% of Balance 90+ Days Delinquent") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Data",subtitle = "Student Loans Had High Delinquency Rates Pre-Pandemic") +
  theme_apricitas + theme(legend.position = c(0.12,0.85)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c(c("Student Loan","Credit Card","Auto","HELOC","Mortgage"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*0.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FRBNY_CONSUMER_CREDIT_GRAPH, "FRBNY Consumer Credit Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#DOE FINANCIAL REPORT
ANNUAL_PAYMENTS <- data.frame(date =  seq.Date(from = as.Date("2012-01-01"), to = as.Date("2022-01-01"), by = "year"),
                              Principal = c(18,26.4,36.3,50,55.9,62.6,63.5,67,55.3,33.3,41.3),
                              Interest = c(5.5,8.1,10.8,13.4,15.5,17.6,19.5,22.4,12.9,2.3,2.6),
                              Fees = c(1.7,1.7,1.8,1.8,1.8,1.9,1.9,1.9,1.7,1.6,1.6)) %>%
                    pivot_longer(cols = Principal:Fees)

ANNUAL_DISBURSEMENTS <- data.frame(date =  seq.Date(from = as.Date("2012-01-01"), to = as.Date("2022-01-01"), by = "year"),
                                   `Stafford Subsidized` = c(27.1,26.5,25.9,24,23.8,23.4,20.3,20,19.1,18.3,15.7),
                                   `Stafford Unsubsidized` = c(58.4,56.1,54.7,52.7,52.3,51.4,49,48.1,46.1,44.1,45.5),
                                   `Parent PLUS` = c(20.7,19.4,18.9,19.2,19,18.7,23.1,22.7,21.7,20.8,22.2),
                                   `Consolidation and Other` = c(36,27.5,34.5,46.4,45.5,49,41.6,39.8,30.4,21.5,36.9)) %>%
  setNames(c("date","Stafford Subsidized","Stafford Unsubsidized","Parent PLUS","Consolidation and Other")) %>%
  pivot_longer(cols = `Stafford Subsidized`:`Consolidation and Other`)

ANNUAL_PAYMENTS_GRAPH <- ggplot(data = ANNUAL_PAYMENTS, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,25,50,75,100), limits = c(0,105), expand = c(0,0)) +
  ggtitle("Federal Student Loans Payments, Incl. Refinancing") +
  labs(caption = "Graph created by @JosephPolitano using Department of Education Annual Financial Report Data", subtitle = "The Pandemic-era Forebearance Caused Student Loan Payments to Fall Dramatically") +
  theme_apricitas + theme(legend.position = c(.12,.82)) +
  theme(plot.title = element_text(size = 23)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("Principal", "Interest", "Fees")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 0-(.3*100), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ANNUAL_PAYMENTS_GRAPH, "Annual Student Loan Payments.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

ANNUAL_DISBURSEMENTS_GRAPH <- ggplot(data = ANNUAL_DISBURSEMENTS, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,25,50,75,100,125,150,175), limits = c(0,175), expand = c(0,0)) +
  ggtitle("Federal Student Loans Disbursements, Incl. Refinancing") +
  labs(caption = "Graph created by @JosephPolitano using Department of Education Annual Financial Report Data", subtitle = "Student Loan Disbursements Have Been Declining For Years, Especially During COVID") +
  theme_apricitas + theme(legend.position = c(.82,.9)) +
  theme(plot.title = element_text(size = 20)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#6A4C93","#3083DC","#A7ACD9"), breaks = c("Stafford Subsidized", "Stafford Unsubsidized", "Parent PLUS","Consolidation and Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 0-(.3*175), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ANNUAL_DISBURSEMENTS_GRAPH, "Annual Disbursements Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DOE_TGA_DEPOSITS <- read.csv("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/dts/deposits_withdrawals_operating_cash?format=csv&page[size]=10000&fields=record_date,transaction_type,transaction_catg,transaction_mtd_amt&filter=transaction_type:eq:Deposits,transaction_catg:in:(Education%20Department%20programs,Dept%20of%20Education%20(ED))") %>%
  transmute(date = as.Date(record_date), value = transaction_mtd_amt) %>%
  group_by(year = year(date), month = month(date)) %>% 
  filter(date == max(date)) %>%
  filter(!(year == year(Sys.Date()) & month == month(Sys.Date())))

DOE_TGA_DEPOSITS_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = DOE_TGA_DEPOSITS, aes(x = date, y = (value*12)/1000, color = "Department of Education Monthly Receipts, Annual Rate (TGA)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,100), breaks = c(25,50,75,100), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Monthly Payments to the Department of Education") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "Payments to the Department of Education, Most of them Student Loans, Are Now Rising Rapidly") +
  theme_apricitas + theme(legend.position = c(0.5,0.95)) +
  theme(plot.title = element_text(size = 23)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-10-01")-(.1861*(today()-as.Date("2005-10-01"))), xmax = as.Date("2005-10-01")-(0.049*(today()-as.Date("2005-10-01"))), ymin = 0-(.3*100), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DOE_TGA_DEPOSITS_GRAPH, "DOE TGA DEPOSITS GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DOE_TGA_DEPOSITS_2018_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = filter(DOE_TGA_DEPOSITS, date >= as.Date("2018-01-01")), aes(x = date, y = (value*12)/1000, color = "Department of Education Monthly Receipts, Annual Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,105), breaks = c(25,50,75,100), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Monthly Payments to the Department of Education") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "Payments to the Department of Education, Most of them Student Loans, Have Rebounded") +
  theme_apricitas + theme(legend.position = c(0.5,0.95)) +
  theme(plot.title = element_text(size = 23)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*105), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DOE_TGA_DEPOSITS_2018_GRAPH, "DOE TGA DEPOSITS 2018 GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


DOE_TGA_DEPOSITS_WEEKLY <- read.csv("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/dts/deposits_withdrawals_operating_cash?format=csv&page[size]=10000&fields=record_date,transaction_type,transaction_catg,transaction_today_amt&filter=transaction_type:eq:Deposits,transaction_catg:in:(Education%20Department%20programs,Dept%20of%20Education%20(ED))") %>%
  transmute(date = as.Date(record_date), value = transaction_today_amt) %>%
  mutate(value = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,rollmean(value, 21))) %>%
  filter(value != 0)

DOE_TGA_DEPOSITS_DAILY <- ggplot() + #plotting components of annual inflation
  geom_line(data = DOE_TGA_DEPOSITS_WEEKLY, aes(x = date, y = (value*251)/1000, color = "Department of Education Daily Receipts\nRolling 21-Business-Day (Roughly 1 Month) Average\nAnnual Rate (TGA)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,125), breaks = c(25,50,75,100,125), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Payments to the Department of Education") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "Payments to the Department of Education, Most of them Student Loans, Are Quickly Rising") +
  theme_apricitas + theme(legend.position = c(0.4,0.82)) +
  theme(plot.title = element_text(size = 24)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-10-01")-(.1861*(today()-as.Date("2005-10-01"))), xmax = as.Date("2005-10-01")-(0.049*(today()-as.Date("2005-10-01"))), ymin = 0-(.3*100), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DOE_TGA_DEPOSITS_DAILY, "DOE TGA DEPOSITS DAILY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

ENROLLMENT_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Student%20Loan%20Resumption/ENROLLMENT_NUMBERS.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  setNames(c("date","Associate","Bachelor's","Other Undergrad","Graduate/Professional")) %>%
  pivot_longer(cols = c(Associate:`Graduate/Professional`)) %>%
  mutate(name = factor(name, levels = rev(c("Bachelor's","Associate","Other Undergrad","Graduate/Professional"))))

ENROLLMENT_DATA_GRAPH <- ggplot(data = ENROLLMENT_DATA, aes(x = date, y = value/1000000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Enrollment, Millions") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  scale_x_date(breaks = as.Date(c("2019-07-01","2020-07-01","2021-07-01","2022-07-01","2023-07-01")), labels = c("2019","2020","2021","2022","2023")) +
  ggtitle("Spring Semester Enrollment by Year") +
  labs(caption = "Graph created by @JosephPolitano using National Student Clearinghouse Research Center Data", subtitle = "Undergrad and Graduate Enrollment Has Been Declining For Years, Especially During COVID") +
  theme_apricitas + theme(legend.position = c(.82,.85)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#6A4C93","#3083DC","#A7ACD9"), breaks = c("Bachelor's","Associate","Other Undergrad","Graduate/Professional")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ENROLLMENT_DATA_GRAPH, "ENROLLMENT DATA GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Who is repaying graph
SHED_2019 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202019.csv")
SHED_2022 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Household%20Illiquidity/SHED%20Data%202022.csv")

replacement_vec <- c("Less than \\$5,000" = "<$10k",
                     "\\$5,000 to \\$7,499" = "<$10k",
                     "\\$7,500 to \\$9,999" = "<$10k",
                     "\\$10,000 to \\$12,499" = "$10k-$25k",
                     "\\$12,500 to \\$14,999" = "$10k-$25k",
                     "\\$15,000 to \\$19,999" = "$10k-$25k",
                     "\\$20,000 to \\$24,999" = "$10k-$25k",
                     "\\$25,000 to \\$29,999" = "$25k-$50k",
                     "\\$30,000 to \\$34,999" = "$25k-$50k",
                     "\\$35,000 to \\$39,999" = "$25k-$50k",
                     "\\$40,000 to \\$49,999" = "$25k-$50k",
                     "\\$50,000 to \\$59,999" = "$50k-$75k",
                     "\\$60,000 to \\$74,999" = "$50k-$75k",
                     "\\$75,000 to \\$84,999" = "$75k-$100k",
                     "\\$85,000 to \\$99,999" = "$75k-$100k",
                     "\\$100,000 to \\$124,999" = "$100k-$150k",
                     "\\$125,000 to \\$149,999" = "$100k-$150k",
                     "\\$150,000 to \\$174,999" = "$150k+",
                     "\\$175,000 to \\$199,999" = "$150k+",
                     "\\$200,000 to \\$249,999" = "$150k+",
                     "\\$250,000 or more" = "$150k+")

for(i in seq_along(replacement_vec)) {
  SHED_2019$ppincimp <- gsub(names(replacement_vec)[i], replacement_vec[i], SHED_2019$ppincimp)
}

replacement_vec_2022 <- c("Less than \\$10,000" = "<$10k",
                     "\\$10,000 to \\$24,999" = "$10k-$25k",
                     "\\$25,000 to \\$49,999" = "$25k-$50k",
                     "\\$50,000 to \\$74,999" = "$50k-$75k",
                     "\\$75,000 to \\$99,999" = "$75k-$100k",
                     "\\$100,000 to \\$149,999" = "$100k-$150k",
                     "\\$150,000 or more" = "$150k+")

for(i in seq_along(replacement_vec_2022)) {
  SHED_2022$ppinc7 <- gsub(names(replacement_vec_2022)[i], replacement_vec_2022[i], SHED_2022$ppinc7)
}

SLOAN_SHED_2019 <- crosstab(df = SHED_2019, x = ppincimp, y = SL4, weight = weight,format = "long") %>%
  filter(!(str_starts(SL4, "D") | str_starts(SL4, "I") | str_starts(SL4, "R"))) %>%
  transmute(Income = ppincimp, Payment = SL4, pct, year = as.character("2019"))
SLOAN_SHED_2022 <- crosstab(df = SHED_2022, x = ppinc7, y = SL4, weight = weight,format = "long") %>%
  filter(!str_starts(SL4, "D")) %>%
  transmute(Income = ppinc7, Payment = SL4, pct, year = as.character("2022"))

total_weight <- sum(SHED_2022$weight)

SLOAN_SHED_2022 <- SHED_2022 %>%
  group_by(ppinc7, SL4) %>%
  summarise(weighted_count = sum(weight), 
            raw_count = n()) %>%
  mutate(percentage = weighted_count / total_weight * 100) %>%
  filter(!grepl('^(R|D|I|$)', SL4)) %>%
  mutate(SL4 = factor(SL4, levels = rev(c("$1,000 or above","$500 to $999","$400 to $499","$300 to $399","$200 to $299","$100 to $199","$1 to $99")))) %>%
  mutate(ppinc7 = factor(ppinc7, levels = c("<$10k","$10k-$25k","$25k-$50k","$50k-$75k","$75k-$100k","$100k-$150k","$150k+")))
  
SLOAN_SHED_2019 <- SHED_2019 %>%
  group_by(ppincimp, SL4) %>%
  summarise(weighted_count = sum(weight), 
            raw_count = n()) %>%
  mutate(percentage = weighted_count / total_weight * 100) %>%
  filter(!grepl('^(R|D|I|$)', SL4)) %>%
  mutate(SL4 = factor(SL4, levels = rev(c("$1,000 or above","$500 to $999","$400 to $499","$300 to $399","$200 to $299","$100 to $199","$1 to $99")))) %>%
  mutate(ppincimp = factor(ppincimp, levels = c("<$10k","$10k-$25k","$25k-$50k","$50k-$75k","$75k-$100k","$100k-$150k","$150k+")))
  
SLOAN_SHED_2019_GRAPH <- ggplot(data = SLOAN_SHED_2019, aes(x = ppincimp, y = percentage/100, fill = SL4)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Date") +
  ylab("Percent of General Population") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = c(0,0.001,0.002,0.003,0.004,0.005,0.006), limits = c(0,0.006), expand = c(0,0)) +
  ggtitle("2019 Required Student Debt Payment Distribution") +
  labs(caption = "Graph created by @JosephPolitano using National Student Clearinghouse Research Center Data", subtitle = "Student Loans Were Distributed, But Payments Were Concentrated Among the Upper-Middle Class") +
  theme_apricitas + theme(legend.position = c(.15,.75)) +
  theme(plot.title = element_text(size = 23)) +
  scale_fill_manual(name= "Monthly Payment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#6A4C93","#3083DC","#A7ACD9"))

SLOAN_SHED_2022 <- SHED_2022 %>%
  group_by(ppinc7, SL4) %>%
  summarise(weighted_count = sum(weight), 
            raw_count = n()) %>%
  mutate(percentage = weighted_count / total_weight * 100) %>%
  filter(!grepl('^(R|D|I|$)', SL4)) %>%
  mutate(SL4 = factor(SL4, levels = rev(c("$1,000 or above","$500 to $999","$400 to $499","$300 to $399","$200 to $299","$100 to $199","$1 to $99")))) %>%
  mutate(ppinc7 = factor(ppinc7, levels = c("<$10k","$10k-$25k","$25k-$50k","$50k-$75k","$75k-$100k","$100k-$150k","$150k+")))

SLOAN_SHED_2022_GRAPH <- ggplot(data = SLOAN_SHED_2022, aes(x = ppinc7, y = percentage/100, fill = SL4)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Date") +
  ylab("Percent of General Population") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = c(0,0.001,0.002,0.003,0.004,0.005,0.006), limits = c(0,0.006), expand = c(0,0)) +
  ggtitle("2022 Required Student Debt Payment Distribution") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Survey of Household Economic Decisionmaking Data", subtitle = "Total Private and Public Payments Declined Dramatically, Especially Among Lower-Income Cohorts") +
  theme_apricitas + theme(legend.position = c(.15,.75)) +
  theme(plot.title = element_text(size = 23)) +
  scale_fill_manual(name= "Monthly Payment",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#6A4C93","#3083DC","#A7ACD9"))

ggsave(dpi = "retina",plot = SLOAN_SHED_2019_GRAPH, "SLOAN SHED 2019 DATA GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = SLOAN_SHED_2022_GRAPH, "SLOAN SHED 2022 DATA GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#Tax Numbers
REPAY_EXAMPLE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Student%20Loan%20Resumption/REPAYE.csv")

REPAY_EXAMPLE_GRAPH <- ggplot() + #plotting components of annual inflation
  annotate("hline", y = 272, yintercept = 272, color = "white", size = 1.25, linetype = "dashed") +
  annotate("text",label = c("Standard Repayment Plan"), y = 260, x = 35, color = "white", size = 5, linetype = "dashed") +
  geom_line(data = REPAY_EXAMPLE, aes(x = Earnings/1000, y = (REPAYE), color = "REPAYE Income Driven Repayment Plan"), size = 1.25) +
  geom_line(data = REPAY_EXAMPLE, aes(x = Earnings/1000, y = (SAVE_2023), color = "New SAVE Income Driven Repayment Plan (2023 Terms)"), size = 1.25) +
  geom_line(data = REPAY_EXAMPLE, aes(x = Earnings/1000, y = (SAVE_2024), color = "New SAVE Income Driven Repayment Plan (2024 Terms)"), size = 1.25) +
  xlab("Annual Earnings") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),limits = c(0,400), breaks = c(100,200,300,400), expand = c(0,0)) +
  ylab("Monthly Payment") +
  ggtitle("Structure of New Income-Driven Repayment Plans") +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"),limits = c(0,100), breaks = c(0,25,50,75,100), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Department of Education data",subtitle = "Payments to the Department of Education, Most of them Student Loans, Have Hit a 10-Year Low") +
  theme_apricitas + theme(legend.position = c(0.485,0.86)) +
  theme(plot.title = element_text(size = 23)) +
  scale_color_manual(name = "Monthly Payments For a Hypothetical Single Borrower With $27k in Debt at 3.9% ", values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("REPAYE Income Driven Repayment Plan","New SAVE Income Driven Repayment Plan (2023 Terms)","New SAVE Income Driven Repayment Plan (2024 Terms)")) +
  annotation_custom(apricitas_logo_rast, xmin = 5-(.1861*100), xmax = 5-(0.049*100), ymin = 0-(.3*400), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REPAY_EXAMPLE_GRAPH, "REPAYE EXAMPLE Graph GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CAPITAL_TRANSFERS_PAID_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U51100',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2019, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

CAPITAL_TRANSFERS_PAID <- beaGet(CAPITAL_TRANSFERS_PAID_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2019-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names() %>%
  drop_na()

CAPITAL_TRANSFERS_PAID_GRAPH <- ggplot(data = CAPITAL_TRANSFERS_PAID, aes(x = date, y = u51100_w027rc_14_other_capital_transfers_paid_to_persons_current_dollars_level_6/4000, fill = "Federal Government, Other Capital Transfers to Persons, Quarterly")) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("text", label = "Spike Caused By\nEmergency Rental Assistance and\nHomeowner Assistance Programs",y = 40, x = as.Date("2019-12-01"), color = "white", size = 4) +
  annotate("text", label = "Spike Caused By\nStudent Loan Forgiveness",y = 75, x = as.Date("2022-07-01"), color = "white", size = 5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,20,40,60,80,100,120), limits = c(0,120), expand = c(0,0)) +
  ggtitle("Billions in Student Loans Have Been Forgiven") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "Despite the Supreme Court Ruling, The Government Has Still Been Able to Forgive Billions") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  theme(plot.title = element_text(size = 25)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*120), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAPITAL_TRANSFERS_PAID_GRAPH, "Capital Transfers Paid Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


DELIQUENCY_EXPECTATIONS_CREDIT <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY-SCE-Data.xlsx?sc_lang=en", sheet = "Delinquency expectations Demo") %>%
  slice(-(1:3)) %>%
  setnames(c("date","Under 40","40-60","Over 60","High School or Less","Some College","BA Or Higher","Under 50k","50-100k","Over 100k","Low Numeracy","High Numeracy","West","Midwest","South","Northeast")) %>%
  mutate(date = seq.Date(from = as.Date("2013-06-01"), by = "1 months", length = nrow(.))) %>%
  mutate(across(where(is.character), ~ as.numeric(.))) %>%
  filter(date >= as.Date("2018-01-01"))

DELIQUENCY_EXPECTATIONS_CREDIT_GRAPH <- ggplot() + #plotting components of annual inflation
  #geom_line(data = DELIQUENCY_EXPECTATIONS_CREDIT, aes(x = date, y = `High School or Less`/100, color = "High School or Less"), size = 1.25) +
  geom_line(data = DELIQUENCY_EXPECTATIONS_CREDIT, aes(x = date, y = `Some College`/100, color = "Some College"), size = 1.25) +
  geom_line(data = DELIQUENCY_EXPECTATIONS_CREDIT, aes(x = date, y = `BA Or Higher`/100, color = "BA or Higher"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.21), breaks = c(0,.05,.10,.15,.2), expand = c(0,0)) +
  ylab("Mean Probability, %") +
  ggtitle("Debt Delinquency Expectations Are Up\nAmong Former College Attendees") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Data",subtitle = "Those Most Likely to Have Student Loans Worry Most About Falling Behind on Debt Soon") +
  theme_apricitas + theme(legend.position = c(0.5,0.89)) +
  scale_color_manual(name = "Mean Probability of Not Making Minimum Debt Payments Over the Next 3 Months", values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("Some College","BA or Higher")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.21), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DELIQUENCY_EXPECTATIONS_CREDIT_GRAPH, "Debt Delinquency Expectations Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DELINQUENCY_RATE_DEBT_CATEGORIES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Student%20Loan%20Resumption/DELINQUENCY_RATE_DEBT_CATEGORIES.csv") %>%
  mutate(date = as.Date(date)) %>%
  pivot_wider(names_from = category, values_from = delinquency_rate) %>%
  setNames(c("date","Auto & Credit Card Debt","Student, Auto, & Credit Card Debt","Credit Card Debt Only","Mortgage & Credit Card Debt","Student & Credit Card Debt")) %>%
  pivot_longer(-date) %>%
  filter(date >= as.Date("2017-12-01"))

DELINQUENCY_RATE_DEBT_CATEGORIES_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = DELINQUENCY_RATE_DEBT_CATEGORIES, aes(x = date, y = value/100, color = name), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.045), breaks = c(0,.01,.02,.03,.04), expand = c(0,0)) +
  ylab("Share of Borrowers, %") +
  ggtitle("Share of Credit Card Borrowers\nWho are Newly Delinquent, NSA") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Data via Haughwout, Lee, Mangrum, Rodriguez, van der Klaauw, Scally, & Wang",subtitle = "Borrowers With Student Debt are More Likely to be Falling Behind on Their Credit Cards") +
  theme_apricitas + theme(legend.position = c(0.5,0.89), legend.key.height = unit(0,"cm")) +
  scale_color_manual(name = "Borrowers With", values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("Student, Auto, & Credit Card Debt","Student & Credit Card Debt","Auto & Credit Card Debt","Credit Card Debt Only","Mortgage & Credit Card Debt")) +
  guides(color = guide_legend(nrow = 3, ncol = 2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-12-01")-(.1861*(today()-as.Date("2017-12-01"))), xmax = as.Date("2017-12-01")-(0.049*(today()-as.Date("2017-12-01"))), ymin = 0-(.3*.045), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DELINQUENCY_RATE_DEBT_CATEGORIES_graph, "Delinquency Rate Debt Categories Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#DATE HAS TO BE UPDATES EVERY THREE MONTHS
EGI_RETAIL_DATA <- read.xlsx("https://www.newyorkfed.org/medialibrary/Research/Interactives/Data/equitable-growth-indicators/downloads/10-2023-EGI-National_Data", sheet = "Real Retail Spending I") %>%
  slice(-(1:3)) %>%
  setnames(c("date","25-34","35-44","45-54",">54","College","Grad School","HS","Some College","$100-150k","$150k-$200k",">$200k","<50k","50k-100k")) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))

EGI_RETAIL_DATA_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = EGI_RETAIL_DATA, aes(x = date, y = `>54`/100, color = ">54"), size = 1.25) +
  geom_line(data = EGI_RETAIL_DATA, aes(x = date, y = `45-54`/100, color = "45-54"), size = 1.25) +
  geom_line(data = EGI_RETAIL_DATA, aes(x = date, y = `35-44`/100, color = "35-44"), size = 1.25) +
  geom_line(data = EGI_RETAIL_DATA, aes(x = date, y = `25-34`/100, color = "25-34"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.25,.32), breaks = c(-.2,-.1,0,.1,.2,.3), expand = c(0,0)) +
  ylab("Percent Change From Jan 2020") +
  ggtitle("Real Retail Spending, % Change From Jan 2020") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY Data",subtitle = "Young Households Had a Small Relative Cutback in Spending as Student Loan Payments Resume") +
  theme_apricitas + theme(legend.position = c(0.125,0.85)) +
  theme(plot.title = element_text(size = 25)) +
  scale_color_manual(name = "Age Group", values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("25-34","35-44","45-54",">54")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-08"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -0.25-(.3*.57), ymax = -0.25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EGI_RETAIL_DATA_GRAPH, "EGI Retail Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

STUDENT_LOAN_GROWTH <- fredr(series_id = "SLOAS", observation_start = as.Date("2016-01-01"), units = "pc1")

STUDENT_LOAN_GROWTH_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = STUDENT_LOAN_GROWTH, aes(x = date, y = value/100, color = "Year-on-Year Growth in Outstanding Student Loan Debt"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(-0.025,.075), breaks = c(-0.025,0,0.025,0.05,0.075), expand = c(0,0)) +
  ylab("Year on Year Growth, %") +
  ggtitle("Student Loan Debt Levels are Shrinking") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "Forgiveness, Forbearance, and Generous Repayment Plans Have Shrunk Student Debt Levels") +
  theme_apricitas + theme(legend.position = c(0.55,0.925)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -0.025-(.3*.1), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STUDENT_LOAN_GROWTH_GRAPH, "Student Loan Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


PERSONAL_INCOME <- fredr(series_id = "PI", observation_start = as.Date("1975-01-01")) %>%
  select(date,value)
PERSONAL_INTEREST_PAYMENTS <- fredr(series_id = "B069RC1", observation_start = as.Date("1975-01-01")) %>%
  select(date,value)

PERSONAL_INCOME_INTEREST_MERGE <- merge(PERSONAL_INTEREST_PAYMENTS,PERSONAL_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

PERSONAL_INCOME_INTEREST_GRAPH <- ggplot() +
    annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
    geom_line(data = PERSONAL_INCOME_INTEREST_MERGE, aes(x = date, y = value, color = "Interest Payments on Nonmortgage Consumer Debt\nAs a Share of Aggregate Personal Income"), size = 1.25) +
    xlab("Date") +
    scale_y_continuous(labels = scales::percent_format(accuracy = .5),limits = c(0,.031), breaks = c(0,0.01,0.02,0.03), expand = c(0,0)) +
    ylab("Share of Personal Income, %") +
    ggtitle("American's Interest Expenses are Rising") +
    labs(caption = "Graph created by @JosephPolitano using Federal Reserve Data",subtitle = "The Share of Aggregate Income Going to Interest Payments Has Surged Above 2019 Levels") +
    theme_apricitas + theme(legend.position = c(0.5,0.95)) +
    scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
    annotation_custom(apricitas_logo_rast, xmin = as.Date("1975-01-01")-(.1861*(today()-as.Date("1975-01-01"))), xmax = as.Date("1975-01-01")-(0.049*(today()-as.Date("1975-01-01"))), ymin = 0-(.3*.031), ymax = 0) +
    coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = PERSONAL_INCOME_INTEREST_GRAPH, "Personal Income Interest Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


https://fred.stlouisfed.org/series/SLOAS
PERSONAL INTEREST PAID
NONMORTGAGE
https://fred.stlouisfed.org/series/B069RC1

cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

p_unload(all)

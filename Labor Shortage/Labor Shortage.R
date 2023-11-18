pacman::p_load(dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

ATL_WAG_INCOME <- read.csv("C:/Users/josep/Documents/Labor Shortage/ATL_WAG_INCOME.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na()

ATL_WAG_INCOME_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=ATL_WAG_INCOME, aes(x=date,y= X1st/100,color= "1st (Lowest-Income) Quartile"), size = 1.25) +
  geom_line(data=ATL_WAG_INCOME, aes(x=date,y= X2nd/100,color= "2nd Quartile"), size = 1.25) +
  geom_line(data=ATL_WAG_INCOME, aes(x=date,y= X3rd/100,color= "3rd Quartile"), size = 1.25) +
  geom_line(data=ATL_WAG_INCOME, aes(x=date,y= X4th/100,color= "4th (Highest-Income) Quartile"), size = 1.25) +
  annotate("text",label = "NOTE: Data Lags Significantly Due to Being a 12M Moving Average of Annual Growth", x = as.Date("2010-08-01"), y =0.005, color = "white", size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.08), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed Data",subtitle = "US Wage Growth Has Been Stongest for the Lowest-Paid Workers") +
  theme_apricitas + theme(legend.position = c(.5,.78)) +
  scale_color_manual(name= "12-Month Moving Average of Median Wage Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-12-01")-(.1861*(today()-as.Date("1997-12-01"))), xmax = as.Date("1997-12-01")-(0.049*(today()-as.Date("1997-12-01"))), ymin = 0-(.3*0.08), ymax = 0) +
  coord_cartesian(clip = "off")
 
ggsave(dpi = "retina",plot = ATL_WAG_INCOME_graph, "ATL Wage Income Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#ATL/SWITCH/STAY
ATL_SWITCH_STAY <- read.csv("C:/Users/josep/Documents/Labor Shortage/ATL_WAG_SWITCH_STAY.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na()

ATL_SWITCH_STAY_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=ATL_SWITCH_STAY, aes(x=date,y= Job.Switcher/100,color= "Job Switchers"), size = 1.25) +
  geom_line(data=ATL_SWITCH_STAY, aes(x=date,y= Job.Stayer/100,color= "Job Stayers"), size = 1.25) +
  geom_line(data=ATL_SWITCH_STAY, aes(x=date,y= Overall/100,color= "Overall"), size = 1.25) +
  annotate("text",label = "NOTE: Data Lags Significantly Due to Being a 12M Moving Average of Annual Growth", x = as.Date("2010-08-01"), y =0.005, color = "white", size = 5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.08), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed Data",subtitle = "Most Job-Switchers are Searching for Higher Wages—and Getting Them") +
  theme_apricitas + theme(legend.position = c(.5,.78)) +
  scale_color_manual(name= "12-Month Moving Average of Median Wage Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Job Switchers","Job Stayers","Overall")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-12-01")-(.1861*(today()-as.Date("1997-12-01"))), xmax = as.Date("1997-12-01")-(0.049*(today()-as.Date("1997-12-01"))), ymin = 0-(.3*0.08), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ATL_SWITCH_STAY_graph, "ATL Switch Stay Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIE_INF_COMP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Labor%20Shortage/BIE_inf_comp.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na()

BIE_INF_graph <- ggplot() + #plotting Wage Growth
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= labor_costs,color= "Labor Costs"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= sales_level,color= "Sales Levels"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= productivity,color= "Productivity"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= non_labor_costs,color= "Non-Labor Costs"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= margin_adjustment,color= "Margin Adjustment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-10,85), expand = c(0,0)) +
  ylab("Index, 0 = No Influence") +
  ggtitle("Fading Cost Pushes") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed Data",subtitle = "Businesses Expect Lower Inflation Over the Next Year—Especially From Nonlabor Costs") +
  theme_apricitas + theme(legend.position = c(.52,.78)) +
  scale_color_manual(name= "Business Forecasts of Future Influence of Different Factors on Their Prices",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Labor Costs","Non-Labor Costs","Margin Adjustment","Sales Levels","Productivity")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-12-18")-(.1861*(today()-as.Date("2011-12-18"))), xmax = as.Date("2011-12-18")-(0.049*(today()-as.Date("2011-12-18"))), ymin = -10-(.3*95), ymax = -10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_INF_graph, "BIE Contrib Influence.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


LOW_TO_HIGH <- read.csv("C:/Users/josep/Documents/Labor Shortage/LOW_TO_HIGH.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na() %>%
  transmute(flow = Job.to.Job.Flows/Tot_emp) %>%
  ts(., start = c(2010,3), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2010-10-01"), to = as.Date("2021-10-01"),by = "3 months"))


LOW_TO_HIGH_graph <- ggplot() + #plotting Wage Growth
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  geom_line(data=LOW_TO_HIGH, aes(x=date,y= x,color= "Share of Low-Pay Industries' Workers Moving to Jobs in High-Pay Industries, Quarterly"), size = 1.25) +
  annotate("text",label = "Low-Pay Industries = Retail Trade, Leisure and Hospitality", x = as.Date("2013-7-01"), y =0.00335, color = "white", size = 4) +
  annotate("text",label = "High-Pay Industries = Consulting, Professional & Technical Services, Information, and Finance", x = as.Date("2015-07-07"), y =0.00315, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1),limits = c(0.003,0.007), expand = c(0,0)) +
  ylab("Percent, Quarterly") +
  ggtitle("The Great Reshuffling") +
  labs(caption = "Graph created by @JosephPolitano using Census Data Seasonally Adjusted with X-13ARIMA",subtitle = "A Labor Shortage Meant More Workers Could Transition Into Higher-Paying Roles") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-10-01")-(.1861*(today()-as.Date("2010-10-01"))), xmax = as.Date("2010-10-01")-(0.049*(today()-as.Date("2010-10-01"))), ymin = 0.003-(.3*0.004), ymax = 0.003) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LOW_TO_HIGH_graph, "Low to High.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


NFIB <- read.csv("C:/Users/josep/Documents/Labor Shortage/NFIB.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Percent= (100-Percent)/100)

NFIB_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=NFIB, aes(x=Date,y= Percent,color= "Percent of Small Businesses with at Least One Unfilled Position"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.00,0.6),breaks = c(0,.20,.40,.60), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("What Does a Labor Shortage Mean?") +
  labs(caption = "Graph created by @JosephPolitano using NFIB data",subtitle = "More Small Businesses are Complaining About Unfilled Positions") +
  theme_apricitas + theme(legend.position = c(.52,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0.0-(.3*0.6), ymax = 0.0) +
  coord_cartesian(clip = "off")
  
ggsave(dpi = "retina",plot = NFIB_graph, "NFIB.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SMALL_BUSINESS_OUT <- read.csv("C:/Users/josep/Documents/Labor Shortage/SMALL_BUSINESS_OUT.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na() %>%
  transmute(X0.19.Employees = X0.19.Employees,X500..Employees = X500..Employees) %>%
  ts(., start = c(2010,3), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2010-10-01"), to = as.Date("2021-10-01"),by = "3 months"))

SMALL_BUSINESS_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=SMALL_BUSINESS_OUT, aes(x=date,y= X019Employees/1000,color= "To Firms With 0-19 Employees"), size = 1.25) +
  geom_line(data=SMALL_BUSINESS_OUT, aes(x=date,y= X500Employees/1000,color= "To Firms With 500+ Employees"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"),limits = c(170,350),breaks = c(175,200,225,250,275,300,325,350), expand = c(0,0)) +
  ylab("Number, Quarterly, Thousands") +
  ggtitle("Moving Up to Bigger Things") +
  labs(caption = "Graph created by @JosephPolitano using Census Data Seasonally Adjusted with X-13ARIMA",subtitle = "Employees of Small Businesses Are Increasingly Moving to Big Businesses") +
  theme_apricitas + theme(legend.position = c(.47,.15)) +
  scale_color_manual(name= "Job To Job Transitions From Small Businesses (0-19 Employees)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-10-01")-(.1861*(today()-365-as.Date("2010-10-01"))), xmax = as.Date("2010-10-01")-(0.049*(today()-365-as.Date("2010-10-01"))), ymin = 170-(.3*180), ymax = 170) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SMALL_BUSINESS_graph, "Small Businesses.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


TXFLGA_FLOWS <- read.csv("C:/Users/josep/Documents/Labor Shortage/TXFLGA_FLOWS.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na() %>%
  transmute(net_all = net_all, net_coll = net_coll, net_prof = net_prof) %>%
  ts(., start = c(2010,3), frequency = 4) %>%
  seas() %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq(from = as.Date("2010-10-01"), to = as.Date("2021-10-01"),by = "3 months"))

TXFLGA_FLOWS_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=TXFLGA_FLOWS, aes(x=date,y= net_all/1000,color= "All Workers"), size = 1.25) +
  geom_line(data=TXFLGA_FLOWS, aes(x=date,y= net_coll/1000,color= "College-Educated Workers"), size = 1.25) +
  geom_line(data=TXFLGA_FLOWS, aes(x=date,y= net_prof/1000,color= "Workers in High-Pay Industries"), size = 1.25) +
  annotate("text",label = "High-Pay Industries = Consulting, Professional & Technical Services, Information, and Finance", x = as.Date("2015-07-07"), y =50, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"),limits = c(0,80),breaks = c(0,20,40,60,80), expand = c(0,0)) +
  ylab("Number, Quarterly, Thousands") +
  ggtitle("The New Geography of US Jobs") +
  labs(caption = "Graph created by @JosephPolitano using Census Data Seasonally Adjusted with X-13ARIMA",subtitle = "Employees are Increasingly Moving Into Lower-Cost States") +
  theme_apricitas + theme(legend.position = c(.47,.85)) +
  scale_color_manual(name= "Net Job-to-Job Transitions to Lower-Cost States (TX,FL,NC,GA,AZ,ID,UT)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-10-01")-(.1861*(today()-465-as.Date("2010-10-01"))), xmax = as.Date("2010-10-01")-(0.049*(today()-465-as.Date("2010-10-01"))), ymin = 0-(.3*80), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TXFLGA_FLOWS_graph, "TXFLGA graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ECI_PRIV_WAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2002-01-01"), units = "pc1")

ECI_PRIV_WAG_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=ECI_PRIV_WAG, aes(x=date,y= value/100,color= "Employment Cost Index: Private Wage and Salary Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.06), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("A Rising Tide") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "US Wage Growth is at the Highest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.5,.70)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*0.06), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECI_PRIV_WAG_graph, "ECI_PRIV_WAG graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PRIME_AGE_EPOP <- fredr(series_id = "LNS12300060",observation_start = as.Date("1995-01-01"))
EPOP_55_64 <- fredr(series_id = "LREM55TTUSM156S",observation_start = as.Date("1995-01-01"))
EPOP_65_69_2022 <- bls_api("LNU02324938", startyear = 2015) %>%
  transmute(year,period,periodName,value,footnotes,seriesID)
EPOP_65_69 <- bls_api("LNU02324938", startyear = 1995) %>%
  rbind(., bls_api("LNU02324938", startyear = 2005)) %>%
  rbind(.,EPOP_65_69_2022) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

EPOP_20_24_2022 <- bls_api("LNS12300036", startyear = 2015) %>%
  transmute(year,period,periodName,value,footnotes,seriesID)
EPOP_20_24 <- bls_api("LNS12300036", startyear = 1995) %>%
  rbind(., bls_api("LNS12300036", startyear = 2005)) %>%
  rbind(., EPOP_20_24_2022) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
  
EPOP_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=PRIME_AGE_EPOP, aes(x=date,y= value/100,color= "25-54 (Prime Age)"), size = 1.25) +
  geom_line(data=EPOP_55_64, aes(x=date,y= value/100,color= "55-64"), size = 1.25) +
  geom_line(data=EPOP_65_69, aes(x=date,y= value/100,color= "65-69"), size = 1.25) +
  geom_line(data=EPOP_20_24, aes(x=date,y= value/100,color= "20-24"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.90), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("What Does A Labor Shortage Mean?") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "US Employment Rates Have Largely Recovered to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.5,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = 0-(.3*0.90), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_graph, "Epop graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GLI_BLS <- fredr(series_id = "CES0500000017",observation_start = as.Date("2018-01-01"), units = "pc1")
GLI_BEA <- fredr(series_id = "A132RC1",observation_start = as.Date("2018-01-01"), units = "pc1")
GLI_EPOP <- fredr(series_id = "LNS12300060",observation_start = as.Date("2017-01-01"), aggregation_method = "avg", frequency = "q") %>%
  merge(.,fredr(series_id = "ECIWAG",observation_start = as.Date("2017-01-01")),by = "date") %>%
  mutate(value = value.x*value.y) %>%
  mutate(value = (value-lag(value,4))/lag(value,4)) %>%
  drop_na()


GLI_GROWTH_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=GLI_BLS, aes(x=date,y= value/100,color= "Non-Farm Payrolls Data"), size = 1.25) +
  geom_line(data=GLI_BEA, aes(x=date,y= value/100,color= "BEA Data"), size = 1.25) +
  geom_line(data=GLI_EPOP, aes(x=date,y= value,color= "Employment Cost Index * Prime Age Employment"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "5% Pre-COVID Normal Growth Rate", x = as.Date("2022-03-01"), y =0.042, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.10,0.18), breaks = c(-.1,-0.05,0,0.05,.1,.15), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("Is the Labor Shortage Ending?") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data",subtitle = "Gross Labor Income Growth Looks to Be Declining Back to Pre-COVID Normal Levels") +
  theme_apricitas + theme(legend.position = c(.33,.72)) +
  scale_color_manual(name= "Private-Sector Gross Labor Income Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Non-Farm Payrolls Data","BEA Data","Employment Cost Index * Prime Age Employment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.10-(.3*0.28), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_GROWTH_graph, "GLI Growth graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Quits <- fredr(series_id = "JTS1000QUR",observation_start = as.Date("2001-01-01"))
Quits_LAH <- fredr(series_id = "JTS7000QUR",observation_start = as.Date("2001-01-01"))
Quits_RETAIL <- fredr(series_id = "JTS4400QUR",observation_start = as.Date("2001-01-01"))

QUITS_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=Quits, aes(x=date,y= value/100,color= "Quit Rate: Total Private"), size = 1.25) +
  geom_line(data=Quits_LAH, aes(x=date,y= value/100,color= "Quit Rate: Leisure and Hospitality"), size = 1.25) +
  geom_line(data=Quits_RETAIL, aes(x=date,y= value/100,color= "Quit Rate: Retail Trade"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.065), expand = c(0,0)) +
  ylab("Quit Rate, Monthly") +
  ggtitle("The Great Reshuffling") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Americans Were Quitting Their Jobs at a Record Rate, Especially in Low-Pay Industries") +
  theme_apricitas + theme(legend.position = c(.6,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Quit Rate: Total Private","Quit Rate: Leisure and Hospitality","Quit Rate: Retail Trade")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = 0-(.3*0.065), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = QUITS_graph, "Quits graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
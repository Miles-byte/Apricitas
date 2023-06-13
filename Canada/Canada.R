pacman::p_load(statcanR,cansim,valet,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CAPACITY_SOME <- get_series("CAPACITYSOME") %>%
  transmute(date, value = CAPACITYSOME) %>%
  mutate(name = "Some Difficulty")
  
CAPACITY_SIGNIF <- get_series("CAPACITYSIGNIF") %>%
  transmute(date, value = CAPACITYSIGNIF) %>%
  mutate(name = "Significant Difficulty")

CAPACITY <- rbind(CAPACITY_SOME,CAPACITY_SIGNIF) %>%
  mutate(name = factor(name, levels = c("Some Difficulty","Significant Difficulty"))) %>%
  pivot_wider() %>%
  mutate(sum = `Some Difficulty`+`Significant Difficulty`)

CAPACITY_GRAPH <- ggplot(data = CAPACITY, aes(x = date, y = value/100, fill = name)) +
  geom_col(stat = "identity", position = "stack", color = NA, width = 92) +
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.25,0.5,0.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("Canadian Capacity Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data", subtitle = "Canadian Firms Say They Would Have Difficulty Meeting Increased Demand at Elevated Rates") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_fill_manual(name= "Rate the Current Ability of Your Firm to Meet an Unexpected Increase in Demand",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#3083DC"), breaks = c("Significant Difficulty","Some Difficulty")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1999-07-01")-(.1861*(today()-as.Date("1999-07-01"))), xmax = as.Date("1999-07-01")-(0.049*(today()-as.Date("1999-07-01"))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAPACITY_GRAPH, "Canadian Capacity Crunch.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

LABSHORTYES <- get_series("LABSHORTYES") %>%
  mutate(value = LABSHORTYES)

LABSHORTYES_GRAPH <- ggplot() + #plotting Labor Shortage
  geom_line(data=LABSHORTYES, aes(x=date,y= value/100, color= "Share of Firms Saying Labor Shortages Restricts Their Ability to Meet Demand"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Canadian Labor Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data",subtitle = "Canadian Firms Cite Labor Shortages More than Pre-COVID—But About in Line With Pre-2008") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1998-07-01")-(.1861*(today()-as.Date("1998-07-01"))), xmax = as.Date("1998-07-01")-(0.049*(today()-as.Date("1998-07-01"))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LABSHORTYES_GRAPH, "LABSHORTYES.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INPUTS <- get_series("INPUTS")
OUTPUTS <- get_series("OUTPUTS")
COSTS <- get_series("COSTS")
  
INPUTS_OUTPUTS_COSTS_GRAPH <- ggplot() + #plotting Input and Output Price Expectations
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=INPUTS, aes(x=date,y= INPUTS, color= "Inputs"), size = 1.25) +
  geom_line(data=OUTPUTS, aes(x=date,y= OUTPUTS, color= "Outputs"), size = 1.25) +
  geom_line(data=COSTS, aes(x=date,y= COSTS, color= "Labor Costs"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-55,80), expand = c(0,0)) +
  ylab("Balance") +
  ggtitle("(Dis)inflation Expectations") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data",subtitle = "Canadian Firms Expect Input and Output Prices to Decelerate—But For Labor Costs to Accelerate") +
  theme_apricitas + theme(legend.position = c(.455,.885)) +
  scale_color_manual(name= "Net Share of Firms Expecting Faster Price Increases in the Next 12M vs Last 12M" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-07-01")-(.1861*(today()-as.Date("1997-07-01"))), xmax = as.Date("1997-07-01")-(0.049*(today()-as.Date("1997-07-01"))), ymin = -55-(.3*125), ymax = -55) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INPUTS_OUTPUTS_COSTS_GRAPH, "INPUTS OUTPUTS GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EPOP_US <- fredr("LNS12300060", observation_start = as.Date("1990-01-01"))
EPOP_CANADA <- get_cansim_vector("v2062952") %>%
  subset(Date >= as.Date("1990-01-01"))

EPOP_US_CANADA_GRAPH <- ggplot() + #plotting rent by A/B/C City Size
  geom_line(data=EPOP_CANADA, aes(x=Date,y= VALUE/100, color= "Canada"), size = 1.25) +
  geom_line(data=EPOP_US, aes(x=date,y= value/100, color= "United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.675,.875), breaks = c(.70,.74,.78,.82,.86), expand = c(0,0)) +
  ylab("Employment Rate") +
  ggtitle("Fuller Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS and Stats Canada data",subtitle = "Canada's Employment Rates Have Exceeded Pre-Pandemic Levels—And Beaten America's") +
  theme_apricitas + theme(legend.position = c(.2825,.225)) +
  scale_color_manual(name= "25-54 Employment-Population Ratios" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .675-(.3*.20), ymax = .675) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_US_CANADA_GRAPH, "EPOP US GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CANADA_LEAVE_JOB <- statcan_data("14-10-0125-01", "eng")

CANADA_LEAVE_JOB <- CANADA_LEAVE_JOB %>%
  subset(REF_DATE >= as.Date("2018-01-01")) %>%
  subset(Characteristics == "Total, unemployed and not in the labour force") %>%
  subset(Sex == "Both sexes") %>%
  subset(`Age group` == "15 years and over") %>%
  subset(GEO == "Canada") %>%
  select(REF_DATE, Reason, VALUE) %>%
  pivot_wider(names_from = "Reason", values_from = "VALUE")

JOB_LEAVE_CANADA_LAYOFF_GRAPH <- ggplot() + #
  geom_line(data=CANADA_LEAVE_JOB, aes(x=REF_DATE,y= `Temporary layoff`/1000, color= "Temporary Layoff"), size = 1.25) +
  geom_line(data=CANADA_LEAVE_JOB, aes(x=REF_DATE,y= `Permanent layoff`/1000, color= "Permanent Layoff"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), limits = c(0,3), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Number, Millions") +
  ggtitle("Labor Supply and Demand") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Temporary and Permanent Layoffs Have Fallen to Below Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.3825,.85)) +
  scale_color_manual(name= "Reason for Leaving Job During Previous Year (NSA)" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

JOB_LEAVE_CANADA_SUPPLY_GRAPH <- ggplot() + #
  geom_line(data=CANADA_LEAVE_JOB, aes(x=REF_DATE,y= `Retired`, color= "Retired"), size = 1.25) +
  geom_line(data=CANADA_LEAVE_JOB, aes(x=REF_DATE,y= `Own illness or disability`, color= "Own Illness or Disability"), size = 1.25) +
  geom_line(data=CANADA_LEAVE_JOB, aes(x=REF_DATE,y= `Personal or family reasons`, color= "Personal or Family Reasons"), size = 1.25) +
  geom_line(data=CANADA_LEAVE_JOB, aes(x=REF_DATE,y= `Other reasons`, color= "Other Reasons"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,430), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ylab("Employment Rate") +
  ggtitle("Labor Supply and Demand") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Retirements Sagged During COVID, But Have Returned to Normal Levels") +
  theme_apricitas + theme(legend.position = c(.3825,.845)) +
  scale_color_manual(name= "Reason for Leaving Job During Previous Year (NSA)" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*420), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = JOB_LEAVE_CANADA_LAYOFF_GRAPH, "CANADA LAYOFF GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = JOB_LEAVE_CANADA_SUPPLY_GRAPH, "CANADA SUPPLY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CANADA_INDPRO <- statcan_data("14-10-0287-02", "eng") %>%
  subset(`North American Industry Classification System (NAICS)`=="Motor vehicle manufacturing [3361]") %>%
  subset(`Prices`=="Chained (2012) dollars") %>%
  subset(`Seasonal adjustment`=="Seasonally adjusted at annual rates") %>%
  subset(REF_DATE >= as.Date("2018-01-01"))

CANADA_INDPRO <- CANADA_INDPRO %>%
  subset(REF_DATE >= as.Date("2022-11-01")) %>%
  subset(GEO = "Canada") %>%
  subset(sex = "Both sexes")

EPOP_CANADA <- get_cansim_vector("v2062952","1990-01-01")

BCPI <- get_series("M.BCPI") %>%
  subset(date >= as.Date("2000-01-01"))

BCPI_GRAPH <- ggplot() + #
  geom_line(data=BCPI, aes(x=date,y= M.BCPI/4.1167, color= "Bank of Canada Commodity Price Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,250), breaks = c(0,100,200), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Canadian Commodity Check") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data",subtitle = "Prices for Canadian Export Commodities—Especiall Oil—Surged at the Start of the Year") +
  theme_apricitas + theme(legend.position = c(.3825,.95)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BCPI_GRAPH, "BCPI GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SLOS <- get_series("SLOS_BUS_LEND") %>%
  subset(date >= as.Date("2000-01-01")) %>%
  drop_na()

SLOS_GRAPH <- ggplot() + #plotting net tightening data
  geom_line(data=SLOS, aes(x=date,y= SLOS_BUS_LEND/100,color= "Net Percent of Domestic Banks Tightening Business Credit, Canada"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.40,-.20,0,.20,.40,.60,.80,1), limits = c(-.5,1), expand = c(0,0)) +
  ggtitle("Tightening Up (A Bit)") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data", subtitle = "Banks are Slowly Tightening Business Lending Standards in Canada") +
  theme_apricitas + theme(legend.position = c(.475,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.50-(.3*1.50), ymax = -0.50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SLOS_GRAPH, "SLOS GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI <- get_series("STATIC_TOTALCPICHANGE") %>%
  subset(date >= as.Date("2018-01-01"))
CPI_TRIM <- get_series("CPI_TRIM") %>%
  subset(date >= as.Date("2018-01-01"))
CPI_COMMON <- get_series("CPI_COMMON") %>%
  subset(date >= as.Date("2018-01-01"))
CPI_X <- get_series("ATOM_V41693242") %>%
  subset(date >= as.Date("2018-01-01"))
CPIW <- get_series("CPIW") %>%
  subset(date >= as.Date("2018-01-01"))

CPIS_GRAPH <- ggplot() + #plotting net tightening data
  geom_line(data=CPI, aes(x=date,y= STATIC_TOTALCPICHANGE/100,color= "CPI"), size = 1.25)+ 
  geom_line(data=CPI_TRIM, aes(x=date,y= CPI_TRIM/100,color= "Trimmed CPI"), size = 1.25)+ 
  geom_line(data=CPI_COMMON, aes(x=date,y= CPI_COMMON/100,color= "Common Factor CPI"), size = 1.25)+ 
  geom_line(data=CPI_X, aes(x=date,y= ATOM_V41693242/100,color= "CPIX (Excluding Volatile Components and Indirect Taxes)"), size = 1.25)+ 
  geom_line(data=CPIW, aes(x=date,y= CPIW/100,color= "CPIW (Low Variability Exluding Indirect Taxes)"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent of Loan and Leases") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.02,0.04,0.06,0.08,0.1), limits = c(-.01,0.09), expand = c(0,0)) +
  ggtitle("Canadian CPI") +
  labs(caption = "Graph created by @JosephPolitano using Stats Canada and Bank of Canada data", subtitle = "No Matter How You Slice It, Canadian Inflation is High—But Might Be Taking Its First Step Downward") +
  theme_apricitas + theme(legend.position = c(.375,.75)) +
  scale_color_manual(name= "Canadian Inflation:",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("CPI","Common Factor CPI","Trimmed CPI","CPIX (Excluding Volatile Components and Indirect Taxes)","CPIW (Low Variability Exluding Indirect Taxes)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.01-(.3*0.10), ymax = -0.01) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPIS_GRAPH, "CPIS GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

WAGE_COMMON <- get_series("INDINF_WC_Q") %>%
  subset(date >= as.Date("2002-01-01"))
  
CANADA_FIXED_WEIGHT_WAGE <- get_cansim_vector("v1606080") %>%
  mutate(pct = VALUE/lag(VALUE,12)-1)

WAGES_GRAPH <- ggplot() + #plotting net tightening data
  geom_line(data=WAGE_COMMON, aes(x=date,y= INDINF_WC_Q/100,color= "Canadian Wage-Common Index"), size = 1.25)+ 
  #geom_line(data=CANADA_FIXED_WEIGHT_WAGE, aes(x=Date,y= pct,color= "Fixed-Weight Average Hourle Earnings Index"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Annual Growth, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.02,0.04,0.06,0.08,0.1), limits = c(0,0.06), expand = c(0,0)) +
  ggtitle("Labor Supply and Demand") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data", subtitle = "Canadian Wage Growth is Only at the Higher End of Normal") +
  theme_apricitas + theme(legend.position = c(.375,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*0.06), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WAGES_GRAPH, "WAGES GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_JOB_GROWTH <- statcan_data("14-10-0355-01", "eng") %>%
  subset(GEO == "Canada") %>%
  subset(Statistics == "Estimate") %>%
  subset(`Data type` == "Seasonally adjusted") %>%
  subset(REF_DATE >= "2020-01-01") %>%
  select(REF_DATE,VALUE,`North American Industry Classification System (NAICS)`) %>%
  pivot_wider(names_from = `North American Industry Classification System (NAICS)`, values_from = VALUE) %>%
  mutate(`Professional & Business Services, Finance, and Real Estate` = `Professional, scientific and technical services [54]`+`Business, building and other support services [55-56]` + `Finance, insurance, real estate, rental and leasing [52-53]`) %>%
  mutate(`Trade, Transportation, and Utilities` = `Utilities [22]`+`Wholesale and retail trade [41, 44-45]`+`Transportation and warehousing [48-49]`) %>%
  mutate(`Goods-producing` = `Goods-producing sector`-`Utilities [22]`) %>%
  mutate(`Education and Health Services` = `Educational services [61]`+`Health care and social assistance [62]`) %>%
  mutate(`Other Services Including Information & Recreation` = `Other services (except public administration) [81]`+`Information, culture and recreation [51, 71]`) %>%
  mutate(`Accomodation and Food Services` = `Accommodation and food services [72]`) %>%
  mutate(`Public Administration` = `Public administration [91]`) %>%
  select(REF_DATE,`Professional & Business Services, Finance, and Real Estate`,`Trade, Transportation, and Utilities`,`Goods-producing`,`Education and Health Services`,`Other Services Including Information & Recreation`,`Accomodation and Food Services`,`Public Administration`) %>%
  mutate_if(is.numeric, funs(.-.[1])) %>%
  pivot_longer(cols = `Professional & Business Services, Finance, and Real Estate`:`Public Administration`) %>%
  mutate(name = factor(name,levels = c("Accomodation and Food Services","Trade, Transportation, and Utilities","Goods-producing","Education and Health Services","Professional & Business Services, Finance, and Real Estate","Public Administration","Other Services Including Information & Recreation")))

CAN_JOB_GROWTH_GRAPH <- ggplot(data = CAN_JOB_GROWTH, aes(x = REF_DATE, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(-3,-2,-1,0,1), limits = c(-3.5,1.1), expand = c(0,0)) +
  ggtitle("The Shape of Canadian Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "There are Now More Jobs Than Pre-Pandemic—and Most Sectors Have Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.625,.315)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Accomodation and Food Services","Trade, Transportation, and Utilities","Goods-producing","Education and Health Services","Professional & Business Services, Finance, and Real Estate","Public Administration","Other Services Including Information & Recreation")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -3.5-(.3*4.6), ymax = -3.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_JOB_GROWTH_GRAPH, "Canada Job Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CANADA_IMMIGRANT <- statcan_data("14-10-0083-01", "eng") %>%
  subset(GEO == "Canada") %>%
  subset(`Labour force characteristics` %in% c("Employment","Population")) %>%
  subset(`Age group` == "25 to 54 years") %>%
  subset(`Immigrant status` == "Immigrants, landed 5 or less years earlier") %>%
  select(REF_DATE, `Labour force characteristics`, VALUE) %>%
  pivot_wider(names_from = `Labour force characteristics`, values_from = VALUE)
  
CANADA_IMMIGRANT_GRAPH <- ggplot() + #plotting Canada Immigrant
  geom_line(data=CANADA_IMMIGRANT, aes(x=REF_DATE, y= Employment,color= "Employment"), size = 1.25)+ 
  geom_line(data=CANADA_IMMIGRANT, aes(x=REF_DATE, y= Population,color= "Population"), size = 1.25)+ 
  xlab("Date") +
  ylab("People, Thousands") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "k"), breaks = c(0,250,500,750,1000), limits = c(0,1000), expand = c(0,0)) +
  ggtitle("Bienvenue au Canada") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "The Number of Recent Prime-Age Canadian Immigrants Stalled in 2020 But Has Surged Recently") +
  theme_apricitas + theme(legend.position = c(.55,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Immigrants Landed in the Last 5 Years, Age 25-54, Canada",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Population","Employment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*1000), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CANADA_IMMIGRANT_GRAPH, "Canada Immigrant Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CANADA_GDP <- statcan_data("36-10-0104-01", "eng")

CANADA_GDP <- get_cansim_vector("v62305752") %>%
  subset(REF_DATE >= as.Date("2000-01-01")) %>%
  mutate(value = VALUE/VALUE[1]*100)
  
US_GDP <- fredr("GDPC1", observation_start = as.Date("2000-01-01")) %>%
  mutate(value = value/value[1]*100)

CANADA_GDP_GRAPH <- ggplot() + #plotting Canada Immigrant
  geom_line(data=US_GDP, aes(x=date, y= value,color= "United States"), size = 1.25)+ 
  geom_line(data=CANADA_GDP, aes(x=Date, y= value,color= "Canada"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan 2000 = 100") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), breaks = c(0,50,100,150), limits = c(75,160), expand = c(0,0)) +
  ggtitle("Can Canada Catch?") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada and BEA data", subtitle = "After Falling Behind the US Early in the Pandemic, Canada is Closing the Output Gap") +
  theme_apricitas + theme(legend.position = c(.55,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Real GDP",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 75-(.3*85), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CANADA_GDP_GRAPH, "Canada GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#fixed investment
#net exports
#residential investment

CAN_STARTS <- statcan_data("34-10-0141-01", "eng") %>%
  subset(REF_DATE >= as.Date("2000-01-01")) %>%
  #subset(GEO == "Canada") %>%
  #subset(`Housing estimates` == "Housing starts") %>%
  subset(`Type of unit` %in% c("Single-detached units","Multiples")) %>%
  #subset(`Seasonal adjustment` == "Seasonally adjusted at annual rates") %>%
  select(REF_DATE,`Type of unit`,VALUE) %>%
  mutate(`Type of unit` = gsub("Single-detached units","Single-detached",`Type of unit`))
  
CAN_STARTS_GRAPH <- ggplot(data = CAN_STARTS, aes(x = REF_DATE, y = VALUE, fill = `Type of unit`)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA, width = 92) +
  xlab("Date") +
  ylab("Thousands of Units, Seasonally Adjusted at Annual Rates") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,50,100,150,200,250,300), limits = c(0,300), expand = c(0,0)) +
  ggtitle("Canadian Construction") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "Canadian Housing Starts are Near Record Highs—Despite Rising Rates") +
  theme_apricitas + theme(legend.position = c(.525,.885)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Housing Starts, Canada",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Single-detached","Multiples")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_STARTS_GRAPH, "Canada Starts Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GOODS_TRADE_US_NONUS <- statcan_data("36-10-0023-01", "eng") %>%
  subset(REF_DATE >= as.Date("2000-01-01")) %>%
  subset(`Seasonal adjustment` == "Seasonally adjusted") %>%
  subset(`Principal trading partners` %in% c("United States","Total of all countries")) %>%
  subset(Trade == "Balances") %>%
  select(REF_DATE,`Principal trading partners`,VALUE) %>%
  pivot_wider(names_from = `Principal trading partners`, values_from = VALUE) %>%
  mutate(`All Other Countries` = `Total of all countries`-`United States`) %>%
  select(-`Total of all countries`) %>%
  pivot_longer(cols = c(`United States`,`All Other Countries`))

GOODS_TRADE_TOTAL <- get_cansim_vector("v91578557") %>%
  subset(Date >= as.Date("2000-01-01")) %>%
  mutate(name = "Total")

CAN_TRADE_GRAPH <- ggplot(GOODS_TRADE_US_NONUS, aes(fill=name, x=REF_DATE, y=value/1000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 93) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = GOODS_TRADE_TOTAL, aes(x=Date, y = VALUE/1000, color = "Total"), size = 1.25) +
  xlab("Date") +
  ylab("Billions of Canadian Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-20,0,20,40), limits = c(-30,40), expand = c(0,0)) +
  ggtitle("Canadian Commodity Check") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "A Surge in Net Exports to the US Has Brought Canada's Goods Trade Balance Positive") +
  theme_apricitas + theme(legend.position = c(.62,.82), legend.spacing.y = unit(-0.2, "cm"), legend.title = element_text(vjust = unit(+0.4,"cm"))) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= "Goods Trade Balance, Canada",values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("United States","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -30-(.3*70), ymax = -30) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_TRADE_GRAPH, "Canada Trade Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


SUPPLY_BOTTLENECKS <- get_series("BOS_2023Q1_C7_S1")

LABOR_BOTTLENECKS <- get_series("BOS_2023Q1_C7_S2")

BOTTLENECKS_GRAPH <- ggplot() + #plotting Labor Shortage
  geom_line(data=LABOR_BOTTLENECKS, aes(x=date,y= BOS_2023Q1_C7_S2/100, color= "Share of Firms With Broad Labor Bottlenecks"), size = 1.25) +
  geom_line(data=SUPPLY_BOTTLENECKS, aes(x=date,y= BOS_2023Q1_C7_S1/100, color= "Share of Firms With Broad Supply-Chain Bottlenecks"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Canadian Capacity Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data",subtitle = "Broadly, Canadian Labor and Supply Bottlenecks are Tight but Easing") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BOTTLENECKS_GRAPH, "Bottlenecks Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager




# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
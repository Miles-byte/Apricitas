pacman::p_load(forcats,rnaturalearth,rnaturalearthdata,ggpubr,canadianmaps,statcanR,cansim,valet,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Logo.png")
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CAPACITY_SOME <- get_series("CAPACITYSOME") %>%
  transmute(date, value = CAPACITYSOME) %>%
  mutate(name = "Some Difficulty")
  
CAPACITY_SIGNIF <- get_series("CAPACITYSIGNIF") %>%
  transmute(date, value = CAPACITYSIGNIF) %>%
  mutate(name = "Significant Difficulty")

CAPACITY <- rbind(CAPACITY_SOME,CAPACITY_SIGNIF) %>%
  mutate(name = factor(name, levels = c("Some Difficulty","Significant Difficulty"))) #%>%
  # pivot_wider() %>%
  # mutate(sum = `Some Difficulty`+`Significant Difficulty`)

CAPACITY_GRAPH <- ggplot(data = CAPACITY, aes(x = date, y = value/100, fill = name)) +
  geom_col(stat = "identity", position = "stack", color = NA, width = 92) +
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.25,0.5,0.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("Canadian Capacity Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data", subtitle = "Canadian Firms Now Say They Would Have Difficulty Meeting Increased Demand at Normal Rates") +
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
  subset(Gender == "Total - Gender") %>%
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


CAN_JOB_GROWTH <- statcan_data("14-10-0355-02", "eng") %>%
  subset(GEO == "Canada") %>%
  subset(Statistics == "Estimate") %>%
  subset(`Data type` == "Seasonally adjusted") %>%
  subset(REF_DATE >= "2020-01-01") %>%
  select(REF_DATE,VALUE,`North American Industry Classification System (NAICS)`) %>%
  pivot_wider(names_from = `North American Industry Classification System (NAICS)`, values_from = VALUE) %>%
  mutate(`Professional & Business Services, Finance, and Real Estate` = `Professional, scientific and technical services [54]`+`Business, building and other support services [55-56]` + `Finance, insurance, real estate, rental and leasing [52-53]`) %>%
  mutate(`Trade, Transportation, and Utilities` = `Utilities [22]`+`Wholesale and retail trade [41, 44-45]`+`Transportation and warehousing [48-49]`) %>%
  mutate(`Goods Producing Industries and Construction` = `Goods-producing sector`-`Utilities [22]`) %>%
  mutate(`Education and Health Services` = `Educational services [61]`+`Health care and social assistance [62]`) %>%
  mutate(`Other Services Including Information & Recreation` = `Other services (except public administration) [81]`+`Information, culture and recreation [51, 71]`) %>%
  mutate(`Accomodation and Food Services` = `Accommodation and food services [72]`) %>%
  mutate(`Public Administration` = `Public administration [91]`) %>%
  select(REF_DATE,`Professional & Business Services, Finance, and Real Estate`,`Trade, Transportation, and Utilities`,`Goods Producing Industries and Construction`,`Education and Health Services`,`Other Services Including Information & Recreation`,`Accomodation and Food Services`,`Public Administration`) %>%
  mutate_if(is.numeric, funs(.-.[1])) %>%
  pivot_longer(cols = `Professional & Business Services, Finance, and Real Estate`:`Public Administration`) %>%
  mutate(name = factor(name,levels = c("Accomodation and Food Services","Trade, Transportation, and Utilities","Goods Producing Industries and Construction","Education and Health Services","Professional & Business Services, Finance, and Real Estate","Public Administration","Other Services Including Information & Recreation")))

CAN_JOB_GROWTH_LINE <- CAN_JOB_GROWTH %>%
  group_by(REF_DATE) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(REF_DATE >= as.Date("2020-01-01")) %>%
  mutate(name = NA)

CAN_JOB_GROWTH_LINE_MAX <- CAN_JOB_GROWTH %>%
  filter(value > 0) %>%
  group_by(REF_DATE) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(REF_DATE >= as.Date("2020-01-01")) %>%
  mutate(name = NA)

CAN_JOB_GROWTH_GRAPH <- ggplot(data = CAN_JOB_GROWTH, aes(x = REF_DATE, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_point(data = CAN_JOB_GROWTH_LINE, aes(x=REF_DATE, y = value/1000), size = 2, fill ="#970C10", color = "#970C10", shape = 23) +
  geom_line(data = CAN_JOB_GROWTH_LINE, aes(x=REF_DATE, y = value/1000, color = "null"), size = 1, show.legend = FALSE) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), breaks = c(-3,-2,-1,0,1,2), limits = c(-3.5,2), expand = c(0,0)) +
  ggtitle("The Shape of Canadian Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "There are Now More Jobs Than Pre-Pandemic—and Most Sectors Have Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.7,.3)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Accomodation and Food Services","Trade, Transportation, and Utilities","Goods Producing Industries and Construction","Education and Health Services","Professional & Business Services, Finance, and Real Estate","Public Administration","Other Services Including Information & Recreation")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  scale_color_manual(name= NULL,values = c("#970C10")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -3.5-(.3*5.5), ymax = -3.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "k"), breaks = c(0,250,500,750,1000,1250), limits = c(0,1250), expand = c(0,0)) +
  ggtitle("Bienvenue au Canada") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "The Number of Recent Prime-Age Canadian Immigrants Stalled in 2020 But Has Surged Recently") +
  theme_apricitas + theme(legend.position = c(.55,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Immigrants Landed in the Last 5 Years, Age 25-54, Canada",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Population","Employment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*1250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), breaks = c(0,50,100,150,200), limits = c(75,175), expand = c(0,0)) +
  ggtitle("Can Canada Catch?") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada and BEA data", subtitle = "After Falling Behind the US Early in the Pandemic, Canada is Closing the Output Gap") +
  theme_apricitas + theme(legend.position = c(.55,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Real GDP",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 75-(.3*100), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
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
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "Canadian Housing Starts Have Fallen Thanks to Rising Rates") +
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
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-20,0,20,40), limits = c(-40,40), expand = c(0,0)) +
  ggtitle("Canadian Goods Trade") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "A Surge in Net Exports to the US Has Brought Canada's Goods Trade Balance Positive") +
  theme_apricitas + theme(legend.position = c(.62,.82)) +
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

DEBT_SERVICE_INDICATORS <- statcan_data("11-10-0065-01", "eng") %>%
  filter(Estimates %in% c("Debt service ratio","Mortgage debt service ratio","Non-mortgage debt service ratio","Debt service ratio, interest only","Mortgage debt service ratio, interest only","Non-mortgage debt service ratio, interest only","Of which: consumer debt service ratio, interest only") & `Seasonal adjustment` == "Seasonally adjusted at annual rates") %>%
  transmute(date = REF_DATE, value = VALUE, name = Estimates) %>%
  pivot_wider()
  
DEBT_SERVICE_INDICATORS_GRAPH <- ggplot() +
  geom_line(data=DEBT_SERVICE_INDICATORS, aes(x=date,y= `Mortgage debt service ratio`/100, color= "Mortgage Debt Service Ratio"), size = 1.25) +
  geom_line(data=DEBT_SERVICE_INDICATORS, aes(x=date,y= `Mortgage debt service ratio, interest only`/100, color= "Mortgage Debt Service Ratio, Interest Only"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.10), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Rising Cost of Canadian Mortgages") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data",subtitle = "Rising Rates Have Dramatically Raised Mortgage Servicing Costs for Canadian Households") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*.1), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEBT_SERVICE_INDICATORS_GRAPH, "Debt Service Indicators Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FIVE_YEAR_FIXED <- get_series("FVI_MTG_RATE_5Y_FIX")
FIVE_YEAR_VARIABLE <- get_series("FVI_MTG_RATE_5Y_VAR")

CANADIAN_MORTGAGE_RATES_GRAPH <- ggplot() +
  geom_line(data=FIVE_YEAR_VARIABLE, aes(x=date,y= FVI_MTG_RATE_5Y_VAR/100, color= "5-Year Variable Mortgage Rate"), size = 1.25) +
  geom_line(data=FIVE_YEAR_FIXED, aes(x=date,y= FVI_MTG_RATE_5Y_FIX/100, color= "5-Year Fixed Mortgage Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.08), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Rising Canadian Mortgage Rates") +
  labs(caption = "Graph created by @JosephPolitano using Bank of Canada data",subtitle = "Canadian Mortgage Rates Have Substantially Increased as the Bank of Canada Tightens Policy") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*.08), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CANADIAN_MORTGAGE_RATES_GRAPH, "Canadian Mortgage Rates Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CANADA_GDP_BULK <- statcan_data("36-10-0104-01", "eng")

CANADA_GDP_CONTRIB <- CANADA_GDP_BULK %>%
  subset(REF_DATE >= as.Date("2022-01-01")) %>%
  subset(Prices == "Contributions to percent change, annualized") %>%
  filter(Estimates %in% c(
    "Final consumption expenditure",
    "General governments final consumption expenditure",
    "Gross fixed capital formation",
    "General governments gross fixed capital formation",
    "Investment in inventories",
    "Exports of goods and services",
    "Less: imports of goods and services",
    "Statistical discrepancy"
  )) %>%
  select(REF_DATE,Estimates,VALUE) %>%
  pivot_wider(names_from = "Estimates", values_from = "VALUE") %>%
  transmute(date = REF_DATE,
            Consumption = `Final consumption expenditure`-`General governments final consumption expenditure`,
            Investment = `Gross fixed capital formation`-`General governments gross fixed capital formation`,
            Inventories = `Investment in inventories`,
            `Net Exports` = `Exports of goods and services`-`Less: imports of goods and services`,
            Government = `General governments final consumption expenditure` + `General governments gross fixed capital formation`
  ) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c("Consumption","Investment","Inventories","Net Exports","Government")))


CANADA_RGDPQuarterly <- CANADA_GDP_BULK %>%
  subset(REF_DATE >= as.Date("2022-01-01")) %>%
  subset(Prices == "Contributions to percent change, annualized") %>%
  subset(Estimates == "Gross domestic product at market prices") %>%
  transmute(date = REF_DATE, value = VALUE)

GDPContribCANADA_Graph <- ggplot(CANADA_GDP_CONTRIB, aes(fill=name, x=date, y=value/100)) +
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = CANADA_RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.1,0.1), breaks = c(-0.1,-0.05,0,0.05,0.1), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Contributions to Canadian Real GDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = "right") +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#3083DC","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Inventories","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-12-01")-(.1861*(today()-as.Date("2021-12-01"))), xmax = as.Date("2021-12-01")-(0.049*(today()-as.Date("2021-12-01"))), ymin = -0.1-(.3*0.2), ymax = -0.1) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = GDPContribCANADA_Graph, "GDP Contributions Canada.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CANADA_CONS_INVESTMENT_BULK <- statcan_data("34-10-0293-01", "eng") 

CANADA_CONS_INVESTMENT <- CANADA_CONS_INVESTMENT_BULK %>%
  filter(GEO == "Canada") %>%
  filter(`Type of work` == "Types of work, total") %>%
  filter(`Investment Value` == "Seasonally adjusted - constant") %>%
  filter(`Type of structure` %in% c("Single dwelling building total", "Multiple dwelling building total", "Total non-residential")) %>%
  transmute(date = REF_DATE, value = VALUE, name = `Type of structure`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider()

CANADIAN_CONS_INVESTMENT_GRAPH <- ggplot() +
  geom_line(data=CANADA_CONS_INVESTMENT, aes(x=date,y= `Total non-residential`/1000000000, color= "Nonresidential"), size = 1.25) +
  geom_line(data=CANADA_CONS_INVESTMENT, aes(x=date,y= `Single dwelling building total`/1000000000, color= "Single-Family Housing"), size = 1.25) +
  geom_line(data=CANADA_CONS_INVESTMENT, aes(x=date,y= `Multiple dwelling building total`/1000000000, color= "Multi-Family Housing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,11), expand = c(0,0)) +
  ylab("Billions of 2017 Canadian Dollars, Monthly") +
  ggtitle("Canadian Real Construction Investment") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Overall Canadian Construction Continues to Hold Steady, With Multifamily Rising") +
  theme_apricitas + theme(legend.position = c(.25,.25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Multi-Family Housing","Single-Family Housing","Nonresidential")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*11), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CANADIAN_CONS_INVESTMENT_GRAPH, "Canadian Construction Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CANADA_CONS_INVESTMENT_NEW <- CANADA_CONS_INVESTMENT_BULK %>%
  filter(GEO == "Canada") %>%
  filter(`Type of work` == "New construction") %>%
  filter(`Investment Value` == "Seasonally adjusted - constant") %>%
  filter(`Type of structure` %in% c("Single", "Multiple dwelling building total")) %>%
  transmute(date = REF_DATE, value = VALUE, name = `Type of structure`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider()

CANADIAN_CONS_INVESTMENT_GRAPH <- ggplot() +
  geom_line(data=CANADA_CONS_INVESTMENT_NEW, aes(x=date,y= `Single`/1000000000, color= "New Single-Family Housing"), size = 1.25) +
  geom_line(data=CANADA_CONS_INVESTMENT_NEW, aes(x=date,y= `Multiple dwelling building total`/1000000000, color= "New Multi-Family Housing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,6), expand = c(0,0)) +
  ylab("Billions of 2017 Canadian Dollars, Monthly") +
  ggtitle("Canadian Real Construction Investment") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Canadian Construction of Multifamily Housing Has Risen Significantly Over the Last 7 Years") +
  theme_apricitas + theme(legend.position = c(.25,.25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("New Multi-Family Housing","New Single-Family Housing","Nonresidential")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*6), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CANADIAN_CONS_INVESTMENT_GRAPH, "Canadian Construction Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


NEW_BRUNSWICK_CONS_INVESTMENT_NEW <- CANADA_CONS_INVESTMENT_BULK %>%
  filter(GEO == "New Brunswick") %>%
  filter(`Type of work` == "New construction") %>%
  filter(`Investment Value` == "Seasonally adjusted - constant") %>%
  filter(`Type of structure` %in% c("Single", "Multiple dwelling building total")) %>%
  transmute(date = REF_DATE, value = VALUE, name = `Type of structure`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider()

NEW_BRUNSWICK_CONS_INVESTMENT_GRAPH <- ggplot() +
  geom_line(data=NEW_BRUNSWICK_CONS_INVESTMENT_NEW, aes(x=date,y= `Single`/1000000, color= "New Single-Family Housing"), size = 1.25) +
  geom_line(data=NEW_BRUNSWICK_CONS_INVESTMENT_NEW, aes(x=date,y= `Multiple dwelling building total`/1000000, color= "New Multi-Family Housing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "M"), limits = c(0,1500), expand = c(0,0)) +
  ylab("Billions of 2017 Canadian Dollars, Monthly") +
  ggtitle("Quebec Real Construction Investment") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Quebec Construction of Multifamily Housing Has Held Steady Over the Last 7 Years") +
  theme_apricitas + theme(legend.position = c(.25,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("New Multi-Family Housing","New Single-Family Housing","Nonresidential")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = NEW_BRUNSWICK_CONS_INVESTMENT_GRAPH, "New Brunswick Construction Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CANADA_CONS_INVESTMENT_BULK <- statcan_data("34-10-0293-01", "eng") 

CANADA_CONS_INVESTMENT_INDUSTRIAL <- CANADA_CONS_INVESTMENT_BULK %>%
  filter(GEO == "Canada") %>%
  filter(`Type of work` == "Types of work, total") %>% #guess
  filter(`Investment Value` == "Unadjusted - constant") %>% #guess
  filter(`Type of structure` %in% c("Factories, plants","Transportation, utilities","Mining and agriculture")) %>%
  transmute(date = REF_DATE, name = recode(`Type of structure`, "Factories, plants" = "Factories and Plants", "Transportation, utilities" = "Transportation and Utilities", "Mining and agriculture" = "Mining and Agriculture"), value = VALUE/1e6) %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(roll = roll_sumr(value, 12)) %>%
  ungroup() %>%
  mutate(value = value*12) %>%
  filter(date >= as.Date("2018-01-01"))

CANADA_CONS_INVESTMENT_INDUSTRIAL_GRAPH <- ggplot(data = CANADA_CONS_INVESTMENT_INDUSTRIAL, aes(x = date, y = value/1000, color = name)) +
  geom_line(size = 1.25) +
  xlab("Date") +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = 7, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,NA), expand = c(0,0)) +
  ylab("Billions of 2017 Canadian Dollars, Monthly Annualized") +
  ggtitle("Canadian Industrial Construction Investment") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Not Seasonally Adjusted, Constant Prices", subtitle = "Canadian Investment in Factories Has Slid, but Spending on Transportation & Utility Infrastructure is Up") +
  theme_apricitas + theme(legend.position = c(.35,.16)) +
  scale_color_manual(name = "Real Investment in Industrial Building Construction", values = c("Factories and Plants" = "#FFE98F", "Transportation and Utilities" = "#00A99D", "Mining and Agriculture" = "#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*8), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina", plot = CANADA_CONS_INVESTMENT_INDUSTRIAL_GRAPH, "Canada Industrial Construction Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CANADA_HOUSING_PERMITS_BULK <- statcan_data("34-10-0292-01", "eng") 

CANADA_TOTAL_HOUSING_PERMITS <- CANADA_HOUSING_PERMITS_BULK %>%
  filter(GEO == "Canada" & Variables == "Number of dwelling-units created" & `Seasonal adjustment, value type` == "Seasonally adjusted, current") %>%
  filter(`Type of building` != "Total residential") %>%
  select(REF_DATE, `Type of building`,VALUE) %>%
  mutate(`Type of building` = case_when(
    `Type of building` == "Single dwelling building total"   ~ "Single-Family",
    `Type of building` == "Multiple dwelling building total" ~ "Multi-Family",
    TRUE ~ `Type of building`
  ))

CANADA_TOTAL_HOUSING_PERMITS_graph <- ggplot(data = CANADA_TOTAL_HOUSING_PERMITS, aes(x = REF_DATE, y = VALUE*12/1000, fill = `Type of building`)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  xlab("Date") +
  ylab("Housing Units, Seasonally Adjusted Annual Rate") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200,300,400), limits = c(0,400), expand = c(0,0)) +
  ggtitle("Canadian Housing Permits") +
  labs(caption = "Graph created by @JosephPolitano using Stastistics Canada data (table 34-10-0292-01)", subtitle = "Canada Mostly Builds Multi-Family Housing, With Permits Hitting a Record High in Late 2024") +
  theme_apricitas + theme(legend.position = c(0.35,0.83)) +
  scale_fill_manual(name= NULL, values = c("#FFE98F","#00A99D","#9A348E","#3083DC","#6A4C93")) +
  theme_apricitas + theme(legend.position = c(.25,.86)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*400), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CANADA_TOTAL_HOUSING_PERMITS_graph, "Canada Total Housing Permits.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


NEW_BRUNSWICK_CONS_INVESTMENT_GRAPH <- ggplot() +
  geom_line(data=CANADA_HOUSING_PERMITS_BULK, aes(x=date,y= `Single`/1000000, color= "New Single-Family Housing"), size = 1.25) +
  geom_line(data=CANADA_HOUSING_PERMITS_BULK, aes(x=date,y= `Multiple dwelling building total`/1000000, color= "New Multi-Family Housing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "M"), limits = c(0,1500), expand = c(0,0)) +
  ylab("Billions of 2017 Canadian Dollars, Monthly") +
  ggtitle("Quebec Real Construction Investment") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Quebec Construction of Multifamily Housing Has Held Steady Over the Last 7 Years") +
  theme_apricitas + theme(legend.position = c(.25,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("New Multi-Family Housing","New Single-Family Housing","Nonresidential")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = NEW_BRUNSWICK_CONS_INVESTMENT_GRAPH, "New Brunswick Construction Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CANADA_TOURISM_ARRIVALS_BULK <- statcan_data("24-10-0054-01", "eng") 

CANADA_TOURISM_ARRIVALS <- CANADA_TOURISM_ARRIVALS_BULK %>%
  filter(`Traveller characteristics` %in% c("United States of America residents entering Canada", "Canadian residents returning from the United States of America","Canadian residents returning from countries other than the United States of America")) %>%
  filter(`Traveller type` == "Travellers") %>%
  filter(GEO == "Canada") %>%
  #filter(`Investment Value` == "Seasonally adjusted - constant") %>%
  #filter(`Type of structure` %in% c("Single", "Multiple dwelling building total")) %>%
  transmute(date = REF_DATE, value = VALUE, name = `Traveller characteristics`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider()


CANADA_TOURISM_ARRIVALS_GRAPH <- ggplot() +
  geom_line(data=CANADA_TOURISM_ARRIVALS, aes(x=date,y= `United States of America residents entering Canada`/1000000, color= "US Visitors to Canada"), size = 1.25) +
  geom_line(data=CANADA_TOURISM_ARRIVALS, aes(x=date,y= `Canadian residents returning from the United States of America`/1000000, color= "Canadian Visitors to the US"), size = 1.25) +
  geom_line(data=CANADA_TOURISM_ARRIVALS, aes(x=date,y= `Canadian residents returning from countries other than the United States of America`/1000000, color= "Canadian Visitors to other countries"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"), limits = c(0,4), expand = c(0,0)) +
  ylab("Tourists, Monthly, Seasonally adjusted") +
  ggtitle("Canadian Tourists are Avoiding the US") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Canadian vists counted from returns data",subtitle = "Tourist Visits from Canada to the US Are Down Significantly Since Trump's Election") +
  theme_apricitas + theme(legend.position = c(.65,.95)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Canadian Visitors to the US","US Visitors to Canada","Canadian Visitors to other countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*4), ymax = 0) +
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = CANADA_TOURISM_ARRIVALS_GRAPH, "Canadian Tourism Arrivals Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CANADA_TOURISM_ARRIVALS_TYPE <- CANADA_TOURISM_ARRIVALS_BULK %>%
  filter(`Traveller characteristics` == "Canadian residents returning from the United States of America") %>%
  #filter(`Traveller type` == "Travellers") %>%
  filter(GEO == "Canada") %>%
  #filter(`Investment Value` == "Seasonally adjusted - constant") %>%
  #filter(`Type of structure` %in% c("Single", "Multiple dwelling building total")) %>%
  transmute(date = REF_DATE, value = VALUE, name = `Traveller type`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider()

CANADA_TOURISM_TRAVELLER_TYPE_GRAPH <- ggplot() +
  geom_line(data=CANADA_TOURISM_ARRIVALS_TYPE, aes(x=date,y= `Excursionists (same-day)`/1000000, color= "Excursionists (same-day)"), size = 1.25) +
  geom_line(data=CANADA_TOURISM_ARRIVALS_TYPE, aes(x=date,y= `Tourists (overnight)`/1000000, color= "Tourists (overnight)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"), limits = c(0,2.5), expand = c(0,0)) +
  ylab("Tourists, Monthly, Seasonally adjusted") +
  ggtitle("Canadian Visits to the US by Type") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Canadian vists counted from returns data",subtitle = "Canadians are taking fewer trips, both long and short, to the US") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Excursionists (same-day)","Tourists (overnight)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*2.5), ymax = 0) +
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = CANADA_TOURISM_TRAVELLER_TYPE_GRAPH, "Canadian Tourism Arrivals Type Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CANADA_TOURISM_ARRIVALS_PROVINCE <- CANADA_TOURISM_ARRIVALS_BULK %>%
  filter(`Traveller characteristics` == "Canadian residents returning from the United States of America") %>%
  filter(`Traveller type` == "Travellers") %>%
  filter(GEO != "Canada") %>%
  #filter(`Investment Value` == "Seasonally adjusted - constant") %>%
  #filter(`Type of structure` %in% c("Single", "Multiple dwelling building total")) %>%
  transmute(date = REF_DATE, value = VALUE, name = GEO) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider() %>%
  mutate(across(where(is.numeric), ~ . / .[82] -1 )) %>%
  filter(date >= as.Date("2023-01-01"))

CANADA_TOURISM_TRAVELLER_PROVINCE_GRAPH <- ggplot() +
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=CANADA_TOURISM_ARRIVALS_PROVINCE, aes(x=date,y= Ontario, color= "Ontario"), size = 1.25) +
  geom_line(data=CANADA_TOURISM_ARRIVALS_PROVINCE, aes(x=date,y= `British Columbia`, color= "British Columbia"), size = 1.25) +
  geom_line(data=CANADA_TOURISM_ARRIVALS_PROVINCE, aes(x=date,y= Quebec, color= "Quebec"), size = 1.25) +
  geom_line(data=CANADA_TOURISM_ARRIVALS_PROVINCE, aes(x=date,y= Alberta, color= "Alberta"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.50,.25), expand = c(0,0)) +
  ylab("Tourists, Monthly, Seasonally adjusted") +
  ggtitle("Canadian Visits to the US by Province") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Canadian vists counted from returns data",subtitle = "Canadians are taking fewer trips, both long and short, to the US") +
  theme_apricitas + theme(legend.position = c(.5,.25)) +
  scale_color_manual(name= "Percent Change Since October 2024" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Alberta","Ontario","British Columbia","Quebec")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = -0.5-(.3*0.75), ymax = -0.5) +
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = CANADA_TOURISM_TRAVELLER_PROVINCE_GRAPH, "Canadian Tourism Arrivals Province Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TOURISM_SPEND_QUARTERLY_BULK <- statcan_data("24-10-0070-01", "eng")

CAN_TOURISM_SHARE <- CAN_TOURISM_SPEND_QUARTERLY_BULK %>%
  filter(GEO %in% c("Canada","United States of America","Overseas")) %>%
  filter(`Main trip purpose` == "Holidays, leisure and recreation") %>%
  filter(`Visit duration` == "All visit durations") %>%
  filter(`Visit statistics` == "Expenditures") %>%
  transmute(date = REF_DATE, name = GEO, value = VALUE) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  mutate(value = value/1000000) %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(roll = roll_sumr(value, 4)) %>%
  group_by(date) %>%
  mutate(share = roll/sum(roll)) %>%
  ungroup() %>%
  drop_na()

CAN_TOURISM_SHARE_GRAPH <- ggplot() +
  #geom_line(data = filter(CAN_TOURISM_SHARE, name == "Canada"), aes(x = date, y = share, color = "Domestic"), size = 1.25) +
  geom_line(data = filter(CAN_TOURISM_SHARE, name == "Overseas"), aes(x = date, y = value*4, color = "All Other Countries"), size = 0.75, linetype = "dashed", alpha = 0.4) +
  geom_line(data = filter(CAN_TOURISM_SHARE, name == "United States of America"), aes(x = date, y = value*4, color = "United States"), size = 0.75, linetype = "dashed", alpha = 0.4) +
  geom_line(data = filter(CAN_TOURISM_SHARE, name == "Overseas"), aes(x = date, y = roll, color = "All Other Countries"), size = 1.25) +
  geom_line(data = filter(CAN_TOURISM_SHARE, name == "United States of America"), aes(x = date, y = roll, color = "United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,35), breaks = seq(0,50,5), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars") +
  ggtitle("Canadian International Tourism Spending") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Personal Holidays, Leisure and Recreation Trips, All Visit Durations", subtitle = "Canadians Have Shifted Tourism Spending Away From the US") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_color_manual(name = "Dashed = Quarterly Annualized, Solid = Yearly Rolling Total", values = c("#FFE98F","#00A99D"), breaks = c("United States","All Other Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = min(CAN_TOURISM_SHARE$date)-(.1861*(today()-min(CAN_TOURISM_SHARE$date))), xmax = min(CAN_TOURISM_SHARE$date)-(0.049*(today()-min(CAN_TOURISM_SHARE$date))), ymin = 0-(.3*35), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina", plot = CAN_TOURISM_SHARE_GRAPH, "Canada Tourism Spending Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAN_GDP_MONTHLY_BULK <- statcan_data("36-10-0434-01", "eng")

CAN_GDP_MONTHLY <- CAN_GDP_MONTHLY_BULK %>%
  filter(`Seasonal adjustment` == "Seasonally adjusted at annual rates") %>%
  filter(`Prices` == "Chained (2017) dollars") %>%
  select(REF_DATE,`North American Industry Classification System (NAICS)`, VALUE) %>%
  pivot_wider(names_from = `North American Industry Classification System (NAICS)`, values_from = VALUE)

CAN_GDP_MONTHLY_GRAPH <- ggplot() +
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(CAN_GDP_MONTHLY, REF_DATE >= as.Date("2022-01-01")), aes(x=REF_DATE,y= `All industries [T001]`/1000000, color= "Canadian Monthly Real GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01, suffix = "T"), limits = c(2.15,2.4), expand = c(0,0)) +
  ylab("Trillions of Chained 2017 Canadian Dollars") +
  ggtitle("Canadian Real GDP") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Canada's Economy Has Been Flat Overall in 2025") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 2.15-(.3*0.25), ymax = 2.15) +
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = CAN_GDP_MONTHLY_GRAPH, "Canada GDP Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAN_GDP_MONTHLY_MANU_GRAPH <- ggplot() +
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(CAN_GDP_MONTHLY, REF_DATE >= as.Date("2018-01-01")), aes(x=REF_DATE,y= `Manufacturing [31-33]`/1000, color= "Canadian Manufacturing, Real Value Added"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(150,230), expand = c(0,0)) +
  ylab("Billions of Chained 2017 Canadian Dollars") +
  ggtitle("Canadian Manufacturing Output is Falling") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Canadian Manufacturing Output Continues Declining Admist Tariffs & General Economic Weakness") +
  theme_apricitas + theme(legend.position = c(.4,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 150-(.3*75), ymax = 150) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_GDP_MONTHLY_MANU_GRAPH, "Canada GDP Monthly Manu Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_GDP_MONTHLY_CARS_GRAPH <- ggplot() +
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(CAN_GDP_MONTHLY, REF_DATE >= as.Date("2018-01-01")), aes(x=REF_DATE,y= `Motor vehicles and parts manufacturing [336Y]`/1000, color= "Canadian Motor Vehicle & Parts Manufacturing, Real Value Added"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,25), expand = c(0,0)) +
  ylab("Billions of Chained 2017 Canadian Dollars") +
  ggtitle("Canadian Car Output Amid US Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Canadian Car Industry Output Continues Struggling Amidst 25% Tariffs on US Vehilces Imposed by the US") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = CAN_GDP_MONTHLY_CARS_GRAPH, "Canada GDP Monthly Cars Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_GDP_MONTHLY_INDEX <- CAN_GDP_MONTHLY %>%
  arrange(REF_DATE) %>%
  transmute(date = REF_DATE, `Real GDP` = `All industries [T001]`, `Manufacturing` = `Manufacturing [31-33]`, `Construction` = `Construction [23]`, `Energy` = `Energy sector [T016]`) %>%
  mutate(across(-date, ~ .x/.x[date == as.Date("2025-01-01")]*100)) %>%
  filter(date >= as.Date("2024-01-01")) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c("Real GDP","Manufacturing","Construction","Energy")))

CAN_GDP_MONTHLY_INDEX_GRAPH <- ggplot() +
  annotate(geom = "hline", y = 100, yintercept = 100, size = .25, color = "white") +
  geom_line(data = filter(CAN_GDP_MONTHLY_INDEX, name == "Manufacturing"), aes(x = date, y = value, color = "Manufacturing"), size = 1.25) +
  geom_line(data = filter(CAN_GDP_MONTHLY_INDEX, name == "Construction"), aes(x = date, y = value, color = "Construction"), size = 1.25) +
  geom_line(data = filter(CAN_GDP_MONTHLY_INDEX, name == "Energy"), aes(x = date, y = value, color = "Energy"), size = 1.25) +
  geom_line(data = filter(CAN_GDP_MONTHLY_INDEX, name == "Real GDP"), aes(x = date, y = value, color = "Real GDP"), size = 2.25) +
  annotate("segment", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), y = 94, yend = 102, color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-25"), y = 101, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(limits = c(94,104), breaks = seq(90,110,1), expand = c(0,0)) +
  ylab("Index, Jan 2025 = 100") +
  ggtitle("Real Canadian Output Amid US Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "Canadian Manufacturing Has Contracted Amid US Tariffs, But Construction, Energy, & Total GDP are Up") +
  theme_apricitas + theme(legend.position = c(.25,.85), legend.key.height = unit(0.5,"cm")) +
  scale_color_manual(name = "Real Value Added, Index Jan 2025 = 100", values = c("Real GDP" = "#FFE98F", "Manufacturing" = "#00A99D", "Construction" = "#EE6055", "Energy" = "#A7ACD9"), breaks = c("Real GDP","Manufacturing","Construction","Energy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2024-02-01")-(.1861*(today()-as.Date("2024-02-01"))), xmax = as.Date("2024-02-01")-(0.049*(today()-as.Date("2024-02-01"))), ymin = 94-(.3*10), ymax = 94) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina", plot = CAN_GDP_MONTHLY_INDEX_GRAPH, "Canada GDP Monthly Index Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


LFS_BULK <- statcan_data("14-10-0287-03", "eng")

LABORFORCE <- LFS_BULK %>%
  filter(GEO == "Canada" & Gender == "Total - Gender" & `Data type` == "Seasonally adjusted" & `Age group` == "15 years and over" & Statistics == "Estimate") %>%
  filter(`Labour force characteristics` == "Labour force") %>%
  transmute(date = REF_DATE, value = VALUE)

EMPLEVEL <- LFS_BULK %>%
  filter(GEO == "Canada" & Gender == "Total - Gender" & `Data type` == "Seasonally adjusted" & `Age group` == "15 years and over" & Statistics == "Estimate") %>%
  filter(`Labour force characteristics` == "Employment") %>%
  transmute(date = REF_DATE, value = VALUE) 

UNRATE <- LFS_BULK %>%
  filter(GEO == "Canada" & Gender == "Total - Gender" & `Data type` == "Seasonally adjusted" & `Age group` == "15 years and over" & Statistics == "Estimate") %>%
  filter(`Labour force characteristics` == "Unemployment") %>%
  transmute(date = REF_DATE, value = VALUE) %>%
  merge(., LABORFORCE, by = "date") %>%
  transmute(date, value = value.x/value.y)

POP2554 <- LFS_BULK %>%
  filter(GEO == "Canada" & Gender == "Total - Gender" & `Data type` == "Seasonally adjusted" & `Age group` == "25 to 54 years" & Statistics == "Estimate") %>%
  filter(`Labour force characteristics` == "Population") %>%
  transmute(date = REF_DATE, value = VALUE)


PRIMEEPOP <- LFS_BULK %>%
  filter(GEO == "Canada" & Gender == "Total - Gender" & `Data type` == "Seasonally adjusted" & `Age group` == "25 to 54 years" & Statistics == "Estimate") %>%
  filter(`Labour force characteristics` == "Employment") %>%
  transmute(date = REF_DATE, value = VALUE) %>%
  merge(., POP2554, by = "date") %>%
  transmute(date, value = value.x/value.y)



UNRATE_Graph <- ggplot() +
  geom_line(data= filter(UNRATE, date >= as.Date("2021-01-01")), aes(x=date,y= value, color = "layoffs"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.10), breaks = c(0,.02,.04,0.06,0.08,0.1), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Unemployment Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm")) #reducing plot margins makes the ggarrange look better

EPOP_Graph <- ggplot() +
  geom_line(data= filter(PRIMEEPOP, date >= as.Date("2021-01-01")), aes(x=date,y= value, color = "EPOP"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.80,0.86), breaks = c(0.80,0.82,0.84,.86), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Prime Age (25-54) Employment Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm")) #reducing plot margins makes the ggarrange look better

EMPLEVEL_Graph <- ggplot() +
  geom_line(data= filter(EMPLEVEL, date >= as.Date("2021-01-01")), aes(x=date,y= value/1000, color = "layoffs"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(18,21.5), breaks = c(18,19,20,21), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Employment Level") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

EMPGROWTH <- EMPLEVEL %>%
  mutate(value = value-lag(value,1))


EMPGROWTH_Graph <- ggplot(filter(EMPGROWTH, date >= as.Date("2021-01-01")), aes(x = date, y = value)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_bar(stat = "identity", fill = "#FFE98F", color = NA, show.legend = FALSE) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::comma_format(suffix = "k"),limits = c(-100,275), breaks = c(-100,0,100,200), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Employment Growth, Monthly") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,-0.28),"cm")) #reducing plot margins makes the ggarrange look better


tgrob <- text_grob(expression(bold("                                   Canadian Jobs Data")),size = 29, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-.15,-.15,-0.15,-.15),"cm"))  
blank <- ""
blankgrob <- text_grob(blank,size = 20)
plot_blank <- as_ggplot(blankgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))
JOBS_ARRANGE_TITLE_GRAPH <- ggarrange(plot_0,plot_blank,UNRATE_Graph, EMPGROWTH_Graph, EPOP_Graph, EMPLEVEL_Graph ,  ncol = 2, nrow = 3, heights = c(5,20,20), widths = 10, common.legend = TRUE, legend = "none") + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = JOBS_ARRANGE_TITLE_GRAPH, "Jobs Data Update Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

cat(
  "NEW CANADIAN JOBS DATA:\n",
  "Employment Growth: ",
  scales::number(EMPGROWTH %>% tail(1) %>% pull(value), accuracy = 1), "k\n",
  "Unemployment Rate: ",
  scales::percent(UNRATE %>% tail(1) %>% pull(value), accuracy = 0.1),
  " (",
  scales::percent(
    (UNRATE %>% tail(2) %>% pull(value))[2] -
      (UNRATE %>% tail(2) %>% pull(value))[1],
    accuracy = 0.1
  ),
  ")\n",
  "Prime Age (25-54) Employment Rate: ",
  scales::percent(PRIMEEPOP %>% tail(1) %>% pull(value), accuracy = 0.1),
  " (",
  scales::percent(
    (PRIMEEPOP %>% tail(2) %>% pull(value))[2] -
      (PRIMEEPOP %>% tail(2) %>% pull(value))[1],
    accuracy = 0.1
  ),
  ")\n",
  "Employment Level: ",
  scales::number(EMPLEVEL %>% tail(1) %>% pull(value)/1e3, accuracy = 0.1), "M ",
  "\n",
  sep = ""
)

UNRATE_PROVINCE_DATA <- LFS_BULK %>%
  filter(Gender == "Total - Gender" & `Data type` == "Seasonally adjusted" & `Age group` == "15 years and over" & Statistics == "Estimate") %>%
  filter(`Labour force characteristics` == "Unemployment rate") %>%
  transmute(date = REF_DATE, value = VALUE/100, province = GEO) %>%
  filter(date == max(date)) %>%
  arrange(desc(value))


UNRATE_PROVINCE_DATA_MAP <- ne_states(country = "canada", returnclass = "sf") %>%
  select(province = name_en, geometry) %>%
  left_join(UNRATE_PROVINCE_DATA, by = "province") %>%
  mutate(province_abbrev = c("BC","AB","SK","MB","ON","QC","NB","YT","NU","NL","NS","NT","PE"))

CANADA_UNEPLOYMENT_MAP <- UNRATE_PROVINCE_DATA_MAP  %>%
  ggplot(aes(fill = value)) +
  geom_sf(color = NA) +
  geom_sf(aes(fill = value), colour = "black", size = 0.1) +
  ggtitle(paste0("Canada, Unemployment % by Province ", format(UNRATE_PROVINCE_DATA$date[1], "%b %Y"))) +
  coord_sf(crs = 3347, xlim = c(2700000, 9300000), ylim = c(900000, 5000000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data") +
  geom_label(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("NL")), 
    aes(x = 9500000, 2700000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("PE")), 
    aes(x = 9500000, 2430000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("NS")), 
    aes(x = 9500000, 2160000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("NB")), 
    aes(x = 9500000, 1890000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("BC")), 
    aes(x = 4400000, 2400000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("AB")), 
    aes(x = 5000000, 2300000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("SK")), 
    aes(x = 5500000, 2200000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("MB")), 
    aes(x = 6000000, 2300000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("ON")), 
    aes(x = 6700000, 1800000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(UNRATE_PROVINCE_DATA_MAP, province_abbrev %in% c("QC")), 
    aes(x = 7600000, 1700000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  scale_fill_gradientn(colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(-0.02,0,0.02,0.04,0.06,0.08,0.1), expand = c(0,0)) +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(t = 0.15, r = 0, b= 0,l = 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 27))

ggsave(dpi = "retina",plot = CANADA_UNEPLOYMENT_MAP, "Canada Unemployment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PROVINCE_GDP_BULK <- statcan_data("36-10-0222-01", "eng")

PROVINCE_GDP_YOY <- PROVINCE_GDP_BULK %>%
  filter(Estimates == "Gross domestic product at market prices") %>%
  filter(Prices == "Chained (2017) dollars") %>%
  select(REF_DATE, GEO, VALUE) %>%
  pivot_wider(names_from = GEO, values_from = VALUE) %>%
  mutate(across(where(is.numeric), ~ .x / lag(.x) - 1)) %>%
  filter(REF_DATE == max(REF_DATE)) %>%
  pivot_longer(cols = -REF_DATE) %>%
  transmute(province = name, value)


GDP_PROVINCE_YOY_DATA_MAP <- ne_states(country = "canada", returnclass = "sf") %>%
  select(province = name_en, geometry) %>%
  left_join(PROVINCE_GDP_YOY, by = "province") %>%
  mutate(province_abbrev = c("BC","AB","SK","MB","ON","QC","NB","YT","NU","NL","NS","NT","PE"))

CANADA_GDP_PROVINCE_YOY_DATA_MAP_GRAPH <- GDP_PROVINCE_YOY_DATA_MAP  %>%
  ggplot(aes(fill = value)) +
  geom_sf(color = NA) +
  geom_sf(aes(fill = value), colour = "black", size = 0.1) +
  ggtitle("Canada, 2024 RGDP Growth by Province/Territory") +
  coord_sf(crs = 3347, xlim = c(2700000, 9300000), ylim = c(900000, 5000000)) +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data") +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("NL")), 
    aes(x = 9500000, 2700000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("PE")), 
    aes(x = 9500000, 2430000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("NS")), 
    aes(x = 9500000, 2160000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("NB")), 
    aes(x = 9500000, 1890000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("YT")), 
    aes(x = 3500000, 4500000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("NT")), 
    aes(x = 3500000, 4230000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("NU")), 
    aes(x = 3500000, 3960000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("BC")), 
    aes(x = 4400000, 2400000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("AB")), 
    aes(x = 5000000, 2300000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("SK")), 
    aes(x = 5500000, 2200000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("MB")), 
    aes(x = 6000000, 2300000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("ON")), 
    aes(x = 6700000, 1800000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_YOY_DATA_MAP, province_abbrev %in% c("QC")), 
    aes(x = 7600000, 1700000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1),breaks = c(-0.02,0,0.02,0.04,0.06,0.08,0.1), expand = c(0,0)) +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(t = 0.15, r = 0, b= 0,l = 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 27))
  
ggsave(dpi = "retina",plot = CANADA_GDP_PROVINCE_YOY_DATA_MAP_GRAPH, "Canada GDP Province Yoy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PROVINCE_GDP_2019 <- PROVINCE_GDP_BULK %>%
  filter(Estimates == "Gross domestic product at market prices") %>%
  filter(Prices == "Chained (2017) dollars") %>%
  select(REF_DATE, GEO, VALUE) %>%
  pivot_wider(names_from = GEO, values_from = VALUE) %>%
  mutate(across(where(is.numeric), ~ .x / .x[39] - 1)) %>%
  filter(REF_DATE == max(REF_DATE)) %>%
  pivot_longer(cols = -REF_DATE) %>%
  transmute(province = name, value)


GDP_PROVINCE_2019_DATA_MAP <- ne_states(country = "canada", returnclass = "sf") %>%
  select(province = name_en, geometry) %>%
  left_join(PROVINCE_GDP_2019, by = "province") %>%
  mutate(province_abbrev = c("BC","AB","SK","MB","ON","QC","NB","YT","NU","NL","NS","NT","PE"))


GDP_PROVINCE_2019_DATA_MAP_GRAPH <- GDP_PROVINCE_2019_DATA_MAP  %>%
  ggplot(aes(fill = value)) +
  geom_sf(color = NA) +
  geom_sf(aes(fill = value), colour = "black", size = 0.1) +
  ggtitle("Canada, 2019-2024 RGDP Growth by Province/Territory") +
  coord_sf(crs = 3347, xlim = c(2700000, 9300000), ylim = c(900000, 5000000)) +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data") +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("NL")), 
    aes(x = 9500000, 2700000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, "   ", "  "), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("PE")), 
    aes(x = 9500000, 2430000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("NS")), 
    aes(x = 9500000, 2160000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("NB")), 
    aes(x = 9500000, 1890000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, "   ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("YT")), 
    aes(x = 3500000, 4500000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("NT")), 
    aes(x = 3500000, 4230000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", "  "), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("NU")), 
    aes(x = 3500000, 3960000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("BC")), 
    aes(x = 4400000, 2400000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("AB")), 
    aes(x = 5000000, 2300000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("SK")), 
    aes(x = 5500000, 2200000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("MB")), 
    aes(x = 6000000, 2300000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("ON")), 
    aes(x = 6700000, 1800000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(GDP_PROVINCE_2019_DATA_MAP, province_abbrev %in% c("QC")), 
    aes(x = 7600000, 1700000, label = paste0(province_abbrev, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1),breaks = c(-0.05,0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(t = 0.15, r = 0, b= 0,l = 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 24.5))

ggsave(dpi = "retina",plot = GDP_PROVINCE_2019_DATA_MAP_GRAPH, "Canada GDP Province 2019 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAN_IIP_BULK <- statcan_data("36-10-0485-01", "eng")

#CAN_IIP_BULK %>% select(-REF_DATE, -VALUE) %>% distinct() %>% View() #check labels

CAN_IIP_US_SHARE <- CAN_IIP_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Valuation == "Market value") %>%
  filter(`Canada's international investment position` %in% c("Total assets","Total liabilities")) %>%
  filter(`Geographic region` %in% c("United States","All countries")) %>%
  transmute(date = REF_DATE, value = VALUE, name = `Canada's international investment position`, region = `Geographic region`) %>%
  pivot_wider(names_from = region, values_from = value) %>%
  transmute(date, name, value = `United States`/`All countries`) %>%
  drop_na() %>%
  pivot_wider() %>%
  filter(date >= as.Date("2015-01-01"))

CAN_IIP_US_SHARE_GRAPH <- ggplot() +
  geom_line(data=CAN_IIP_US_SHARE, aes(x=date,y= `Total assets`, color= "US Share of Canada's International Assets"), size = 1.25) +
  geom_line(data=CAN_IIP_US_SHARE, aes(x=date,y= `Total liabilities`, color= "US Share of Canada's International Liabilities"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.48,.63), breaks = c(.50,.55,.60), expand = c(0,0)) +
  ylab("Percent of Total") +
  ggtitle("Canada's Financial Ties to the US") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "The US Makes Up a Majority of Canada's International Assets and Liabilities") +
  theme_apricitas + theme(legend.position = c(.4,.9)) +
  scale_color_manual(name= "Canada's International Investment Position, Market Value" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Share of Canada's International Assets","US Share of Canada's International Liabilities")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = .48-(.3*.15), ymax = .48) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_IIP_US_SHARE_GRAPH, "Canada IIP US Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_PARTNER_BULK <- statcan_data("12-10-0011-01", "eng")

#CAN_TRADE_PARTNER_BULK %>% select(-REF_DATE, -VALUE) %>% distinct() %>% View() #check labels

CAN_TRADE_NONUS <- CAN_TRADE_PARTNER_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Basis == "Balance of payments") %>%
  filter(`Seasonal adjustment` == "Seasonally adjusted") %>%
  filter(Trade %in% c("Export","Import")) %>%
  filter(`Principal trading partners` %in% c("All countries","United States")) %>%
  transmute(date = REF_DATE, value = VALUE, name = Trade, partner = `Principal trading partners`) %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  transmute(date, name, value = `All countries`-`United States`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider() %>%
  mutate(year = year(date))

CAN_TRADE_NONUS_GRAPH <- ggplot() +
  geom_line(data=CAN_TRADE_NONUS, aes(x=date,y= Export/1000, color= "Exports to Non-US Countries"), size = 1.25) +
  geom_line(data=CAN_TRADE_NONUS, aes(x=date,y= Import/1000, color= "Imports from Non-US Countries"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Monthly") +
  ggtitle("Canada's Non-US Trade") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Canadian Goods Trade With Countries Other Than the US Has Grown Amid US Tariffs") +
  theme_apricitas + theme(legend.position = c(.3,.9)) +
  scale_color_manual(name= "Canadian Goods Trade, Balance of Payments Basis" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Exports to Non-US Countries","Imports from Non-US Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_TRADE_NONUS_GRAPH, "Canada Non US Trade Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_EXPORTS_NONUS_TARGET_GRAPH <- ggplot() +
  geom_line(data=CAN_TRADE_NONUS, aes(x=date,y= Export/1000*12, color= "Exports to Non-US Countries"), size = 1.25) +
  xlab("Date") +
  annotate("segment", x = as.Date("2023-10-01"), xend = as.Date(Inf), y = 187623*2/1000, yend = 187623*2/1000, color = "white", size = 1, linetype = "dashed", alpha = 0.75) + 
  annotate("text", label = "Carney's 2035 Export Target", x = as.Date("2026-09-01"), y = 392, color = "white", size = 5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,425), breaks = c(0,100,200,300,400,500), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Monthly Annualized, Seasonally Adjusted") +
  ggtitle("Canada's Rising Non-US Exports") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Canadian Goods Trade With Countries Other Than the US Has Grown Amid US Tariffs") +
  theme_apricitas + theme(legend.position = c(.35,.6)) +
  scale_color_manual(name= "Canadian Goods Trade, Monthly Annualized" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Exports to Non-US Countries","Imports from Non-US Countries")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*425), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_EXPORTS_NONUS_TARGET_GRAPH, "Canada Exports Nonus Target Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAN_TRADE_NONUS_EXPORTS_PARTNER <- CAN_TRADE_PARTNER_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Basis == "Balance of payments") %>%
  filter(`Seasonal adjustment` == "Seasonally adjusted") %>%
  filter(Trade == "Export") %>%
  filter(`Principal trading partners` %in% c("All countries","United States","European Union","China","United Kingdom","Japan","Mexico","South Korea")) %>%
  transmute(date = REF_DATE, partner = `Principal trading partners`, value = VALUE/1e3) %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  mutate(Other = `All countries`-`United States`-`European Union`-China-`United Kingdom`-Japan-Mexico-`South Korea`) %>%
  select(-`All countries`, -`United States`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_longer(-date, names_to = "partner") %>%
  mutate(partner = fct_reorder(partner, if_else(partner == "Other", -Inf, value), .fun = last, .desc = TRUE))

CAN_TRADE_NONUS_EXPORTS_PARTNER_AREA_GRAPH <- ggplot(data = CAN_TRADE_NONUS_EXPORTS_PARTNER, aes(x = date, y = value*12, fill = partner)) +
  geom_area(position = "stack", color = NA) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = 350, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,375), breaks = c(0,100,200,300,400), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Monthly Annualized, SA") +
  ggtitle("Canada's Non-US Exports by Destination") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. BOP Basis, Seasonally Adjusted", subtitle = "Canadian Exports to Non-US Partners Have Grown Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.235,.72)) +
  scale_fill_manual(name = "Canadian Exports, Excluding US", values = c("China" = "#EE6055", "European Union" = "#3083DC", "United Kingdom" = "#FFE98F", "Japan" = "#6A4C93", "Mexico" = "#00A99D", "South Korea" = "#F5B041", "Other" = "#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*375), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_NONUS_EXPORTS_PARTNER_AREA_GRAPH, "Canada Non-US Exports by Partner Area Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_NONUS_EXPORTS_PARTNER_LINE_GRAPH <- ggplot(data = CAN_TRADE_NONUS_EXPORTS_PARTNER, aes(x = date, y = value*12, color = partner)) +
  geom_line(size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = 115, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,125), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Monthly Annualized, SA") +
  ggtitle("Canada's Non-US Exports by Destination") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. BOP Basis, Seasonally Adjusted", subtitle = "Canadian Exports to Non-US Partners Have Grown Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.235,.72)) +
  scale_color_manual(name = "Canadian Exports, Excluding US", values = c("China" = "#EE6055", "European Union" = "#3083DC", "United Kingdom" = "#FFE98F", "Japan" = "#6A4C93", "Mexico" = "#00A99D", "South Korea" = "#F5B041", "Other" = "#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*125), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_NONUS_EXPORTS_PARTNER_LINE_GRAPH, "Canada Non-US Exports by Partner Line Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_NONUS_IMPORTS_PARTNER <- CAN_TRADE_PARTNER_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Basis == "Balance of payments") %>%
  filter(`Seasonal adjustment` == "Seasonally adjusted") %>%
  filter(Trade == "Import") %>%
  filter(`Principal trading partners` %in% c("All countries","United States","European Union","China","United Kingdom","Japan","Mexico","South Korea")) %>%
  transmute(date = REF_DATE, partner = `Principal trading partners`, value = VALUE/1e3) %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  mutate(Other = `All countries`-`United States`-`European Union`-China-`United Kingdom`-Japan-Mexico-`South Korea`) %>%
  select(-`All countries`, -`United States`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_longer(-date, names_to = "partner") %>%
  mutate(partner = fct_reorder(partner, if_else(partner == "Other", -Inf, value), .fun = last, .desc = TRUE))

CAN_TRADE_NONUS_IMPORTS_PARTNER_AREA_GRAPH <- ggplot(data = CAN_TRADE_NONUS_IMPORTS_PARTNER, aes(x = date, y = value*12, fill = partner)) +
  geom_area(position = "stack", color = NA) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = 350, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,525), breaks = c(0,100,200,300,400,500), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Monthly Annualized, SA") +
  ggtitle("Canada's Non-US Imports by Source") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. BOP Basis, Seasonally Adjusted", subtitle = "Canadian Exports to Non-US Partners Have Grown Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.235,.72)) +
  scale_fill_manual(name = "Canadian Imports, Excluding US", values = c("China" = "#EE6055", "European Union" = "#3083DC", "United Kingdom" = "#FFE98F", "Japan" = "#6A4C93", "Mexico" = "#00A99D", "South Korea" = "#F5B041", "Other" = "#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*525), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_NONUS_IMPORTS_PARTNER_AREA_GRAPH, "Canada Non-US Imports by Partner Area Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_NONUS_IMPORTS_PARTNER_LINE_GRAPH <- ggplot(data = CAN_TRADE_NONUS_IMPORTS_PARTNER, aes(x = date, y = value*12, color = partner)) +
  geom_line(size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = 115, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,175), breaks = c(0,25,50,75,100,125,150,175), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Monthly Annualized, SA") +
  ggtitle("Canada's Non-US Imports by Source") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. BOP Basis, Seasonally Adjusted", subtitle = "Canadian Exports to Non-US Partners Have Grown Amidst the Trade War") +
  theme_apricitas + theme(legend.position = c(.235,.72)) +
  scale_color_manual(name = "Canadian Imports, Excluding US", values = c("China" = "#EE6055", "European Union" = "#3083DC", "United Kingdom" = "#FFE98F", "Japan" = "#6A4C93", "Mexico" = "#00A99D", "South Korea" = "#F5B041", "Other" = "#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*175), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_NONUS_IMPORTS_PARTNER_LINE_GRAPH, "Canada Non-US Imports by Partner Line Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_NONUS_SHARE <- CAN_TRADE_PARTNER_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Basis == "Balance of payments") %>%
  filter(`Seasonal adjustment` == "Seasonally adjusted") %>%
  filter(Trade %in% c("Export","Import")) %>%
  filter(`Principal trading partners` %in% c("All countries","United States")) %>%
  transmute(date = REF_DATE, value = VALUE, name = Trade, partner = `Principal trading partners`) %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  transmute(date, name, value = 1-`United States`/`All countries`) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider()

CAN_TRADE_NONUS_SHARE_GRAPH <- ggplot() +
  geom_line(data=CAN_TRADE_NONUS_SHARE, aes(x=date,y= Export, color= "Non-US Share of Canadian Exports"), size = 1.25) +
  geom_line(data=CAN_TRADE_NONUS_SHARE, aes(x=date,y= Import, color= "Non-US Share of Canadian Imports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5), breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0)) +
  ylab("Percent of Total") +
  ggtitle("Canada's Non-US Trade") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "A Growing Share of Canadian Goods Trade is With Countries Other Than the US") +
  theme_apricitas + theme(legend.position = c(.3,.9)) +
  scale_color_manual(name= "Canadian Goods Trade, Balance of Payments Basis" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Non-US Share of Canadian Exports","Non-US Share of Canadian Imports")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_TRADE_NONUS_SHARE_GRAPH, "Canada Non US Trade Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_SERVICES_CAT_BULK <- statcan_data("12-10-0157-01", "eng")

CAN_SERVICES_CAT_BULK %>% select(-REF_DATE, -VALUE) %>% distinct() %>% View() #check labels

CAN_SERVICES_NONUS <- CAN_SERVICES_CAT_BULK %>%
  filter(GEO == "Canada") %>%
  filter(`Seasonal adjustment` == "Seasonally adjusted") %>%
  filter(Trade %in% c("Receipts","Payments")) %>%
  filter(`Principal trading partners` %in% c("Total of all countries","United States")) %>%
  transmute(date = REF_DATE, value = VALUE, name = Trade, partner = `Principal trading partners`) %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  transmute(date, name, level = `Total of all countries`-`United States`, share = 1-`United States`/`Total of all countries`) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  pivot_wider(values_from = c(level,share))

CAN_SERVICES_NONUS_SHARE_GRAPH <- ggplot() +
  geom_line(data=CAN_SERVICES_NONUS, aes(x=date,y= share_Receipts, color= "Non-US Share of Canadian Services Exports"), size = 1.25) +
  geom_line(data=CAN_SERVICES_NONUS, aes(x=date,y= share_Payments, color= "Non-US Share of Canadian Services Imports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.375,.500), breaks = c(.4,.45,.5), expand = c(0,0)) +
  ylab("Percent of Total") +
  ggtitle("Canada's Non-US Services Trade") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "A Growing Share of Canadian Services Trade is With Countries Other Than the US") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= "Canadian Services Trade, Balance of Payments Basis" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Non-US Share of Canadian Services Exports","Non-US Share of Canadian Services Imports")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*.75), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_SERVICES_NONUS_SHARE_GRAPH, "Canada Non US Services Trade Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_NAPCS_BULK <- statcan_data("12-10-0175-01", "eng")

CAN_TRADE_NAPCS_BULK %>% select(where(is.character), -any_of(c("DGUID","UOM","SCALAR_FACTOR","VECTOR","COORDINATE","STATUS","SYMBOL","TERMINATED","DECIMALS"))) %>% purrr::map(unique) #check labels

CAN_TRADE_US_SHARE_ENERGY <- CAN_TRADE_NAPCS_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Trade %in% c("Domestic export","Import")) %>%
  filter(`Principal trading partners` %in% c("All countries","United States")) %>%
  filter(`North American Product Classification System (NAPCS)` %in% c("Total of all merchandise","Energy products [C12]")) %>%
  transmute(date = REF_DATE, value = VALUE, trade = Trade, partner = `Principal trading partners`, product = `North American Product Classification System (NAPCS)`) %>%
  pivot_wider(names_from = product, values_from = value) %>%
  transmute(date, trade, partner, Total = `Total of all merchandise`, ExEnergy = `Total of all merchandise`-`Energy products [C12]`) %>%
  pivot_longer(cols = c(Total,ExEnergy), names_to = "product") %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  group_by(trade, product) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(monthly = `United States`/`All countries`, roll = roll_sumr(`United States`, 12)/roll_sumr(`All countries`, 12)) %>%
  ungroup() %>%
  transmute(date, name = paste(trade, product), monthly, roll) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider(values_from = c(monthly, roll)) %>%
  drop_na()


CAN_TRADE_US_SHARE_EXPORT_ENERGY_GRAPH <- ggplot() +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `monthly_Domestic export Total`, color= "Exports"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `monthly_Domestic export ExEnergy`, color= "Exports ex-Energy"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `roll_Domestic export Total`, color= "Exports"), size = 1.25) +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `roll_Domestic export ExEnergy`, color= "Exports ex-Energy"), size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = .79, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.60,.80), breaks = c(.60,.65,.70,.75,.80), expand = c(0,0)) +
  ylab("Percent of Total") +
  ggtitle("Canada's Export Reliance on the US") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "The US Share of Canadian Goods Trade Has Fallen, Especially Outside of Energy") +
  theme_apricitas + theme(legend.position = c(.25,.2)) +
  scale_color_manual(name= "Share of Canadian Exports Going to US\nSolid = Rolling 12M, Dashed = Monthly" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Exports","Exports ex-Energy")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.25))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = .6-(.3*.2), ymax = .6) +
  coord_cartesian(clip = "off")


CAN_TRADE_US_SHARE_IMPORT_ENERGY_GRAPH <- ggplot() +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `monthly_Import Total`, color= "Imports"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `monthly_Import ExEnergy`, color= "Imports ex-Energy"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `roll_Import Total`, color= "Imports"), size = 1.25) +
  geom_line(data=CAN_TRADE_US_SHARE_ENERGY, aes(x=date,y= `roll_Import ExEnergy`, color= "Imports ex-Energy"), size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = .525, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.40,.55), breaks = c(0.4,0.45,0.5,0.55), expand = c(0,0)) +
  ylab("Percent of Total") +
  ggtitle("Canada's Import Reliance on the US") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "The US Share of Canadian Goods Trade Has Fallen, Especially Outside of Energy") +
  theme_apricitas + theme(legend.position = c(.25,.2)) +
  scale_color_manual(name= "US Share of Canadian Goods Imports\nSolid = Rolling 12M, Dashed = Monthly" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Imports","Imports ex-Energy")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.25))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = .40-(.3*.15), ymax = .40) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = CAN_TRADE_US_SHARE_IMPORT_ENERGY_GRAPH, "Canada US Import Share Energy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CAN_TRADE_US_SHARE_EXPORT_ENERGY_GRAPH, "Canada US Export Share Energy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_US_SHARE_TOTAL_ENERGY <- CAN_TRADE_NAPCS_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Trade %in% c("Domestic export","Import")) %>%
  #filter(`Principal trading partners` %in% c("All countries","United States")) %>%
  filter(`North American Product Classification System (NAPCS)` %in% c("Total of all merchandise","Energy products [C12]")) %>%
  group_by(date = REF_DATE, partner = `Principal trading partners`, product = `North American Product Classification System (NAPCS)`) %>%
  summarise(value = sum(VALUE), .groups = "drop") %>%
  pivot_wider(names_from = product, values_from = value) %>%
  transmute(date, partner, Total = `Total of all merchandise`, ExEnergy = `Total of all merchandise`-`Energy products [C12]`) %>%
  pivot_longer(cols = c(Total,ExEnergy), names_to = "product") %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  group_by(product) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(monthly = `United States`/`All countries`, roll = roll_sumr(`United States`, 12)/roll_sumr(`All countries`, 12)) %>%
  ungroup() %>%
  select(date, product, monthly, roll) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  pivot_wider(names_from = product, values_from = c(monthly, roll)) %>%
  drop_na()

CAN_TRADE_US_SHARE_TOTAL_ENERGY_GRAPH <- ggplot() +
  geom_line(data=CAN_TRADE_US_SHARE_TOTAL_ENERGY, aes(x=date,y=monthly_Total, color= "Total Trade"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data=CAN_TRADE_US_SHARE_TOTAL_ENERGY, aes(x=date,y=monthly_ExEnergy, color= "Total Trade ex-Energy"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data=CAN_TRADE_US_SHARE_TOTAL_ENERGY, aes(x=date,y=roll_Total, color= "Total Trade"), size = 1.25) +
  geom_line(data=CAN_TRADE_US_SHARE_TOTAL_ENERGY, aes(x=date,y=roll_ExEnergy, color= "Total Trade ex-Energy"), size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = .69, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.50,.70), breaks = c(.50,.55,.60,.65,.70), expand = c(0,0)) +
  ylab("Percent of Total") +
  ggtitle("Canada's Trade Reliance on the US") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Total Trade = Domestic Exports + Imports",subtitle = "The US Share of Canadian Goods Trade Has Fallen, Especially Outside of Energy") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= "US Share of Canadian Goods Trade\nSolid = Rolling 12M, Dashed = Monthly" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total Trade","Total Trade ex-Energy")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.25))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = .50-(.3*.20), ymax = .50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAN_TRADE_US_SHARE_TOTAL_ENERGY_GRAPH, "Canada US Total Trade Share Energy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_US_NAPCS_YTD <- CAN_TRADE_NAPCS_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Trade %in% c("Domestic export","Import")) %>%
  filter(`Principal trading partners` == "United States") %>%
  filter(year(REF_DATE) %in% c(2024, 2026)) %>%
  filter(month(REF_DATE) <= month(max(REF_DATE))) %>%
  group_by(trade = Trade, product = `North American Product Classification System (NAPCS)`, year = year(REF_DATE)) %>%
  summarise(value = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "Y") %>%
  mutate(change = Y2026 - Y2024, pct_change = Y2026/Y2024 - 1) %>%
  arrange(trade, change)

#top-level sections only
CAN_TRADE_US_NAPCS_YTD %>%
  filter(str_detect(product, "\\[C\\d{2}\\]") | product == "Total of all merchandise") %>%
  View()




CAN_US_EXPORT_DROP_TOP5 <- CAN_TRADE_US_NAPCS_YTD %>%
  filter(trade == "Domestic export", str_detect(product, "\\[C\\d{2}\\]")) %>%
  slice_min(change, n = 4) %>%
  mutate(product = str_remove(product, " \\[C\\d+\\]") %>% str_to_title() %>% str_replace_all(c(" And " = " & ", " Or " = " or ")),
         product = case_when(str_starts(product, "Forestry") ~ "Forestry & Paper Products", str_starts(product, "Basic") ~ "Chemical, Plastic, & Rubber Products", TRUE ~ product),
         decline = -change/1e6) %>%
  mutate(product = factor(product, levels = rev(product)))

CAN_US_IMPORT_DROP_TOP5 <- CAN_TRADE_US_NAPCS_YTD %>%
  filter(trade == "Import", str_detect(product, "\\[C\\d{2}\\]")) %>%
  slice_min(change, n = 4) %>%
  mutate(product = str_remove(product, " \\[C\\d+\\]") %>% str_to_title() %>% str_replace_all(c(" And " = " & ", " Or " = " or ")),
         product = case_when(str_starts(product, "Forestry") ~ "Forestry & Paper Products", str_starts(product, "Basic") ~ "Chemical, Plastic, & Rubber Products", TRUE ~ product),
         decline = -change/1e6) %>%
  mutate(product = factor(product, levels = rev(product)))

CAN_US_EXPORT_DROP_GRAPH <- ggplot(data = CAN_US_EXPORT_DROP_TOP5, aes(x = product, y = decline)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#EE6055") +
  geom_text(aes(label = paste0(" ", product)), position = position_stack(vjust = 0), angle = 0, hjust = 0, size = 5.25, color = "white", fontface = "bold") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0, ceiling(max(c(CAN_US_EXPORT_DROP_TOP5$decline, CAN_US_IMPORT_DROP_TOP5$decline))/5)*5), expand = c(0,0)) +
  labs(subtitle = "Canadian Exports to the US") +
  theme_apricitas + theme(legend.position = "none", plot.margin = grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"), axis.text.y = element_blank()) +
  coord_flip()

CAN_US_IMPORT_DROP_GRAPH <- ggplot(data = CAN_US_IMPORT_DROP_TOP5, aes(x = product, y = decline)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#00A99D") +
  geom_text(aes(label = paste0(" ", product)), position = position_stack(vjust = 0), angle = 0, hjust = 0, size = 5.25, color = "white", fontface = "bold") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0, ceiling(max(c(CAN_US_EXPORT_DROP_TOP5$decline, CAN_US_IMPORT_DROP_TOP5$decline))/5)*5), expand = c(0,0)) +
  labs(subtitle = "Canadian Imports from the US") +
  theme_apricitas + theme(legend.position = "none", plot.margin = grid::unit(c(0, .2, 0, .2), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"), axis.text.y = element_blank()) +
  coord_flip()

tgrob <- text_grob("Largest Declines in Canada-US Trade, 2026 vs 2024", face = "bold", size = 26, color = "white")

plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0.5,0,0.5, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin = unit(c(-0.15,-.15,-0.15,-.15),"cm"))

CAN_US_TRADE_DROP <- ggarrange(CAN_US_EXPORT_DROP_GRAPH, CAN_US_IMPORT_DROP_GRAPH, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

CAN_US_TRADE_DROP <- ggarrange(plot_0, CAN_US_TRADE_DROP, nrow = 2, heights = c(4,20), widths = 10) %>%
  annotate_figure(., bottom = text_grob(paste0("\nGraph Created by @Josephpolitano Using Statistics Canada Data\nNOTE: Jan-", month.abb[month(max(CAN_TRADE_NAPCS_BULK$REF_DATE))], " 2026 vs Same Period of 2024. Nominal CAD, NAPCS Sections. Exports Are Domestic Exports"), color = "grey55", hjust = 1, x = 1, size = 10)) + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina", plot = CAN_US_TRADE_DROP, "Canada US Trade Drop Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_UK_EXPORTS <- CAN_TRADE_NAPCS_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Trade == "Domestic export") %>%
  filter(`Principal trading partners` == "United Kingdom") %>%
  filter(str_detect(`North American Product Classification System (NAPCS)`, "\\[C12\\]|\\[C14\\]|\\[C15\\]") | `North American Product Classification System (NAPCS)` == "Total of all merchandise") %>%
  transmute(date = REF_DATE, product = case_when(str_detect(`North American Product Classification System (NAPCS)`, "\\[C14\\]") ~ "Metal & Mineral Products (Mostly Gold)", str_detect(`North American Product Classification System (NAPCS)`, "\\[C12\\]") ~ "Energy Products", str_detect(`North American Product Classification System (NAPCS)`, "\\[C15\\]") ~ "Chemical, Plastic, & Rubber Products", TRUE ~ "Total"), value = VALUE/1e6) %>%
  pivot_wider(names_from = product, values_from = value) %>%
  mutate(Other = Total - `Metal & Mineral Products (Mostly Gold)` - `Energy Products` - `Chemical, Plastic, & Rubber Products`) %>%
  select(-Total) %>%
  pivot_longer(-date) %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(monthly = value*12, roll = roll_sumr(value, 12)) %>%
  ungroup() %>%
  filter(date >= as.Date("2018-01-01"))

CAN_TRADE_UK_EXPORTS_GRAPH <- ggplot() +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Metal & Mineral Products (Mostly Gold)"), aes(x = date, y = monthly, color = "Metal & Mineral Products (Mostly Gold)"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Energy Products"), aes(x = date, y = monthly, color = "Energy Products"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Chemical, Plastic, & Rubber Products"), aes(x = date, y = monthly, color = "Chemical, Plastic, & Rubber Products"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Other"), aes(x = date, y = monthly, color = "Other"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Metal & Mineral Products (Mostly Gold)"), aes(x = date, y = roll, color = "Metal & Mineral Products (Mostly Gold)"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Energy Products"), aes(x = date, y = roll, color = "Energy Products"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Chemical, Plastic, & Rubber Products"), aes(x = date, y = roll, color = "Chemical, Plastic, & Rubber Products"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_UK_EXPORTS, name == "Other"), aes(x = date, y = roll, color = "Other"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,NA), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars") +
  ggtitle("Canadian Exports to the UK") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Domestic Exports, Not Seasonally Adjusted", subtitle = "Canadian Exports to the UK Have Surged, Driven Primarily by Gold") +
  theme_apricitas + theme(legend.position = c(.3,.75)) +
  scale_color_manual(name = "Canadian Domestic Exports to the UK\nSolid = Rolling 12M, Dashed = Monthly Annualized", values = c("Metal & Mineral Products (Mostly Gold)" = "#FFE98F", "Energy Products" = "#EE6055", "Chemical, Plastic, & Rubber Products" = "#00A99D", "Other" = "#A7ACD9"), breaks = c("Metal & Mineral Products (Mostly Gold)","Energy Products","Chemical, Plastic, & Rubber Products","Other")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.25, linetype = "solid"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*max(CAN_TRADE_UK_EXPORTS$monthly, na.rm = TRUE)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_UK_EXPORTS_GRAPH, "Canada UK Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAN_TRADE_NONUS_NAPCS_MONTHLY <- CAN_TRADE_NAPCS_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Trade == "Domestic export") %>%
  filter(`Principal trading partners` %in% c("All countries","United States")) %>%
  filter(str_detect(`North American Product Classification System (NAPCS)`, "\\[C1[1-4]\\]|\\[C2[12]\\]") | `North American Product Classification System (NAPCS)` == "Total of all merchandise") %>%
  transmute(date = REF_DATE, partner = `Principal trading partners`,
            product = recode(`North American Product Classification System (NAPCS)`,
                             "Metal and non-metallic mineral products [C14]" = "Metal & Mineral Products",
                             "Energy products [C12]" = "Energy Products",
                             "Farm, fishing and intermediate food products [C11]" = "Farm, Fishing, & Food Products",
                             "Metal ores and non-metallic minerals [C13]" = "Metal Ores & Minerals",
                             "Consumer goods [C22]" = "Consumer Goods",
                             "Aircraft and other transportation equipment and parts [C21]" = "Aircraft & Transportation Equipment",
                             "Total of all merchandise" = "Total"),
            value = VALUE) %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  transmute(date, product, value = (`All countries` - `United States`)/1e6) %>%
  pivot_wider(names_from = product, values_from = value) %>%
  mutate(Other = Total - `Metal & Mineral Products` - `Energy Products` - `Farm, Fishing, & Food Products` - `Metal Ores & Minerals` - `Consumer Goods` - `Aircraft & Transportation Equipment`) %>%
  select(-Total) %>%
  pivot_longer(-date) %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(monthly = value*12, roll = roll_sumr(value, 12)) %>%
  ungroup() %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(name = factor(name, levels = c("Metal & Mineral Products","Energy Products","Farm, Fishing, & Food Products","Metal Ores & Minerals","Consumer Goods","Aircraft & Transportation Equipment","Other")))


CAN_TRADE_NONUS_NAPCS_AREA_GRAPH <- ggplot(data = CAN_TRADE_NONUS_NAPCS_MONTHLY, aes(x = date, y = roll, fill = name)) +
  geom_area(position = "stack", color = NA) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = .9*(CAN_TRADE_NONUS_NAPCS_MONTHLY %>% group_by(date) %>% summarise(t = sum(roll)) %>% pull(t) %>% max(na.rm = TRUE)), color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,375), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars, Rolling 12M Total") +
  ggtitle("Canada's Non-US Exports by Product") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Domestic Exports", subtitle = "Canada's Non-US Export Growth Has Been Driven by Metals & Energy") +
  theme_apricitas + theme(legend.position = c(.295,.70)) +
  scale_fill_manual(name = "Canadian Domestic Exports, Excluding US\nRolling 12M Total", values = c("Metal & Mineral Products" = "#FFE98F", "Energy Products" = "#EE6055", "Farm, Fishing, & Food Products" = "#00A99D", "Metal Ores & Minerals" = "#F5B041", "Consumer Goods" = "#3083DC", "Aircraft & Transportation Equipment" = "#6A4C93", "Other" = "#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*375), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_NONUS_NAPCS_AREA_GRAPH, "Canada Non-US Exports by Product Area Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CAN_TRADE_NONUS_NAPCS_LINE_GRAPH <- ggplot() +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Other"), aes(x = date, y = monthly, color = "Other"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Aircraft & Transportation Equipment"), aes(x = date, y = monthly, color = "Aircraft & Transportation Equipment"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Consumer Goods"), aes(x = date, y = monthly, color = "Consumer Goods"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Metal Ores & Minerals"), aes(x = date, y = monthly, color = "Metal Ores & Minerals"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Farm, Fishing, & Food Products"), aes(x = date, y = monthly, color = "Farm, Fishing, & Food Products"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Energy Products"), aes(x = date, y = monthly, color = "Energy Products"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Metal & Mineral Products"), aes(x = date, y = monthly, color = "Metal & Mineral Products"), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Other"), aes(x = date, y = roll, color = "Other"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Aircraft & Transportation Equipment"), aes(x = date, y = roll, color = "Aircraft & Transportation Equipment"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Consumer Goods"), aes(x = date, y = roll, color = "Consumer Goods"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Metal Ores & Minerals"), aes(x = date, y = roll, color = "Metal Ores & Minerals"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Farm, Fishing, & Food Products"), aes(x = date, y = roll, color = "Farm, Fishing, & Food Products"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Energy Products"), aes(x = date, y = roll, color = "Energy Products"), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_NONUS_NAPCS_MONTHLY, name == "Metal & Mineral Products"), aes(x = date, y = roll, color = "Metal & Mineral Products"), size = 1.25) +
  annotate("vline", x = as.Date("2025-01-01"), xintercept = as.Date("2025-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Trump\nElection", x = as.Date("2024-12-01"), y = .9*max(CAN_TRADE_NONUS_NAPCS_MONTHLY$monthly, na.rm = TRUE), color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), limits = c(0,NA), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars") +
  ggtitle("Canada's Non-US Exports by Product") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Domestic Exports, Not Seasonally Adjusted", subtitle = "Canada's Non-US Export Growth Has Been Driven by Metals & Energy") +
  theme_apricitas + theme(legend.position = c(.325,.70)) +
  scale_color_manual(name = "Canadian Domestic Exports, Excluding US\nSolid = Rolling 12M, Dashed = Monthly Annualized", values = c("Metal & Mineral Products" = "#FFE98F", "Energy Products" = "#EE6055", "Farm, Fishing, & Food Products" = "#00A99D", "Metal Ores & Minerals" = "#F5B041", "Consumer Goods" = "#3083DC", "Aircraft & Transportation Equipment" = "#6A4C93", "Other" = "#A7ACD9"), breaks = c("Metal & Mineral Products","Energy Products","Farm, Fishing, & Food Products","Metal Ores & Minerals","Consumer Goods","Aircraft & Transportation Equipment","Other")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.25, linetype = "solid"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*max(CAN_TRADE_NONUS_NAPCS_MONTHLY$monthly, na.rm = TRUE)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_NONUS_NAPCS_LINE_GRAPH, "Canada Non-US Exports by Product Line Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAN_TRADE_CHINA_EXPORTS <- CAN_TRADE_NAPCS_BULK %>%
  filter(GEO == "Canada") %>%
  filter(Trade == "Domestic export") %>%
  filter(`Principal trading partners` == "China") %>%
  filter(str_detect(`North American Product Classification System (NAPCS)`, "\\[C\\d{2}\\]")) %>%
  transmute(date = REF_DATE, product = str_remove(`North American Product Classification System (NAPCS)`, " \\[C\\d+\\]") %>% str_to_title() %>% str_replace_all(c(" And " = " & ", " Or " = " or ")), value = VALUE/1e6) %>%
  mutate(product = case_when(str_starts(product, "Forestry") ~ "Forestry & Paper Products",
                             str_starts(product, "Basic") ~ "Chemical, Plastic, & Rubber Products",
                             str_starts(product, "Farm") ~ "Farm, Fishing, & Food Products",
                             str_starts(product, "Metal & Non") ~ "Metal & Mineral Products",
                             str_starts(product, "Metal Ores") ~ "Metal Ores & Minerals",
                             str_starts(product, "Aircraft") ~ "Aircraft & Transportation Equipment",
                             str_starts(product, "Industrial Machinery") ~ "Industrial Machinery & Equipment",
                             str_starts(product, "Electronic") ~ "Electronics & Electrical Equipment",
                             TRUE ~ product)) %>%
  group_by(product) %>%
  mutate(recent = sum(value[date > max(date) %m-% months(12)], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(recent)), name = fct_reorder(if_else(rank <= 6, product, "Other"), rank, .fun = min)) %>%
  group_by(date, name) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(monthly = value*12, roll = roll_sumr(value, 12)) %>%
  ungroup() %>%
  filter(date >= as.Date("2018-01-01"))

CAN_TRADE_CHINA_EXPORTS_GRAPH <- ggplot() +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[7]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[7]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[6]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[6]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[5]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[5]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[4]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[4]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[3]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[3]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[2]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[2]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[1]), aes(x = date, y = monthly, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[1]), size = 0.75, alpha = 0.4, linetype = "dashed") +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[7]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[7]), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[6]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[6]), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[5]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[5]), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[4]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[4]), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[3]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[3]), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[2]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[2]), size = 1.25) +
  geom_line(data = filter(CAN_TRADE_CHINA_EXPORTS, name == levels(name)[1]), aes(x = date, y = roll, color = levels(CAN_TRADE_CHINA_EXPORTS$name)[1]), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,25), expand = c(0,0)) +
  ylab("Billions of Canadian Dollars") +
  ggtitle("Canadian Exports to China") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. Domestic Exports, Not Seasonally Adjusted", subtitle = "Canadian Exports to China Have Risen to Record Highs, Driven by Energy & Metal Ores") +
  theme_apricitas + theme(legend.position = c(.33,.69)) +
  scale_color_manual(name = "Canadian Domestic Exports to China\nSolid = Rolling 12M, Dashed = Monthly Annualized", values = setNames(c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#F5B041"), levels(CAN_TRADE_CHINA_EXPORTS$name)), breaks = levels(CAN_TRADE_CHINA_EXPORTS$name)) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.25, linetype = "solid"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = CAN_TRADE_CHINA_EXPORTS_GRAPH, "Canada China Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



library(scatterpie)

CAN_PROV_GDP_EXP_BULK <- statcan_data("36-10-0222-01", "eng")

CAN_PROV_SHAPES <- ne_states(country = "Canada", returnclass = "sf") %>%
  transmute(GEO = recode(name, "Québec" = "Quebec")) %>%
  st_transform(3347)

CAN_PROV_TRADE <- CAN_TRADE_NAPCS_BULK %>%
  filter(year(REF_DATE) == 2024, GEO != "Canada") %>%
  filter(Trade %in% c("Domestic export","Import")) %>%
  filter(`North American Product Classification System (NAPCS)` == "Total of all merchandise") %>%
  filter(`Principal trading partners` %in% c("All countries","United States")) %>%
  group_by(GEO, partner = `Principal trading partners`) %>%
  summarise(value = sum(VALUE, na.rm = TRUE)/1e6, .groups = "drop") %>%
  pivot_wider(names_from = partner, values_from = value) %>%
  transmute(GEO, US = `United States`, NonUS = `All countries` - `United States`) %>%
  inner_join(CAN_PROV_GDP_EXP_BULK %>%
               filter(year(REF_DATE) == 2024, GEO != "Canada") %>%
               filter(Prices == "Current prices") %>% #guess
               filter(Estimates %in% c("Exports of goods to other provinces","Imports of goods from other provinces")) %>% #guess
               group_by(GEO) %>%
               summarise(Interprov = sum(VALUE, na.rm = TRUE)/1000, .groups = "drop"), by = "GEO") %>%
  inner_join(CAN_PROV_SHAPES, by = "GEO") %>%
  st_as_sf() %>%
  mutate(x = st_coordinates(st_point_on_surface(geometry))[,1], y = st_coordinates(st_point_on_surface(geometry))[,2]) %>%
  st_drop_geometry() %>%
  mutate(total = US + NonUS + Interprov, r = sqrt(total/max(total))*450000)

CAN_PROV_TRADE_MAP <- ggplot() +
  geom_sf(data = CAN_PROV_SHAPES, fill = "grey55", color = "black", size = 0.25) +
  geom_sf(color = NA) +
  #geom_sf(aes(fill = value), colour = "black", size = 0.1) +
  #ggtitle("Canada, 2019-2024 RGDP Growth by Province/Territory") +
  coord_sf(crs = 3347, xlim = c(2700000, 9300000), ylim = c(900000, 5000000)) +
  geom_scatterpie(data = CAN_PROV_TRADE, aes(x = x, y = y, r = r), cols = c("US","Interprov","NonUS"), color = NA) +
  geom_scatterpie_legend(CAN_PROV_TRADE$r, x = -2000000, y = 400000, n = 3, labeller = function(r) paste0("$", round((r/450000)^2*max(CAN_PROV_TRADE$total)), "B")) +
  scale_fill_manual(name = NULL, values = c("US" = "#EE6055", "Interprov" = "#FFE98F", "NonUS" = "#00A99D"), breaks = c("US","Interprov","NonUS"), labels = c("Trade with the US","Trade with Other Provinces","Trade with Other Countries")) +
  ggtitle("    Canadian Provinces' Goods Trade by Partner") +
  #labs(caption = "Graph created by @JosephPolitano using Statistics Canada data. 2024. Trade = Exports + Imports. Interprovincial Data From Provincial Economic Accounts", subtitle = "Pie Size Proportional to Total Goods Trade, Including With Other Provinces") +
  theme_apricitas +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = c(.2,.9), panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(t = 0.15, r = 0, b= 0,l = 0), "in"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 24.5))

ggsave(dpi = "retina", plot = CAN_PROV_TRADE_MAP, "Canada Provincial Trade Pie Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BOC_BLP_TRADE_ACTIONS <- tribble(
  ~action,                                                               ~`Taking this action`, ~`Planning to take this action`,
  "Changing suppliers or restructuring supply chains",                   9.7,                   10.9,
  "Reducing operations, capacity or employment",                         6.2,                   7.2,
  "Reducing investment in Canada or redirecting it abroad",              6.9,                   5.6,
  "Changing products or services offered",                               5.6,                   6.2,
  "Increasing spending in technology, artificial intelligence or automation", 5.6,              5.3,
  "Diversifying export markets",                                         6.2,                   4.0,
  "Shifting sales focus toward domestic markets",                        5.9,                   4.0,
  "Expanding operations or production capacity in Canada",               2.5,                   5.0,
  "Changing workforce skills",                                           1.9,                   4.4
) %>%
  mutate(action = str_to_title(action) %>% str_replace_all("(?<=\\s)(And|Or|To|In|Of|The|For|On|A|An)\\b", tolower)) %>%
  mutate(action = factor(str_wrap(action, 40), levels = rev(str_wrap(action, 40)))) %>%
  pivot_longer(-action) %>%
  mutate(value = value/100, name = factor(name, levels = c("Taking this action","Planning to take this action")))

BOC_BLP_TRADE_ACTIONS_GRAPH <- ggplot(data = BOC_BLP_TRADE_ACTIONS, aes(x = action, y = value, fill = name)) +
  geom_col(position = position_stack(reverse = TRUE), color = NA, width = 0.8) +
  xlab(NULL) +
  ylab("Share of Firms") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.25), breaks = seq(0,.25,.05), expand = c(0,0)) +
  ggtitle("How Canadian Firms Are Responding to Tariffs") +
  labs(caption = "Bank of Canada Business Leaders' Pulse Chart recreated by @JosephPolitano Last Observation September 30, 2026", subtitle = "Share of Firms Taking or Planning Actions in Response to Tariffs or Trade Tensions") +
  theme_apricitas + theme(legend.position = c(.75,.5), axis.text.y = element_text(size = 11, color = "white", lineheight = 0.8)) +
  scale_fill_manual(name = NULL, values = c("Taking this action" = "#00A99D", "Planning to take this action" = "#FFE98F"), labels = c("Taking This Action","Planning to Take This Action")) +
  coord_flip() +
  theme(plot.title.position = "plot", plot.title = element_text(size = 26))

ggsave(dpi = "retina", plot = BOC_BLP_TRADE_ACTIONS_GRAPH, "BOC BLP Trade Actions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
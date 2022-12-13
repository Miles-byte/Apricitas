pacman::p_load(eia,eurostat,restatapi,stringi,jsonlite,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

install_github("keberwein/blscrapeR")
library(blscrapeR)

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

EU_RU_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","RU","UA","BY","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(BY:RU))) %>%
  select(time,values) %>%
  mutate(partner = "Russia, Ukraine, and Belarus")

EU_US_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","US","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  select(partner, time, values) %>%
  mutate(partner = "United States")

EU_NO_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","NO","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values)%>%
  mutate(partner = "Norway")

EU_QA_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","QA","NG","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(QA:NG))) %>%
  select(time,values) %>%
  mutate(partner = "Qatar and Nigeria")

EU_AL_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","DZ","MA","TN","LY","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  rowwise() %>%
  mutate(values = sum(c_across(DZ:TN))) %>%
  select(time,values) %>%
  mutate(partner = "Algeria, Tunisia, Morocco, and Libya")

EU_OTHER_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                                       filters=c("EU27_2020","MIO_M3","G3000"),
                                       date_filter=">2018-01-01") %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  subset(geo == "EU27_2020") %>%
  select(partner, time, values) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  select(-TOTAL,-EUR_OTH,-BE,-BG,-CZ,-DK,-DE,-EE,-IE,-EL,-ES,-FR,-HR,-IT,-CY,-LV,-LT,-LU,-HU,-MT,-NL,-AT,-PL,-PT,-RO,-SI,-SK,-FI,-SE,-NO,-DZ,-US,-QA,-RU,-UA,-BY,-CH,-MA,-TN,-LY,-NG) %>%
  rowwise() %>%
  mutate(values = sum(c_across(AD:ZA))) %>%
  select(time,values) %>%
  mutate(partner = "Other (Including Re-Exports from UK/Turkey/etc)")

EU_STACKED_GAS_IMPORTS <- rbind(EU_OTHER_GAS_IMPORTS,EU_AL_GAS_IMPORTS,EU_QA_GAS_IMPORTS,EU_NO_GAS_IMPORTS,EU_US_GAS_IMPORTS,EU_RU_GAS_IMPORTS) %>%
  pivot_wider(names_from = partner, values_from = values) %>%
  pivot_longer(cols = c(`Other (Including Re-Exports from UK/Turkey/etc)`:`Russia, Ukraine, and Belarus`)) %>%
  mutate(name = factor(name,levels = c("Other (Including Re-Exports from UK/Turkey/etc)","United States","Qatar and Nigeria","Algeria, Tunisia, Morocco, and Libya","Norway","Russia, Ukraine, and Belarus")))

EU_STACKED_GAS_IMPORTS_graph <- ggplot(data = EU_STACKED_GAS_IMPORTS, aes(x = time, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  ylab("Cubic Meters") +
  ggtitle("EU-27 Natural Gas Imports") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40), limits = c(0,47.5), expand = c(0,0)) +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data", subtitle = "Imports Through Russia are Down Significantly, But the EU is Making Up the Difference") +
  theme_apricitas + theme(legend.position = c(.325,.85)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Russia, Ukraine, and Belarus","Norway","Algeria, Tunisia, Morocco, and Libya","Qatar and Nigeria","United States","Other (Including Re-Exports from UK/Turkey/etc)")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*47.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_STACKED_GAS_IMPORTS_graph, "EU Stacked Gas Imports.png", type = "cairo-png") #cairo gets rid of anti aliasing

#crude, gas, and diesel prices
WTIEIA <- eia_series("PET.RWTC.D", start = 2019, end = 2022)
WTIEIA <- as.data.frame(WTIEIA$data) %>% mutate(product = "Crude")
GASEIA <- eia_series("PET.EER_EPMRU_PF4_RGC_DPG.D", start = "2019", end = today())
GASEIA <- as.data.frame(GASEIA$data) %>% mutate(product = "Gasoline")
DIESELEIA <- eia_series("PET.EER_EPD2DXL0_PF4_RGC_DPG.D", start = "2019", end = today())
DIESELEIA <- as.data.frame(DIESELEIA$data) %>% mutate(product = "Diesel")
DIESELEIA <- subset(DIESELEIA, date != as.Date("2022-02-21")) #random date has a 0 here, likely due to some error in EIA
KEROSENEEIA <- eia_series("PET.EER_EPJK_PF4_RGC_DPG.D", start = "2019", end = today())
KEROSENEEIA <- as.data.frame(KEROSENEEIA$data)  %>% mutate(product = "Kerosene_Jet")

SPREADS_Graph <- ggplot() + #plotting Gas Prices
  geom_line(data=drop_na(WTIEIA), aes(x=date,y= value, color= "Crude Oil (WTI)"), size = 1.25) +
  geom_line(data=drop_na(DIESELEIA), aes(x=date,y= value*42, color= "Diesel"), size = 1.25) +
  geom_line(data=drop_na(KEROSENEEIA), aes(x=date,y= value*42, color= "Kerosene Type Jet Fuel"), size = 1.25) +
  geom_line(data=drop_na(GASEIA), aes(x=date,y= value*42, color= "Gas (Regular)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,225), expand = c(0,0)) +
  ylab("There and Back Again") +
  ggtitle("Dawn of the Spread") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Refinery Spreads are Easing But Still High-Especially for Diesel and Jet Fuel") +
  theme_apricitas + theme(legend.position = c(.6,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Crude Oil (WTI)","Gas (Regular)","Diesel","Kerosene Type Jet Fuel")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1400), xmax = as.Date("2019-01-01")-(0.049*1400), ymin = 0-(.3*225), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#HYOAS Rbind
HYOAS <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  select(date,value) %>%
  drop_na()#5 year nominal interest rates
HY <- fredr(series_id = "BAMLH0A0HYM2EY",observation_start = as.Date("2010-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  select(date,value) %>%
  drop_na() #effective yield
BORROW_YIELD <- merge(HYOAS,HY, by = "date") %>%
  mutate(value = value.y-value.x) %>%
  select(-value.x,-value.y) %>%
  mutate(type = "Approximate Risk-Free Average Borrowing Rate")

HYOAS <- HYOAS %>%
  mutate(type = "Option-Adjusted High Yield Credit Spread")

HYOAS_rbind <- rbind(BORROW_YIELD,HYOAS) %>%
  pivot_wider(names_from = "type") %>%
  drop_na() %>%
  pivot_longer(cols = `Approximate Risk-Free Average Borrowing Rate`:`Option-Adjusted High Yield Credit Spread`)

HY <- HY %>%
  mutate(name = "Approximate Average High Yield Borrowing Rate")

HYOAS_RBIND_Graph <- ggplot(HYOAS_rbind, aes(fill=name, x=date, y=value/100)) + 
  geom_area(position=position_stack(reverse = TRUE), stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  geom_line(data = HY, aes(x=date, y = value/100, color = "Approximate Average High Yield Borrowing Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.13), breaks = c(0,0.02,0.04,0.06,0.08,0.10,.12), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Breaking Down Rising Rates") +
  labs(caption = "Graph created by @JosephPolitano using ICE-BofA data",subtitle = "The Relationship Between Risk-Free Rates and Financial Conditions is Not Perfectly Stable") +
  theme_apricitas + theme(legend.position = c(.35,.88), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Option-Adjusted High Yield Credit Spread","Approximate Risk-Free Average Borrowing Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HYOAS_RBIND_Graph, "HYOAS Rbind.png", type = "cairo-png") #cairo gets rid of anti aliasing

#assemblies
Assemblies <- fredr(series_id = "MVATOTASSS",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL) %>%
  mutate(Shortfall = 10.91-value) %>%
  mutate(Cumulative_Shortfall = (cumsum(Shortfall)+4.5665)/12)

Assemblies_Graph <- ggplot() + #plotting auto assemblies
  geom_line(data=Assemblies, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies"), size = 1.25)+ 
  annotate(geom = "hline", y = 10.91, yintercept = 10.91, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "2019 Average", x = as.Date("2021-07-01"), y = 11.5, color ="#FFE98F") +
  #geom_line(data=AssembliesNSA, aes(x=date,y= value, color = "Total Motor Vehicle Assemblies (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Motor Vehicle Assemblies, Millions, Annual Rate") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,14), breaks = c(0,4,8,12), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2021-8-01"))) +
  ggtitle("Fixing the Assembly Line") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "Motor Vehicle Assemblies Have Almost Climbed Back to Their Pre-Pandemic Average") +
  theme_apricitas + theme(legend.position = c(0.25,0.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Assemblies_Graph, "Assemblies Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
#out of work due to illness
OwnIllnessNoWork <- bls_api("LNU02006735", startyear = 2018, endyear = 2022, Sys.getenv("BLS_KEY"))%>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

OwnIllnessPartTime <- bls_api("LNU02028296", startyear = 2018, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

OwnIllness_Graph <- ggplot() + #plotting the number of people out due to illnesses
  geom_line(data=OwnIllnessNoWork, aes(x=date,y= value/1000,color= "Employed But Not At Work, Own Illness"), size = 1.25)+ 
  geom_line(data=OwnIllnessPartTime, aes(x=date,y= value/1000,color= "Work Part-time, Usually Work Full Time, Own Illness"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), limits = c(0,4.5), expand = c(0,0)) +
  ggtitle("Omicron and the Labor Market") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Millions Were Out Sick or Had Reduced Hours Due to Omicron") +
  theme_apricitas + theme(legend.position = c(.42,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*4.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = OwnIllness_Graph, "Own Illness Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

#Housing starts single family
HOUSING_STARTS_SFH <- fredr(series_id = "HOUST1F", observation_start = as.Date("2000-01-01")) #Single Family Home
HOUSING_COMPS_SFH <- fredr(series_id = "COMPU1USA", observation_start = as.Date("2000-01-01")) #Single Family Home

SFH_STARTS_COMPS_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=HOUSING_COMPS_SFH, aes(x=date,y= value/1000, color= "Single-Family Housing Completions"), size = 1.25) +
  geom_line(data=HOUSING_STARTS_SFH, aes(x=date,y= value/1000, color= "Single-Family Housing Starts"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Demand Destruction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Single Family Housing Starts Dropped Nearly 30% as Mortgage Rates Rose") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Single-Family Housing Starts","Single-Family Housing Completions")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SFH_STARTS_COMPS_Graph, "SFH Starts and Comps.png", type = "cairo-png") #cairo gets rid of anti aliasing

DSPI <- fredr(series_id = "DSPI",observation_start = as.Date("2018-01-01")) #downloading Disposable Personal Income data
POUT <- fredr(series_id = "A068RC1",observation_start = as.Date("2018-01-01")) #downloading Personal Outlays
DSPITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(DSPI$date, n=1), "months")), trend = 16622.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(DSPI$date, n=1), by = 'month')) - 1))) #trend variable is just compounding income/outlays monthly at a 4% annual rate 
POUTTrend <- data.frame(date = c(seq(as.Date("2020-01-01"), tail(POUT$date, n=1), "months")), trend = 15328.8*1.003274^(0:(length(seq(from = as.Date("2020-01-01"), to = tail(POUT$date, n=1), by = 'month')) - 1)))

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Personal Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Personal Income is on Trend, But Consumers Are Spending Down Their Excess Savings") +
  theme_apricitas + theme(legend.position = c(.32,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75)))) +
  scale_fill_manual(name = NULL, values = c("#EE6055","#A7ACD9")) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

DSPImerge <- merge(DSPI, DSPITrend, by = "date")
POUTmerge <- merge(POUT, POUTTrend, by = "date")
CUMSUMDSPImerge <- DSPImerge
CUMSUMPOUTmerge <- POUTmerge
CUMSUMDSPImerge$value <- cumsum(CUMSUMDSPImerge$value/12)
CUMSUMDSPImerge$trend <- cumsum(CUMSUMDSPImerge$trend/12)
CUMSUMPOUTmerge$value <- cumsum(CUMSUMPOUTmerge$value/12)
CUMSUMPOUTmerge$trend <- cumsum(CUMSUMPOUTmerge$trend/12)


CUMSUMDSPImerge$total <- (CUMSUMDSPImerge$value-CUMSUMDSPImerge$trend)-(CUMSUMPOUTmerge$value-CUMSUMPOUTmerge$trend) #graphing total excess savings

Total_Excess_Savings_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = CUMSUMDSPImerge, aes(x = date, y = total/1000, color = "Estimated 'Excess' Savings"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(0,2.75), breaks = c(0.5,1,1.5,2,2.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("Breaking the (Piggy) Bank") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Excess Savings are down Nearly $700 Billion as Americans Spend More Money") +
  theme_apricitas + theme(legend.position = c(.20,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = 0-(.3*2.75), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Total_Excess_Savings_Graph, "Excess Savings.png", type = "cairo-png") #cairo gets rid of anti aliasing

#ICECCC Corporate
ICECCCCORPORATE <- fredr(series_id = "BAMLH0A0HYM2",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
ICECCCCORPORATE <- drop_na(ICECCCCORPORATE)


ICECCCCORPORATE_Graph <- ggplot() + #plotting ICE CCC Corporate Index
  annotate("hline", y = 0.089, yintercept = 0.089, color = "#EE6055", size = 1.25, linetype = "dashed") +
  annotate(geom = "text", label = "2016/2012 Cycle Peak", x = as.Date("2019-01-01"), y = 0.095, color ="#EE6055", size = 5) +
  geom_line(data=ICECCCCORPORATE, aes(x=date,y= value/100,color= "ICE BofA US High Yield Index Option-Adjusted Spread"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.13), breaks = c(0,0.05,0.1), expand = c(0,0)) +
  ylab("Spread, %") +
  ggtitle("Tightening Up") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "High Yield Credit Spreads Remain Elevated as the Federal Reserve Tightens Policy") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ICECCCCORPORATE_Graph, "HYOAS.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

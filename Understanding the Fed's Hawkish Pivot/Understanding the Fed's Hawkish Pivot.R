pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

CPI <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2000-01-01"), units = "pc1")

TWOYR <- fredr(series_id = "DGS2",observation_start = as.Date("2019-11-04")) #downloading 2yr yields
TWOYR <- drop_na(TWOYR)

CPIRent <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"), units = "pc1")
CPIServicesLessRent <- fredr(series_id = "CUSR0000SASL2RS",observation_start = as.Date("2019-01-01"), units = "pc1")

ECIWAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2019-01-01"), units = "pc1")

PPIINTUNP <- fredr(series_id = "WPSID61",observation_start = as.Date("2019-01-01"))
PPIINTPRO <- fredr(series_id = "WPSID62",observation_start = as.Date("2019-01-01"))
PPIFINAL <- fredr(series_id = "WPSFD49207",observation_start = as.Date("2019-01-01"))

GDP <- fredr(series_id = "GDP",observation_start = as.Date("2018-01-01")) #downloading GDP goods
ECI <- fredr(series_id = "ECIWAG",observation_start = as.Date("2018-01-01")) #downloading "Employment Cost Index - Wages and Salaries for Private Industry workers" data from Fred to calculate Gross Labor Income using a second method
EPOP <- fredr(series_id = "LNS12300060",observation_start = as.Date("2018-01-01"))#downloading "Employment Population Ratio - 25-54 Yrs" data from Fred to calculate Gross Labor Income using a second method
GLI_CPS_NCS <- merge(ECI,EPOP, by = "date") #merging ECI and EPOP data for the GLI calculation method
GLI_CPS_NCS <- subset(GLI_CPS_NCS, select = c("date","value.x","value.y")) #cleaning up data frame
colnames(GLI_CPS_NCS) <- c("date","ECI","EPOP") #renaming columns for ease of use

GLITrend <- data.frame(date = c(seq(as.Date("2020-01-01"), as.Date("2022-04-01"), "months")), trend = 100*1.003274^(0:27)) #trend variable is just compounding income/outlays monthly at a 4% annual rate 

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CPI_Graph <- ggplot() + 
  geom_line(data = CPI, aes(x = date, y = value/100, color = "Consumer Price Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.02,.08), breaks = c(-.02,0,.02,.04,.06,.08), expand = c(0,0)) +
  ylab("Change From Year Ago, %") +
  ggtitle("The Fed's Pivot") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is at the Highest Levels in Decades") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7800), xmax = as.Date("2000-01-01")-(0.049*7800), ymin = -.02-(.3*.1), ymax = -.02) +
  coord_cartesian(clip = "off")

TWOYR_Graph <- ggplot() + 
  geom_line(data = TWOYR, aes(x = date, y = value/100, color = "Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.04), breaks = c(0,.01,.02,.03,.04), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("The Fed's Pivot") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Yield on 2YR Treasury Notes Has Rapidly Exceeded Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-11-04")-(.1861*(today()-as.Date("2019-11-04"))), xmax = as.Date("2019-11-04")-(0.049*(today()-as.Date("2019-11-04"))), ymin = 0-(.3*0.04), ymax = 0) +
  coord_cartesian(clip = "off")

Rent_LessRent_Graph <- ggplot() + 
  geom_line(data = CPIRent, aes(x = date, y = value/100, color = "CPI: Rent of Primary Residences"), size = 1.25) +
  geom_line(data = CPIServicesLessRent, aes(x = date, y = value/100, color = "CPI: Services Less Rent of Shelter"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,0.05), expand = c(0,0)) +
  ylab("Change from Year Ago, %") +
  ggtitle("The Fed's Pivot") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is Becoming More Broad Based as Prices for Rent and Other Services Jump") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

Wage_Graph <- ggplot() + 
  geom_line(data = ECIWAG, aes(x = date, y = value/100, color = "Employment Cost Index: Wages and Salaries: Private Industry Workers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,0.05), expand = c(0,0)) +
  ylab("Change from Year Ago, %") +
  ggtitle("Wages and Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Fed is Coy About it, but they are Likely Worried by the Pace of Wage Growth") +
  theme_apricitas + theme(legend.position = c(.50,.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-02-01")-(.1861*1100), xmax = as.Date("2019-01-15")-(0.049*1100), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

PPI_Graph <- ggplot() + 
  geom_line(data = PPIINTUNP, aes(x = date, y = value/1.969, color = "PPI: Processed Goods for Intermediate Demand"), size = 1.25) +
  geom_line(data = PPIINTPRO, aes(x = date, y = value/1.862, color = "PPI: Unprocessed Goods for Intermediate Demand"), size = 1.25) +
  geom_line(data = PPIFINAL, aes(x = date, y = value/2.079, color = "PPI: Finished Goods for Final Demand"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(70,150), breaks = c(80,100,120,140), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Producer Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Given the Rise in Materials Costs, Prices for Final Goods May Have Room to Rise") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = 70-(.3*80), ymax = 70) +
  coord_cartesian(clip = "off")

GLI_GDP_Graph <- ggplot() +
  geom_line(data = GLI_CPS_NCS, aes(x=date + 90, y = ECI*EPOP/111.5367, color = "Nominal Gross Labor Income: ECI Method"), size = 1.25) + 
  geom_line(data = GDP, aes(x=date + 90, y = value/216.9446, color = "Nominal Gross Domestic Product"), size = 1.25) + 
  geom_line(data = GLITrend, aes(x=date, y = trend, color = "4% Pre-Pandemic Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(limits = c(85,115), breaks = c(85,90,95,100,105,110,115), expand = c(0,0)) +
  ylab("Index, January 2020 = 100") +
  ggtitle("Nominal Trends") +
  labs(caption = "Graph created by @JosephPolitano using BEA, BLS, and Census data",subtitle = "NGDP is Above Trend, and GLI is Likely to Go Above Trend Soon") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#A7ACD9","#9A348E","#EE6055"),breaks = c("Nominal Gross Domestic Product","Nominal Gross Labor Income: ECI Method","4% Pre-Pandemic Growth Trend"),guide=guide_legend(override.aes=list(linetype=c(1,1,2), lwd = c(1.25,1.25,.75)))) +
  theme(legend.key.width =  unit(.82, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-04-01")-(.1861*1400), xmax = as.Date("2018-04-01")-(0.049*1400), ymin = 85-(.3*30), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TWOYR_Graph, "Two Year Treasury Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Rent_LessRent_Graph, "Rent and Services Less Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Wage_Graph, "Wage Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = PPI_Graph, "PPI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = CPI_Graph, "CPI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GLI_GDP_Graph, "GLI GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
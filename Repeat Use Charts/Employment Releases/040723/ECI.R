pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)


ECI_WAG <- bls_api("CIS2020000000000I", startyear = 2006, endyear = 2023, calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  mutate(qoqpctann = ((1+(value-dplyr::lead(value, 1))/dplyr::lead(value, 1))^4)-1) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2006-01-01"), as.Date("2023-01-01"), by = "quarter")))

ECI_WAG_EX_INC <- bls_api("CIU2020000000710I", startyear = 2006, endyear = 2023, calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 4))/dplyr::lead(value, 4)) %>%
  mutate(qoqpctann = ((1+(value-dplyr::lead(value, 1))/dplyr::lead(value, 1))^4)-1) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2006-01-01"), as.Date("2023-01-01"), by = "quarter")))

ECI_WAG_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=ECI_WAG, aes(x=date,y= qoqpctann ,color= "Quarter-on-Quarter Percent Growth, Annualized"), size = 1.25) +
  geom_line(data=ECI_WAG, aes(x=date,y= annualpct ,color= "Annualized Percent Growth"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07), breaks = c(0,0.03,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Core Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "ECI, Private Industry Wages and Salaries Growth Continues to Cool") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "ECI Private Sector Wages and Salaries",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

ECI_WAG_Ex_Inc_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=ECI_WAG_EX_INC, aes(x=date,y= annualpct ,color= "Annual Percent Growth"), size = 1.25) +
  geom_line(data=ECI_WAG_EX_INC, aes(x=date,y= qoqpctann ,color= "Quarter-on-Quarter Percent Growth, Annualized (NSA)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07), breaks = c(0,0.03,0.06), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("Core Wage Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "ECI, Private Industry Wages and Salaries Growth Was in Line With Expectations") +
  theme_apricitas + theme(legend.position = c(.50,.80)) +
  scale_color_manual(name= "ECI Private Sector Wages and Salaries Excluding Incentive Paid",values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*0.07), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ECI_WAG_Ex_Inc_Graph, "ECI Core Wage Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ECI_WAG_Graph, "ECI Wage Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GLI_BLS_YOY <- bls_api("CES0500000017", startyear = 2017, endyear = 2023, calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(annualpct = (value-dplyr::lead(value, 12))/dplyr::lead(value, 12)) %>%
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2017-01-01"), length = nrow(.), by = "month"))) %>%
  select(-latest) %>%
  drop_na()

#GLI_BLS_YOY <- fredr(series_id = "CES0500000017",observation_start = as.Date("2018-01-01"), units = "pc1")
GLI_BEA_YOY <- fredr(series_id = "A132RC1",observation_start = as.Date("2018-01-01"), units = "pc1")
#GLI_EPOP_YOY <- fredr(series_id = "LNS12300060",observation_start = as.Date("2017-01-01"), aggregation_method = "avg", frequency = "q") %>%
  #merge(.,ECI_WAG,by = "date") %>%
  #mutate(value = value.x*value.y) %>%
  #select(date, value) %>%
  #mutate(value = (value-lag(value,4))/lag(value,4)) %>%
  #drop_na()

GLI_EPOP_YOY <- bls_api("LNS12000060", startyear = 2017, endyear = 2023, calculations = TRUE, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  .[nrow(.):1,] %>%
  mutate(date =(seq(as.Date("2017-01-01"), length = nrow(.), by = "month"))) %>%
  select(-latest) %>%
  mutate(quarters = as.yearqtr(date)) %>%
  group_by(quarters) %>%
  mutate(value = AVERAGE(value)) %>%
  merge(.,ECI_WAG,by = "date") %>%
  mutate(value = value.x*value.y) %>%
  select(date, value) %>%
  mutate(value = (value-lag(value,4))/lag(value,4)) %>%
  drop_na()

GLI_GROWTH_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=GLI_BLS_YOY, aes(x=date,y= annualpct,color= "Non-Farm Payrolls Data"), size = 1.25) +
  geom_line(data=GLI_BEA_YOY, aes(x=date,y= value/100,color= "BEA Data"), size = 1.25) +
  geom_line(data=GLI_EPOP_YOY, aes(x=date,y= value,color= "ECI * Prime Age Employment"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  annotate("hline", y = 0.05, yintercept = 0.05, color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "5% Pre-COVID Normal Growth Rate", x = as.Date("2022-05-01"), y =0.042, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.10,0.18), breaks = c(-.1,-0.05,0,0.05,.1,.15), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("US Gross Labor Income Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data",subtitle = "Gross Labor Income Growth is Cooling, with BEA Numbers Returning to Normal") +
  theme_apricitas + theme(legend.position = c(.33,.75)) +
  scale_color_manual(name= "Private-Sector Gross Labor Income Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Non-Farm Payrolls Data","BEA Data","ECI * Prime Age Employment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.10-(.3*0.28), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_GROWTH_graph, "GLI Growth graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


GLI_BEA_YOY_NORM <- fredr(series_id = "A132RC1",observation_start = as.Date("2017-01-01")) %>%
  subset(date <= as.Date("2020-01-01")) %>%
  mutate(norm = (value[nrow(.)]/value[1])^(1/3)-1)
  
GLI_BLS_YOY_NORM <- fredr(series_id = "CES0500000017",observation_start = as.Date("2017-01-01")) %>%
  subset(date <= as.Date("2020-01-01")) %>%
  mutate(norm = (value[nrow(.)]/value[1])^(1/3)-1)

GLI_EPOP_YOY_NORM <- fredr(series_id = "LNS12000060",observation_start = as.Date("2017-01-01"), frequency = "q", aggregation_method = "avg") %>%
  subset(date <= as.Date("2020-01-01")) %>%
  merge(.,fredr(series_id = "ECIWAG",observation_start = as.Date("2010-01-01")),by = "date") %>%
  transmute(date = date, value = value.x*value.y) %>%
  mutate(norm = (value[nrow(.)]/value[1])^(1/((nrow(.)-1)/4))-1)

GLI_GROWTH_NORM_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=GLI_BLS_YOY, aes(x=date,y= annualpct-GLI_BLS_YOY_NORM$norm,color= "Non-Farm Payrolls Data"), size = 1.25) +
  geom_line(data=GLI_BEA_YOY, aes(x=date,y= value/100-GLI_BEA_YOY_NORM$norm,color= "BEA Data"), size = 1.25) +
  geom_line(data=GLI_EPOP_YOY, aes(x=date,y= value-GLI_EPOP_YOY_NORM$norm,color= "ECI * Prime Age Employment"), size = 1.25) +
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  annotate("text",label = "Deviation from 2017-2020", x = as.Date("2023-01-01"), y =-0.011, color = "white", size = 4) +
  annotate("text",label = "Avg Annual Growth Rate", x = as.Date("2023-01-01"), y =-0.023, color = "white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.14,0.125), breaks = c(-.1,-0.05,0,0.05,.1,.15), expand = c(0,0)) +
  ylab("Percent Growth, Year-on-Year") +
  ggtitle("US Gross Labor Income Growth Deviation") +
  labs(caption = "Graph created by @JosephPolitano using BLS and BEA Data",subtitle = "Gross Labor Income Growth is Cooling, with BEA Numbers Returning to Normal") +
  theme_apricitas + theme(legend.position = c(.33,.75)) +
  scale_color_manual(name= "Private-Sector Gross Labor Income Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Non-Farm Payrolls Data","BEA Data","ECI * Prime Age Employment")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.14-(.3*0.265), ymax = -.14) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLI_GROWTH_NORM_graph, "GLI Growth Norm graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EPOP_1990 <- bls_api("LNS12300060", startyear = 1990) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_2000 <- bls_api("LNS12300060", startyear = 2000) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_2010 <- bls_api("LNS12300060", startyear = 2010) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_2020 <- bls_api("LNS12300060", startyear = 2020) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPop <- rbind(EPOP_1990,EPOP_2000,EPOP_2010,EPOP_2020)

EPop_Graph <- ggplot() + #plotting Emplyoment-population ratio
  geom_line(data=EPop, aes(x=date,y= value/100,color= "Prime Age (25-54) Employment-Population Ratio"), size = 1.25)+ 
  xlab("Date") +
  ylab("Prime Age (25-54) Employment-Population Ratio, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Still Below Full Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.68,.88)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotate(geom = "hline", y = 0.819, yintercept = .819, color = "#FFE98F", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "Lowest Possible Estimate of 'Full Employment'", x = as.Date("2000-06-01"), y = 0.825, color ="#FFE98F", size = 5) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*11535), xmax = as.Date("1990-01-01")-(0.049*11535), ymin = 0.69-(.3*0.14), ymax = 0.69) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPop_Graph, "EPopUSA.png", type = "cairo-png") #cairo gets rid of anti aliasing


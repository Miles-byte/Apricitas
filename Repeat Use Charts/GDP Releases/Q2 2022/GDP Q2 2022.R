pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)


theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

RGDP <- fredr(series_id = "GDPC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP
RGDI <- fredr(series_id = "A261RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDI

PAYEMS <- fredr(series_id = "PAYEMS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Nonfarm Payrolls
ELEV <- fredr(series_id = "CE16OV",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Employment Levels
CPSADJ <- bls_api("LNS16000000", startyear = 2019) %>% #headline cpiadj
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

RFSALEDOMPRIV <- fredr(series_id = "LB0000031Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

RFSALEDOMPRIV_PCT <- fredr(series_id = "LB0000031Q020SBEA",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL, units = "cca")

REAL_PERSONAL_INCOME_LESS_TRANSFERS <- fredr(series_id = "W875RX1",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL)

RealPrivateInventories <- fredr(series_id = "A371RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) 

RGDPQuarterly <- fredr(series_id = "A191RL1Q225SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
PCEContribQuarterly <- fredr(series_id = "DPCERY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
IVSTContribQuarterly <- fredr(series_id = "A006RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
NEXContribQuarterly <- fredr(series_id = "A019RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 
GOVContribQuarterly <- fredr(series_id = "A822RY2Q224SBEA",observation_start = as.Date("2020-10-01"),realtime_start = NULL, realtime_end = NULL) 

ContribQuarterlyBind <- rbind(PCEContribQuarterly,IVSTContribQuarterly,NEXContribQuarterly,GOVContribQuarterly)

ContribQuarterlyBind$series_id <- gsub("DPCERY2Q224SBEA", "Consumption", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A006RY2Q224SBEA", "Investment", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A019RY2Q224SBEA", "Net Exports", ContribQuarterlyBind$series_id)
ContribQuarterlyBind$series_id <- gsub("A822RY2Q224SBEA", "Government", ContribQuarterlyBind$series_id)

LaborProductivity <- fredr(series_id = "OPHNFB",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pca") #labor productivity

TradeDeficit <- fredr(series_id = "BOPGTB",observation_start = as.Date("2000-01-01"),realtime_start = NULL, realtime_end = NULL) #trade deficit

Port_Throughput <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/021022/PortThroughput.csv")
Port_Throughput$Date <- as.Date(Port_Throughput$Date, "%m/%d/%Y")

REAL_GAS <- fredr(series_id = "DGOERX1Q020SBEA",observation_start = as.Date("2002-01-01"),realtime_start = NULL, realtime_end = NULL)

#Industrial Production
INDPRO <- fredr(series_id = "INDPRO",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
IPMAN <- fredr(series_id = "IPMAN",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

#Fixed investment
FIXED_RESIDENTIAL <- fredr(series_id = "PRFIC1",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)
FIXED_INDUSTRIAL <- fredr(series_id = "A680RX1Q020SBEA",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)


#Graphing GDP
RGDP_Graph <- ggplot() +
  geom_line(data = RGDP, aes(x=date, y = value/1000, color = "Real GDP"), size = 1.25) + 
  geom_line(data = RGDI, aes(x=date, y = value/1000, color = "Real GDI"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(17,21), breaks = c(17,18,19,20,21), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "'Equivalent' Official Measures of Aggregate Output Are Diverging") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D"), breaks = c("Real GDP","Real GDI")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 17-(.3*4), ymax = 17) +
  coord_cartesian(clip = "off")

REAL_GAS_Graph <- ggplot() +
  geom_line(data = REAL_GAS, aes(x=date, y = value, color = "Real Personal Consumption Expenditures: Gasoline And Other Energy Goods"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(340,480), breaks = c(350,400,450), expand = c(0,0)) +
  ylab("Billions of 2012 US Dollars") +
  ggtitle("Demand Destruction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Gasoline Consumption Has Fallen Below Pre-COVID Lows") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 340-(.3*140), ymax = 340) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Employment_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = PAYEMS, aes(x=date, y = value/1521.28, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = ELEV, aes(x=date, y = value/1586.53, color = "Employment Level (Household Survey)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(82,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 82-(.3*23), ymax = 82) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Employment_Graph <- ggplot() + #CPS with NFP adjusted concepts
  geom_line(data = PAYEMS, aes(x=date, y = value/1000, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = CPSADJ, aes(x=date, y = value/1000, color = "Employment Ajusted to Nonfarm Payrolls Concepts (Household Survey)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(120,160), breaks = c(120,130,140,150,160), expand = c(0,0)) +
  ylab("Payrolls/Employees, Millions") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 120-(.3*40), ymax = 120) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Final Sales to Domestic Final Purchasers
RFSALEDOMPRIV_Graph <- ggplot() + 
  geom_line(data = RFSALEDOMPRIV, aes(x=date, y = value/1000, color = "Real Final Sales to Private Domestic Purchasers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(14.5,18), breaks = c(15,16,17,18), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Growth In Real Final Private Domestic Consumption and Investment was Robust") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 14.5-(.3*3.5), ymax = 14.5) +
  coord_cartesian(clip = "off")

#Real Final Sales to Domestic Final Purchasers Percent Growth
RFSALEDOMPRIV_PCT_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data = RFSALEDOMPRIV_PCT, aes(x=date, y = value/100, color = "Real Final Sales to Private Domestic Purchasers"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.40,0.40), breaks = c(-0.4,-.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  ylab("Continuously Compounded Annual Change") +
  ggtitle("Is the US Economy Shrinking?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Growth In Real Final Private Domestic Consumption and Investment was 0 in Q2 2022") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = -0.40-(.3*0.80), ymax = -0.40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Personal Income Less Transfers
REAL_PERSONAL_INCOME_LESS_TRANSFERS_Graph <- ggplot() + 
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  geom_line(data = REAL_PERSONAL_INCOME_LESS_TRANSFERS, aes(x=date, y = value/1000, color = "Real Personal Income Excluding Current Transfer Receipts"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(9,15), breaks = c(9,10,11,12,13,14,15), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Personal Income is Stalling-But Not Yet Shrinking") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 9-(.3*6), ymax = 9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

#Real Private Inventories
RealPrivateInventories_Graph <- ggplot() + 
  geom_line(data = RealPrivateInventories, aes(x=date, y = value/1000, color = "Real Private Inventories"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = .05),limits = c(2.75,2.95), breaks = c(2.75,2.8,2.85,2.9,2.95), expand = c(0,0)) +
  ylab("Trillions of 2012 US Dollars") +
  ggtitle("Taking Inventory") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Inventory Growth Was High-But Lower Than Last Quarter") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 2.75-(.3*.2), ymax = 2.75) +
  coord_cartesian(clip = "off")

GDPMonthlyContrib_Graph <- ggplot(ContribQuarterlyBind, aes(fill=series_id, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = RGDPQuarterly, aes(x=date, y = value/100), size = 3, fill ="black", color = "black", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.04,0.09), breaks = c(-0.04,-.02,0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("What's Got You Down?") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "A Drop In Consumption Growth and Negative Investment/Inventories Pulled GDP Down") +
  theme_apricitas + theme(legend.position = c(.92,.85)) +
  scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-01")-(.1861*(today()-as.Date("2020-09-01"))), xmax = as.Date("2020-09-01")-(0.049*(today()-as.Date("2020-09-01"))), ymin = -0.04-(.3*.13), ymax = -0.04) +
  coord_cartesian(clip = "off")

LaborProductivity_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = LaborProductivity, aes(x=date, y = value/100, color = "Nonfarm Business Sector: Labor Productivity for All Employed Persons"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.09,0.11), breaks = c(-0.05,0,0.05,.1), expand = c(0,0)) +
  ylab("Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Do You Buy It?") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Drop in Output and Increase in Employment Caused Productivity to `Fall` Dramatically") +
  theme_apricitas + theme(legend.position = c(.45,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7000), xmax = as.Date("2000-01-01")-(0.049*7000), ymin = -0.09-(.3*.20), ymax = -0.09) +
  coord_cartesian(clip = "off")

TradeDeficit_Graph <- ggplot() + 
  geom_line(data = TradeDeficit, aes(x=date, y = -value/1000, color = "US Goods Trade Deficit, Balance of Payments Basis"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,140), breaks = c(0,25,50,75,100,125), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Trading Up") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "The US Trade Deficit Hit A Record High in March, but Then Pulled Back") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*140), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

Port_Throughput_Graph <- ggplot() + 
  geom_line(data = Port_Throughput, aes(x = Date, y = LA/1000, color = "Los Angeles"), size = 1.25) +
  geom_line(data = Port_Throughput, aes(x = Date, y = LB/1000, color = "Long Beach"), size = 1.25) +
  geom_line(data = Port_Throughput, aes(x = Date, y = NYNJ/1000, color = "New York/New Jersey"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"),limits = c(0,610), breaks = c(0,200,400,600), expand = c(0,0)) +
  ylab("Loaded Imports, TEUs") +
  ggtitle("A Crisis of Abundance") +
  labs(caption = "Graph created by @JosephPolitano using LA,LB,and NY/NJ Port data",subtitle = "Import Throughput Has Jumped up at Major Ports During March") +
  theme_apricitas + theme(legend.position = c(.45,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 0-(.3*610), ymax = 0) +
  coord_cartesian(clip = "off")

INDUSTRIAL_PRODUCTION_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = INDPRO, aes(x=date, y = value, color = "Industrial Production"), size = 1.25) + 
  geom_line(data = IPMAN, aes(x=date, y = value, color = "Industrial Production: Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2017 = 100") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Industrial Production Data Has Slown Down, But Remains High") +
  theme_apricitas + theme(legend.position = c(.70,.40)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 80-(.3*25), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

FIXED_INVESTMENT_Index_Graph <- ggplot() + #indexed employment rate
  geom_line(data = FIXED_RESIDENTIAL, aes(x=date, y = value/5.99, color = "Real Fixed Investment: Residential"), size = 1.25) + 
  geom_line(data = FIXED_INDUSTRIAL, aes(x=date, y = value/2.39, color = "Real Fixed Investment: Industrial Equipment"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(85,90,95,100,105,110,115,120,125), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("Are We In A Recession?") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Real Fixed Investment is Weaking in Key Sectors") +
  theme_apricitas + theme(legend.position = c(.70,.20)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 80-(.3*45), ymax = 80) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDPMonthlyContrib_Graph, "Monthly GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RFSALEDOMPRIV_Graph, "Real Final Private Sales.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RFSALEDOMPRIV_PCT_Graph, "Real Final Private Sales PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RealPrivateInventories_Graph, "Real Private Inventories.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RGDP_Graph, "RGDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = LaborProductivity_Graph, "Labor Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = TradeDeficit_Graph, "Trade Deficit Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Port_Throughput_Graph, "Port Throughput.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_PERSONAL_INCOME_LESS_TRANSFERS_Graph, "Real Personal Income Less Transfers.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Employment_Index_Graph, "Employment Indexed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = REAL_GAS_Graph, "Real Gas.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = INDUSTRIAL_PRODUCTION_Index_Graph, "Industrial Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FIXED_INVESTMENT_Index_Graph, "Real Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
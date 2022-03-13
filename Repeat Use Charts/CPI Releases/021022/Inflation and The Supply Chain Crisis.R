pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

ISMCustomersInventories <- Quandl("ISM/MAN_CUSTINV")
ISMBacklog <- Quandl("ISM/MAN_BACKLOG")
ISMProduction <- Quandl("ISM/MAN_PROD")
ISMManufacturingDeliveries <- Quandl("ISM/MAN_DELIV")
ISMPRicesPaid <- Quandl("ISM/MAN_PRICES")
ISMProductionLead <- Quandl("ISM/BUY_PROD_MAT")

Quandl.api_key("fEweXnVbZ38SxxcytVJY")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Durable <- fredr(series_id = "N241RX1Q020SBEA",observation_start = as.Date("2018-01-01")) #downloading durable Goods Inventories
Invent  <- fredr(series_id = "A371RX1Q020SBEA",observation_start = as.Date("2018-01-01")) #downloading total inventories
MotorVehicle <- fredr(series_id = "N864RX1Q020SBEA",observation_start = as.Date("2018-01-01")) #downloading motor vehicle and parts dealers inventories
Flexport <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/021022/Flexport_Data.csv")
Flexport$Date <- as.Date(Flexport$Date)
#baker hughes rig count data
Baker_Hughes <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/021022/Baker_Hughes.csv")
Baker_Hughes$Date <- as.Date(Baker_Hughes$Date, "%m/%d/%Y")
#port throughput data
Port_Throughput <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/021022/PortThroughput.csv")
Port_Throughput$Date <- as.Date(Port_Throughput$Date)
#durablesCPI data
DurablesCPI <- fredr(series_id = "CUSR0000SAD",observation_start = as.Date("2018-01-01")) #downloading durable Goods CPI
AutoCapacity <- fredr(series_id = "CAPUTLG33611S",observation_start = as.Date("2018-01-01")) #downloading automobile capacity utilization data

DurablesCPI_Graph <- ggplot() + 
  geom_line(data = DurablesCPI, aes(x = date, y = value/1.04327, color = "CPI: Durables"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(90,130), breaks = c(90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Durable Goods Prices Have Increased Nearly 25% Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.86)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 90-(.3*40), ymax = 90) +
  coord_cartesian(clip = "off")

AutoCapacity_Graph <- ggplot() + 
  geom_line(data = AutoCapacity, aes(x = date, y = value/100, color = "Capacity Utilization, Automobile and Light Duty Motor Vehicles"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), breaks = c(0,.25,.50,.75,1), expand = c(0,0)) +
  ylab("Capacity Utilization, %") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Automobile Capacity Utilization is Suffering Under the Chip Shortage") +
  theme_apricitas + theme(legend.position = c(.50,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

Inventories_Graph <- ggplot() + 
  geom_line(data = MotorVehicle, aes(x = date, y = value/2.33, color = "Real Private Inventories: Retail trade: Motor Vehicle and Parts Dealers"), size = 1.25) +
  geom_line(data = Durable, aes(x = date, y = value/12.11, color = "Real Private Inventories: Durable goods Industries"), size = 1.25) +
  geom_line(data = Invent, aes(x = date, y = value/27.82, color = "Real Private Inventories"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(75,130), breaks = c(80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Q4 Saw Motor Vehicle Inventories Grow For the First Time in 2021") +
  theme_apricitas + theme(legend.position = c(.50,.86)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 75-(.3*55), ymax = 75) +
  coord_cartesian(clip = "off")

Flexport_Graph <- ggplot() + 
  geom_line(data = Flexport, aes(x = Date, y = Days, color = "Transpacific Eastbound Ocean Timeliness Indicator"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(40,120), breaks = c(40,60,80,100,120), expand = c(0,0)) +
  ylab("Days") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Flexport data",subtitle = "Ocean Cargo Freight Times are Nearly 3x Their Pre-Pandemic Level") +
  theme_apricitas + theme(legend.position = c(.50,.76)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-05-01")-(.1861*1000), xmax = as.Date("2019-05-01")-(0.049*1000), ymin = 40-(.3*80), ymax = 40) +
  coord_cartesian(clip = "off")

Baker_Hughes_Graph <- ggplot() + 
  geom_line(data = Baker_Hughes, aes(x = Date, y = Rig.Count, color = "Baker Hughes Oil and Gas Rig Count"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,1200), breaks = c(0,250,500,750,1000), expand = c(0,0)) +
  ylab("Rig Count") +
  ggtitle("Where's the Drilling?") +
  labs(caption = "Graph created by @JosephPolitano using Flexport data",subtitle = "Oil Rigs are Still Catching up to Price Levels") +
  theme_apricitas + theme(legend.position = c(.50,.76)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 0-(.3*1000), ymax = 0) +
  coord_cartesian(clip = "off")

ISMBacklog_Graph <- ggplot() + 
  geom_line(data = subset(ISMBacklog, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Greater`/100, color = "ISM Order Backlogs, % Greater than Previous Month"), size = 1.25) +
  geom_line(data = subset(ISMBacklog, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Less`/100, color = "ISM Order Backlogs, % Less than Previous Month"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.7), breaks = c(0,.20,.40,.60), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Order Backlogs Grew Throughout 2021, But Are Showing the First Signs of Shrinking") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 0-(.3*.7), ymax = 0) +
  coord_cartesian(clip = "off")

ISMCustomerInventories_Graph <- ggplot() + 
  geom_line(data = subset(ISMCustomersInventories, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Too Low`/100, color = "ISM Customer Inventories, % Too Low"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.7), breaks = c(0,.20,.40,.60), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Customer Inventories Remain Too Low, But Show Signs of Stabilizing") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 0-(.3*.7), ymax = 0) +
  coord_cartesian(clip = "off")

ISMProductionLead_Graph <- ggplot() + 
  geom_line(data = subset(ISMProductionLead, Date > as.Date("2018-12-01")), aes(x = Date, y = `Average Days`, color = "ISM Average Lead Times, Production Materials"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,100), breaks = c(50,60,70,80,90,100), expand = c(0,0)) +
  ylab("Days") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Input Lead Times are at Record Highs") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 50-(.3*50), ymax = 50) +
  coord_cartesian(clip = "off")

ISMSupplier_Graph <- ggplot() + 
  geom_line(data = subset(ISMManufacturingDeliveries, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Slower`/100, color = "ISM Supplier Delivery Times, % Slower than Previous Month"), size = 1.25) +
  geom_line(data = subset(ISMManufacturingDeliveries, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Faster`/100, color = "ISM Supplier Delivery Times, % Faster than Previous Month"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.7), breaks = c(0,.20,.40,.60), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Supplier Delivery Times Remain High and Rising") +
  theme_apricitas + theme(legend.position = c(.50,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 0-(.3*.7), ymax = 0) +
  coord_cartesian(clip = "off")

ISMPRicesPaid_Graph <- ggplot() + 
  geom_line(data = subset(ISMPRicesPaid, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Higher`/100, color = "ISM Prices Paid, % Higher than Previous Month"), size = 1.25) +
  geom_line(data = subset(ISMPRicesPaid, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Lower`/100, color = "ISM Prices Paid, % Lower than Previous Month"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), breaks = c(0,.25,.50,.75,1), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Input Price Growth Remains High but is Cooling Off") +
  theme_apricitas + theme(legend.position = c(.50,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

ISMProduction_Graph <- ggplot() + 
  geom_line(data = subset(ISMProduction, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Better`/100, color = "ISM Production, % Better than Previous Month"), size = 1.25) +
  geom_line(data = subset(ISMProduction, Date > as.Date("2018-12-01")), aes(x = Date, y = `% Worse`/100, color = "ISM Production, % Worse than Previous Month"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.7), breaks = c(0,.20,.40,.60), expand = c(0,0)) +
  ylab("%") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ISM data",subtitle = "Production Improved Throughout Most of 2021, but is Slowing Down") +
  theme_apricitas + theme(legend.position = c(.50,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1095), xmax = as.Date("2019-01-01")-(0.049*1095), ymin = 0-(.3*.7), ymax = 0) +
  coord_cartesian(clip = "off")

Port_Throughput_Graph <- ggplot() + 
  geom_line(data = Port_Throughput, aes(x = Date, y = LA/1000, color = "Los Angeles"), size = 1.25) +
  geom_line(data = Port_Throughput, aes(x = Date, y = LB/1000, color = "Long Beach"), size = 1.25) +
  geom_line(data = Port_Throughput, aes(x = Date, y = NYNJ/1000, color = "New York/New Jersey"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"),limits = c(0,610), breaks = c(0,200,400,600), expand = c(0,0)) +
  ylab("Loaded Imports, TEUs") +
  ggtitle("A Crisis of Abundance") +
  labs(caption = "Graph created by @JosephPolitano using LA,LB,and NY/NJ Port data",subtitle = "Import Throughput Has Jumped up at Major Ports Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.45,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = 0-(.3*610), ymax = 0) +
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = Inventories_Graph, "Inventories.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Flexport_Graph, "Flexport.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Baker_Hughes_Graph, "Baker Hughes.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ISMBacklog_Graph, "ISM Backlog.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ISMProductionLead_Graph, "ISM Lead Times.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ISMPRicesPaid_Graph, "ISM Prices Paid.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ISMProduction_Graph, "ISM Production.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ISMCustomerInventories_Graph, "ISM Customer Inventories.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = ISMSupplier_Graph, "ISM Supplier.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Port_Throughput_Graph, "Port Throughput.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = DurablesCPI_Graph, "Durables.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AutoCapacity_Graph, "Auto Capacity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
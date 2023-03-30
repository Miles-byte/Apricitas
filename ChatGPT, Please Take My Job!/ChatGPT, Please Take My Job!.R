
GDP <- fredr(series_id = "GDP")
Services_Produced <- merge(GDP, fredr(series_id = "A341RC1Q027SBEA"), by = "date") %>%
  mutate(value = value.y/value.x)
Goods_Produced <- merge(GDP, fredr(series_id = "A353RC1Q027SBEA"), by = "date") %>%
  mutate(value = value.y/value.x)

Goods_Services <- ggplot() + 
  geom_line(data = Goods_Produced, aes(x=date, y = value, color = "Goods"),size = 1.25) + 
  geom_line(data = Services_Produced, aes(x=date, y = value, color = "Services"),size = 1.25) + 
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.25,.65), breaks = c(0.3,0.4,0.5,0.6), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("% of Total GDP") +
  ggtitle("At Your Service") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "The Service Sector Represents the Vast Majority of Output in America") +
  theme_apricitas + theme(legend.position = c(.85,.6)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*(today()-as.Date("1947-01-01"))), xmax = as.Date("1947-01-01")-(0.049*(today()-as.Date("1947-01-01"))), ymin = .25-(.3*.35), ymax = .25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Goods_Services, "Goods Services Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GDP_PER_CAP <- read.csv("C:/Users/josep/Documents/ChatGPT, Please Take My Job!/gdp-per-capita-maddison-2020.csv") %>%
  subset(Code %in% c("USA","OWID_WRL")) %>%
  subset(Year > 1870)

GDP_PER_CAP_Graph <- ggplot() + 
  annotate(geom = "vline",x = 1908, xintercept = 1908, size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Model T",x = 1901, y = 4, size = 4,color = "white") +
  annotate(geom = "vline",x = 1938, xintercept = 1938, size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Nuclear Fission",x = 1926, y = 5, size = 4,color = "white") +
  annotate(geom = "vline",x = 1973, xintercept = 1973, size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Personal Computer",x = 1958, y = 10, size = 4,color = "white") +
  annotate(geom = "vline",x = 1989, xintercept = 1989, size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "Internet",x = 1982, y = 20, size = 4,color = "white") +
  annotate(geom = "vline",x = 2007, xintercept = 2007, size = 0.75,color = "white", linetype = "dashed") +
  annotate(geom = "text", label = "iPhone",x = 2001, y = 25, size = 4,color = "white") +
  geom_line(data = GDP_PER_CAP, aes(x=Year, y = GDP.per.capita/1000, color = Entity),size = 1.25) + 
  xlab("Year") +
  scale_y_log10(labels = scales::dollar_format(suffix = "k", accuracy = 1), breaks = c(3,10,30,60))+
  #scale_y_log10(labels = scales::dollar_format(suffix = "k"), limits = c(0,70), breaks = c(20,40,60), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("Real GDP Per Capita") +
  ggtitle("The Long Industrial Revolution") +
  labs(caption = "Graph created by @JosephPolitano using OWID data",subtitle = "US and Global GDP Per Capita Growth Has Been Generally Constant For a Century") +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  scale_color_manual(name= "Real GDP Per Capita, Log Scale",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 1870-(.1861*(2018-1870)), xmax = 1870-(0.049*(2018-1870)), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_PER_CAP_Graph, "GDP Per Cap.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

IPMAN <- fredr(series_id = "IPMAN")
MANEMP <- fredr(series_id = "MANEMP", observation_start = as.Date("1972-01-01"))

IPMAN_MANEMP_GRAPH <- ggplot() + 
  geom_line(data = IPMAN, aes(x=date, y = value/value[1]*100, color = "Industrial Production: Manufacturing"),size = 1.25) + 
  geom_line(data = MANEMP, aes(x=date, y = value/value[1]*100, color = "All Employees: Manufacturing"),size = 1.25) + 
  xlab("Year") +
  scale_y_continuous(labels = scales::number_format(),limits = c(0,300), breaks = c(0,50,100,150,200,250,300), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("Index, Jan 1972 = 100") +
  ggtitle("Doing More For Less") +
  labs(caption = "Graph created by @JosephPolitano using BLS and FRB data",subtitle = "US Industrial Output Has Risen Even as Manufacturing Employment Fell") +
  theme_apricitas + theme(legend.position = c(.75,.6)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1972-01-01")-(.1861*(today()-as.Date("1972-01-01"))), xmax = as.Date("1972-01-01")-(0.049*(today()-as.Date("1972-01-01"))), ymin = 0-(.3*300), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IPMAN_MANEMP_GRAPH, "IP MANEMP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RANDD <- merge(GDP, fredr(series_id = "Y694RC1Q027SBEA"), by = "date") %>%
  mutate(value = value.y/value.x)

RANDD_Graph <- ggplot() + 
  geom_line(data = RANDD, aes(x=date, y = value, color = "US R&D Spending as a Share of GDP"),size = 1.25) + 
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.04), breaks = c(0,0.01,0.02,0.03,0.04), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("% of Total GDP") +
  ggtitle("The Research Boom") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "US R&D Spending as a Share of GDP is at an All-Time High") +
  theme_apricitas + theme(legend.position = c(.65,.4)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1947-01-01")-(.1861*(today()-as.Date("1947-01-01"))), xmax = as.Date("1947-01-01")-(0.049*(today()-as.Date("1947-01-01"))), ymin = 0-(.3*.04), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RANDD_Graph, "RANDD.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Employ_by_sector <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Interpersonal%20Economy%20-%20Automation%20and%20You/employment-by-economic-sector.csv") #importing data on jobs by sector from OWID
colnames(Employ_by_sector) <- c("Country","Country_Code","Year","Services","Manufacturing","Agriculture") #changing colnames to make more sense
Employ_by_sector <- pivot_longer(Employ_by_sector, cols = Services:Agriculture, names_to = "Sector", values_to = "Employment") #gathering data from sectoral employment for geom_area

Employ_by_sector_graph <- ggplot() + 
  geom_area(data = Employ_by_sector[Employ_by_sector$Country_Code == "USA", ], aes(x=Year, y = Employment/1000000, fill = Sector),color = NA, size = 0) +
  xlab("Year") +
  scale_y_continuous(limits = c(0,150), breaks = c(0,50,100,150), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Employment by Sector, Millions") +
  ggtitle("At Your Service") +
  labs(caption = "Graph created by @JosephPolitano using OWID data based on Herrendorf et al. (2014)",subtitle = "The Service Sector Represents the Vast Majority of Employment in America") +
  theme_apricitas + theme(legend.position = c(.60,.8)) +
  scale_fill_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))

ggsave(dpi = "retina",plot = Employ_by_sector_graph, "Employ by Sector.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PAYEMS <- fredr(series_id = "PAYEMS")

PAYEMS_MANU <- merge(PAYEMS, fredr(series_id = "MANEMP"), by = "date") %>%
  mutate(value = value.y/value.x)
PAYEMS_HEALTH <- merge(PAYEMS, fredr(series_id = "USEHS"), by = "date") %>%
  mutate(value = value.y/value.x)
PAYEMS_PROF <- merge(PAYEMS, fredr(series_id = "USPBS"), by = "date") %>%
  mutate(value = value.y/value.x)
PAYEMS_LAH <- merge(PAYEMS, fredr(series_id = "USLAH"), by = "date") %>%
  mutate(value = value.y/value.x)

PAYEMS_SHARE <- ggplot() + 
  geom_line(data = PAYEMS_MANU, aes(x=date, y = value, color = "Manufacturing"),size = 1.25) + 
  geom_line(data = PAYEMS_HEALTH, aes(x=date, y = value, color = "Health Services and Private Education"),size = 1.25) + 
  geom_line(data = PAYEMS_PROF, aes(x=date, y = value, color = "Professional and Business Services"),size = 1.25) + 
  geom_line(data = PAYEMS_LAH, aes(x=date, y = value, color = "Leisure and Hospitality"),size = 1.25) + 
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.40), breaks = c(0,0.1,0.2,0.3,0.4), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("% of Total Nonfarm Employment") +
  ggtitle("The Interpersonal Economy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Interpersonal Services are a Growing Share of US Employment") +
  theme_apricitas + theme(legend.position = c(.75,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Manufacturing","Health Services and Private Education","Professional and Business Services","Leisure and Hospitality")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1939-01-01")-(.1861*(today()-as.Date("1939-01-01"))), xmax = as.Date("1939-01-01")-(0.049*(today()-as.Date("1939-01-01"))), ymin = 0-(.3*.40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PAYEMS_SHARE, "PAYEMS SHARE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

pacman::p_load(jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("rOpenGov/dkstat")
library("dkstat")

Meta_test <- dst_meta("IPOP2021", lang = "en")

IND_PRO_PHARMA <- dst_get_data(table = "IPOP2021", 
                 SÆSON = "Seasonally adjusted", 
                 BRANCHE07 = c("C31 Manufacturing excl. Pharmaceuticals","CF Pharmaceuticals","C Manufacturing"), 
                 Tid = "*", 
                 lang = "en") %>%
  select(-SÆSON) %>%
  setNames(c("branch","date","value")) %>%
  pivot_wider(names_from = branch) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(across(where(is.numeric), ~ . / first(.)*100))

IND_PRO_PHARMA_graph <- ggplot() + #plotting pharmaceutical manufacturing
  geom_line(data=IND_PRO_PHARMA, aes(x=date,y= `C31 Manufacturing excl. Pharmaceuticals`,color="Manufacturing ex-Pharmaceuticals"), size = 1.25) +
  geom_line(data=IND_PRO_PHARMA, aes(x=date,y= `C Manufacturing`,color="Total Manufacturing"), size = 1.25) +
  geom_line(data=IND_PRO_PHARMA, aes(x=date,y= `CF Pharmaceuticals`,color="Manufacturing of Pharmaceuticals"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,(ceiling(max(IND_PRO_PHARMA$`CF Pharmaceuticals`) / 25) * 25)), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Denmark's Weight Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "A Boom in Weight Loss & Diabetes Drugs Has Dramatically Increased Danish Pharma Production") +
  theme_apricitas + theme(legend.position = c(.42,.64)) +
  scale_color_manual(name= "Denmark, Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Manufacturing of Pharmaceuticals","Total Manufacturing","Manufacturing ex-Pharmaceuticals")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*(ceiling(max(IND_PRO_PHARMA$`CF Pharmaceuticals`) / 25) * 25)), ymax =0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_PHARMA_graph, "Ind Pro Pharma.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("OMS5", lang = "en")

TURNOVER_PHARMA_TOTAL <- dst_get_data(table = "OMS5", 
                               SÆSON = "Seasonally adjusted",
                               OMSTYPE = "TOTAL TURNOVER",
                               BRANCHE07 = c("C Manufacturing","CF Pharmaceuticals"), 
                               Tid = "*", 
                               lang = "en") %>%
  select(-SÆSON,-OMSTYPE) %>%
  setNames(c("branch","date","value")) %>%
  pivot_wider(names_from = branch) %>%
  filter(date >= as.Date("2015-01-01")) %>%
  mutate(`C Manufacturing` = `C Manufacturing`-`CF Pharmaceuticals`) %>%
  setNames(c("date","Manufacturing Ex-Pharmaceuticals","Manufacturing of Pharmaceuticals")) %>%
  pivot_longer(cols = `Manufacturing Ex-Pharmaceuticals`:`Manufacturing of Pharmaceuticals`) %>%
  mutate(name = factor(name, levels = c("Manufacturing of Pharmaceuticals","Manufacturing Ex-Pharmaceuticals")))

TURNOVER_PHARMA_TOTAL_graph <- ggplot(data = TURNOVER_PHARMA_TOTAL, aes(x = date, y = value/1000000, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Danish Krone") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B kr."), expand = c(0,0), limits = c(0,300)) +
  ggtitle("Denmark's Weight Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark data", subtitle = "Growth in Pharmaceutical Turnover is Driving Essentially All Growth in Manufacturing Turnover") +
  theme_apricitas + theme(legend.position = c(.45,.845)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Manufacturing of Pharmaceuticals","Manufacturing Ex-Pharmaceuticals")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TURNOVER_PHARMA_TOTAL_graph, "Turnover Pharma Total.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

TURNOVER_PHARMA_BREAKDOWN <- dst_get_data(table = "OMS5", 
                                      SÆSON = "Non-seasonally adjusted",
                                      OMSTYPE = c("Production on own account","Production outsourced to others","TOTAL INDUSTRIAL SERVICES","Commercial (resale) turnover","Other turnover"),
                                      BRANCHE07 = "CF Pharmaceuticals", 
                                      Tid = "*", 
                                      lang = "en") %>%
  select(-SÆSON,-BRANCHE07) %>%
  setNames(c("Category","date","value")) %>%
  filter(date >= as.Date("2021-01-01")) %>%
  mutate(Category = gsub("TOTAL INDUSTRIAL SERVICES","Industrial services",Category)) %>%
  mutate(Category = gsub("Commercial (resale) turnover","Commercial (resale) sales",Category)) %>%
  mutate(Category = gsub("Other turnover","Other sales",Category)) %>%
  mutate(Category = factor(Category, levels = rev(c("Production on own account","Production outsourced to others","Industrial services","Commercial (resale) turnover","Other sales"))))

TURNOVER_PHARMA_BREAKDOWN_graph <- ggplot(data = TURNOVER_PHARMA_BREAKDOWN, aes(x = date, y = value/1000000, fill = Category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Danish Krone") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B kr."), expand = c(0,0), limits = c(0,100), breaks = c(0,25,50,75,100)) +
  ggtitle("Denmark's Weight Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark data", subtitle = "Production Outsourcing is Rising Among Danish Pharmaceutical Manufacturers") +
  theme_apricitas + theme(legend.position = c(.4,.775)) +
  scale_fill_manual(name= "Sales, Danish Pharmaceutical Manufacturers",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Production on own account","Production outsourced to others","Industrial services","Commercial (resale) turnover","Other sales")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(today()-as.Date("2021-01-01"))), xmax = as.Date("2021-01-01")-(0.049*(today()-as.Date("2021-01-01"))), ymin = 0-(.3*100), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TURNOVER_PHARMA_BREAKDOWN_graph, "Turnover Pharma Breakdown.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("NKN1", lang = "en")

REAL_GDP_DOMESTIC_DEMAND <- dst_get_data(table = "NKN1",
                                          TRANSAKT = c("B.1*g Gross domestic product","Final domestic demand","P.31 Household consumption expenditure"),
                                          PRISENHED = "2020-prices, chained values, (bill. DKK.)",
                                          SÆSON = "Seasonally adjusted",
                                          Tid = "*", 
                                          lang = "en") %>%
  select(-SÆSON,-PRISENHED) %>%
  setNames(c("Category","date","value")) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  pivot_wider(names_from = Category) %>%
  setNames(c("date","Gross Domestic Product","Household Consumption Expenditure","Final Domestic Demand")) %>%
  mutate(across(where(is.numeric), ~ . / first(.)*100))

Meta_test <- dst_meta("NKN2", lang = "en")

REAL_NNI <- dst_get_data(table = "NKN2",
                                         TRANSAKT = c("B.5*n Net national income"),
                                         PRISENHED = "2020-prices, real value",
                                         SÆSON = "Seasonally adjusted",
                                         Tid = "*", 
                                         lang = "en") %>%
  select(-SÆSON,-PRISENHED) %>%
  setNames(c("Category","date","value")) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(across(where(is.numeric), ~ . / first(.)*100))

REAL_GDP_DOMESTIC_DEMAND_graph <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=REAL_GDP_DOMESTIC_DEMAND, aes(x=date,y= `Final Domestic Demand`,color="Real Final Domestic Demand"), size = 1.25) +
  geom_line(data=REAL_GDP_DOMESTIC_DEMAND, aes(x=date,y= `Household Consumption Expenditure`,color="Real Household Consumption"), size = 1.25) +
  geom_line(data=REAL_GDP_DOMESTIC_DEMAND, aes(x=date,y= `Gross Domestic Product`,color="Real Gross Domestic Product"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,125), expand = c(0,0)) +
  ylab("Index, Jan 2016 = 100") +
  ggtitle("Denmark's Slowing Domestic Economy") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "Danish Pharma Exports are Boosting GDP—But Domestic Demand and Consumption Has Fallen") +
  theme_apricitas + theme(legend.position = c(.42,.825), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Denmark, Real Economic Measures",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Real Gross Domestic Product","Real Household Consumption","Real Final Domestic Demand")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 95-(.3*25), ymax =95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_GDP_DOMESTIC_DEMAND_graph, "Real GDP Domestic Demand.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PHARMA_GDP_CONTRIB <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Denmark/Pharma_Contrib.csv") %>%
  mutate(date = seq.Date(from = as.Date("1991-01-01"), by = "year", length.out = nrow(.))) %>%
  select(-Total) %>%
  setNames(c("Pharmaceuticals","Rest of Economy", "date")) %>%
  pivot_longer(cols = Pharmaceuticals:`Rest of Economy`) %>%
  add_row(date = as.Date("2026-01-01"), name = "Pharmaceuticals", value = 2.1) %>%
  add_row(date = as.Date("2026-01-01"), name = "Rest of Economy", value = -0.9) 
  
PHARMA_GDP_CONTRIB_graph <- ggplot(data = PHARMA_GDP_CONTRIB, aes(x = date, y = value/100, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("vline", x = as.Date("2023-03-01"), xintercept = as.Date("2023-03-01"), color = "white", size = 3) +
  annotate("text", label = "Q2 2023\nYear-on-year\nGrowth Estimate", x = as.Date("2026-10-01"), y = 0.035, color = "white", size = 4, lineheight = .85) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Contribution to Annual GDP Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.055,.07), expand = c(0,0)) +
  ggtitle("How Weight Loss Drugs Stopped a Danish Recession") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data", subtitle = "Pharmaceutical Output is Keeping the Danish Economy From Outright Contraction") +
  theme_apricitas + theme(legend.position = c(.45,.9), plot.title = element_text(size = 22)) +
  scale_fill_manual(name= "Contributions to Annual Danish GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-10-01"))), xmax = as.Date("1990-10-01")-(0.049*(today()+1000-as.Date("1990-10-01"))), ymin = -0.055-(.3*.125), ymax = -0.055) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PHARMA_GDP_CONTRIB_graph, "Pharma GDP Contrib.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PHARMA_GDP_CONTRIB2 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Denmark/Pharma_Contrib.csv") %>%
  mutate(date = seq.Date(from = as.Date("1991-01-01"), by = "year", length.out = nrow(.))) %>%
  select(-Total) %>%
  setNames(c("Pharmaceuticals","Rest of Economy", "date")) %>%
  pivot_longer(cols = Pharmaceuticals:`Rest of Economy`) %>%
  add_row(date = as.Date("2024-01-01"), name = "Pharmaceuticals", value = 2.1) %>%
  add_row(date = as.Date("2024-01-01"), name = "Rest of Economy", value = -0.9) %>%
  filter(date >= as.Date("2015-01-01"))

PHARMA_GDP_CONTRIB2_graph <- ggplot(data = PHARMA_GDP_CONTRIB2, aes(x = date, y = value/100, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("vline", x = as.Date("2023-03-01"), xintercept = as.Date("2023-03-01"), color = "white", size = 3) +
  annotate("text", label = "Q2 2023\nYear-on-year\nGrowth Estimate", x = as.Date("2024-03-01"), y = 0.035, color = "white", size = 4, lineheight = .85) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Contribution to Annual GDP Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.03,.07), expand = c(0,0)) +
  scale_x_date(breaks = c(as.Date("2016-01-01"),as.Date("2018-01-01"),as.Date("2020-01-01"),as.Date("2022-01-01")), labels = c("2016","2018","2020","2022")) +
  ggtitle("How Weight Loss Drugs Stopped a Danish Recession") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data", subtitle = "Pharmaceutical Output is Keeping the Danish Economy From Outright Contraction") +
  theme_apricitas + theme(legend.position = c(.3,.9), plot.title = element_text(size = 22)) +
  scale_fill_manual(name= "Contributions to Annual Danish GDP Growth",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()+300-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()+300-as.Date("2015-01-01"))), ymin = -0.03-(.3*.10), ymax = -0.03) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PHARMA_GDP_CONTRIB2_graph, "Pharma GDP Contrib2.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


Meta_test <- dst_meta("NKBP10", lang = "en")

MANUFACTURING_GVA <- dst_get_data(table = "NKBP10",
                         TRANSAKT = c("B.1g Gross value added"),
                         BRANCHE = c("Total","C Manufacturing"),
                         PRISENHED = "2020-prices, chained values",
                         SÆSON = "Seasonally adjusted",
                         Tid = "*", 
                         lang = "en") %>%
  select(-SÆSON,-PRISENHED,-TRANSAKT) %>%
  setNames(c("Category","date","value")) %>%
  pivot_wider(names_from = Category) %>%
  drop_na()

MANUFACTURING_GVA_Graph <- ggplot() + 
  geom_line(data=MANUFACTURING_GVA, aes(x=date,y= `C Manufacturing`/1000 ,color="Denmark, Real Manufacturing Gross Value Added,\nIncluding Pharmaceutical Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B kr."), expand = c(0,0), limits = c(0,(ceiling(max(MANUFACTURING_GVA$`C Manufacturing`) / 25000) * 25))) +
  ylab("Billions of 2020 Kroner") +
  ggtitle("Denmark's Weight Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "Danish Manufacturing Real Gross Value Added is Booming Thanks to Weight Loss Drugs") +
  theme_apricitas + theme(legend.position = c(.42,.7), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1991-01-01")-(.1861*(today()-as.Date("1991-01-01"))), xmax = as.Date("1991-01-01")-(0.049*(today()-as.Date("1991-01-01"))), ymin = 0-(.3*(ceiling(max(MANUFACTURING_GVA$`C Manufacturing`) / 25000) * 25)), ymax =0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANUFACTURING_GVA_Graph, "Manufacturing GVA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

MANUFACTURING_GVA_SURPLUS <- dst_get_data(table = "NKBP10",
                                  TRANSAKT = c("B.2g+B.3g Gross operating surplus and mixed income","D.1 Compensation of employees"),
                                  BRANCHE = "C Manufacturing",
                                  PRISENHED = "Current prices",
                                  SÆSON = "Seasonally adjusted",
                                  Tid = "*", 
                                  lang = "en") %>%
  select(-SÆSON,-PRISENHED,-BRANCHE) %>%
  setNames(c("Category","date","value")) %>%
  pivot_wider(names_from = Category) %>%
  drop_na() %>%
  setNames(c("date","Compensation of Employees","Gross Operating Surplus & Mixed Income")) %>%
  pivot_longer(cols = `Compensation of Employees`:`Gross Operating Surplus & Mixed Income`)

REAL_GDP_DOMESTIC_DEMAND <- dst_get_data(table = "NKN1",
                                         TRANSAKT = "B.1*g Gross domestic product",
                                         PRISENHED = "Current prices, (bill. DKK.)",
                                         SÆSON = "Seasonally adjusted",
                                         Tid = "*", 
                                         lang = "en") %>%
  select(-SÆSON,-PRISENHED) %>%
  setNames(c("name","date","value"))

MANUFACUTRING_GVA_SURPLUS_GDP <- rbind(MANUFACTURING_GVA_SURPLUS,REAL_GDP_DOMESTIC_DEMAND) %>%
  pivot_wider() %>%
  transmute(date, `Compensation of Employees` = `Compensation of Employees`/(`B.1*g Gross domestic product`*1000), `Gross Operating Surplus & Mixed Income` = `Gross Operating Surplus & Mixed Income`/(`B.1*g Gross domestic product`*1000)) %>%
  pivot_longer(cols = `Compensation of Employees`:`Gross Operating Surplus & Mixed Income`) %>%
  mutate(name = factor(name, levels = rev(c("Compensation of Employees","Gross Operating Surplus & Mixed Income"))))

MANUFACUTRING_GVA_SURPLUS_GDP_graph <- ggplot(data = MANUFACUTRING_GVA_SURPLUS_GDP, aes(x = date, y = value, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA, width = 100) +
  xlab("Date") +
  ylab("% of GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.20), expand = c(0,0)) +
  ggtitle("Denmark's Booming Pharma Profits") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data", subtitle = "Operating Surplus and Other Returns to Capital Make up More than Half of Manufacturing GVA") +
  theme_apricitas + theme(legend.position = c(.45,.89)) +
  scale_fill_manual(name= "Components of Danish Manufacturing Gross Value Added, % of GDP",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*.20), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANUFACUTRING_GVA_SURPLUS_GDP_graph, "Manufacturing GVA Surplus Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


Meta_test <- dst_meta("BARO4", lang = "en")

CAPUTE_PHARMA <- dst_get_data(table = "BARO4",
                                  BRANCHE07 = "CF Pharmaceuticals",
                                  Tid = "*", 
                                  lang = "en") %>%
  setNames(c("Category","date","value"))

CAPUTE_PHARMA_graph <- ggplot() + #plotting capacity utilization
  geom_line(data=CAPUTE_PHARMA, aes(x=date,y= value/100,color="Capacity Utilization of Pharmaceutical Manufacturing, Denmark"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0.675,1)) +
  ylab("Percent") +
  ggtitle("Denmark's Pharma Production Struggles") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "Danish Pharmaceutical Manufacturers are Running Near 100% Capacity Utilization") +
  theme_apricitas + theme(legend.position = c(.45,.975)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = .675-(.3*(.325)), ymax = .675) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAPUTE_PHARMA_graph, "CAPUTE Pharma Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("BARO3", lang = "en")

PROD_LIMITATIONS_PHARMA <- dst_get_data(table = "BARO3",
                              BRANCHE07 = "CF Pharmaceuticals",
                              TYPE = "Shortage of material and/or equipment",
                              Tid = "*", 
                              lang = "en") %>%
  setNames(c("Category","limitations","date","value"))

PROD_LIMITATIONS_PHARMA_graph <- ggplot() + #plotting capacity utilization
  geom_line(data=PROD_LIMITATIONS_PHARMA, aes(x=date,y= value/100,color="Share of Pharmaceutical Manufacturers with Materials Shortages, Denmark"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) +
  ylab("Percent") +
  ggtitle("Denmark's Pharma Production Struggles") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "Danish Pharmaceutical Manufacturers are Citing Materials Shortages at Record Rates") +
  theme_apricitas + theme(legend.position = c(.5,.975)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0-(.3*(1)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PROD_LIMITATIONS_PHARMA_graph, "Prod Limitations Pharma Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("UHQ", lang = "en")

EXPORTS_GOODS_QUARTERLY <- dst_get_data(table = "UHQ",
                                        POST = "GOODS (FOB)",
                                        INDUD = "Exports",
                                        LAND = c("United States","Germany","Sweden","Norway"),
                                        SÆSON = "Seasonally adjusted",
                                        Tid = "*", 
                                        lang = "en") %>%
  select(LAND,TID,value) %>%
  setNames(c("Country","date","value")) %>%
  pivot_wider(names_from = Country)

EXPORTS_GOODS_QUARTERLY_Graph <- ggplot() + 
  geom_line(data=EXPORTS_GOODS_QUARTERLY, aes(x=date,y= Sweden/1000 ,color="Sweden"), size = 1.25) +
  geom_line(data=EXPORTS_GOODS_QUARTERLY, aes(x=date,y= Germany/1000 ,color="Germany"), size = 1.25) +
  geom_line(data=EXPORTS_GOODS_QUARTERLY, aes(x=date,y= Norway/1000 ,color="Norway"), size = 1.25) +
  geom_line(data=EXPORTS_GOODS_QUARTERLY, aes(x=date,y= `United States`/1000 ,color="United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B kr."), expand = c(0,0), limits = c(0,(ceiling(max(EXPORTS_GOODS_QUARTERLY$`United States`) / 10000) * 10))) +
  ylab("Billions of Kroner") +
  ggtitle("Denmark's Weight Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "America Has Become the Largest Destination for Danish Exports Amidst a Pharma Boom") +
  theme_apricitas + theme(legend.position = c(.35,.8), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Denmark, Gross Exports of Goods by Destination",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United States","Germany","Sweden","Norway")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*(ceiling(max(EXPORTS_GOODS_QUARTERLY$`United States`) / 10000) * 10)), ymax =0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXPORTS_GOODS_QUARTERLY_Graph, "Exports Goods Quarterly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("UHQ", lang = "en")

EXPORTS_SERVICES_IP_QUARTERLY <- dst_get_data(table = "UHQ",
                                        POST = "Charges for the use of intellectual properties",
                                        INDUD = c("Exports","Imports"),
                                        LAND = "*",
                                        SÆSON = "Non-seasonally adjusted",
                                        Tid = "*", 
                                        lang = "en") %>%
  select(LAND,TID,INDUD,value) %>%
  setNames(c("country","date","direction","value")) %>%
  group_by(country, date) %>%
  summarise(net_value = sum(ifelse(direction == "Exports", value, -value))) %>%
  spread(country, net_value, fill = 0) %>%
  select("date","REST OF THE WORLD","United States") %>%
  setNames(c("date","World ex-US","United States")) %>%
  mutate(`World ex-US`= `World ex-US`-`United States`) %>%
  pivot_longer(cols = `World ex-US`:`United States`) %>%
  mutate(name = factor(name, levels = rev(c("United States","World ex-US"))))

EXPORTS_SERVICES_IP_QUARTERLY_graph <- ggplot(data = EXPORTS_SERVICES_IP_QUARTERLY, aes(x = date, y = value/1000, fill = name)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Net Exports, Billions of Krone, Not Seasonally Adjusted") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B kr."), limits = c(-0.5,12), expand = c(0,0)) +
  ggtitle("Denmark's Booming IP Exports") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data", subtitle = "Danish IP Exports to the US are Rising in the Wake of the Nation's Pharma Boom") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_fill_manual(name= "Net Danish Exports, Charges for the Use of Intellectual Property",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("United States","World ex-US")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = -0.5-(.3*12.5), ymax = -0.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXPORTS_SERVICES_IP_QUARTERLY_graph, "Exports Services Quarterly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
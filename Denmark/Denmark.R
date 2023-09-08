pacman::p_load(jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("rOpenGov/dkstat")
library("dkstat")

Meta_test <- dst_meta("IPOP2015", lang = "en")

IND_PRO_PHARMA <- dst_get_data(table = "IPOP2015", 
                 SÆSON = "Seasonally adjusted", 
                 BRANCHE07 = c("C31 Manufacturing excl. Pharmaceuticals","CF Pharmaceuticals","C Manufacturing"), 
                 Tid = "*", 
                 lang = "en") %>%
  select(-SÆSON) %>%
  setNames(c("branch","date","value")) %>%
  pivot_wider(names_from = branch) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(across(where(is.numeric), ~ . / first(.)*100))

IND_PRO_PHARMA <- ggplot() + #plotting pharmaceutical manufacturing
  geom_line(data=IND_PRO_PHARMA, aes(x=date,y= `C31 Manufacturing excl. Pharmaceuticals`,color="Manufacturing ex-Pharmaceuticals"), size = 1.25) +
  geom_line(data=IND_PRO_PHARMA, aes(x=date,y= `C Manufacturing`,color="Total Manufacturing"), size = 1.25) +
  geom_line(data=IND_PRO_PHARMA, aes(x=date,y= `CF Pharmaceuticals`,color="Manufacturing of Pharmaceuticals"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,(ceiling(max(IND_PRO_PHARMA$`CF Pharmaceuticals`) / 25) * 25)), expand = c(0,0)) +
  ylab("Index, Jan 2018 = 100") +
  ggtitle("Denmark's Weight-Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "A Boom in Weight-Loss & Diabetes Drugs Has More Than Doubled Danish Pharmaceutical Production") +
  theme_apricitas + theme(legend.position = c(.42,.64)) +
  scale_color_manual(name= "Denmark, Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Manufacturing of Pharmaceuticals","Total Manufacturing","Manufacturing ex-Pharmaceuticals")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*(ceiling(max(IND_PRO_PHARMA$`CF Pharmaceuticals`) / 25) * 25)), ymax =0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IND_PRO_PHARMA, "Ind Pro Pharma.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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
  ggtitle("Denmark's Weight-Loss Pharma Boom") +
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
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B kr."), expand = c(0,0), limits = c(0,75), breaks = c(0,25,50,75)) +
  ggtitle("Denmark's Weight-Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark data", subtitle = "Production Outsourcing is Rising Among Danish Pharmaceutical Manufacturers") +
  theme_apricitas + theme(legend.position = c(.4,.775)) +
  scale_fill_manual(name= "Sales, Danish Pharmaceutical Manufacturers",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Production on own account","Production outsourced to others","Industrial services","Commercial (resale) turnover","Other sales")) +
  theme(legend.text =  element_text(size = 13, color = "white")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(today()-as.Date("2021-01-01"))), xmax = as.Date("2021-01-01")-(0.049*(today()-as.Date("2021-01-01"))), ymin = 0-(.3*75), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TURNOVER_PHARMA_BREAKDOWN_graph, "Turnover Pharma Breakdown.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("NKN1", lang = "en")

REAL_GDP_DOMESTIC_DEMAND <- dst_get_data(table = "NKN1",
                                          TRANSAKT = c("B.1*g Gross domestic product","Final domestic demand"),
                                          PRISENHED = "2010-prices, chained values, (bill. DKK.)",
                                          SÆSON = "Seasonally adjusted",
                                          Tid = "*", 
                                          lang = "en") %>%
  select(-SÆSON,-PRISENHED) %>%
  setNames(c("Category","date","value")) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  pivot_wider(names_from = Category) %>%
  setNames(c("date","Gross Domestic Product","Final Domestic Demand")) %>%
  mutate(across(where(is.numeric), ~ . / first(.)*100))

Meta_test <- dst_meta("NKN2", lang = "en")

REAL_NNI <- dst_get_data(table = "NKN2",
                                         TRANSAKT = c("B.5*n Net national income"),
                                         PRISENHED = "2010-prices, real value",
                                         SÆSON = "Seasonally adjusted",
                                         Tid = "*", 
                                         lang = "en") %>%
  select(-SÆSON,-PRISENHED) %>%
  setNames(c("Category","date","value")) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(across(where(is.numeric), ~ . / first(.)*100))

REAL_GDP_DOMESTIC_DEMAND_NNI <- ggplot() + #plotting energy intensive manufacturing
  geom_line(data=REAL_GDP_DOMESTIC_DEMAND, aes(x=date,y= `Final Domestic Demand`,color="Real Final Domestic Demand"), size = 1.25) +
  geom_line(data=REAL_NNI, aes(x=date,y= value,color="Real Net National Income"), size = 1.25) +
  geom_line(data=REAL_GDP_DOMESTIC_DEMAND, aes(x=date,y= `Gross Domestic Product`,color="Real Gross Domestic Product"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,125), expand = c(0,0)) +
  ylab("Index, Jan 2016 = 100") +
  ggtitle("Is Denmark's Pharma Boom Distorting GDP?") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "Danish GDP is Diverging From National Income and Final Domestic Demand") +
  theme_apricitas + theme(legend.position = c(.42,.7), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Denmark, Real Output Measures",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Real Gross Domestic Product","Real Net National Income","Real Final Domestic Demand")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 95-(.3*30), ymax =95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_GDP_DOMESTIC_DEMAND_NNI, "Real GDP Domestic Demand.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Meta_test <- dst_meta("NKBP10", lang = "en")

MANUFACTURING_GVA <- dst_get_data(table = "NKBP10",
                         TRANSAKT = c("B.1g Gross value added"),
                         BRANCHE = c("Total","C Manufacturing"),
                         PRISENHED = "2010-prices, chained values",
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
  ylab("Billions of 2010 Kroner") +
  ggtitle("Denmark's Weight-Loss Pharma Boom") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Denmark Data",subtitle = "Danish Manufacturing Real Gross Value Added is Booming Thanks to Weight-Loss Drugs") +
  theme_apricitas + theme(legend.position = c(.42,.7), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1991-01-01")-(.1861*(today()-as.Date("1991-01-01"))), xmax = as.Date("1991-01-01")-(0.049*(today()-as.Date("1991-01-01"))), ymin = 0-(.3*(ceiling(max(MANUFACTURING_GVA$`C Manufacturing`) / 25000) * 25)), ymax =0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANUFACTURING_GVA_Graph, "Manufacturing GVA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

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
  theme_apricitas + theme(legend.position = c(.45,.975)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0-(.3*(1)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAPUTE_PHARMA_graph, "CAPUTE Pharma Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
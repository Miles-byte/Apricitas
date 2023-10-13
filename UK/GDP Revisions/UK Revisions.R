pacman::p_load(ggpubr,sf,onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

GDP_REVISIONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/UK/GDP%20Revisions/GDP_REVISIONS.csv") %>%
  mutate(date = as.Date(date))

GDP_Revision_Graph <- ggplot() +
  geom_line(data = filter(GDP_REVISIONS, date >= as.Date("2018-01-01")), aes(x=date, y = GDP_old*4/1000000, color = "Real UK GDP: Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_REVISIONS, date >= as.Date("2018-01-01")), aes(x=date, y = GDP_new*4/1000000, color = "Real UK GDP: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.1, prefix = "£"),limits = c(1.7,2.3), breaks = c(1.7,1.8,1.9,2,2.1,2.2,2.3), expand = c(0,0)) +
  ylab("Trillions of 2019 Pounds") +
  ggtitle("Revisions to UK GDP") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Real UK GDP Has Been Revised, Showing the Economy Has Now Exceeded Pre-COVID Levels") +
  theme_apricitas + theme(legend.position = c(.80,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 1.7-(.3*0.6), ymax = 1.7) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_Revision_Graph, "UK GDP Revisions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GDP_Category_Breakdown <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/UK/GDP%20Revisions/GDP_REVISIONS_MAJOR_CATEGORIES.csv") %>%
  mutate(date = as.Date(date))

GFCF_REVISIONS_Graph <-  ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = gfcf_old*4/1000000, color = "Real UK Gross Fixed Capital Formation: Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = gfcf_new*4/1000000, color = "Real UK Gross Fixed Capital Formation: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.05, prefix = "£"),limits = c(0.3,0.45), breaks = c(0.3,0.35,0.4,0.45), expand = c(0,0)) +
  ylab("Trillions of 2019 Pounds") +
  ggtitle("Revisions to UK Investment") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Real UK Fixed Investment Has Been Revised Upward and is Now Hitting New Record Highs") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0.3-(.3*.15), ymax = 0.3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GFCF_REVISIONS_Graph, "UK GFCF Revisions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GFCF_REVISIONS_Graph <-  ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = gfcf_old*4/1000000, color = "Real UK Gross Fixed Capital Formation: Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = gfcf_new*4/1000000, color = "Real UK Gross Fixed Capital Formation: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.05, prefix = "£"),limits = c(0.3,0.45), breaks = c(0.3,0.35,0.4,0.45), expand = c(0,0)) +
  ylab("Trillions of 2019 Pounds, Annualized") +
  ggtitle("Revisions to UK Investment") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Real UK Fixed Investment Has Been Revised Upward and is Now Hitting New Record Highs") +
  theme_apricitas + theme(legend.position = c(.45,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0.3-(.3*.15), ymax = 0.3) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GFCF_REVISIONS_Graph, "UK GFCF Revisions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GOV_CONSUMPTION_REVISIONS_Graph <-  ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = gov_consumption_old*4/1000000, color = "Real UK Government Consumption Expenditure: Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = gov_consumption_new*4/1000000, color = "Real UK Government Consumption Expenditure: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.05, prefix = "£"),limits = c(0.325,0.5), breaks = c(0.3,0.35,0.4,0.45,0.5), expand = c(0,0)) +
  ylab("Trillions of 2019 Pounds, Annualized") +
  ggtitle("Revisions to UK Government Spending") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Real UK Government Consumption Was Revised Up, Thanks in Part to Higher Healthcare Output") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0.325-(.3*.175), ymax = 0.325) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GOV_CONSUMPTION_REVISIONS_Graph, "UK Gov Consumption Revisions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INVENTORIES_REVISIONS_Graph <-  ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = inventories_old/1000, color = "Real UK Changes in Inventories, Quarterly: Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = inventories_new/1000, color = "Real UK Changes in Inventories, Quarterly: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1, prefix = "£"),limits = c(-13.5,10), breaks = c(-10,-5,0,5,10), expand = c(0,0)) +
  ylab("Billions of 2019 Pounds") +
  ggtitle("Revisions to UK Government Spending") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Shifts in Inventories Trends, Especially in 2020, Have Boosted Real UK GDP") +
  theme_apricitas + theme(legend.position = c(.45,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -13.5-(.3*23.5), ymax = -13.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INVENTORIES_REVISIONS_Graph, "Inventories Revisions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CONSUMPTION_Graph <- ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = consumption_old*4/1000000, color = "Real UK Household Consumption, Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = consumption_new*4/1000000, color = "Real UK Household Consumption, New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.1, prefix = "£"),limits = c(0.95,1.4), breaks = c(1,1.1,1.2,1.3,1.4), expand = c(0,0)) +
  ylab("Trillions of 2019 Pounds") +
  ggtitle("Revisions to UK Consumption") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Even After Revisions, British Consumption Remains Below Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.5,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0.95-(.3*0.45), ymax = 0.95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSUMPTION_Graph, "Consumption Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NPISH_Graph <- ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = NPISH_old*4/1000, color = "Real UK Consumption of Nonprofit Institutions Serving Households, Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = NPISH_new*4/1000, color = "Real UK Consumption of Nonprofit Institutions Serving Households, New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1, prefix = "£"),limits = c(25,55), breaks = c(25,30,35,40,45,50,55), expand = c(0,0)) +
  ylab("Billions of 2019 Pounds") +
  ggtitle("Revisions to UK Consumption") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Real Services From UK Nonprofits Have Been Revised Upward") +
  theme_apricitas + theme(legend.position = c(.52,.1)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 25-(.3*30), ymax = 25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NPISH_Graph, "NPISH Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EXPORTS_Graph <- ggplot() +
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = exports_old*4/1000000, color = "Real UK Gross Exports: Old Data"), size = 1.25) + 
  geom_line(data = filter(GDP_Category_Breakdown, date >= as.Date("2018-01-01")), aes(x=date, y = exports_new*4/1000000, color = "Real UK Gross Exports: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.05, prefix = "£"),limits = c(.575,.775), breaks = c(.6,.65,.7,.75), expand = c(0,0)) +
  ylab("Billions of 2019 Pounds") +
  ggtitle("Revisions to UK Gross Exports") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Real Exports From the UK  Have Been Revised Upward") +
  theme_apricitas + theme(legend.position = c(.52,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = .575-(.3*.20), ymax = .575) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXPORTS_Graph, "Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

HEALTHCARE_REVISIONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/fcd5b3c8e452ecab364180724feebafec005cca5/UK/GDP%20Revisions/HEALTH_REVISIONS.csv") %>%
  mutate(date = as.Date(date))

HEALTHCARE_Graph <- ggplot() +
  geom_line(data = filter(HEALTHCARE_REVISIONS), aes(x=date, y = health_output_index_old, color = "Real UK Health and Social Work Activities: Old Data"), size = 1.25) + 
  geom_line(data = filter(HEALTHCARE_REVISIONS), aes(x=date, y = health_output_index_new, color = "Real UK Health and Social Work Activities: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(50,120), breaks = c(50,60,70,80,90,100,110), expand = c(0,0)) +
  ylab("Index: 2019 = 100") +
  ggtitle("Revisions to UK Healthcare Activities") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The Second-Largest Sectoral Driver of Upward Revisions to UK GDP Came from Healthcare") +
  theme_apricitas + theme(legend.position = c(.375,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 50-(.3*60), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HEALTHCARE_Graph, "Healthcare Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PRODUCTION_CVSM <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/fcd5b3c8e452ecab364180724feebafec005cca5/UK/GDP%20Revisions/PRODUCTION_CVM.csv") %>%
  mutate(date = as.Date(date))

PRODUCTION_Graph <- ggplot() +
  geom_line(data = filter(PRODUCTION_CVSM), aes(x=date, y = cvm_prod_old, color = "Real UK Production: Mining, Manufacturing, Energy, Water, and Waste Management: Old Data"), size = 1.25) + 
  geom_line(data = filter(PRODUCTION_CVSM), aes(x=date, y = cvm_prod_new, color = "Real UK Production: Mining, Manufacturing, Energy, Water, and Waste Management: New Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(85,110), breaks = c(85,90,95,100,105,110), expand = c(0,0)) +
  ylab("Index: 2019 = 100") +
  ggtitle("Revisions to UK Industrial Output") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Revisions Show That the UK's Industry Was Hit Harder by the Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.5,.105), legend.text = element_text(size = 12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 85-(.3*25), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PRODUCTION_Graph, "Production Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#I Messed up the labels here—the new data is filed under cvm_elec_old and the old data is under cvm_elec_new. 
ENERGY_Graph <- ggplot() +
  geom_line(data = filter(PRODUCTION_CVSM), aes(x=date, y = cvm_elec_new, color = "Real UK Electricity, Gas, Steam, and Air Conditioning Output: Old Data"), size = 1.25) + 
  geom_line(data = filter(PRODUCTION_CVSM), aes(x=date, y = cvm_elec_old, color = "Real UK Electricity, Gas, Steam, and Air Conditioning Output: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(55,135), breaks = c(60,70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index: 2019 = 100") +
  ggtitle("Revisions to UK Energy Output") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Revisions Show That the UK's Energy Sector Was Much Smaller in the Wake of Rising Prices") +
  theme_apricitas + theme(legend.position = c(.5,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 55-(.3*80), ymax = 55) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ENERGY_Graph, "Energy Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MANUFACTURING_Graph <- ggplot() +
  geom_line(data = filter(PRODUCTION_CVSM), aes(x=date, y = cvm_manu_old, color = "Real UK Manufacturing Output: Old Data"), size = 1.25) + 
  geom_line(data = filter(PRODUCTION_CVSM), aes(x=date, y = cvm_manu_new, color = "Real UK Manufacturing Output: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(80,115), breaks = c(90,90,100,110), expand = c(0,0)) +
  ylab("Index: 2019 = 100") +
  ggtitle("Revisions to UK Manufacturing Output") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Revisions Show That the UK's Industry Was Hit Harder by the Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.5,.975)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 80-(.3*35), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANUFACTURING_Graph, "Manufacturing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GOVERNMENT_CONSUMPTION_REVISIONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/fcd5b3c8e452ecab364180724feebafec005cca5/UK/GDP%20Revisions/GOVERNMENT_CONSUMPTION_REVISIONS.csv") %>%
  mutate(date = as.Date(date, format("%m/%d/%Y")))

GOVERNMENT_CONSUMPTION_REVISIONS_Graph <- ggplot() +
  geom_line(data = filter(GOVERNMENT_CONSUMPTION_REVISIONS), aes(x=date, y = edu_old*4/1000, color = "Real UK Government Output: Education: Old Data"), size = 1.25) + 
  geom_line(data = filter(GOVERNMENT_CONSUMPTION_REVISIONS), aes(x=date, y = edu_new*4/1000, color = "Real UK Government Output: Education: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(30,85), breaks = c(30,40,50,60,70,80), expand = c(0,0)) +
  ylab("Index: 2019 = 100") +
  ggtitle("Revisions to UK Government Education Output") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Revisions Show That the UK's Government Education Output Was Stronger than Previously Estimated") +
  theme_apricitas + theme(legend.position = c(.5,.95), plot.title = element_text(size = 24)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 30-(.3*55), ymax = 30) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GOVERNMENT_CONSUMPTION_REVISIONS_Graph, "Government Consumption Education Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


RETAIL_REVISIONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/UK/GDP%20Revisions/RETAIL_REVISIONS.csv") %>%
  mutate(date = as.Date(data))

RETAIL_REVISIONS_Graph <- ggplot() +
  geom_line(data = RETAIL_REVISIONS, aes(x=date, y = RETAIL_OLD, color = "Real UK Output of Wholesale & Retail Trade incl. Repair of Motor Vehicles: Old Data"), size = 1.25) + 
  geom_line(data = RETAIL_REVISIONS, aes(x=date, y = RETAIL_NEW, color = "Real UK Output of Wholesale & Retail Trade incl. Repair of Motor Vehicles: New Revised Data"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,110), breaks = c(70,80,90,100,110), expand = c(0,0)) +
  ylab("Index: 2019 = 100") +
  ggtitle("Revisions to UK Retail/Wholesale Output") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Revisions to Retail/Wholesale Trade Data Have Pushed up UK Economic Output") +
  theme_apricitas + theme(legend.position = c(.52,.95), plot.title = element_text(size = 25), legend.text = element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 65-(.3*45), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_REVISIONS_Graph, "Retail Revisions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
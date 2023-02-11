pacman::p_load(eurostat,restatapi,stringi,jsonlite,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

install_github("rOpenGov/eurostat")
library("eurostat")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


CHEMICALS_IMPORT_VOLUME <- get_eurostat("ext_st_eu27_2020sitc",
                      filters=c("IVOL_SCA","EXT_EU27_2020","SITC5","IMP"),
                      date_filter=">2018-01-01") %>%
                      mutate(time = as.Date(as.yearmon(time)))

EU_Chemical_Imports_Graph <- ggplot() + #EU chemical imports
  geom_line(data=CHEMICALS_IMPORT_VOLUME, aes(x=time,y= values/1.033,color= "EU-27 Imports of Chemicals and Related Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,170), breaks = c(100,120,140,160), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Elemental Shift") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Imports of Chemical Products are Increasing Amidst Natural Gas Shortages") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*80), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_Chemical_Imports_Graph, "EU Chemical Imports graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_CHEMICAL_IMPORTS_CHINA <- get_eurostat_data("ext_st_eu27_2020sitc",
                                               filters=c("TRD_VAL_SCA","CN_X_HK","SITC5","BAL_RT"),
                                               date_filter=">2018-01-01") %>%
                                               mutate(time = as.Date(as.yearmon(time)))
EU_CHEMICAL_IMPORTS_EUROS <- get_eurostat_data("ext_st_eu27_2020sitc",
                                               filters=c("TRD_VAL_SCA","EXT_EU27_2020","SITC5","BAL_RT"),
                                               date_filter=">2018-01-01") %>%
                                                mutate(time = as.Date(as.yearmon(time)))


EU_Chemical_Imports_China_Graph <- ggplot() + #EU chemical imports
  geom_line(data=EU_CHEMICAL_IMPORTS_CHINA, aes(x=time,y= values/1000,color= "EU-27 Net Exports of Chemicals and Related Products to China"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(-6.5,1.5), breaks = c(-6,-5,-4,-3,-2,-1,0,1), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("Elemental Shift") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "The EU Now Has a Trade Deficit in Chemical Products With China") +
  theme_apricitas + theme(legend.position = c(.50,.10)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -6.5-(.3*8), ymax = -6.5) +
  coord_cartesian(clip = "off")

EU_Chemical_Imports_Euros_Graph <- ggplot() + #EU chemical imports
  geom_line(data=EU_CHEMICAL_IMPORTS_EUROS, aes(x=time,y= values/1000,color= "EU-27 Net Exports of Chemicals and Related Products"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(0,20), breaks = c(0,5,10,15,20), expand = c(0,0)) +
  ylab("Billions of Euros") +
  ggtitle("Elemental Shift") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "The EU Still Has a Trade Surplus in Chemical Products") +
  theme_apricitas + theme(legend.position = c(.50,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*20), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_Chemical_Imports_China_Graph, "EU Chemical Imports China graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = EU_Chemical_Imports_Euros_Graph, "EU Chemical Imports Euros graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_CHEMICAL_IMPORTS_US <- get_eurostat_data("ext_st_eu27_2020sitc",
                                               filters=c("IVOL_SCA","US","SITC5","IMP"),
                                               date_filter=">2018-01-01") %>%
                                               mutate(time = as.Date(as.yearmon(time)))

EU_Chemicals_Imports_US_Index <- ggplot() + #EU chemical imports
  geom_line(data=EU_CHEMICAL_IMPORTS_US, aes(x=time,y= values/.909,color= "EU-27 Imports of Chemicals and Related Products from the US"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(90,170), breaks = c(100,120,140,160), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Elemental Shift") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Imports of Chemical Products are Increasing Amidst Natural Gas Shortages") +
  theme_apricitas + theme(legend.position = c(.50,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 90-(.3*80), ymax = 90) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_Chemicals_Imports_US_Index, "EU US Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_Manufacturing_Materials_Imports_Real <- get_eurostat_data("ext_st_eu27_2020sitc",
                                           filters=c("IVOL_SCA","EXT_EU27_2020","SITC6","IMP"),
                                           date_filter=">2018-01-01") %>%
                                           mutate(time = as.Date(as.yearmon(time)))


EU_Manufacturing_Materials_Exports_Real <- get_eurostat_data("ext_st_eu27_2020sitc",
                                              filters=c("IVOL_SCA","EXT_EU27_2020","SITC6","EXP"),
                                              date_filter=">2018-01-01") %>%
                                              mutate(time = as.Date(as.yearmon(time)))

EU_MANUFACTURING_MATERIALS_REAL <- ggplot() + #EU chemical imports
  geom_line(data=EU_Manufacturing_Materials_Imports_Real, aes(x=time,y= values/1.10,color= "EU-27 Imports of Manufactured Goods Classified Chiefly by Material"), size = 1.25) +
  geom_line(data=EU_Manufacturing_Materials_Exports_Real, aes(x=time,y= values/1.03,color= "EU-27 Exports of Manufactured Goods Classified Chiefly by Material"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,120), breaks = c(60,80,100,120), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Material Rise") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Imports of Manufactured Materials (Rubber, Paper, Steel, etc) are Rising") +
  theme_apricitas + theme(legend.position = c(.40,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*60), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MANUFACTURING_MATERIALS_REAL, "EU Real Materials.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_MANUFACTURING_MATERIALS_BALANCE_NOMINAL <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                           filters=c("TRD_VAL_SCA","EXT_EU27_2020","SITC6","BAL_RT"),
                                                           date_filter=">2018-01-01") %>%
                                                           mutate(time = as.Date(as.yearmon(time)))

EU_MANUFACTURING_MATERIALS_NOMINAL_graph <- ggplot() + #EU manufactured materials
  geom_line(data=EU_MANUFACTURING_MATERIALS_BALANCE_NOMINAL, aes(x=time,y= values/1000,color= "EU-27 Net Exports of Manufactured Goods Classified Chiefly by Material"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(-5,5), breaks = c(-5,0,5), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Material Drop") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "The EU Now Has a Trade Deficit in Manufactured Materials (Rubber, Paper, Steel, etc)") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -5-(.3*10), ymax = -5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MANUFACTURING_MATERIALS_NOMINAL_graph, "EU Manufacturing Materials Nominal.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_MACHINERY_TRANSPORT_BALANCE_NOMINAL <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                               filters=c("TRD_VAL_SCA","EXT_EU27_2020","SITC7","BAL_RT"),
                                                               date_filter=">2018-01-01") %>%
                                                               mutate(time = as.Date(as.yearmon(time)))

EU_MACHINERY_TRANSPORT_BALANCE_NOMINAL_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=EU_MACHINERY_TRANSPORT_BALANCE_NOMINAL, aes(x=time,y= values/1000,color= "EU-27 Net Exports of Machinery and Transport Equipment"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(-1,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Driving Down") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "The EU's Vaunted Trade Surpus in Machinery and Transport Equipment is Dropping") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -1-(.3*26), ymax = -1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_MACHINERY_TRANSPORT_BALANCE_NOMINAL_graph, "EU Transport Balance Nominal.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_China_Imports_Chemicals <- get_eurostat_data("ext_st_eu27_2020sitc",
                              filters=c("TRD_VAL_SCA","CN_X_HK","SITC5","IMP"),
                              date_filter=">2018-01-01") %>%
                              mutate(time = as.Date(as.yearmon(time)))

EU_China_Imports_Transport <- get_eurostat_data("ext_st_eu27_2020sitc",
                              filters=c("TRD_VAL_SCA","CN_X_HK","SITC7","IMP"),
                              date_filter=">2018-01-01") %>%
                              mutate(time = as.Date(as.yearmon(time)))


EU_China_Imports_Materials <- get_eurostat_data("ext_st_eu27_2020sitc",
                              filters=c("TRD_VAL_SCA","CN_X_HK","SITC6","IMP"),
                              date_filter=">2018-01-01") %>%
                              mutate(time = as.Date(as.yearmon(time)))


EU_CHINA_IMPORTS_NOMINAL_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=EU_China_Imports_Chemicals, aes(x=time,y= values/1000,color= "Chemicals and Related Products"), size = 1.25) +
  geom_line(data=EU_China_Imports_Transport, aes(x=time,y= values/1000,color= "Machinery and Transport Equipment"), size = 1.25) +
  geom_line(data=EU_China_Imports_Materials, aes(x=time,y= values/1000,color= "Manufactured Goods Classified Chiefly by Material"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Import Substitution") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Imports from China in Key Sectors are Rising") +
  theme_apricitas + theme(legend.position = c(.40,.80)) +
  scale_color_manual(name= "EU-27 Imports from China",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*35), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_CHINA_IMPORTS_NOMINAL_graph, "EU China Imports Nominal.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_US_Imports_Chemicals <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                filters=c("TRD_VAL_SCA","US","SITC5","IMP"),
                                                date_filter=">2018-01-01") %>%
                                                mutate(time = as.Date(as.yearmon(time)))

EU_US_Imports_Transport <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                filters=c("TRD_VAL_SCA","US","SITC7","IMP"),
                                                date_filter=">2018-01-01") %>%
                                                mutate(time = as.Date(as.yearmon(time)))


EU_US_Imports_Materials <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                filters=c("TRD_VAL_SCA","US","SITC6","IMP"),
                                                date_filter=">2018-01-01") %>%
                                                mutate(time = as.Date(as.yearmon(time)))


EU_US_IMPORTS_NOMINAL_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=EU_US_Imports_Chemicals, aes(x=time,y= values/1000,color= "Chemicals and Related Products"), size = 1.25) +
  geom_line(data=EU_US_Imports_Transport, aes(x=time,y= values/1000,color= "Machinery and Transport Equipment"), size = 1.25) +
  geom_line(data=EU_US_Imports_Materials, aes(x=time,y= values/1000,color= "Manufactured Goods Classified Chiefly by Material"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(0,13), breaks = c(0,5,10,15,20,25,30,35), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Import Substitution") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Imports from the US in Key Sectors are Rising") +
  theme_apricitas + theme(legend.position = c(.35,.85)) +
  scale_color_manual(name= "EU-27 Imports from the USA",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_US_IMPORTS_NOMINAL_graph, "EU US Imports Nominal.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_US_FUEL_IMPORTS_NOMINAL <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                filters=c("TRD_VAL_SCA","US","SITC3","IMP"),
                                                date_filter=">2018-01-01") %>%
                                                mutate(time = as.Date(as.yearmon(time)))

EU_RUSSIA_FUEL_IMPORTS_NOMINAL <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                    filters=c("TRD_VAL_SCA","RU","SITC3","IMP"),
                                                    date_filter=">2018-01-01") %>%
                                                    mutate(time = as.Date(as.yearmon(time)))

EU_US_RUSSIA_FUEL_IMPORTS_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=EU_US_FUEL_IMPORTS_NOMINAL, aes(x=time,y= values/1000,color= "United States"), size = 1.25) +
  geom_line(data=EU_RUSSIA_FUEL_IMPORTS_NOMINAL, aes(x=time,y= values/1000,color= "Russia"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(0,15), breaks = c(0,5,10,15,20,25,30,35), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Import Substitution") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Energy Imports from the US Have Caught Up With Russia") +
  theme_apricitas + theme(legend.position = c(.40,.85)) +
  scale_color_manual(name= "EU-27 Energy Imports (Mineral Fuels, Lubricants, & Related)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_US_RUSSIA_FUEL_IMPORTS_graph, "EU US Russia Energy Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

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
  mutate(partner = "Russia (including via Ukraine and Belarus)")

EU_US_GAS_IMPORTS <- get_eurostat_data("nrg_ti_gasm",
                      filters=c("EU27_2020","US","MIO_M3","G3000"),
                      date_filter=">2018-01-01") %>%
                      mutate(time = as.Date(as.yearmon(time)))

EU_US_RUSSIA_NAT_GAS_IMPORTS_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=EU_RU_GAS_IMPORTS, aes(x=time,y= values/1000,color= "Russia (including via Ukraine and Belarus)"), size = 1.25) +
  geom_line(data=EU_US_GAS_IMPORTS, aes(x=time,y= values/1000,color= "United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B"),limits = c(0,12), breaks = c(0,5,10), expand = c(0,0)) +
  ylab("Cubic Meters") +
  ggtitle("Import Substitution") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "The EU Now Gets More Natural Gas from the US than Russia") +
  theme_apricitas + theme(legend.position = c(.35,.4)) +
  scale_color_manual(name= "EU-27 Imports of Natural Gas",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*12), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_US_RUSSIA_NAT_GAS_IMPORTS_graph, "EU US Russia Nat Gas Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#European Industrial Production
EU_27_IP_TEXTILES <- get_eurostat_data("sts_inpr_m",
                                    filters=c("EU27_2020","SCA","I15","C13-C15"),
                                    date_filter=">2018-01-01") %>%
                                    mutate(time = as.Date(as.yearmon(time))) %>%
                                    mutate(values = (values/values[1])*100)

EU_27_IP_CHEMICAL <- get_eurostat_data("sts_inpr_m",
                                       filters=c("EU27_2020","SCA","I15","C20"),
                                       date_filter=">2018-01-01") %>%
                                       mutate(time = as.Date(as.yearmon(time))) %>%
                                       mutate(values = (values/values[1])*100)

EU_27_IP_METALS <- get_eurostat_data("sts_inpr_m",
                                     filters=c("EU27_2020","SCA","I15","C24"),
                                     date_filter=">2018-01-01") %>%
                                     mutate(time = as.Date(as.yearmon(time))) %>%
                                     mutate(values = (values/values[1])*100)

EU_27_IP_MAIN_graph <- ggplot() + #EU Main Industrial Production
  #geom_line(data=EU_27_IP_TEXTILES, aes(x=time,y= values,color= "Textiles, Apparel, Leather and Related"), size = 1.25) +
  geom_line(data=EU_27_IP_CHEMICAL, aes(x=time,y= values,color= "Chemicals and Chemical Products"), size = 1.25) +
  geom_line(data=EU_27_IP_METALS, aes(x=time,y= values,color= "Basic Metals"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,110), breaks = c(40,60,80,100,120), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Production Problems") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "The EU's Metal and Chemical Industries are Struggling Amidst the Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.25,.35)) +
  scale_color_manual(name= "EU-27 Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*50), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_27_IP_MAIN_graph, "EU_27_IP_MAIN_graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_27_IP_MANUFACTURING <- get_eurostat_data("sts_inpr_m",
                                            filters=c("EU27_2020","SCA","I15","C"),
                                            date_filter=">2018-01-01") %>%
                                            mutate(time = as.Date(as.yearmon(time))) %>%
                                            mutate(values = (values/values[1])*100)

GERMANY_IP_MANUFACTURING <- get_eurostat_data("sts_inpr_m",
                                              filters=c("DE","SCA","I15","C"),
                                              date_filter=">2018-01-01") %>%
                                              mutate(time = as.Date(as.yearmon(time))) %>%
                                              mutate(values = (values/values[1])*100)

FRANCE_IP_MANUFACTURING <- get_eurostat_data("sts_inpr_m",
                                             filters=c("FR","SCA","I15","C"),
                                             date_filter=">2018-01-01") %>%
                                             mutate(time = as.Date(as.yearmon(time))) %>%
                                             mutate(values = (values/values[1])*100)

SPAIN_IP_MANUFACTURING <- get_eurostat_data("sts_inpr_m",
                                            filters=c("ES","SCA","I15","C"),
                                            date_filter=">2018-01-01") %>%
                                            mutate(time = as.Date(as.yearmon(time))) %>%
                                            mutate(values = (values/values[1])*100)

ITALY_IP_MANUFACTURING <- get_eurostat_data("sts_inpr_m",
                                            filters=c("IT","SCA","I15","C24"),
                                            date_filter=">2018-01-01") %>%
                                            mutate(time = as.Date(as.yearmon(time))) %>%
                                            mutate(values = (values/values[1])*100)

EU_27_IP_MANUFACTURING_graph <- ggplot() + #EU Main Industrial Production
  geom_line(data=EU_27_IP_MANUFACTURING, aes(x=time,y= values,color= "EU-27"), size = 1.25) +
  geom_line(data=GERMANY_IP_MANUFACTURING, aes(x=time,y= values,color= "Germany"), size = 1.25) +
  geom_line(data=FRANCE_IP_MANUFACTURING, aes(x=time,y= values,color= "France"), size = 1.25) +
  geom_line(data=SPAIN_IP_MANUFACTURING, aes(x=time,y= values,color= "Spain"), size = 1.25) +
  geom_line(data=ITALY_IP_MANUFACTURING, aes(x=time,y= values,color= "Italy"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(39,110), breaks = c(40,60,80,100,120), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Bent, Not Broken") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "European Manufacturing is Taking a Hit From the Energy Crisis, But Hasn't Collapsed") +
  theme_apricitas + theme(legend.position = c(.25,.35)) +
  scale_color_manual(name= "Industrial Production, Manufacturing",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("EU-27","Germany","France","Italy","Spain")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 40-(.3*71), ymax = 40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_27_IP_MANUFACTURING_graph, "EU_27_IP_MANUFACTURING_graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EU_27_IP_PLASTICS <- get_eurostat_data("sts_inpr_m",
                                       filters=c("EU27_2020","SCA","I15","C2016"),
                                       date_filter=">2018-01-01") %>%
                                       mutate(time = as.Date(as.yearmon(time))) %>%
                                       mutate(values = (values/values[1])*100)

EU_27_IP_RUBBER <- get_eurostat_data("sts_inpr_m",
                                       filters=c("EU27_2020","SCA","I15","C2017"),
                                       date_filter=">2018-01-01") %>%
                                       mutate(time = as.Date(as.yearmon(time))) %>%
                                       mutate(values = (values/values[1])*100)

EU_27_IP_FIBRES <- get_eurostat_data("sts_inpr_m",
                                     filters=c("EU27_2020","SCA","I15","C206"),
                                     date_filter=">2018-01-01") %>%
                                     mutate(time = as.Date(as.yearmon(time))) %>%
                                     mutate(values = (values/values[1])*100)

EU_27_IP_CHEMICALS_graph <- ggplot() + #EU Chemicals Industrial Production
  geom_line(data=EU_27_IP_PLASTICS, aes(x=time,y= values,color= "Plastics in Primary Form"), size = 1.25) +
  geom_line(data=EU_27_IP_RUBBER, aes(x=time,y= values,color= "Synthetic Rubber in Primary Form"), size = 1.25) +
  geom_line(data=EU_27_IP_FIBRES, aes(x=time,y= values,color= "Man-Made Fibres"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(60,110), breaks = c(40,60,80,100,120), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Production Problems") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Chemical Manufacturing is Lagging Amidst High Energy Prices and a Natural Gas Shortage") +
  theme_apricitas + theme(legend.position = c(.25,.35)) +
  scale_color_manual(name= "EU-27 Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 60-(.3*50), ymax = 60) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_27_IP_CHEMICALS_graph, "EU_27_IP_CHEMICALS_graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_27_IP_IRON <- get_eurostat_data("sts_inpr_m",
                                     filters=c("EU27_2020","SCA","I15","C241"),
                                     date_filter=">2018-01-01") %>%
                                     mutate(time = as.Date(as.yearmon(time))) %>%
                                     mutate(values = (values/values[1])*100)

EU_27_IP_ALUMINIUM <- get_eurostat_data("sts_inpr_m",
                                     filters=c("EU27_2020","SCA","I15","C2442"),
                                     date_filter=">2018-01-01") %>%
                                     mutate(time = as.Date(as.yearmon(time))) %>%
                                     mutate(values = (values/values[1])*100)

EU_27_IP_COPPER <- get_eurostat_data("sts_inpr_m",
                                     filters=c("EU27_2020","SCA","I15","C2444"),
                                     date_filter=">2018-01-01") %>%
                                     mutate(time = as.Date(as.yearmon(time))) %>%
                                     mutate(values = (values/values[1])*100)

EU_27_IP_CASTING <- get_eurostat_data("sts_inpr_m",
                                     filters=c("EU27_2020","SCA","I15","C245"),
                                     date_filter=">2018-01-01") %>%
                                     mutate(time = as.Date(as.yearmon(time))) %>%
                                     mutate(values = (values/values[1])*100)

EU_27_IP_FABRICATED <- get_eurostat_data("sts_inpr_m",
                                      filters=c("EU27_2020","SCA","I15","C25"),
                                      date_filter=">2018-01-01") %>%
                                      mutate(time = as.Date(as.yearmon(time))) %>%
                                      mutate(values = (values/values[1])*100)


EU_27_IP_METALS_graph <- ggplot() + #EU Metals Industrial Production
  geom_line(data=EU_27_IP_IRON, aes(x=time,y= values,color= "Iron, Steel, and Ferroalloys"), size = 1.25) +
  geom_line(data=EU_27_IP_ALUMINIUM, aes(x=time,y= values,color= "Aluminium"), size = 1.25) +
  geom_line(data=EU_27_IP_COPPER, aes(x=time,y= values,color= "Copper"), size = 1.25) +
  geom_line(data=EU_27_IP_CASTING, aes(x=time,y= values,color= "Casting of Metals"), size = 1.25) +
  geom_line(data=EU_27_IP_FABRICATED, aes(x=time,y= values,color= "Fabricated Metal Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(39,110), breaks = c(40,60,80,100,120), expand = c(0,0)) +
  ylab("Volume Index, Jan 2018 = 100") +
  ggtitle("Production Problems") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Metal Manufacturing is Lagging Amidst High Energy Prices and a Natural Gas Shortage") +
  theme_apricitas + theme(legend.position = c(.3,.35)) +
  scale_color_manual(name= "EU-27 Industrial Production",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 39-(.3*71), ymax = 39) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_27_IP_METALS_graph, "EU_27_IP_METALS_graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Stacked Graph
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

#Aggregate Energy Imports
EU_NOMINAL_ENERGY <- get_eurostat_data("ext_st_eu27_2020sitc",
                                                    filters=c("TRD_VAL_SCA","EXT_EU27_2020","SITC3","BAL_RT"),
                                                    date_filter=">2018-01-01") %>%
                                                    mutate(time = as.Date(as.yearmon(time)))

EU_NOMINAL_ENERGY_graph <- ggplot() + #EU machinery and transport equipment
  geom_line(data=EU_NOMINAL_ENERGY, aes(x=time,y= -values/1000,color= "EU-27 Net Energy Imports (Mineral Fuels, Lubricants, & Related)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "€", suffix = "B"),limits = c(0,70), breaks = c(0,10,20,30,40,50,60,70), expand = c(0,0)) +
  ylab("Billions of Euros, Monthly") +
  ggtitle("Paying the Energy Bill") +
  labs(caption = "Graph created by @JosephPolitano using EuroStat data",subtitle = "EU Energy Import Costs Rose to Record Levels and Remain Extremely High") +
  theme_apricitas + theme(legend.position = c(.45,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*70), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_NOMINAL_ENERGY_graph, "EU Nominal Energy.png", type = "cairo-png") #cairo gets rid of anti aliasing

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

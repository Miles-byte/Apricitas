install_github("keberwein/blscrapeR")
library(blscrapeR)


CHINA_IMPORT_PRICES <- bls_api("EIUCOCHNTOT", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value/value[37]*100) %>%
  mutate(value_tariff = case_when(
    date == as.Date("2025-02-01") ~ value * 1.1,
    date == as.Date("2025-03-01") ~ value * 1.2,
    date == as.Date("2025-04-01") ~ NA,
    TRUE ~ value
  )) %>%
  mutate(value_tariff_proj = case_when(
    date == as.Date("2025-02-01") ~ value * 1.1,
    date == as.Date("2025-03-01") ~ value * 1.2,
    date == as.Date("2025-04-01") ~ value * 1.94,
    TRUE ~ value
  )) 

CHINA_IMPORT_PRICES_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=CHINA_IMPORT_PRICES, aes(x=date,y= value_tariff,color= "Including Tariffs"), size = 1.25) + 
  geom_line(data=CHINA_IMPORT_PRICES, aes(x=date,y= value_tariff_proj,color= "Including Tariffs"), size = 1.25, linetype = "dashed") + 
  geom_line(data=CHINA_IMPORT_PRICES, aes(x=date,y= value ,color= "Prices for US Imports from China"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,200), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Import Price Index, Jan 2025 = 100, NSA") +
  ggtitle("Americans are Paying the Costs of Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data. NOTE: April Tariff Projection Based on 74% Increase Including Exemptions",subtitle = "Chinese Import Prices Have Barely Fallen—Implying Americans are Primarily Absorbing the Costs") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Prices for US Imports from China","Including Tariffs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_IMPORT_PRICES_GRAPH, "China Import Prices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



PACIFIC_RIM_CARS <- bls_api("EIUCOPRIMZ3361", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value/value[37]*100) %>%
  mutate(value_tariff = case_when(
    date >= as.Date("2025-04-01") ~ value*1.25,
    TRUE ~ value
  ))


PACIFIC_RIM_CARS_PRICES_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=PACIFIC_RIM_CARS, aes(x=date,y= value_tariff,color= "Including Tariffs"), size = 1.25) + 
  geom_line(data=PACIFIC_RIM_CARS, aes(x=date,y= value ,color= "Prices for US Car Imports from Pacific Rim (Japan/Korea)"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Import Price Index, Jan 2025 = 100, NSA") +
  ggtitle("Americans are Paying the Costs of Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Pacific Rim Car Import Prices Are Rising—Implying Americans are Primarily Absorbing the Costs") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Prices for US Car Imports from Pacific Rim (Japan/Korea)","Including Tariffs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PACIFIC_RIM_CARS_PRICES_GRAPH, "Pacific Rim Car Prices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


EU_CARS <- bls_api("EIUCOEECZ3361", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value/value[37]*100) %>%
  mutate(value_tariff = case_when(
    date >= as.Date("2025-04-01") ~ value*1.25,
    TRUE ~ value
  ))


EU_CARS_PRICES_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=EU_CARS, aes(x=date,y= value_tariff,color= "Including Tariffs"), size = 1.25) + 
  geom_line(data=EU_CARS, aes(x=date,y= value ,color= "Prices for US Car Imports from European Union"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Import Price Index, Jan 2025 = 100, NSA") +
  ggtitle("Americans are Paying the Costs of Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "EU Car Import Prices Are Flat—Implying Americans are Primarily Absorbing the Costs") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Prices for US Car Imports from European Union","Including Tariffs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_CARS_PRICES_GRAPH, "EU Car Prices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MX_CARS <- bls_api("EIUCOMEXZ3361", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value/value[26]*100) %>%
  mutate(value_tariff = case_when(
    date >= as.Date("2025-04-01") ~ value*1.25,
    TRUE ~ value
  ))



MX_CARS_PRICES_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=MX_CARS, aes(x=date,y= value_tariff,color= "Including Tariffs (Varying, Up to 25% Depending on Make/Model)"), size = 0.75, linetype = "dashed") + 
  geom_line(data=MX_CARS, aes(x=date,y= value ,color= "Prices for US Car Imports from Mexico"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Import Price Index, Jan 2025 = 100, NSA") +
  ggtitle("Americans are Paying the Costs of Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Mexican Car Import Prices Are Flat—Implying Americans are Primarily Absorbing the Costs") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Prices for US Car Imports from Mexico","Including Tariffs (Varying, Up to 25% Depending on Make/Model)"), guide=guide_legend(override.aes=list(lwd = c(1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-12-01")-(.1861*(today()-as.Date("2022-12-01"))), xmax = as.Date("2022-12-01")-(0.049*(today()-as.Date("2022-12-01"))), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MX_CARS_PRICES_GRAPH, "MX Car Prices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


ALL_CARS <- bls_api("EIUCOINDUSZ3361", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value/value[37]*100) %>%
  mutate(value_tariff = case_when(
    date >= as.Date("2025-04-01") ~ value*1.25,
    TRUE ~ value
  ))



ALL_CARS_PRICES_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=ALL_CARS, aes(x=date,y= value_tariff,color= "Including Tariffs (Varying, Up to 25% Depending on Make/Model/Country)"), size = 0.75, linetype = "dashed") + 
  geom_line(data=ALL_CARS, aes(x=date,y= value ,color= "Prices for US Car Imports"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Import Price Index, Jan 2025 = 100, NSA") +
  ggtitle("Americans are Paying the Costs of Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data.",subtitle = "Mexican Car Import Prices Are Flat—Implying Americans are Primarily Absorbing the Costs") +
  theme_apricitas + theme(legend.position = c(.46,.90), plot.title = element_text(size = 27)) +
  theme(legend.key.width =  unit(.82, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Prices for US Car Imports","Including Tariffs (Varying, Up to 25% Depending on Make/Model/Country)"), guide=guide_legend(override.aes=list(lwd = c(1.25,0.75)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_CARS_PRICES_GRAPH, "All Car Prices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


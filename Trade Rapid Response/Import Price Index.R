install_github("keberwein/blscrapeR")
library(blscrapeR)


CHINA_IMPORT_PRICES <- bls_api("EIUCOCHNTOT", startyear = 2022, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(value = value/value[37]*100) %>%
  mutate(value_tariff = case_when(
    date >= as.Date("2025-03-01") ~ value * 1.2,
    date >= as.Date("2025-02-01") ~ value * 1.1,
    TRUE ~ value
  ))

CHINA_IMPORT_PRICES_GRAPH <- ggplot() + #plotting integrated circuits exports
  geom_line(data=CHINA_IMPORT_PRICES, aes(x=date,y= value_tariff,color= "Including Tariffs"), size = 1.25) + 
  geom_line(data=CHINA_IMPORT_PRICES, aes(x=date,y= value ,color= "Prices for US Imports from China"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(97.5,112.5), breaks = c(95,100,105,110,115), expand = c(0,0)) +
  ylab("Import Price Index, Jan 2025 = 100, NSA") +
  ggtitle("Americans are Paying the Costs of Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices for Chinese Imports Haven't Fallen—Implying Americans are Primarily Absorbing the Costs") +
  theme_apricitas + theme(legend.position = c(.42,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC"), breaks = c("Prices for US Imports from China","Including Tariffs")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 97.5-(.3*15), ymax = 97.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_IMPORT_PRICES_GRAPH, "China Import Prices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

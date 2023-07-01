pacman::p_load(censusapi,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Graphing US Net Energy Exports
US_ENERGY_EXPORTS <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("MONTH", "YEAR", "ALL_VAL_MO", "E_COMMODITY", "CTY_CODE"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  E_COMMODITY = "27", #energy commodity code
  #CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(value = as.numeric(ALL_VAL_MO)) %>%
  select(time, value)

US_ENERGY_IMPORTS <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("MONTH", "YEAR", "GEN_VAL_MO", "I_COMMODITY", "CTY_CODE"), 
  time = paste("from 2013 to", format(Sys.Date(), "%Y")),
  I_COMMODITY = "27", #energy commodity code
  #CTY_CODE = "4XXX", # europe country code
  CTY_CODE = "-" #world country code
) %>%
  mutate(time = as.Date(as.yearmon(time))) %>%
  mutate(value = as.numeric(GEN_VAL_MO)) %>%
  select(time, value)

US_ENERGY_NET_EXP_MERGE <- merge(US_ENERGY_IMPORTS,US_ENERGY_EXPORTS, by = "time") %>%
  mutate(Net = value.y-value.x)

US_ENERGY_NET_EXP_Graph <- ggplot() + #plotting nat gas exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = US_ENERGY_NET_EXP_MERGE, aes(x = time, y = Net/1000000000, color = "US Net Energy Exports"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(-25,10), breaks = c(-25,-20,-15,-10,-5,0,5,10), expand = c(0,0)) +
  ylab("Dollars") +
  ggtitle("The US is Now an Energy Exporter") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "US Net Energy Exports are at a Modern Record High") +
  theme_apricitas + theme(legend.position = c(.20,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -25-(.3*35), ymax = -25) +
  coord_cartesian(clip = "off")

#US Import Price Index Growth
IR <- fredr(series_id = "IR", observation_start = as.Date("2019-01-01"), units = "pc1")
IRLFE <- fredr(series_id = "IREXFDFLS", observation_start = as.Date("2019-01-01"), units = "pc1")

IMPORT_PRICE_INDEX_Graph <- ggplot() + #plotting import price index
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = IR, aes(x = date, y = value/100, color = "Import Price Index (End Use)"), size = 1.25) +
  geom_line(data = IRLFE, aes(x = date, y = value/100, color = "Import Price Index (End Use) Less Food and Fuels"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.075,.15), breaks = c(-0.05,0,0.05,0.1,0.15), expand = c(0,0)) +
  ylab("Percent Change From Last Year") +
  ggtitle("Import Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "US Import Price Growth is High but Declining") +
  theme_apricitas + theme(legend.position = c(.33,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.075-(.3*.225), ymax = -0.075) +
  coord_cartesian(clip = "off")

#Nominal Broad US Dollar Index
NOMINAL_BROAD_DOLLAR <- fredr(series_id = "DTWEXBGS", observation_start = as.Date("2019-01-01")) %>%
  drop_na()

NOMINAL_BROAD_DOLLAR_Graph <- ggplot() + #plotting nominal broad US dollar index
  geom_line(data = NOMINAL_BROAD_DOLLAR, aes(x = date, y = value/1.157681, color = "Nominal Broad (Trade-Weighted) US Dollar Index"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(95,112.5), breaks = c(95,100,105,110), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("Dollar Dominance") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The US Dollar Has Appreciated Significantly Since Mid-2021 Thanks to a Confluence of Factors") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 95-(.3*17.5), ymax = 95) +
  coord_cartesian(clip = "off")

#us Imports Share of GDP
IMPORTS <- fredr(series_id = "IMPGS", observation_start = as.Date("1990-01-01"))
GDP <- fredr(series_id = "GDP", observation_start = as.Date("1990-01-01"))

IMPORTS_SHARE_GDP <- merge(IMPORTS,GDP, by = "date") %>%
  mutate(value = value.x/value.y) %>%
  select(date, value)

IMPORTS_SHARE_GDP_Graph <- ggplot() + #plotting import price index
  geom_line(data = IMPORTS_SHARE_GDP, aes(x = date, y = value, color = "US Imports of Goods and Services as a Share of GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.20), breaks = c(-0.05,0,0.05,0.1,0.15,0.20), expand = c(0,0)) +
  ylab("Percent of GDP") +
  ggtitle("Import Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Imports Make Up a Relatively Low Share of GDP") +
  theme_apricitas + theme(legend.position = c(.43,.34)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*.2), ymax = 0) +
  coord_cartesian(clip = "off")

#FX Contribution Graph

FX_CONTRIB <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Can%20a%20Strong%20Dollar%20Stop%20Inflation/FX_Exchange_Contrib.csv") %>%
  mutate(Date = as.Date(Date))

FX_CONTRIB_Graph <- ggplot(FX_CONTRIB, aes(fill="Exchange Rate Contribution to Core PCE Inflation", x=Date, y=FX_Contrib/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = FX_CONTRIB, aes(x=Date, y = Core_Inflation/100, color = "Core PCE Inflation"), size = 1.25) +
  geom_line(data = FX_CONTRIB, aes(x=Date, y = Import_Contrib/100, color = "Import Price Contribution to Core PCE Inflation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.01,0.055), breaks = c(-0.01,0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Inflation and a Stronger Dollar") +
  labs(caption = "Graph created by @JosephPolitano using Matschke and Sattiraju (2022) data",subtitle = "Dollar Appreciation Has Likely Had Limited Impacts on Overall Inflation") +
  theme_apricitas + theme(legend.position = c(.42,.67), legend.spacing.y = unit(-0.2, "cm")) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D")) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#A7ACD9"), breaks = c("Exchange Rate Contribution to Core PCE Inflation")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -.01-(.3*0.065), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ENERGY_NET_EXP_Graph, "US Net Energy Exports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = IMPORT_PRICE_INDEX_Graph, "US Import Price Indices.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = NOMINAL_BROAD_DOLLAR_Graph, "Nominal Broad US Dollar Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = IMPORTS_SHARE_GDP_Graph, "Imports Share of GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = FX_CONTRIB_Graph, "FX Contrib.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
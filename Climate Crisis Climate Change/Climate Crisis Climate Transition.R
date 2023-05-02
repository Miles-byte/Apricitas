pacman::p_load(eia,eurostat,restatapi,bea.R,sidrar,htmltools,devtools,onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

p_load(rjson)

eia_get("c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d","ELEC.GEN.TSN-US-99.A")

df <- eia_get(
  api_key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d",
  api_url = "https://api.eia.gov/v2/seriesid/WNTCBUS",
  data = "value"
)

SOLAR <- eia_series("ELEC.GEN.TSN-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d")%>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Solar")
WIND <- eia_series("ELEC.GEN.WND-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Wind")
HYDRO <- eia_series("ELEC.GEN.HYC-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Hydro")
NUCLEAR <- eia_series("ELEC.GEN.NUC-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Nuclear")
COAL <- eia_series("ELEC.GEN.COW-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Coal")
NATURAL_GAS <- eia_series("ELEC.GEN.NG-US-99.A", start = 2001, key = "c6Lz6T5XAYcd7V4uXueRVFQV7vanUycmwngbbH2d") %>%
  unnest(data) %>%
  select(date, value) %>%
  mutate(sector = "Natural Gas")

ALL_SECTOR_GENERATION <- rbind(SOLAR,WIND,HYDRO,NUCLEAR,COAL,NATURAL_GAS) %>%
  mutate(sector = factor(sector, levels = c("Coal","Natural Gas","Nuclear","Hydro","Solar","Wind")))

ALL_SECTOR_GENERATION_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = ALL_SECTOR_GENERATION, aes(x = date, y = value/1000000, fill = sector), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "PWh"),limits = c(0,4.2), breaks = c(0,1,2,3,4), expand = c(0,0)) +
  ylab("PWh") +
  ggtitle("Going Green") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "Low Carbon Energy Sources Make Up a Record Share of America's Electricity") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#FFE98F","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_SECTOR_GENERATION_GRAPH, "All Sector Generation Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NUCLEAR_GENERATION_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = NUCLEAR, aes(x = date, y = value/1000000, color = "Nuclear"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, suffix = "TWh"),limits = c(.750,.850), breaks = c(.750,.800,.850), expand = c(0,0)) +
  ylab("TWh") +
  ggtitle("Nuclear Falloff") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "US Nuclear Power Generation Has Fallen to a 10-Year Low") +
  theme_apricitas + theme(legend.position = "right") +
  scale_color_manual(name= NULL,values = c("#EE6055","#A7ACD9","#00A99D","#3083DC","#FFE98F","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

#Downloading data for Ember Climate's Annual Electricity Report. Dates likely have to be updated manually each year.
EMBER_ELECTRICITY <- read.csv("https://ember-climate.org/app/uploads/2022/07/yearly_full_release_long_format.csv")

WORLD_POWER_GENERATION <- EMBER_ELECTRICITY %>%
  subset(Area == "World") %>%
  subset(Subcategory == "Fuel") %>%
  subset(Category == "Electricity generation") %>%
  subset(Unit == "TWh") %>%
  select(Year, Variable, Value) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(`Bio & Other Renew.` = Bioenergy + `Other Renewables`) %>%
  mutate(`Natural Gas` = `Gas`) %>%
  mutate(`Other Fossil Fuels` = `Other Fossil`) %>%
  select(-`Other Fossil`,-Bioenergy,-`Other Renewables`,-`Gas`) %>%
  pivot_longer(cols = Coal:`Other Fossil Fuels`) %>%
  mutate(name = factor(name, levels = c("Other Fossil Fuels","Coal","Natural Gas","Bio & Other Renew.","Nuclear","Hydro","Solar","Wind")))

ALL_SECTOR_GENERATION_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = WORLD_POWER_GENERATION, aes(x = Year, y = value/1000, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "PWh"),limits = c(0,30), breaks = c(0,10,20,30), expand = c(0,0)) +
  ylab("PWh") +
  ggtitle("Global Power Generation") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "Low Carbon Energy Sources Make Up a Record Share of Global Electricity") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#FF8E72","#EE6055","#A7ACD9","#6A4C93","#00A99D","#3083DC","#FFE98F","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("1994-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("1994-01-01"))), ymin = 0-(.3*30), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ALL_SECTOR_GENERATION_GRAPH, "All Sector Generation Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHINA_COAL_SHARE <- EMBER_ELECTRICITY %>%
  subset(Area %in% c("World","China")) %>%
  subset(Subcategory == "Fuel") %>%
  subset(Category == "Electricity generation") %>%
  subset(Unit == "TWh") %>%
  subset(Variable == "Coal") %>%
  select(Year, Area, Value) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Area, values_from = "Value") %>%
  transmute(Year, China, `Rest of World` = World-China) %>%
  pivot_longer(cols = c(China, `Rest of World`)) %>%
  mutate(name = factor(name, levels = c("Rest of World","China")))

CHINA_COAL_SHARE_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = CHINA_COAL_SHARE, aes(x = Year, y = value/1000, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "PWh"),limits = c(0,12.5), breaks = c(0,5,10,15), expand = c(0,0)) +
  ylab("PWh") +
  ggtitle("King Coal") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "China Now Makes Up the Majority of the World's Coal Electricity") +
  theme_apricitas + theme(legend.position = c(.2,.8)) +
  scale_fill_manual(name= "Coal Electricity Generation",values = c("#EE6055","#FFE98F","#364652","#A7ACD9","#6A4C93","#00A99D","#3083DC","#9A348E"), breaks = c("China", "Rest of World")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*12.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_COAL_SHARE_GRAPH, "China Coal Share.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

WORLD_EMISSIONS <- read.csv("C:/Users/josep/Documents/Climate Crisis Climate Transition/CO2_YEARLY.csv") %>%
  mutate(year = as.Date(year))

WORLD_EMISSIONS_GRAPH <- ggplot() +
  geom_line(data=WORLD_EMISSIONS, aes(x=year,y= emissions,color= "Global CO2 Emissions From Energy and Industrial Processes"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "Gt"),limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Gt") +
  ggtitle("The Global Climate Crisis") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Global CO2 Emissions Hit a Record High in 2022") +
  theme_apricitas + theme(legend.position = c(.40,.875)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*(today()-as.Date("1950-01-01"))), xmax = as.Date("1950-01-01")-(0.049*(today()-as.Date("1950-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WORLD_EMISSIONS_GRAPH, "World Emissions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_COAL <- EMBER_ELECTRICITY %>%
  subset(Area %in% c("World","EU")) %>%
  subset(Subcategory == "Fuel") %>%
  subset(Category == "Electricity generation") %>%
  subset(Unit == "TWh") %>%
  subset(Variable == "Coal") %>%
  select(Year, Area, Value) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Area, values_from = "Value")

EU_COAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = EU_COAL, aes(x = Year, y = EU, fill = "European Union Coal Electricity Production"), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "TWh"),limits = c(0,860), breaks = c(0,200,400,600,800), expand = c(0,0)) +
  ylab("TWh") +
  ggtitle("Bringing Back Coal") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "European Union Coal Electricity Consumption Rebounded in 2022") +
  theme_apricitas + theme(legend.position = c(.7,.95)) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#FFE98F","#364652","#A7ACD9","#6A4C93","#00A99D","#3083DC","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*800), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_COAL_GRAPH, "EU COAL GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GLOBAL_WIND_SOLAR <- EMBER_ELECTRICITY %>%
  subset(Area == c("World")) %>%
  subset(Subcategory == "Fuel") %>%
  subset(Category == "Electricity generation") %>%
  subset(Unit == "TWh") %>%
  subset(Variable %in% c("Solar","Wind")) %>%
  select(Year, Value, Variable) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Variable, values_from = "Value")

GLOBAL_WIND_SOLAR_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = GLOBAL_WIND_SOLAR, aes(x = Year, y = Solar/1000, color = "Solar"), size = 1.25) +
  geom_line(data = GLOBAL_WIND_SOLAR, aes(x = Year, y = Wind/1000, color = "Wind"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "PWh"),limits = c(0,2.5), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("PWh") +
  ggtitle("The Renewable Revolution") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "Global Wind and Solar Power Generation is Growing Exponentially") +
  theme_apricitas + theme(legend.position = c(.6,.75)) +
  scale_color_manual(name= "Global Electricity Generation",values = c("#9A348E","#FFE98F","#364652","#A7ACD9","#6A4C93","#00A99D","#3083DC","#9A348E"), breaks = c("Wind","Solar")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*2.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLOBAL_WIND_SOLAR_GRAPH, "GLOBAL WIND SOLAR GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_POWER_GENERATION <- EMBER_ELECTRICITY %>%
  subset(Area == "United States of America") %>%
  subset(Subcategory == "Fuel") %>%
  subset(Category == "Electricity generation") %>%
  subset(Unit == "TWh") %>%
  select(Year, Variable, Value) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(`Bio & Other Renew.` = Bioenergy + `Other Renewables`) %>%
  mutate(`Natural Gas` = `Gas`) %>%
  mutate(`Other Fossil Fuels` = `Other Fossil`) %>%
  select(-`Other Fossil`,-Bioenergy,-`Other Renewables`,-`Gas`) %>%
  pivot_longer(cols = Coal:`Other Fossil Fuels`) %>%
  mutate(name = factor(name, levels = c("Other Fossil Fuels","Coal","Natural Gas","Bio & Other Renew.","Nuclear","Hydro","Solar","Wind")))

US_ALL_SECTOR_GENERATION_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = US_POWER_GENERATION, aes(x = Year, y = value/1000, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "PWh"),limits = c(0,4.5), breaks = c(0,1,2,3,4), expand = c(0,0)) +
  ylab("PWh") +
  ggtitle("US Power Generation") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "Low Carbon Energy Sources Make Up a Record Share of US Electricity") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#FF8E72","#EE6055","#A7ACD9","#6A4C93","#00A99D","#3083DC","#FFE98F","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("1994-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("1994-01-01"))), ymin = 0-(.3*4.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_ALL_SECTOR_GENERATION_GRAPH, "US All Sector Generation Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_INTENSITY <- EMBER_ELECTRICITY %>%
  subset(Area %in% c("Germany","France","Italy","Spain","Poland","EU")) %>%
  #subset(Subcategory == "Fuel") %>%
  #subset(Category == "Electricity generation") %>%
  subset(Unit == "gCO2/kWh") %>%
  select(Year, Value, Area) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Area, values_from = "Value")

EU_INTENSITY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = EU_INTENSITY, aes(x = Year, y = France, color = "France"), size = 1.25) +
  geom_line(data = EU_INTENSITY, aes(x = Year, y = Germany, color = "Germany"), size = 1.25) +
  geom_line(data = EU_INTENSITY, aes(x = Year, y = Italy, color = "Italy"), size = 1.25) +
  geom_line(data = EU_INTENSITY, aes(x = Year, y = Spain, color = "Spain"), size = 1.25) +
  geom_line(data = EU_INTENSITY, aes(x = Year, y = EU, color = "EU"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,820), breaks = c(0,200,400,600,800), expand = c(0,0)) +
  ylab("gCO2e per kWh") +
  ggtitle("Falling Back") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "Electric Emissions Intensity Increased Across Europe After Russia's Invasion") +
  theme_apricitas + theme(legend.position = c(.8,.81)) +
  scale_color_manual(name= "Emissions Intensity of Electricity",values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("EU","Germany","France","Italy","Spain","Poland"), guide=guide_legend(override.aes=list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*820), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_INTENSITY_GRAPH, "EU Intensity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

EU_EMISSIONS <- get_eurostat("env_ac_aigg_q") %>%
  subset(geo == "EU27_2020") %>%
  subset(unit == "T_HAB") %>%
  subset(nace_r2 %in% c("C","D")) %>%
  select(nace_r2, time, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values) %>%
  arrange(time) %>%
  mutate(Manufacturing = c(0,0,0,rollmean(C,4))) %>%
  mutate(Electric = c(0,0,0,rollmean(D,4)))

EU_EMISSIONS_GRAPH <- ggplot() + #plotting Emissions in the EU
  geom_line(data = subset(EU_EMISSIONS, time >= as.Date("2010-10-01")), aes(x = time, y = Manufacturing*1000, color = "Manufacturing"), size = 1.25) +
  geom_line(data = subset(EU_EMISSIONS, time >= as.Date("2010-10-01")), aes(x = time, y = Electric*1000, color = "Electricity, Gas, Steam, and Air Conditioning Supply"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,820), breaks = c(0,200,400,600,800), expand = c(0,0)) +
  ylab("Tonnes of CO2 Equivalents") +
  ggtitle("Cooling Off") +
  labs(caption = "Graph created by @JosephPolitano using Eurostat data",subtitle = "Per-Capita Emissions in Key European Areas Have Fallen Since the Energy Crisis") +
  theme_apricitas + theme(legend.position = c(.4,.21)) +
  scale_color_manual(name= "EU Emissions Per Capita, Quarterly, 1yr Rolling Average",values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-10-01")-(.1861*(today()-as.Date("2010-10-01"))), xmax = as.Date("2010-10-01")-(0.049*(today()-as.Date("2010-10-01"))), ymin = 0-(.3*800), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EU_EMISSIONS_GRAPH, "EU Emissions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_SOLAR_WIND_STEO <- read.csv("C:/Users/josep/Documents/Climate Crisis Climate Transition/Wind_Solar_STEO.csv") %>%
  mutate(Year = as.Date(Year))

US_Solar_Wind_Graph <- ggplot() + #plotting
  annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2024-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2020-01-01"), y = 4.5, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=US_SOLAR_WIND_STEO, aes(x=Year,y= Wind, color= "US Wind Power Consumption"), size = 1.25) +
  geom_line(data=US_SOLAR_WIND_STEO, aes(x=Year,y= Solar, color= "US Solar Power Consumption"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,5),breaks = c(0,1,2,3,4,5), expand = c(0,0)) +
  ylab("Consumption, Quadrillion BTU") +
  ggtitle("The Long Term Transition") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "US Renewable Power Generation is Growing Rapidly") +
  theme_apricitas + theme(legend.position = c(.35,.93)) +
  scale_color_manual(name= NULL ,values = c("#9A348E","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("US Wind Power Consumption","US Solar Power Consumption")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*(today()-as.Date("1992-01-01")+365)), xmax = as.Date("1992-01-01")-(0.049*(today()-as.Date("1992-01-01")+365)), ymin = 0-(.3*5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_Solar_Wind_Graph, "US Solar Wind Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

GLOBAL_LIQUID_FUEL <- read.csv("C:/Users/josep/Documents/Climate Crisis Climate Transition/World_Liquid_Fuel.csv") %>%
  mutate(Year = as.Date(Year))

GLOBAL_LIQUID_FUEL_Graph <- ggplot() + #plotting
  annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2024-12-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Forecast", x = as.Date("2020-01-01"), y = 57, color = "#EE6055", size = 5, alpha = 0.6) +
  geom_line(data=GLOBAL_LIQUID_FUEL, aes(x=Year,y= Liquid, color= "Total World Petroleum & Other Liquid Fuel Consumption"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,110),breaks = c(0,25,50,75,100), expand = c(0,0)) +
  ylab("Mbbl/Day") +
  ggtitle("Driving Along") +
  labs(caption = "Graph created by @JosephPolitano using EIA STEO data",subtitle = "Global Liquid Fuel Consumption is Still Growing") +
  theme_apricitas + theme(legend.position = c(.4,.43)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*(today()-as.Date("1992-01-01")+365)), xmax = as.Date("1992-01-01")-(0.049*(today()-as.Date("1992-01-01")+365)), ymin = 0-(.3*100), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GLOBAL_LIQUID_FUEL_Graph, "Global Liquid.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INVESTMENT <- read.csv("C:/Users/josep/Documents/Climate Crisis Climate Transition/Investment_Global.csv") %>%
  mutate(Year = as.Date(Year)) %>%
  mutate(Fossil = as.numeric(gsub(" ","",Fossil)))
  
INVESTMENT_Graph <- ggplot() + #plotting
  geom_line(data=INVESTMENT, aes(x=Year,y= Fossil/1000, color= "Fossil Fuels"), size = 1.25) +
  geom_line(data=INVESTMENT, aes(x=Year,y= Renewables/1000, color= "Renewables"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"), limits = c(0,1.25),breaks = c(0,.5,1), expand = c(0,0)) +
  ylab("Trillions of 2021 Dollars") +
  ggtitle("The Investment Transition") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Global Renewable Investment is Still Rising—But Below Fossil Fuels") +
  theme_apricitas + theme(legend.position = c(.2,.63)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*1.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INVESTMENT_Graph, "Global Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INTENSITY <- read.csv("C:/Users/josep/Documents/Climate Crisis Climate Transition/Emissions_Intensity_GDP.csv") %>%
  mutate(Year = as.Date(Year, "%m/%d/%Y")) 

INTENSITY_GRAPH <- ggplot() + #plotting
  geom_line(data=INTENSITY, aes(x=Year,y= World, color= "Emissions Intensity of Global Real GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,0.5),breaks = c(0,0.25,0.5), expand = c(0,0)) +
  ylab("tC02 per USD 1000") +
  ggtitle("Green(er) Growth") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Global Emissions Intensity of GDP has Been Falling For Decades") +
  theme_apricitas + theme(legend.position = c(.3,.53)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1980-01-01")-(.1861*(today()-as.Date("1980-01-01"))), xmax = as.Date("1980-01-01")-(0.049*(today()-as.Date("1980-01-01"))), ymin = 0-(.3*0.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INTENSITY_GRAPH, "Global Intensity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


GLOBAL_INTENSITY <- EMBER_ELECTRICITY %>%
  subset(Area == c("World")) %>%
  #subset(Subcategory == "Fuel") %>%
  #subset(Category == "Electricity generation") %>%
  subset(Unit == "gCO2/kWh") %>%
  select(Year, Value, Area) %>%
  mutate(Year = as.Date(as.yearmon(Year))) %>%
  pivot_wider(names_from = Area, values_from = "Value")

WORLD_INTENSITY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = GLOBAL_INTENSITY, aes(x = Year, y = World, color = "World"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,820), breaks = c(0,200,400,600,800), expand = c(0,0)) +
  ylab("gCO2e per kWh") +
  ggtitle("Slow Progress") +
  labs(caption = "Graph created by @JosephPolitano using Ember Climate data",subtitle = "Per kWh, Global Electricity Hasn't Gotten Much Cleaner Over The Last Two Decades") +
  theme_apricitas + theme(legend.position = c(.8,.81)) +
  scale_color_manual(name= "Emissions Intensity of Electricity",values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*820), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WORLD_INTENSITY_GRAPH, "World Intensity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

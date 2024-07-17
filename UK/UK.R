pacman::p_load(tibble,readODS,ggpubr,sf,onsr,dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

US <- fredr(series_id = "GDPC1",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
UK <- fredr(series_id = "NGDPRSAXDCGBQ",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
GER <- fredr(series_id = "CLVMNACSCAB1GQDE",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
ITA <- fredr(series_id = "CLVMNACSCAB1GQIT",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
FRA <- fredr(series_id = "CLVMNACSCAB1GQFR",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
JPN <- fredr(series_id = "JPNRGDPEXP",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
CAN <- fredr(series_id = "NGDPRSAXDCCAQ",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
AUS_GDP <- read_abs(series_id = "A2304402X") %>%
  subset(date >= as.Date("2018-01-01")) %>%
  mutate(date = date - 60) %>%
  mutate(value = value/value[8]*100)

RGDP_G7_Graph <- ggplot() + #RGDP Index
  #geom_line(data=AUS_GDP, aes(x=date,y= value,color= "Australia"), size = 1.25) +
  geom_line(data=US, aes(x=date,y= value,color= "United States"), size = 1.25) +
  geom_line(data=UK, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN, aes(x=date,y= value,color= "Canada"), size = 1.25) +
  geom_line(data=GER, aes(x=date,y= value,color= "Germany"), size = 1.25) +
  geom_line(data=ITA, aes(x=date,y= value,color= "Italy"), size = 1.25) +
  geom_line(data=FRA, aes(x=date,y= value,color= "France"), size = 1.25) +
  geom_line(data=JPN, aes(x=date,y= value,color= "Japan"), size = 1.25) +
  annotate("text",label = "Pre-COVID GDP", x = as.Date("2019-01-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,110), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("GDP Growth in the G7") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED",subtitle = "The US is Leading the Recovery, with Japan, the UK, and Germany Still Below pre-COVID GDP") +
  theme_apricitas + theme(legend.position = c(.22,.30)) +
  scale_color_manual(name= "Real GDP 2019 Q3 = 100",values = c("#B30089","#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("Australia","United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 75-(.3*35), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RGDP_G7_Graph, "G7 Renamed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#25-54 Employment Rates
US_EPOP <- fredr(series_id = "LNS12300060",observation_start = as.Date("2018-01-01"), frequency = "q") %>%
  mutate(value = value/value[7]*100)
UK_EPOP <- fredr(series_id = "LREM25TTGBQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
GER_EPOP <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
ITA_EPOP <- fredr(series_id = "LREM25TTITQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
FRA_EPOP <- fredr(series_id = "LREM25TTFRQ156S",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[7]*100)
JPN_EPOP <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("2018-01-01"), frequency = "q") %>%
  mutate(value = value/value[7]*100)
CAN_EPOP <- fredr(series_id = "LREM25TTCAM156S",observation_start = as.Date("2018-01-01"), frequency = "q") %>%
  mutate(value = value/value[7]*100)

EPOP_G7_Graph <- ggplot() + #EU chemical imports
  geom_line(data=US_EPOP, aes(x=date,y= value,color= "United States"), size = 1.25) +
  geom_line(data=UK_EPOP, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_EPOP, aes(x=date,y= value,color= "Canada"), size = 1.25) +
  geom_line(data=GER_EPOP, aes(x=date,y= value,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_EPOP, aes(x=date,y= value,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_EPOP, aes(x=date,y= value,color= "France"), size = 1.25) +
  geom_line(data=JPN_EPOP, aes(x=date,y= value,color= "Japan"), size = 1.25) +
  annotate("text",label = "Pre-COVID Prime-Age Employment %", x = as.Date("2018-10-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(85,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("25-54 Employment Rate, Index 2019 Q3 = 100") +
  ggtitle("The UK's Terrible Employment Recovery") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "The UK is the Only G7 Nation Whose Prime-Age Employment Rate Hasn't Fully Recovered") +
  theme_apricitas + theme(legend.position = c(.82,.29)) +
  scale_color_manual(name= "25-54 Employment %, Index 2019 Q3",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 85-(.3*20), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_G7_Graph, "G7epop.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_EPOP_PA <- fredr(series_id = "LNS12300060",observation_start = as.Date("1990-01-01"), frequency = "q")
UK_EPOP_PA <- fredr(series_id = "LREM25TTGBQ156S",observation_start = as.Date("1990-01-01"))
GER_EPOP_PA <- fredr(series_id = "LREM25TTDEQ156S",observation_start = as.Date("1990-01-01"))
ITA_EPOP_PA <- fredr(series_id = "LREM25TTITQ156S",observation_start = as.Date("1990-01-01"))
FRA_EPOP_PA <- fredr(series_id = "LREM25TTFRQ156S",observation_start = as.Date("1990-01-01"))
JPN_EPOP_PA <- fredr(series_id = "LREM25TTJPM156S",observation_start = as.Date("1990-01-01"), frequency = "q")
CAN_EPOP_PA <- fredr(series_id = "LREM25TTCAM156S",observation_start = as.Date("199-01-01"), frequency = "q") 

EPOP_G7_PA_Graph <- ggplot() + #EU chemical imports
  geom_line(data=US_EPOP_PA, aes(x=date,y= value/100,color= "United States"), size = 1.25) +
  geom_line(data=UK_EPOP_PA, aes(x=date,y= value/100,color= "United Kingdom"), size = 1.25) +
  geom_line(data=CAN_EPOP_PA, aes(x=date,y= value/100,color= "Canada"), size = 1.25) +
  geom_line(data=GER_EPOP_PA, aes(x=date,y= value/100,color= "Germany"), size = 1.25) +
  geom_line(data=ITA_EPOP_PA, aes(x=date,y= value/100,color= "Italy"), size = 1.25) +
  geom_line(data=FRA_EPOP_PA, aes(x=date,y= value/100,color= "France"), size = 1.25) +
  geom_line(data=JPN_EPOP_PA, aes(x=date,y= value/100,color= "Japan"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.67,.87), breaks = c(.70,.75,.80,.85), expand = c(0,0)) +
  ylab("25-54 Employment Rate") +
  ggtitle("The Global Employment Recovery") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Besides the UK, All G7 Nations Have Seen Full Employment Recoveries") +
  theme_apricitas + theme(legend.position = c(.15,.28), legend.title = element_text(size = 13), legend.text = element_text(size = 13)) +
  scale_color_manual(name= "25-54 Employment Rate",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","Canada","France","Germany","Italy","United Kingdom","Japan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 67-(.3*20), ymax = 67) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EPOP_G7_PA_Graph, "G7epoppa.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#GDP data at a high level
GDP <- ons_get("gdp-to-four-decimal-places") %>%
  select(Time,v4_0,`sic-unofficial`) %>%
  mutate(`sic-unofficial` = gsub("G--T","Services", `sic-unofficial`)) %>%
  mutate(`sic-unofficial` = gsub("F","Construction", `sic-unofficial`)) %>%
  mutate(`sic-unofficial` = gsub("B--E","Production Industries", `sic-unofficial`)) %>%
  mutate(`sic-unofficial` = gsub("A--T","Total GDP", `sic-unofficial`)) %>%
  subset(., `sic-unofficial` != "A") %>%
  transmute(date = as.Date(as.yearmon(Time, "%b-%y")), value = v4_0, Category = `sic-unofficial`) %>%
  subset(., date > as.Date("2017-12-31"))
  
GDP_Components_Graph <- ggplot() +
  geom_line(data=subset(GDP, date > as.Date("2017-12-31")), aes(x=date,y= value,color= Category), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(55,115), breaks = c(60,70,80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Average = 100") +
  ggtitle("Breaking Down British GDP") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Output Has Been Stagnant This Year—With Declines in Industrial Production") +
  theme_apricitas + theme(legend.position = c(.20,.40)) +
  scale_color_manual(name= "Real Index, UK, 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Total GDP","Services","Production Industries","Construction")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 55-(.3*60), ymax = 55) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = GDP_Components_Graph, "GDP Components.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#unnesting function
unnest_dataframes <- function(x) {
  
  y <- do.call(data.frame, x)
  
  if("data.frame" %in% sapply(y, class)) unnest_dataframes(y)
  
  y
  
}

Energy_Imports <- ons_get_obs("trade",direction = "IM", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "3", time = "*") %>%
  as.data.frame() 

Energy_Imports <- unnest_dataframes(unnest_dataframes(Energy_Imports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))
  
Energy_Exports <- ons_get_obs("trade",direction = "EX", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "3", time = "*") %>%
  as.data.frame() 

Energy_Exports <- unnest_dataframes(unnest_dataframes(Energy_Exports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

Energy <- merge(Energy_Exports,Energy_Imports, by = "date") %>%
  arrange(desc(date)) %>%
  transmute(date, value = rollsum(value.y-value.x,12, na.pad = TRUE))

Food_Imports <- ons_get_obs("trade",direction = "IM", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "0", time = "*") %>%
  as.data.frame() 

Food_Imports <- unnest_dataframes(unnest_dataframes(Food_Imports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

Food_Exports <- ons_get_obs("trade",direction = "EX", geography = "K02000001", countriesandterritories = "W1", standardindustrialtradeclassification = "0", time = "*") %>%
  as.data.frame() 

Food_Exports <- unnest_dataframes(unnest_dataframes(Food_Exports)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

Food <- merge(Food_Exports,Food_Imports, by = "date") %>%
  arrange(desc(date)) %>%
  transmute(date, value = rollsum(value.y-value.x,12, na.pad = TRUE))


Energy_Food_Imports_Graph <- ggplot() +
  geom_line(data=subset(Energy, date > as.Date("1999-12-31")), aes(x=date,y= value/1000,color= "Energy, Mineral Fuels, Lubricants, and Related Materials)"), size = 1.25) +
  geom_line(data=subset(Food, date > as.Date("1999-12-31")), aes(x=date,y= value/1000,color= "Food and Live Animals"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B", prefix = "£"),limits = c(-10,60), breaks = c(-10,0,10,20,30,40,50,60), expand = c(0,0)) +
  ylab("Billions of Pounds, NSA") +
  ggtitle("The UK's Food & Energy Trade Deficit") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The UK's Energy and Food Import Bill Has Skyrocketed Amidst Shortages") +
  theme_apricitas + theme(legend.position = c(.40,.70)) +
  scale_color_manual(name= "UK Net Imports, 12M Moving Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-90-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-90-as.Date("2000-01-01"))), ymin = -10-(.3*70), ymax = -10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Energy_Food_Imports_Graph, "Energy Imports.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


SIXTEEN_SIXTY4_EPOP <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/timeseries/lf24/lms") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2000-01-01"))

UK_EPOP_PA_Graph <- ggplot() + #EU chemical imports
  geom_line(data=SIXTEEN_SIXTY4_EPOP, aes(x=date,y= value/100,color= "16-64 Employment Rate, UK"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(.69,.77), breaks = c(.69,.70,.71,.72,.73,.74,.75,.76,.77), expand = c(0,0)) +
  ylab("16-64 Employment Rate") +
  ggtitle("The UK's Weak Employment Recovery") +
  labs(caption = "Graph created by @JosephPolitano using OECD data",subtitle = "Broader Employment Rates Have Not Recovered to Pre-Pandemic Highs in the UK") +
  theme_apricitas + theme(legend.position = c(.25,.68), legend.title = element_text(size = 13), legend.text = element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("16-64 Employment Rate, UK")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-90-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-90-as.Date("2000-01-01"))), ymin = .69-(.3*.08), ymax = .69) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_EPOP_PA_Graph, "UK EPOP 1664.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

regional_gdp_quarter <- ons_get("regional-gdp-by-quarter") %>%
  subset(`sic-unofficial` == "A--T" & GrowthRate == "Quarterly index") %>%
  transmute(value = v4_1, date = as.Date(as.yearqtr(`yyyy-qq`,"%Y-q%q")), geo = Geography) %>%
  pivot_wider(names_from = geo) %>%
  subset(date >= as.Date("2019-10-01")) %>%
  arrange(date) %>%
  mutate(across(where(is.numeric), ~ . / .[1]-1)) %>%
  slice(n()) %>%
  pivot_longer(cols = c(`England`:`East Midlands`)) %>%
  select(name,value)

ENG_REG <- st_read("C:/Users/Joseph/Documents/GitHub/Apricitas/UK/NUTS1_Jan_2018_UGCB_in_the_UK.shp") %>%
  mutate(nuts118nm = gsub(" \\(England\\)","",nuts118nm)) %>%
  left_join(regional_gdp_quarter, join_by("nuts118nm"=="name")) %>%
  drop_na()

Regional_GDP <- ggplot() +
  geom_sf(data = ENG_REG, aes(fill = value)) +
  geom_sf(data = ENG_REG, color = "black", fill = NA, lwd = 0.5) + # Black borders for counties
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(-0.05,0,0.05), 
                      labels = c("-5%","+0%","+5%"),
                      limits = c(-0.05,0.05)) +
  #ggtitle("Cumulative GDP Growth During Pandemic, England and Wales") +
  theme(plot.title = element_text(hjust = 0, size = 14)) +
  labs(caption = "Graph created by @JosephPolitano using ONS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))


# Create a text grob
tgrob <- text_grob(expression(bold("         GDP Change From Pre-Pandemic Levels, England & Wales")),size = 22, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob)

final_plot <- ggarrange(plot_0,Regional_GDP,  ncol = 1, nrow = 2, heights = c(10,40), widths = c(10,30), common.legend = TRUE, legend = "right", align = "hv") + bgcolor("#252A32") + border("#252A32") +
  theme(plot.background = element_rect(fill = "#252A32", colour = "#252A32"),
        panel.background = element_rect(fill = "#252A32", colour = "#252A32"))


ggsave(dpi = "retina",plot = final_plot, "Regional GDP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


# ons_get("datasets")
# ons_ids()
# ?ons_get_obs
# #CPI data
# CPI <- ons_get("cpih01")
# 
# #gva by industry
# test <- ons_get("gva-by-industry-by-local-authority")
# #test
# construction <- ons_get("output-in-the-construction-industry")
# 
# retail_sales <- ons_get("retail-sales-index")
# 
# trade <- ons_get("trade")
# 
# cards <- ons_get("uk-spending-on-cards")
# 
# regional_gdp_quarter <- ons_get("regional-gdp-by-quarter")
# 
# test <- ons_ids()
# 
# ons_browse()
# 
# ons_codelists()
# 
# datasets <- ons_datasets()

#High-Energy-Intensity-Manufacturing

IOP_CHEMICAL <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k232/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1) %>%
  mutate_if(is.character,as.numeric)

IOP_PAPER <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k22v/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_FAB <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k23k/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_BASIC_METALS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/k23h/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_MANU <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/economicoutputandproductivity/output/timeseries/kun2/diop") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("Title","value")) %>%
  transmute(date = as.Date(as.yearmon(Title, "%Y %b")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric)

IOP_Graph <- ggplot() + #Energy Intensive Manufacturing
  geom_line(data=subset(IOP_FAB, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "Fabricated Metal Products ex Machinery and Equipment"), size = 1.25) +
  geom_line(data=subset(IOP_PAPER, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "Paper and Paper Products"), size = 1.25) +
  geom_line(data=subset(IOP_CHEMICAL, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "Chemicals and Chemical Products"), size = 1.25) +
  geom_line(data=subset(IOP_MANU, date > as.Date("2018-12-31")), aes(x=date,y= value/value[1]*100,color= "All Manufacuring ex Coke and Refined Petroleum Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(70,130), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  ylab("25-54 Employment Rate, Index 2019 Q3 = 100") +
  ggtitle("UK Energy-Intensive Industrial Production") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "After a Strong Recovery, British Production Has Dwindled, Especially in Energy-Intensive Industries") +
  theme_apricitas + theme(legend.position = c(.36,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("All Manufacuring ex Coke and Refined Petroleum Products","Fabricated Metal Products ex Machinery and Equipment","Paper and Paper Products","Chemicals and Chemical Products")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-90-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-90-as.Date("2019-01-01"))), ymin = 70-(.3*60), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = IOP_Graph, "IOP.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_SERVICES <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7f5/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)
  
SERVICES_DECEMBER <- CPI_SERVICES %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)
  
CPI_SERVICES_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/icvi/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)
  
CPI_SERVICES_CONTRIB_test <- merge(CPI_SERVICES,CPI_SERVICES_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_SERVICES_CONTRIB_test2 <- merge(CPI_SERVICES_CONTRIB_test,SERVICES_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Services")

#Energy
CPI_ENERGY <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/dk9t/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)

ENERGY_DECEMBER <- CPI_ENERGY %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)

CPI_ENERGY_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/a9f3/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)

CPI_ENERGY_CONTRIB_test <- merge(CPI_ENERGY,CPI_ENERGY_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_ENERGY_CONTRIB_test2 <- merge(CPI_ENERGY_CONTRIB_test,ENERGY_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Energy")

#Food
CPI_FOOD <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bu/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)

FOOD_DECEMBER <- CPI_FOOD %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)

CPI_FOOD_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/chzr/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)

CPI_FOOD_CONTRIB_test <- merge(CPI_FOOD,CPI_FOOD_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_FOOD_CONTRIB_test2 <- merge(CPI_FOOD_CONTRIB_test,FOOD_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Food")

#Goods
CPI_GOODS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7f4/mm23") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %b")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(december = value)

GOODS_DECEMBER <- CPI_GOODS %>% 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-value)

CPI_GOODS_WEIGHTS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/icvh/mm23") %>%
  mutate_if(is.character,as.numeric) %>%
  `colnames<-`(c("year","value")) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(year = as.Date(as.yearmon(year))) %>%
  mutate(date = list(seq.Date(year,year + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  select(-year)

CPI_GOODS_CONTRIB_test <- merge(CPI_GOODS,CPI_GOODS_WEIGHTS, by = "date") %>%
  select(-december) %>%
  `colnames<-`(c("date","value","weight"))

CPI_GOODS_CONTRIB_test2 <- merge(CPI_GOODS_CONTRIB_test,GOODS_DECEMBER) %>%
  mutate(weight = weight/1000) %>%
  transmute(date, contribution = lag(weight,12)*((december-lag(value,12))/lag(value,12)) + weight*((value-december)/december)) %>%
  mutate(Category = "Goods")


#NEEDS TO BE ADJUSTED FOR RELATIVE CONTRIBUTION

#https://www.oecd.org/sdd/prices-ppp/OECD-calculation-contributions-annual-inflation.pdf

CPI_CONTRIB <- rbind(CPI_SERVICES_CONTRIB_test2, CPI_GOODS_CONTRIB_test2, CPI_ENERGY_CONTRIB_test2, CPI_FOOD_CONTRIB_test2) %>%
  pivot_wider(values_from = contribution,names_from = Category) %>%
  mutate(Goods = Goods - Energy - Food) %>%
  pivot_longer(cols = Services:Food) %>%
  subset(date > as.Date("2003-12-01"))

CPI_CONTRIBUTION_ANNUAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = CPI_CONTRIB, aes(x = date, y = value, fill = name), color = NA, width = 31, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.015,.11), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Inflation is Broad-Based, but Most Come From Volatile Factors Like Food, Energy, and Goods") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services","Goods","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = -0.015-(.3*.125), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_ANNUAL_GRAPH, "CPI Annual.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#UK NGDP 
NGDP_UK <- fredr(series_id = "UKNGDP",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[8]*100)

NGDP_Trend <- data.frame(date = c(seq(as.Date("2019-10-01"), tail(NGDP_UK$date, n=1), "3 months")), trend = 100*1.009853^(0:(length(seq(from = as.Date("2019-10-01"), to = tail(NGDP_UK$date, n=1), by = '3 month')) - 1)))

NGDP_TREND_graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=NGDP_UK, aes(x=date, y=value, color="UK NGDP"), size = 1.25) +
  geom_line(data=NGDP_Trend, aes(x=date,y= trend,color= "UK NGDP 4% Pre-Covid Trend"), size = .75,linetype = "dashed") + #taking 2010-2020 r and n gdp grown as trend
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(80,90,100,110,120), expand = c(0,0)) +
  ylab("Index, Q1 2018 = 100") +
  ggtitle("Bucking the Trend") +
  labs(caption = "Graph created by @JosephPolitano using IHS Markit data",subtitle = "British Nominal Gross Domestic Product Remains Slightly Below Trend") +
  theme_apricitas + theme(legend.position = c(.35,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,2), lwd = c(1.25,.75)))) +#, labels = c("PCE Price Index","Core PCE Price Index", "Trimmed Mean PCE Price Index"))+
  theme(legend.key.width =  unit(.82, "cm")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 80-(.3*45), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NGDP_TREND_graph, "NGDP Trend.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Total_Fixed_Capital <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/npqt/cxnv") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na()
  
Business_Fixed_Capital <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/npel/cxnv") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na()


FIXED_graph <- ggplot() + #Plotting GDP Growth Rates
  annotate("vline", x = as.Date("2016-06-23"), xintercept = as.Date("2016-06-23"), color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "Brexit Vote", x = as.Date("2017-06-23"), y = 110, color = "white", linetype = "dashed", size = 5) +
  geom_line(data=subset(Total_Fixed_Capital, date > as.Date("2011-12-01")), aes(x=date, y=value/value[1]*100, color="Total Real Gross Fixed Capital Formation"), size = 1.25) +
  geom_line(data=subset(Business_Fixed_Capital, date > as.Date("2011-12-01")), aes(x=date, y=value/value[1]*100, color="Business Real Gross Fixed Capital Formation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(95,145), breaks = c(80,90,100,110,120,130,140), expand = c(0,0)) +
  ylab("Index, Q1 2012 = 100") +
  ggtitle("Breaking Britain's Investment Shortfall") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "UK Real Investment Has Finally Rebounded Significantly Above Pre-Brexit Levels") +
  theme_apricitas + theme(legend.position = c(.70,.96)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E"), breaks = c("Total Real Gross Fixed Capital Formation","Business Real Gross Fixed Capital Formation")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 95-(.3*50), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_graph, "Fixed Investment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPI_IMPORT_INTENSITY <- read.csv("C:/Users/josep/Documents/UK Economy Bad/CPI_IMPORT_INTENSITY.csv") %>%
  `colnames<-`(c("Date","0-10%","10-25%","25-40%","40% or More","Energy")) %>%
  pivot_longer(cols = `0-10%`:`Energy`) %>%
  mutate(Date = as.Date(Date))
  
CPI_CONTRIBUTION_IMPORT_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = CPI_IMPORT_INTENSITY, aes(x = Date, y = value/100, fill = name), color = NA, width = 31, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.015,.115), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Import-Intensive Products Are Driving a Large Amount of British Inflation") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation by Import Intensity",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("0-10%","10-25%","25-40%","40% or More","Energy")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = -0.015-(.3*.13), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_IMPORT_GRAPH, "CPI Contribution Import.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#retail trade indexes

retail_sales_current_growth <- ons_get_obs("retail-sales-index",
                geography = "K03000001", 
                prices = "current-prices-percentage-change-3-months-on-same-period-a-year-earlier",
                seasonaladjustment = "seasonal-adjustment",
                time = "*",
                unofficialstandardindustrialclassification = "all-retailing-excluding-automotive-fuel")

retail_sales_current_growth <- unnest_dataframes(unnest_dataframes(retail_sales_current_growth)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

retail_sales_real_growth <- ons_get_obs("retail-sales-index",
                geography = "K03000001", 
                prices = "chained-volume-percentage-change-3-months-on-same-period-a-year-earlier",
                seasonaladjustment = "seasonal-adjustment",
                time = "*",
                unofficialstandardindustrialclassification = "all-retailing-excluding-automotive-fuel")

retail_sales_real_growth <- unnest_dataframes(unnest_dataframes(retail_sales_real_growth)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation))

RETAIL_SALES_GROWTH_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = subset(retail_sales_current_growth, date > as.Date("2014-12-01")), aes(x = date, y = value/100, color = "Nominal"), size = 1.25) +
  geom_line(data = subset(retail_sales_real_growth,date > as.Date("2014-12-01")), aes(x = date, y = value/100, color = "Real"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.10,.20), breaks = c(-.10,-0.05,0,0.05,.10,.15,.20), expand = c(0,0)) +
  ylab("Annual Growth Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Nominal Retail Sales Growth is at Pre-Pandemic Levels, But Real Growth Has Plummetted") +
  theme_apricitas + theme(legend.position = c(.33,.75)) +
  scale_color_manual(name= "UK 3M Rolling Retail Sales ex-Auto Fuel, Yoy Growth",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.10-(.3*.30), ymax = -0.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_SALES_GROWTH_graph, "Retail Sales Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

retail_sales_current <- ons_get_obs("retail-sales-index",
                                           geography = "K03000001", 
                                           prices = "value-of-retail-sales-at-current-prices",
                                           seasonaladjustment = "seasonal-adjustment",
                                           time = "*",
                                           unofficialstandardindustrialclassification = "all-retailing-excluding-automotive-fuel")

retail_sales_current <- unnest_dataframes(unnest_dataframes(retail_sales_current)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation)) %>%
  arrange(date) %>%
  subset(date >= as.Date("2019-01-01"))%>%
  mutate(value = value/value[1]*100)

retail_sales_real <- ons_get_obs("retail-sales-index",
                                        geography = "K03000001", 
                                        prices = "chained-volume-of-retail-sales",
                                        seasonaladjustment = "seasonal-adjustment",
                                        time = "*",
                                        unofficialstandardindustrialclassification = "all-retailing-excluding-automotive-fuel")

retail_sales_real <- unnest_dataframes(unnest_dataframes(retail_sales_real)) %>%
  transmute(date = as.Date(as.yearmon(Time.label, "%b-%y")), value = as.numeric(observation)) %>%
  arrange(date) %>%
  subset(date >= as.Date("2019-01-01"))%>%
  mutate(value = value/value[1]*100)

RETAIL_SALES_INDEX_graph <- ggplot() + #RGDP Index
  geom_line(data=retail_sales_current, aes(x=date,y= value,color= "Nominal Retail Sales"), size = 1.25) +
  geom_line(data=retail_sales_real, aes(x=date,y= value,color= "Real Retail Sales"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(80,125), breaks = c(80,90,100,110,120), expand = c(0,0)) +
  ylab("Index, Jan 2019 = 100") +
  ggtitle("The Struggling British Consumer") +
  labs(caption = "Graph created by @JosephPolitano using ONS Data",subtitle = "Real UK's Retail Sales Remain at Pre-Pandemic Levels—Even as Nominal Spending Rises 20%") +
  theme_apricitas + theme(legend.position = c(.62,.29)) +
  scale_color_manual(name= "Index, Jan 2019 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-90-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-90-as.Date("2019-01-01"))), ymin = 80-(.3*45), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RETAIL_SALES_INDEX_graph, "Retail Sales Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BICS <- read.csv("C:/Users/josep/Documents/UK Economy Bad/BICS.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))
  
BICS_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = BICS, aes(x = date, y = Energy.prices, color = "Energy Prices"), size = 1.25) +
  #geom_line(data = BICS, aes(x = date, y = Finance.costs, color = "Finance Costs"), size = 1.25) +
  geom_line(data = BICS, aes(x = date, y = Labour.costs, color = "Labor Costs"), size = 1.25) +
  geom_line(data = BICS, aes(x = date, y = Raw.material.prices, color = "Raw Material Prices"), size = 1.25) +
  #geom_line(data = BICS, aes(x = date, y = Other, color = "Other"), size = 1.25) +
  #geom_line(data = BICS, aes(x = date, y = Not.sure, color = "Not Sure"), size = 1.25) +
  geom_line(data = BICS, aes(x = date, y = Business.is.not.considering.raising.prices, color = "Not Considering Raising Prices"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(.10,.20,.30,.40,.50), expand = c(0,0)) +
  scale_x_date(date_labels = "%Y-%b") +
  ylab("Percent") +
  ggtitle("The British Inflation Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Most Businesses Cite Energy Prices as the Biggest Reason They're Considering Raising Prices") +
  theme_apricitas + theme(legend.position = c(.5,.2)) +
  scale_color_manual(name= "UK Businesses: Which Factors Are Causing Your Business to Consider Raising Prices?",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Energy Prices","Raw Material Prices","Labor Costs","Not Considering Raising Prices")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-04-01")-(.1861*(today()-as.Date("2022-04-01"))), xmax = as.Date("2022-04-01")-(0.049*(today()-as.Date("2022-04-01"))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BICS_graph, "BICS.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

AVERAGE_PRIVATE_WAGES <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/kaj3/lms") %>%
  subset(., nchar(Title)==8) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearmon(date, "%Y %B")), value) %>%
  mutate_if(is.character,as.numeric)

AVERAGE_PRIVATE_graph <- ggplot() + #plotting components of annual inflation
  geom_line(data = AVERAGE_PRIVATE_WAGES, aes(x = date, y = value/100, color = "Growth in Avg. Weekly Earnings, Private Sector Regular Pay Ex Arrears"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.015,.09), breaks = c(0,.03,.06,.09), expand = c(0,0)) +
  ylab("Annual Growth Percent") +
  ggtitle("The British Labour Shortage") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Wage Growth Has Hit Multi-Decade Highs") +
  theme_apricitas + theme(legend.position = c(.45,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2001-01-01")-(.1861*(today()-as.Date("2001-01-01"))), xmax = as.Date("2001-01-01")-(0.049*(today()-as.Date("2001-01-01"))), ymin = -0.015-(.3*.105), ymax = -0.015) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AVERAGE_PRIVATE_graph, "Average Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UK_NATIONS_STARTS <- read.csv("C:/Users/Joseph/Downloads/uk_nations_starts.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(Poland = as.numeric(Poland))

CA_STARTS <- fredr(series_id = "CABPPRIVSA", aggregation_method = "sum", frequency = "a")

BUILDING_COUNTRY_Graph <- ggplot() + #plotting new housing starts
  #geom_line(data=UK_NATIONS_STARTS, aes(x=date,y= Texas/1000, color= "Texas (29.5M)"), size = 1.25) +
  geom_line(data=UK_NATIONS_STARTS, aes(x=date,y= Florida/1000, color= "Florida (21.8M)"), size = 1.25) +
  #geom_line(data=CA_STARTS, aes(x=date,y= value/1000, color= "California (2021 Population: 39.2M)"), size = 1.25) +
  geom_line(data=UK_NATIONS_STARTS, aes(x=date,y= Canada/1000, color= "Canada (38.3M)"), size = 1.25) +
  geom_line(data=UK_NATIONS_STARTS, aes(x=date,y= Poland/1000, color= "Poland (37.8M)"), size = 1.25) +
  geom_line(data=UK_NATIONS_STARTS, aes(x=date,y= UK/1000, color= "United Kingdom (2021 Population: 67.3M)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), limits = c(0,400), expand = c(0,0)) +
  ylab("Permits, Monthly") +
  ggtitle("Britain's Housing Shortage") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "The UK Builds Less Housing Than Florida, Canada, or Poland") +
  theme_apricitas + theme(legend.position = c(.65,.75)) +
  scale_color_manual(name= "Housing Units Permitted/Started by Year" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("United Kingdom (2021 Population: 67.3M)","Florida (21.8M)","Canada (38.3M)","Poland (37.8M)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1969-01-01")-(.1861*(today()-as.Date("1969-01-01"))), xmax = as.Date("1969-01-01")-(0.049*(today()-as.Date("1969-01-01"))), ymin = 0-(.3*400), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BUILDING_COUNTRY_Graph, "Building Country Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


GDP_US_IND <- fredr(series_id = "GDPC1", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[8]*100)
GDP_UK_IND <- fredr(series_id = "NGDPRSAXDCGBQ", observation_start = as.Date("2018-01-01")) %>%
  mutate(value = value/value[8]*100)
GDP_IR_IND <- fredr(series_id = "CLVMNACNSAB1GQIE", observation_start = as.Date("2018-01-01"))
GDP_EU_IND <- fredr(series_id = "CLVMEURSCAB1GQEU272020", observation_start = as.Date("2018-01-01"))
GDP_EU_LESS_IR <- merge(GDP_IR_IND,GDP_EU_IND, by = "date") %>%
  transmute(date, value = value.x-value.y) %>%
  mutate(value = value/value[8]*100)

RGDP_US_UK_EU_Graph <- ggplot() + #RGDP Index
  geom_line(data=GDP_US_IND, aes(x=date,y= value,color= "United States"), size = 1.25) +
  geom_line(data=GDP_EU_LESS_IR, aes(x=date,y= value,color= "EU Ex-Ireland"), size = 1.25) +
  geom_line(data=GDP_UK_IND, aes(x=date,y= value,color= "United Kingdom"), size = 1.25) +
  annotate("text",label = "Pre-COVID GDP", x = as.Date("2019-01-01"), y =101, color = "white", size = 4) +
  annotate("hline", y = 100, yintercept = 100, color = "white", size = 1, linetype = "dashed") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,110), breaks = c(80,90,100,110), expand = c(0,0)) +
  ylab("Index, 2019 Q3 = 100") +
  ggtitle("The UK's Stagnation") +
  labs(caption = "Graph created by @JosephPolitano using National Accounts data from FRED",subtitle = "The UK's Economy is Still Smaller Than it was Pre-Pandemic") +
  theme_apricitas + theme(legend.position = c(.22,.29)) +
  scale_color_manual(name= "Real GDP 2019 Q3 = 100",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"),breaks = c("United States","EU Ex-Ireland","United Kingdom")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-90-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-90-as.Date("2018-01-01"))), ymin = 75-(.3*35), ymax = 75) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RGDP_US_UK_EU_Graph, "UK US EU-ex-IRL GDP Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

LABOR_PRODUCTIVITY <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/labourproductivity/timeseries/lzvb/prdy") %>%
  subset(str_detect(Title, "Q")) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric)

LABOR_PRODUCTIVITY_graph <- ggplot() + #Plotting GDP Growth Rates
  #annotate("vline", x = as.Date("2016-06-23"), xintercept = as.Date("2016-06-23"), color = "white", size = 1, linetype = "dashed") +
  #annotate("text", label = "Brexit Vote", x = as.Date("2017-04-23"), y = 110, color = "white", linetype = "dashed", size = 5) +
  geom_line(data=subset(LABOR_PRODUCTIVITY, date >= as.Date("1990-01-01")), aes(x=date, y=value/value[1]*100, color="UK Output Per Hour Worked"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(95,165), breaks = c(90,100,110,120,130,140,150,160), expand = c(0,0)) +
  ylab("Index, Q1 2012 = 100") +
  ggtitle("Britain's Productivity Shortfall") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "UK Labor Productivity Growth Has Stagnated Since the Global Financial Crisis") +
  theme_apricitas + theme(legend.position = c(.70,.53)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 95-(.3*70), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LABOR_PRODUCTIVITY_graph, "Labor Productivity.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#change to pn2 for first estimates
UK_PER_CAPITA <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ihxw/ukea") %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  subset(., value > 1)  %>%
  mutate_if(is.character,as.numeric) %>%
  subset(date >= as.Date("2004-01-01")) %>%
  mutate(value = value/value[1]*100)

US_PER_CAPITA <- fredr(series_id = "A939RX0Q048SBEA",observation_start = as.Date("2004-01-01")) %>%
  mutate(value = value/value[1]*100)

UK_US_PER_CAPITA_GDP_Graph_2014 <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=subset(UK_PER_CAPITA, date >= as.Date("2014-01-01")), aes(x=date, y=value/value[1]*100, color="United Kingdom"), size = 1.25) +
  geom_line(data=subset(US_PER_CAPITA, date >= as.Date("2014-01-01")), aes(x=date, y=value/value[1]*100, color="United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,125), breaks = c(80,90,100,110,120), expand = c(0,0)) +
  ylab("Index, Q1 2014 = 100") +
  ggtitle("Britain's Productivity Shortfall") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "UK Labor Productivity Growth Has Stagnated Since the Global Financial Crisis") +
  theme_apricitas + theme(legend.position = c(.20,.80)) +
  scale_color_manual(name= "GDP Per Capita\nQ1 2014 = 100",values = c("#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 80-(.3*45), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_US_PER_CAPITA_GDP_Graph_2014, "UK US Per Capita GDP 2014.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UK_US_PER_CAPITA_GDP_Graph_2004 <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=subset(UK_PER_CAPITA, date >= as.Date("2004-01-01")), aes(x=date, y=value/value[1]*100, color="United Kingdom"), size = 1.25) +
  geom_line(data=subset(US_PER_CAPITA, date >= as.Date("2004-01-01")), aes(x=date, y=value/value[1]*100, color="United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(85,130), breaks = c(80,90,100,110,120,130), expand = c(0,0)) +
  ylab("Index, Q1 2012 = 100") +
  ggtitle("Britain's Lost Decades") +
  labs(caption = "Graph created by @JosephPolitano using ONS & BEA data",subtitle = "UK Per-Capita GDP Has Barely Grown Since 2008 and Hasn't Grown at All Since COVID") +
  theme_apricitas + theme(legend.position = c(.20,.80)) +
  scale_color_manual(name= "GDP Per Capita\nQ1 2004 = 100",values = c("#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E")) + 
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 85-(.3*45), ymax = 85) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_US_PER_CAPITA_GDP_Graph_2004, "UK US Per Capita GDP 2004.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#PENSIONS SHARE OF GDP


#IMMIGRATION DATA

IMMIGRATION_OVERALL_DATA <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional/yearendingdecember2023/december23ltimaccessible.xlsx", 4) %>%
  row_to_names(.,4) %>%
  slice(-1:-3) %>%
  filter(Flow == "Net") %>%
  filter(grepl("^YE Dec", Period)) %>%
  mutate(date = seq.Date(from = as.Date("2012-01-01"), by = "1 year", length = nrow(.))) %>%
  mutate(across(`All Nationalities`:`Non-EU`, as.numeric)) %>%
  select(-`All Nationalities`) %>%
  pivot_longer(cols = `British`:`Non-EU`)

UK_NET_MIGRATION_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_bar(data = IMMIGRATION_OVERALL_DATA, aes(x = date, y = value/1000, fill = name), color = NA, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("vline", x = as.Date("2020-07-01"), xintercept = as.Date("2020-07-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "Official Brexit\n& Immigration Reform", x = as.Date("2020-05-01"), y = 750, color = "white", size = 5, hjust = 1) +
  xlab("Date") +
  scale_y_continuous(limits = c(-250,1000), breaks = c(-250,0,250,500,750,1000),labels = c("-250k","0","250k","500k","750k","1M"), expand = c(0,0)) +
  ylab("Net Migration") +
  ggtitle("The Growth in British Net Immigration") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Net Migration, Especially From Outside the EU, Has Surged Post-2021 Reforms") +
  theme_apricitas + theme(legend.position = c(.2,.85)) +
  scale_fill_manual(name= "Net Migration by Nationality",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("EU","Non-EU","British")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = -250-(.3*1250), ymax = -250) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_NET_MIGRATION_Graph, "UK Net Migration by Nationality.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


IMMIGRATION_REASON_DATA <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional/yearendingdecember2023/december23ltimaccessible.xlsx", 7) %>%
  row_to_names(.,4) %>%
  slice(-1:-3) %>%
  filter(Flow == "Net") %>%
  filter(grepl("^YE Dec", Period)) %>%
  mutate(across(`All Reasons`:`Other `, as.numeric)) %>%
  select(-`All Reasons`) %>%
  mutate(Humanitarian = `Humanitarian BNO`+`Humanitrian Ukraine`+`Humanitarian Resettlement`) %>%
  select(-`Humanitarian BNO`,-`Humanitrian Ukraine`,-`Humanitarian Resettlement`) %>%
  mutate(date = seq.Date(from = as.Date("2019-01-01"), by = "1 year", length = nrow(.))) %>%
  pivot_longer(cols = `Work`:`Humanitarian`) %>%
  mutate(name = gsub("Dependant", "Dependent", name)) %>%
  mutate(name = factor(name,levels = rev(c("Work","Work Dependent","Study","Study Dependent","Humanitarian","Asylum","Family","Other "))))

UK_NET_MIGRATION_Reason_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_bar(data = IMMIGRATION_REASON_DATA, aes(x = date, y = value/1000, fill = name), color = NA, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  annotate("vline", x = as.Date("2020-07-01"), xintercept = as.Date("2020-07-01"), color = "white", size = 1, linetype = "dashed") +
  annotate("text",label = "Official Brexit\n& Immigration Reform", x = as.Date("2020-06-01"), y = 925, color = "white", size = 4, hjust = 1) +
  xlab("Date") +
  scale_y_continuous(limits = c(-50,1000), breaks = c(0,250,500,750,1000),labels = c("0","250k","500k","750k","1M"), expand = c(0,0)) +
  ylab("Net Migration") +
  ggtitle("British Net Non-EU Immigration by Reason") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "Work & Study Visas, Plus Their Dependents, Represent Most of the Increase in British Immigration") +
  theme_apricitas + theme(legend.position = c(.15,.6), plot.title = element_text(size = 27)) +
  scale_fill_manual(name= "Reason",values = c("#FFE98F","#efc000","#00A99D","#00655e","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Work","Work Dependent","Study","Study Dependent","Humanitarian","Asylum","Family","Other ")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -50-(.3*1050), ymax = -50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_NET_MIGRATION_Reason_Graph, "UK Net Migration by Reason Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#GDP MAP
REGIONAL_GDP_CAPITA <- read.xlsx("https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions/1998to2022/regionalgrossdomesticproductgdpbyallitlregions.xlsx", 10) %>%
  row_to_names(.,1) %>%
  filter(ITL == "ITL1") %>%
  transmute(`Region name`, value = as.numeric(`2022`)) %>%
  mutate(`Region name` = ifelse(grepl("^(North |South|East|West)", `Region name`), paste0(`Region name`, " (England)"), `Region name`)) %>%
  mutate(`Region name` = ifelse(`Region name` == "East (England)", "East of England", `Region name`))

REGIONAL_GDP_CAPITA_MAP <- st_read("C:/Users/Joseph/Documents/GitHub/Apricitas/UK/NUTS1_Jan_2018_UGCB_in_the_UK.shp") %>%
  left_join(REGIONAL_GDP_CAPITA, join_by("nuts118nm"=="Region name")) %>%
  drop_na()

Regional_GDP_Capita <- ggplot() +
  geom_sf(data = REGIONAL_GDP_CAPITA_MAP, aes(fill = value/1000)) +
  geom_sf(data = REGIONAL_GDP_CAPITA_MAP, color = "black", fill = NA, lwd = 0.5) + # Black borders for counties
  scale_fill_viridis_c(labels = scales::dollar_format(prefix = "£",suffix = "k")) +
  ggtitle("GDP Per Capita, UK Regions, 2022") +
  labs(caption = "Graph created by @JosephPolitano using ONS data", subtitle = "London's GDP Per Capita is Extremely Far Ahead of the Rest of the UK's") +
  labs(fill = NULL) +
  theme_apricitas + 
  scale_x_continuous(limits = c(-300000,800000)) +
  scale_y_continuous(limits = c(40000,1000000)) +
  theme(plot.title = element_text(size = 30),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, -1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(color = "none")

ggsave(dpi = "retina",plot = Regional_GDP_Capita, "Regional GDP Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INACTIVE_LONG_TERM_SICKNESS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peoplenotinwork/economicinactivity/timeseries/lf69/lms") %>%
  slice(-1:-7) %>%
  setNames(c("date", "value")) %>%
  filter(nchar(date) > 7) %>%
  mutate(date = ymd(paste(date, "01"))) %>%
  mutate(value = as.numeric(value))
  
INACTIVE_LONG_TERM_SICKNESS_Bar_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_bar(data = filter(INACTIVE_LONG_TERM_SICKNESS, date > as.Date("1993-01-01")), aes(x = date, y = value/1000, fill = "Not in Labor Force: Long Term Sickness or Disability, Ages 16-64"), color = NA, stat= "identity", width = 32) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,3), breaks = c(0,1,2,3),labels = scales::number_format(suffix = "M"), expand = c(0,0)) +
  ylab("Number, Millions") +
  ggtitle("The British Healthcare Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The Number of British Citizens Out of the Labor Force Thanks to Sickness Has Skyrocketed") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-01-01")-(.1861*(today()-as.Date("1993-01-01"))), xmax = as.Date("1993-01-01")-(0.049*(today()-as.Date("1993-01-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INACTIVE_LONG_TERM_SICKNESS_Bar_Graph, "Inactive Long Term Bar Chart.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INACTIVE_LONG_TERM_SICKNESS_Line_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data = filter(INACTIVE_LONG_TERM_SICKNESS, date > as.Date("2004-01-01")), aes(x = date, y = value/1000, color = "Not in Labor Force: Long Term Sickness or Disability, Ages 16-64"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(1.5,3), breaks = c(1.5,2,2.5,3),labels = scales::number_format(suffix = "M"), expand = c(0,0)) +
  ylab("Number, Millions") +
  ggtitle("The British Healthcare Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The Number of British Citizens Out of the Labor Force Thanks to Sickness Has Skyrocketed") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2004-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2004-01-01")-(0.049*(today()-as.Date("2004-01-01"))), ymin = 1.5-(.3*1.5), ymax = 1.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INACTIVE_LONG_TERM_SICKNESS_Line_Graph, "Inactive Long Term Line Chart.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#PENSIONS SHARE OF GDP
#NHS DATA
NHS_WAIT_TIMES <- read.xlsx("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/RTT-Overview-Timeseries-Including-Estimates-for-Missing-Trusts-May24-XLS-113K-89156.xlsx") %>%
  row_to_names(.,7) %>%
  select(`No. within 18 weeks with estimates for missing data`,`No. > 18 weeks with estimates for missing data`,`No. > 52 weeks with estimates for missing data`) %>%
  setNames(c("<18",">18",">52")) %>%
  mutate(across(`<18`:`>52`, as.numeric)) %>%
  transmute(`<18`,`18-52` =`>18`-`>52` ,`>52`) %>%
  drop_na() %>%
  mutate(date = seq.Date(from = as.Date("2007-08-01"), by = "1 month", length.out = nrow(.))) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("<18","18-52",">52"))))

#https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/

NHS_WAIT_TIMES_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_bar(data = filter(NHS_WAIT_TIMES, date > as.Date("2014-01-01")), aes(x = date, y = value/1000000, fill = name), color = NA, stat= "identity", width = 32) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8),labels = scales::number_format(suffix = "M"), expand = c(0,0)) +
  ylab("Number, Millions") +
  ggtitle("NHS Backlog by Wait Time") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The Number of Britons Waiting for Healthcare Has Skyrocketed Since COVID") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +
  scale_fill_manual(name= "Patients Waiting for Treatment\nBy Wait Times",values = c("#EE6055","#00A99D","#FFE98F","#9A348E","#A7ACD9","#3083DC"), labels = c(">52 Weeks","18-52 Weeks","<18 Weeks")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-03-01")-(.1861*(today()-as.Date("2014-03-01"))), xmax = as.Date("2014-03-01")-(0.049*(today()-as.Date("2014-03-01"))), ymin = 0-(.3*8), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = NHS_WAIT_TIMES_Graph, "NHS Backlog Chart.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIZ_INVEST_NEW_DWELLINGS <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ee74/cxnv") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(value = value/value[29]*100)

BIZ_INVEST_NEW_DWELLINGS_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data = filter(BIZ_INVEST_NEW_DWELLINGS, date > as.Date("1997-01-01")), aes(x = date, y = value, color = "UK Real Gross Fixed Capital Formation, New Dwellings"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(), expand = c(0,0), limits = c(40,130)) +
  ylab("GFCF in New Dwellings, Indexed") +
  ggtitle("The British Healthcare Crisis") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "The Number of British Citizens Out of the Labor Force Thanks to Sickness Has Skyrocketed") +
  theme_apricitas + theme(legend.position = c(.35,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*(today()-as.Date("1997-01-01"))), xmax = as.Date("1997-01-01")-(0.049*(today()-as.Date("1997-01-01"))), ymin = 40-(.3*90), ymax = 40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIZ_INVEST_NEW_DWELLINGS_Graph, "Biz Invest New Dwellings Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIZ_INVEST_MANUFACTURING <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ds3r/cxnv") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(value = value*4)

BIZ_INVEST_MANUFACTURING_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data = filter(BIZ_INVEST_MANUFACTURING, date > as.Date("1997-01-01")), aes(x = date, y = value/1000, color = "UK Real Gross Fixed Capital Formation, Manufacturing Structures"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(prefix = "£",suffix = "B"), expand = c(0,0), limits = c(0,8)) +
  ylab("GFCF in New Structures, Annual Rate") +
  ggtitle("The UK Factory Construction Boom") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "British Investment in New Factories Has Boomed to the Highest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.45,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*(today()-as.Date("1997-01-01"))), xmax = as.Date("1997-01-01")-(0.049*(today()-as.Date("1997-01-01"))), ymin = 0-(.3*8), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIZ_INVEST_MANUFACTURING_Graph, "Biz Invest Manufacturing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ENGLAND_POP <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop") %>%
  slice(-1:-7) %>%
  setNames(c("Period","pop"))

ENGLAND_HOUSING_CONS_DATA_ANNUAL <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted/current/ukhousebuilding.xlsx",16) %>%
  row_to_names(.,5) %>%
  select(Period,`Completed - Private Enterprise`,`Completed - Housing Associations`, `Completed - Local Authorities`) %>%
  left_join(.,ENGLAND_POP, by = "Period") %>%
  mutate(Period = as.Date(paste0(Period, "-01-01"))) %>%
  drop_na() %>%
  mutate_if(is.character,as.numeric) %>%
  transmute(date = Period, `Private Enterprise` =`Completed - Private Enterprise`/pop, `Housing Associations` =`Completed - Housing Associations`/pop, `Local Authorities` = `Completed - Local Authorities`/pop) %>%
  pivot_longer(-date)

ENGLAND_HOUSING_CONS_DATA_ANNUAL_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_bar(data = ENGLAND_HOUSING_CONS_DATA_ANNUAL, aes(x = date, y = value*1000, fill = name), color = NA, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,7), labels = scales::number_format(), expand = c(0,0)) +
  ylab("Units Per 1000") +
  ggtitle("England Housing Completions Per Capita") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "English Housing Construction Has Stagnated Well Below Historic Highs") +
  theme_apricitas + theme(legend.position = c(.5,.825)) +
  scale_fill_manual(name= "Housing Completions Per 1000 Residents",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Private Enterprise","Local Authorities","Housing Associations")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1972-01-01")-(.1861*(today()-as.Date("1972-01-01"))), xmax = as.Date("1972-01-01")-(0.049*(today()-as.Date("1972-01-01"))), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ENGLAND_HOUSING_CONS_DATA_ANNUAL_Graph, "England Housing Construction Chart Annual Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TRADE_DATA <- read.csv("https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/balanceofpayments/datasets/tradeingoodsmretsallbopeu2013timeseriesspreadsheet/current/mret.csv") 

SUBNATIONAL_TRADE_DATA_SERVICES <- read.xlsx("https://www.ons.gov.uk/file?uri=/businessindustryandtrade/internationaltrade/datasets/subnationaltradetimeseries/2016to2021/subnationaltradetimeseries2016to20211.xlsx",6) %>%
  row_to_names(.,5) %>%
  filter(`ITL area name` %in% c("London","UK")) %>%
  pivot_longer(`2016`:`2021`) %>%
  select(-`Trade Type`,-`ITL level`,-`ITL code`) %>%
  pivot_wider(names_from = c(`Direction of trade`, `ITL area name`)) %>%
  mutate(date = as.Date(paste0(name, "-01-01")), London_Share_Services = as.numeric(Exports_London)/as.numeric(Exports_UK)) %>%
  transmute(date, Exports_UK_Services = as.numeric(Exports_UK), Exports_London_Services = as.numeric(Exports_London), London_Share_Services)

SUBNATIONAL_TRADE_DATA_GOODS <- read.xlsx("https://www.ons.gov.uk/file?uri=/businessindustryandtrade/internationaltrade/datasets/subnationaltradetimeseries/2016to2021/subnationaltradetimeseries2016to20211.xlsx",5) %>%
  row_to_names(.,5) %>%
  filter(`ITL area name` %in% c("London","UK")) %>%
  pivot_longer(`2016`:`2021`) %>%
  select(-`Trade Type`,-`ITL level`,-`ITL code`) %>%
  pivot_wider(names_from = c(`Direction of trade`, `ITL area name`)) %>%
  mutate(date = as.Date(paste0(name, "-01-01")), London_Share_Goods = as.numeric(Exports_London)/as.numeric(Exports_UK)) %>%
  transmute(date, Exports_UK_Goods = as.numeric(Exports_UK), Exports_London_Goods = as.numeric(Exports_London), London_Share_Goods)

SUBNATIONAL_TRADE_DATA <- merge(SUBNATIONAL_TRADE_DATA_SERVICES,SUBNATIONAL_TRADE_DATA_GOODS, by = "date") %>%
  mutate(London_Services_Share_Total = Exports_London_Services/(Exports_UK_Services+Exports_UK_Goods)) %>%
  select(date,London_Share_Services, London_Services_Share_Total) %>%
  pivot_longer(-date) %>%
  mutate(name = case_when(
    name == "London_Share_Services" ~ "Share of UK Service-Sector Exports",
    name == "London_Services_Share_Total" ~ "Share of All UK Exports",
    TRUE ~ name
  )) %>%
  mutate(name = factor(name, levels = c("Share of UK Service-Sector Exports","Share of All UK Exports")))

LONDON_EXPORTS_SHARE_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_bar(data = SUBNATIONAL_TRADE_DATA, aes(x = date, y = value, fill = ""), color = NA, stat= "identity") +
  facet_wrap(~ name) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,0.5), labels = scales::percent_format(), expand = c(0,0)) +
  ylab("Share of Exports") +
  ggtitle("London's Service Sector Export Strength") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "London's Finance, Consulting, and other Service Industries Make Up A Large Chunk of UK Exports") +
  theme_apricitas + theme(strip.text = element_text(size = 15, color = "white"), legend.position = "none") +
  scale_fill_manual(name= "Housing Completions Per 1000 Residents",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1972-01-01")-(.1861*(today()-as.Date("1972-01-01"))), xmax = as.Date("1972-01-01")-(0.049*(today()-as.Date("1972-01-01"))), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LONDON_EXPORTS_SHARE_Graph, "London Exports Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

url <- "https://assets.publishing.service.gov.uk/media/656e037f9462260705c568cd/LT118.ods"
destfile <- tempfile(fileext = ".ods")

GET(url, write_disk(destfile, overwrite = TRUE))

HOMEBUILDING_ENGLAND <- read_ods(destfile, sheet = 5) %>%
  row_to_names(.,3) %>%
  filter(row_number() < which(`Region/Country Name` == "New build completions [note 5]")) %>%
  tail(5) %>%
  select(-England, -`Region/Country Name`) %>%
  mutate_all(~as.numeric(as.character(.))) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Region") %>%
  setNames(c("Region","New_Homes")) %>%
  mutate(population = c(2647493,7424092,5482455,4881559,5956226,6349646,8804769,9295829,5712822)) %>% #2021 POP ESTIMATES PULLED FROM GVA DATA
  mutate(New_Homes_Per_Capita = (New_Homes/4)/population) %>%
  mutate(`Region` = ifelse(grepl("^(North |South|East|West)", `Region`), paste0(`Region`, " (England)"), `Region`)) %>%
  mutate(`Region` = ifelse(`Region` == "East of England (England)", "East of England", `Region`))
  
HOMEBUILDING_ENGLAND_MAP <- st_read("C:/Users/Joseph/Documents/GitHub/Apricitas/UK/NUTS1_Jan_2018_UGCB_in_the_UK.shp") %>%
  left_join(HOMEBUILDING_ENGLAND, join_by("nuts118nm"=="Region")) 

HOMEBUILDING_ENGLAND_MAP_Graph <- ggplot() +
  geom_sf(data = HOMEBUILDING_ENGLAND_MAP, aes(fill = New_Homes_Per_Capita*1000)) +
  geom_sf(data = HOMEBUILDING_ENGLAND_MAP, color = "black", fill = NA, lwd = 0.5) + # Black borders for counties
  scale_fill_viridis_c(name = "Net New Homes\nPer 1000 Residents",labels = scales::number_format()) +
  ggtitle("Annual Net Housing Additions Per Capita, 2018-2023") +
  labs(caption = "Graph created by @JosephPolitano using ONS data", subtitle = "Homebuilding is Not Particularly Strong In the UK's Most Prosperous Areas in & Around London") +
  labs(fill = NULL) +
  theme_apricitas + 
  scale_x_continuous(limits = c(-200000,800000)) +
  scale_y_continuous(limits = c(40000,650000)) +
  theme(plot.title = element_text(size = 25),
        legend.position = "right",
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(color = "none")

ggsave(dpi = "retina",plot = HOMEBUILDING_ENGLAND_MAP_Graph, "Homebuilding England Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UK_GOODS_IMPORTS_EU <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/jim7/mret") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(category = "Imports", destination = "EU") 

UK_GOODS_EXPORTS_EU <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/jim8/mret") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(category = "Exports", destination = "EU") 

UK_GOODS_IMPORTS_XEU <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/jin2/mret") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(category = "Imports", destination = "Non-EU") 

UK_GOODS_EXPORTS_XEU <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/jin3/mret") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(category = "Exports", destination = "Non-EU") 
  
UK_GOODS_TRADE <- rbind(UK_GOODS_IMPORTS_EU,UK_GOODS_EXPORTS_EU,UK_GOODS_IMPORTS_XEU,UK_GOODS_EXPORTS_XEU)

UK_GOODS_TRADE_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data = filter(UK_GOODS_TRADE, date > as.Date("2009-01-01")), aes(x = date, y = value/1000*4, color = category), size = 1.25) +
  annotate("vline", x = as.Date("2016-06-23"), xintercept = as.Date("2016-06-23"), color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "Brexit\nVote", x = as.Date("2016-04-23"), y = 290, color = "white", size = 5, hjust = 1, lineheight = 0.8) +
  annotate("vline", x = as.Date("2016-06-23"), xintercept = as.Date("2020-12-30"), color = "white", size = 1, linetype = "dashed") +
  annotate("text", label = "Official\nBrexit", x = as.Date("2021-02-27"), y = 290, color = "white", size = 5, hjust = 0, lineheight = 0.8) +
  facet_wrap(~ destination) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(prefix = "£", suffix = "B"), expand = c(0,0)) +
  ylab("Chained Volume Measures, 2019 Pounds, Annual Rate") +
  ggtitle("UK Real Goods Trade Volumes") +
  labs(caption = "Graph created by @JosephPolitano using ONS data. Note: Excludes Precious Metals & Monetary Gold",subtitle = "UK Goods Trade Volumes Have Stagnated Since Brexit & the Pandemic") +
  theme_apricitas + theme(strip.text = element_text(size = 15, color = "white"), legend.position = c(0.5,0.65)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1972-01-01")-(.1861*(today()-as.Date("1972-01-01"))), xmax = as.Date("1972-01-01")-(0.049*(today()-as.Date("1972-01-01"))), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_GOODS_TRADE_Graph, "UK Goods Trade Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

UK_INSURANCE_PENSION_SERVICES_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjsv/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01"))) %>%
  mutate(name = "Insurance & Pension Services")



UK_FUND_MANAGERS_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/zxlw/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_SECURITIES_DEALERS_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/zxlx/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_SHIP_BROKERAGE_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/zxly/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_OTHER_FIN_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/zxlz/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_FUND_SECURITIES_SHIP_OTHER_BALANCE <- rbind(UK_FUND_MANAGERS_BALANCE,UK_SECURITIES_DEALERS_BALANCE,UK_SHIP_BROKERAGE_BALANCE,UK_OTHER_FIN_BALANCE) %>%
  group_by(date) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(name = "Fund Managers, Securities Dealers, & Other Financial Services")




UK_MFI_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/zxlv/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01"))) %>%
  mutate(name = "Banking & Related Services")

UK_COMPUTER_INFO_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjtb/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01"))) %>%
  mutate(name = "Computer, Information, & Telecom Services")

UK_MGMT_CONSULTING_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjgk/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01"))) %>%
  mutate(name = "Management Consulting")

UK_RANDD_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjgs/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01"))) %>%
  mutate(name = "Research & Development")



UK_LEGAL_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjgg/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_ACCOUNTING_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjgi/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_ADVERTISING_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/fjgq/pb") %>%
  setNames(c("date","value")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

UK_LEGAL_ACCOUNTING_ADVERTISING_BALANCE <- rbind(UK_LEGAL_BALANCE,UK_ACCOUNTING_BALANCE,UK_ADVERTISING_BALANCE) %>%
  group_by(date) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(name = "Legal, Accounting, & Advertising Services")


# UK_TECHNICAL_BALANCE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/nationalaccounts/balanceofpayments/timeseries/msg9/pb") %>%
#   setNames(c("date","value")) %>%
#   mutate_if(is.character,as.numeric) %>%
#   drop_na() %>%
#   mutate(date = as.Date(paste0(date, "-01-01"))) %>%
#   mutate(name = "Technical & Trade Services (Incl. Services Between Affiliated Enterprises)")
  
UK_GDP_ANNUAL <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ybha/ukea") %>%
  setNames(c("date","GDP")) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(date = as.Date(paste0(date, "-01-01")))


UK_SELECTED_SERVICES_BALANCE <- rbind(UK_INSURANCE_PENSION_SERVICES_BALANCE,UK_FUND_SECURITIES_SHIP_OTHER_BALANCE,UK_MFI_BALANCE,UK_COMPUTER_INFO_BALANCE,UK_MGMT_CONSULTING_BALANCE,UK_RANDD_BALANCE,UK_LEGAL_ACCOUNTING_ADVERTISING_BALANCE) %>%
  right_join(UK_GDP_ANNUAL, by = "date") %>%
  mutate(value = value/GDP) %>%
  drop_na() %>%
  mutate(name = factor(name, levels = rev(c("Management Consulting","Computer, Information, & Telecom Services","Fund Managers, Securities Dealers, & Other Financial Services","Banking & Related Services","Insurance & Pension Services","Legal, Accounting, & Advertising Services","Research & Development"))))
  

UK_NET_EXPORTS_SELECT_BUSINESS_SERVICES <- ggplot(data = filter(UK_SELECTED_SERVICES_BALANCE, date >= as.Date("1997-01-01")), aes(x = date, y = value, fill = name)) + #plotting net exports
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of UK GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07), limits = c(0,0.065), expand = c(0,0)) +
  ggtitle("UK Net Exports of Select Business Services") +
  labs(caption = "Graph created by @JosephPolitano using ONS data", subtitle = "UK Net Exports of Consulting, Computer, Financial, and Other Business Services Have Boomed") +
  theme_apricitas + theme(legend.position = c(.375,.814), legend.spacing.y = unit(0, 'cm'), legend.key.width = unit(0.45, 'cm'), legend.key.height = unit(0.35, "cm"),legend.text = (element_text(size = 13)), legend.title=element_text(size=14), plot.title = element_text(size = 27)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Net Exports, % of GDP",values = c("#FF8E72","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*(today()-as.Date("1997-01-01"))), xmax = as.Date("1997-01-01")-(0.049*(today()-as.Date("1997-01-01"))), ymin = 0-(.3*0.065), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UK_NET_EXPORTS_SELECT_BUSINESS_SERVICES, "UK Net Exports Select Biz Services Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

REAL_AWE <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/a2fd/emp") %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  mutate(value = value/value[1]*100)

REAL_AIC <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/cryn/ukea")  %>%
  subset(., nchar(Title)==7) %>%
  `colnames<-`(c("date","value")) %>%
  transmute(date = as.Date(as.yearqtr(date, "%Y Q%q")), value) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na() %>%
  filter(date>=as.Date("2000-01-01")) %>%
  mutate(value = value/value[1]*100)

REAL_AWE_Graph <- ggplot() +
  geom_line(data = REAL_AWE, aes(x = date, y = value, color = "Real Average Weekly Earnings"), size = 1.25) +
  geom_line(data = REAL_AIC, aes(x = date, y = value, color = "Real Average Adjusted Individual Consumption"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(expand = c(0,0), limits = c(95,140)) +
  ylab("Chained Volume Measures, Index, Q1 2000 = 100") +
  ggtitle("UK Real Wages & Consumption") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "UK Real Wages & Consumption Have Grown Slowly Over the Last Two Decades") +
  theme_apricitas + theme(strip.text = element_text(size = 15, color = "white"), legend.position = c(0.3,0.8)) +
  scale_color_manual(name= "Index, Q1 2000 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 95-(.3*45), ymax = 95) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_AWE_Graph, "Real AWE Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CONSUMER_ENERGY_PRICE_INDICES_BULK <- read.xlsx("https://assets.publishing.service.gov.uk/media/667c0a86aec8650b1009006a/table_211_213.xlsx", 3)

INDUSTRIAL_ENERGY_PRICE_INDICES_BULK <- read.xlsx("https://assets.publishing.service.gov.uk/media/667c2626aec8650b1009008a/table_331.xlsx", 3)

CONSUMER_ENERGY_PRICE_INDICES <- CONSUMER_ENERGY_PRICE_INDICES_BULK %>%
  row_to_names(.,10) %>%
  drop_na() %>%
  select(`Quarter Code`,`Real terms price indices: Electricity [Note 3]`,`Real terms price indices: Gas [Note 3]`) %>%
  setNames(c("date","Electricity","Gas")) %>%
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q %Y"))) %>%
  pivot_longer(-date) %>%
  mutate(category = "Consumer")

INDUSTRIAL_ENERGY_PRICE_INDICES <- INDUSTRIAL_ENERGY_PRICE_INDICES_BULK %>%
  row_to_names(.,8) %>%
  clean_names() %>%
  drop_na() %>%
  select(year,quarter,seasonally_adjusted_electricity_fuel_price_index_numbers_relative_to_the_gdp_deflator_note_2_4, seasonally_adjusted_gas_fuel_price_index_numbers_relative_to_the_gdp_deflator_note_2_4) %>%
  setNames(c("year","quarter","Electricity","Gas")) %>%
  mutate(quarter = rep(seq(1:4), length.out = n())) %>%
  transmute(date = as.Date(as.yearqtr(paste(year, quarter, sep = "-"), format = "%Y-%q")),Electricity,Gas) %>%
  pivot_longer(-date) %>%
  mutate(category = "Industrial")
  
ENERGY_PRICE_INDICES <- rbind(CONSUMER_ENERGY_PRICE_INDICES,INDUSTRIAL_ENERGY_PRICE_INDICES) %>%
  filter(date >= as.Date("2010-01-01")) %>%
  mutate(value = as.numeric(value))

ENERGY_PRICE_INDICES_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data = ENERGY_PRICE_INDICES, aes(x = date, y = value, color = name), size = 1.25) +
  facet_wrap(~ category) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(), expand = c(0,0), limits = c(0,250)) +
  ylab("Index, 2010 = 100") +
  ggtitle("UK Real Energy Price Indexes") +
  labs(caption = "Graph created by @JosephPolitano using ONS data",subtitle = "UK Energy Prices Remain Significantly Elevanted Compared to Pre-COVID Levels") +
  theme_apricitas + theme(strip.text = element_text(size = 15, color = "white"), legend.position = c(0.65,0.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ENERGY_PRICE_INDICES_Graph, "Energy Price Indices Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()